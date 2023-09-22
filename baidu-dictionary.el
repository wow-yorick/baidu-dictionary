;;; baidu-dictionary.el --- A lightweight baidu dictionary for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 yorick

;; Author: yorick <wowyorick@126.com>
;; Version: 1.0
;; Package-Requires: ((popup "0.5.0") (pos-tip "0.4.6") (chinese-word-at-point "0.2") (names "0.5") (emacs "24"))
;; Keywords: dictionary, baidu-dictionary
;; URL: https://github.com/wow-yorick/baidu-dictionary

;;; Commentary:

;; baidu-dictionary.el is a lightweight dictionary use baidu

;; Full documentation is available as an Info manual.

;;; Commentary:
;;
;; A simple Baidu Dictionary interface for Emacs
;;
;; Below are commands you can use:
;; `baidu-dictionary-search-at-point'
;; Search word at point and display result with buffer
;; `baidu-dictionary-search-at-point+'
;; Search word at point and display result with popup-tip
;; `baidu-dictionary-search-from-input'
;; Search word from input and display result with buffer
;; `baidu-dictionary-search-and-replace'
;; Search word at point and display result with popup-menu, replace word with
;; selected translation.
;; `baidu-dictionary-play-voice-at-point'
;; Play voice of word at point (by [[https://github.com/snyh][@snyh]])
;; `baidu-dictionary-play-voice-from-input'
;; Play voice of word from input (by [[https://github.com/snyh][@snyh]])
;; `baidu-dictionary-search-at-point-tooltip'
;; Search word at point and display result with pos-tip

;;; Code:

(require 'json)
(require 'url)
(require 'org)
(require 'chinese-word-at-point)
(require 'popup)
(require 'pos-tip)
(eval-when-compile (require 'names))

(declare-function pdf-view-active-region-text "pdf-view" ())
(declare-function pdf-view-active-region-p "pdf-view" ())
(declare-function posframe-delete "posframe")
(defvar url-http-response-status)

(defgroup baidu-dictionary nil
  "Baidu dictionary interface for Emacs."
  :prefix "baidu-dictionary-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/wow-yorick/baidu-dictionary"))

;;;###autoload
(define-namespace baidu-dictionary-

(defconst api-url
  "https://fanyi-api.baidu.com/api/trans/vip/translate"
  "Baidu dictionary API template, URL `https://fanyi.baidu.com/`.")


(defcustom secret-key (getenv "BAIDU_SECRET_KEY")
  "Baidu dictionary Secret Key. You can get it from https://fanyi-api.baidu.com/manage/developer."
  :type 'string)

(defcustom app-key (getenv "BAIDU_APP_KEY")
  "Baidu dictionary App Key. You can get it from https://fanyi-api.baidu.com/manage/developer."
  :type 'string)


(defcustom from "auto"
  "Source language. see https://fanyi-api.baidu.com/doc/21"
  :type 'string)

(defcustom to "zh"
  "dest language. see https://fanyi-api.baidu.com/doc/21"
  :type 'string)

(defcustom buffer-name "*Baidu Dictionary*"
  "Result Buffer name."
  :type 'string)

(defcustom search-history-file nil
  "If non-nil, the file be used for saving searching history."
  :type '(choice (const :tag "Don't save history" nil)
                 (string :tag "File path")))

(defcustom use-chinese-word-segmentation nil
  "If Non-nil, support Chinese word segmentation(中文分词).

See URL `https://github.com/xuchunyang/chinese-word-at-point.el' for more info."
  :type 'boolean)

(defface posframe-tip-face
  '((t (:inherit tooltip)))
  "Face for posframe tip."
  :group 'baidu-dictionary)

(defun get-salt ()
  (number-to-string (random 1000)))


(defun get-input (word)
  (let ((len (length word)))
    (if (> len 20)
        (concat (substring word 0 10)
                (number-to-string len)
                (substring word -10))
      word)))

(defun get-sign (salt word)
  (let* ((input (get-input word))
         (signstr (concat app-key input salt  secret-key)))
    (md5 signstr)))


(defun -parse-response ()
  "Parse response as JSON."
  (set-buffer-multibyte t)
  (goto-char (point-min))
  (when (/= 200 url-http-response-status)
    (error "Problem connecting to the server"))
  (re-search-forward "^$" nil 'move)
  (prog1 (json-read)
    (kill-buffer (current-buffer))))


(defun -request (word &optional callback)
  "Request WORD, return JSON as an alist if successes."
  (when (and search-history-file (file-writable-p search-history-file))
    ;; Save searching history
    (append-to-file (concat word "\n") nil search-history-file))
  (let* ((salt (get-salt))
         (sign (get-sign salt word))
         (url-request-data (mapconcat #'identity (list (concat "q=" (url-hexify-string word))
                                                       (concat "from=" from)
                                                       (concat "to=" to)
                                                       (concat "appid=" app-key)
                                                       (concat "salt=" salt)
                                                       (concat "sign=" sign))
                                      "&" ))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (if callback
        (url-retrieve api-url callback)
      (with-current-buffer (url-retrieve-synchronously api-url)
        (-parse-response)))))


(defun -explains (json)
  "Return explains as a vector extracted from JSON."
  (cdr (assoc 'trans_result json)))

(defun -region-or-word ()
  "Return word in region or word at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point (if use-chinese-word-segmentation
                        'chinese-or-other-word
                      'word)
                    t)))

(defun -prompt-input ()
  "Prompt input object for translate."
  (let ((current-word (-region-or-word)))
    (read-string (format "Word (%s): "
                         (or current-word ""))
                 nil nil
                 current-word)))

(defun -format-result (json)
  "Format result in JSON."
  (let* ((query        (assoc-default 'query       json)) ; string
         (translation  (assoc-default 'translation json)) ; array
         (_errorCode    (assoc-default 'errorCode   json)) ; number
         (web          (assoc-default 'web         json)) ; array
         (basic        (assoc-default 'basic       json)) ; alist
         ;; construct data for display
         (phonetic (assoc-default 'phonetic basic))
         (translation-str (mapconcat
                           (lambda (trans) (concat "- " trans))
                           translation "\n"))
         (basic-explains-str (mapconcat
                              (lambda (explain) (concat "- " explain))
                              (assoc-default 'explains basic) "\n"))
         (web-str (mapconcat
                   (lambda (k-v)
                     (format "- %s :: %s"
                             (assoc-default 'key k-v)
                             (mapconcat 'identity (assoc-default 'value k-v) "; ")))
                   web "\n")))
    (if basic
        (format "%s [%s]\n\n* Basic Explains\n%s\n\n* Web References\n%s\n"
                query phonetic basic-explains-str web-str)
      (format "%s\n\n* Translation\n%s\n"
              query translation-str))))

)

(provide 'baidu-dictionary)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; baidu-dictionary.el ends here
