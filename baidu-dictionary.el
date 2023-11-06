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
         (req-data (mapconcat #'identity (list (concat "q=" (url-hexify-string word))
                                               (concat "from=" from)
                                               (concat "to=" to)
                                               (concat "appid=" app-key)
                                               (concat "salt=" salt)
                                               (concat "sign=" sign))
                              "&" ))
         (url-request-data req-data)
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (message req-data)
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
  (let* ((_errorCode   (assoc-default 'error_code   json)) ; number
         (_errorMsg    (assoc-default 'error_msg   json))
         (_transResult  (assoc-default 'trans_result         json)) ; array
         (query (mapconcat
                 (lambda (k-v)
                   (format "-%s"
                           (assoc-default 'src k-v)))
                 _transResult "\n"))
         (dst (mapconcat
               (lambda (k-v)
                 (format "-%s"
                         (assoc-default 'dst k-v)))
               _transResult "\n")))
    (format "%s \n\n* Basic Explains\n%s\n"
            query dst)))

(defun -pos-tip (string)
  "Show STRING using pos-tip-show."
  (pos-tip-show string nil nil nil 0)
  (unwind-protect
      (push (read-event) unread-command-events)
    (pos-tip-hide)))

(defvar current-buffer-word nil)

(defun -posframe-tip (string)
  "Show STRING using posframe-show."
  (unless (and (require 'posframe nil t) (posframe-workable-p))
    (error "Posframe not workable"))

  (let ((word (-region-or-word)))
    (if word
        (progn
          (with-current-buffer (get-buffer-create buffer-name)
            (let ((inhibit-read-only t))
              (erase-buffer)
                                        ;(mode)
              (insert string)
              (goto-char (point-min))
              (set (make-local-variable 'baidu-dictionary-current-buffer-word) word)))
          (posframe-show buffer-name
                         :left-fringe 8
                         :right-fringe 8
                         :internal-border-color (face-foreground 'default)
                         :internal-border-width 1)
          (unwind-protect
              (push (read-event) unread-command-events)
            (progn
              (posframe-delete buffer-name)
              (other-frame 0))))
      (message "Nothing to look up"))))

(defun -search-and-show-in-buffer-subr (word content)
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
                                        ;(mode)
      (insert content)
      (goto-char (point-min))
      (set (make-local-variable 'baidu-dictionary-current-buffer-word) word))
    (unless (get-buffer-window (current-buffer))
      (switch-to-buffer-other-window buffer-name))))


(defun -search-and-show-in-buffer (word &optional async)
  "Search WORD and show result in `baidu-dictionary-buffer-name' buffer."
  (unless word
    (user-error "Nothing to look up"))
  (if async
      (-request word (lambda (_status)
                       (-search-and-show-in-buffer-subr
                        word
                        (-format-result (-parse-response)))))
    (-search-and-show-in-buffer-subr word (-format-result (-request word)))))


:autoload
(defun search-at-point ()
  "Search word at point and display result with buffer."
  (interactive)
  (let ((word (-region-or-word)))
    (-search-and-show-in-buffer word)))


(defun search-at-point- (func)
  "Search word at point and display result with given FUNC."
  (let ((word (-region-or-word)))
    (if word
        (funcall func (-format-result (-request word)))
      (message "Nothing to look up"))))

:autoload
(defun search-at-point+ ()
  "Search word at point and display result with popup-tip."
  (interactive)
  (search-at-point- #'popup-tip))

:autoload
(defun search-at-point-posframe ()
  "Search word at point and display result with posframe."
  (interactive)
  (search-at-point- #'-posframe-tip))

:autoload
(defun search-at-point-tooltip ()
  "Search word at point and display result with pos-tip."
  (interactive)
  (search-at-point- #'-pos-tip))

:autoload
(defun search-from-input ()
  "Search word from input and display result with buffer."
  (interactive)
  (let ((word (-prompt-input)))
    (-search-and-show-in-buffer word)))

:autoload
(defun search-and-replace ()
  "Search word at point and replace this word with popup menu."
  (interactive)
  (if (use-region-p)
      (let ((region-beginning (region-beginning)) (region-end (region-end))
            (selected (popup-menu* (mapcar #'-strip-explain
                                           (append (-explains
                                                    (-request
                                                     (-region-or-word)))
                                                   nil)))))
        (when selected
          (insert selected)
          (kill-region region-beginning region-end)))
    ;; No active region
    (let* ((bounds (bounds-of-thing-at-point (if use-chinese-word-segmentation
                                                 'chinese-or-other-word
                                               'word)))
           (beginning-of-word (car bounds))
           (end-of-word (cdr bounds)))
      (when bounds
        (let ((selected
               (popup-menu* (mapcar
                             #'-strip-explain
                             (append (-explains
                                      (-request
                                       (thing-at-point
                                        (if use-chinese-word-segmentation
                                            'chinese-or-other-word
                                          'word))))
                                     nil)))))
          (when selected
            (insert selected)
            (kill-region beginning-of-word end-of-word)))))))

(defvar history nil)

:autoload
(defun search (query)
  "Show the explanation of QUERY from Baidu dictionary."
  (interactive
   (let* ((string (or (if (use-region-p)
                          (buffer-substring
                           (region-beginning) (region-end))
                        (thing-at-point 'word))
                      (read-string "Search Baidu Dictionary: " nil 'history))))
     (list string)))
  (-search-and-show-in-buffer query))

:autoload
  (defun search-async (query)
    "Show the explanation of QUERY from Baidu dictionary asynchronously."
    (interactive
     (let* ((string (or (if (use-region-p)
                            (buffer-substring
                             (region-beginning) (region-end))
                          (thing-at-point 'word))
                        (read-string "Search Baidu Dictionary: " nil 'history))))
       (list string)))
    (-search-and-show-in-buffer query 'async))

)

(provide 'baidu-dictionary)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; baidu-dictionary.el ends here
