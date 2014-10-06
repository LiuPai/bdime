;;; bdime.el -- Baidu online IME

;; Copyright (C) 2014 Liu Pai
;; Author: Liu Pai
;; Keywords: Chinese

;;; Commentary:
;; Main framework from Emacs Anthy mode
;;; Code:

(require 'json)

(defgroup baidu-input nil "Baidu Online Input Method"
  :group 'leim)

(defvar bdime-mode-map nil)
(or bdime-mode-map
    (let ((map (make-keymap))
          (i 32))
      (define-key map (char-to-string 10) 'bdime-insert)
      (define-key map (char-to-string 17) 'bdime-insert)
      (while (< i 127)
        (define-key map (char-to-string i) 'bdime-insert)
        (setq i (+ 1 i)))
      (setq bdime-mode-map map)))


(defvar bdime-preedit-keymap nil)
(or bdime-preedit-keymap
    (let ((map (make-keymap))
          (i 0))
      ;; 通常の文字に対して
      (while (< i 128)
        (define-key map (char-to-string i) 'bdime-insert)
        (setq i (+ 1 i)))
      ;; 文節の伸縮
      (define-key map [(shift left)] 'bdime-insert)
      (define-key map [(shift right)] 'bdime-insert)
      ;; 文節の移動
      (define-key map [left] 'bdime-insert)
      (define-key map [right] 'bdime-insert)
      (define-key map [backspace] 'bdime-insert)
      (setq bdime-preedit-keymap map)))

;; anthy-agentに送る際にキーをエンコードするためのテーブル
(defvar anthy-keyencode-alist
  '((1 . "(ctrl A)") ;; \C-a
    (2 . "(left)") ;; \C-b
    (4 . "(ctrl D)") ;; \C-d
    (5 . "(ctrl E)") ;; \C-e
    (6 . "(right)") ;; \C-f
    (7 . "(esc)") ;; \C-g
    (8 . "(ctrl H)") ;; \C-h
    (9 . "(shift left)") ;; \C-i
    (10 . "(ctrl J)")
    (11 . "(ctrl K)")
    (13 . "(enter)") ;; \C-m
    (14 . "(space)") ;; \C-n
    (15 . "(shift right)") ;; \C-o
    (16 . "(up)") ;; \C-p
    (32 . "(space)")
    (40 . "(opar)") ;; '('
    (41 . "(cpar)") ;; ')'
    (127 . "(ctrl H)")
    ;; emacs map
    (S-right . "(shift right)")
    (S-left . "(shift left)")
    (right . "(right)")
    (left . "(left)")
    (up . "(up)")
    (backspace . "(ctrl H)")
    ;; xemacs
    ((shift right) . "(shift right)")
    ((shift left) . "(shift left)")
    ((right) . "(right)")
    ((left) . "(left)")
    ((up) . "(up)"))
  "キーのイベントをanthy-agentに送るための対応表")

(defvar bdme-select-candidate-keybind
  '((0 . "a")
    (1 . "o")
    (2 . "e")
    (3 . "u")
    (4 . "i")))

(defun bdime-minibuffer-enter ()
  (setq bdime-saved-mode bdime-mode)
  (setq bdime-mode nil)
  (setq bdime-enable-enum-candidate-p 
        (cons nil bdime-enable-enum-candidate-p))
  (bdime-update-mode))
;;
(defun bdime-minibuffer-exit ()
  (setq bdime-mode bdime-saved-mode)
  (setq bdime-enable-enum-candidate-p 
        (cdr bdime-enable-enum-candidate-p))
  (bdime-update-mode))
;;
(defun bdime-kill-buffer ()
  )
(defun bdime-update-mode-line ()
  t)

(defun bdime-enable-preedit-keymap ()
  "キーマップをプリエディットの存在する時のものに切替える"
  (setcdr
   (assq 'bdime-minor-mode minor-mode-map-alist)
   bdime-preedit-keymap))

(defun bdime-disable-preedit-keymap ()
  "キーマップをプリエディットの存在しない時のものに切替える"
  (setcdr
   (assq 'bdime-minor-mode minor-mode-map-alist)
   bdime-mode-map)
  (bdime-update-mode-line))


;;
(defun bdime-mode-on ()
  (add-hook 'minibuffer-setup-hook 'bdime-minibuffer-enter)
  (add-hook 'minibuffer-exit-hook 'bdime-minibuffer-exit)
  (add-hook 'kill-buffer-hook 'bdime-kill-buffer)
  (setq bdime-minor-mode t)
  (bdime-update-mode-line))
;;
(defun bdime-mode-off ()
  (setq bdime-minor-mode nil)
  (bdime-update-mode-line))
;;
(defun bdime-update-mode ()
  (if (or bdime-mode bdime-leim-active-p)
      (progn
        (bdime-mode-on))
    (bdime-mode-off))
  (run-hooks 'bdime-mode-hook))

(defun bdime-mode (&optional arg)
  "Start Bdime conversion system."
  (interactive "P")
  (setq bdime-mode
        (if (null arg)
            (not bdime-mode)
          (> (prefix-numeric-value arg) 0)))
  (bdime-update-mode))


;; leim の inactivate
;;
(defun bdime-leim-inactivate ()
  (setq bdime-leim-active-p nil)
  (bdime-update-mode))
;;
;; leim の activate
;;
(defun bdime-leim-activate (&optional name)
  (setq inactivate-current-input-method-function 'bdime-leim-inactivate)
  (setq bdime-leim-active-p t)
  (bdime-update-mode)
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'bdime-leim-exit-from-minibuffer)))


(defcustom bdime-candidate-page-content 5
  "Input candidate page content number."
  :type 'integer
  :group 'baidu-input)
(defvar bdime-query-step (* bdime-candidate-page-content 4))

(defvar bdime-request-format "http://olime.baidu.com/py?input=%s&inputtype=py&bg=%d&ed=%d&result=hanzi&resultcoding=unicode&ch_en=0&clientinfo=web&version=1")

(defvar bdime-default-enable-enum-candidate-p t)

;; From skk-macs.el From viper-util.el.  Welcome!
(defmacro deflocalvar (var default-value &optional documentation)
  (` (progn
       (defvar (, var) (, default-value)
         (, (format "%s\n\(buffer local\)" documentation)))
       (make-variable-buffer-local '(, var))
       )))

(defun bdime-handle-normal-key (chenc)
  (setq bdime-input (concat bdime-input chenc))
  (bdime-update-preedit)
  )


(defun bdime-insert (&optional arg)
  (interactive "*p")
  (let* ((ch last-command-char)
         (chenc (bdime-encode-key ch)))
    (bdime-handle-key ch chenc)))

(defun bdime-encode-key (ch)
  (let ((c (assoc ch anthy-keyencode-alist)))
    (if c
        (cdr c)
      (if (and
           (integerp ch)
           (> ch 32))
          (char-to-string ch)
        nil))))

(defun bdime-handle-key (ch chenc)
  (cond
   ((and (or bdime-enum-candidate-p bdime-enum-rcandidate-p)
         (integerp ch)
         (assq (car (rassoc (char-to-string ch)
                            bdime-select-candidate-keybind))
               bdime-enum-candidate-list))
    (bdime-insert-select-candidate ch))
   ;; アルファベットモードの場合は直接入力
   ((and  (string-equal bdime-preedit ""))
    (self-insert-command 1)
    (setq bdime-input (concat bdime-input (char-to-string ch)))
    (bdime-update-preedit))
   ;; プリエディットがなくてスペースが押された
   ((and
     (string-equal bdime-preedit "")
     (= ch 32)
     (not
      (string-equal bdime-current-rkmap "alphabet")))
    (progn
      (insert-and-inherit bdime-wide-space)
      (bdime-do-auto-fill)))
   ((or bdime-enum-candidate-p bdime-enum-rcandidate-p)
    (bdime-handle-enum-candidate-mode chenc))
   ;; 普通の入力
   (t
    (bdime-handle-normal-key chenc))))


;; buffer local variables
;; Mode control
(deflocalvar bdime-minor-mode nil)
(deflocalvar bdime-mode nil)
(deflocalvar bdime-leim-active-p nil)
(deflocalvar bdime-saved-mode nil)

;; Query
(deflocalvar bdime-input "")
(deflocalvar bdime-query-result "")
(deflocalvar bdime-query-start 0)
(deflocalvar bdime-preedit "")
(deflocalvar bdime-preedit-start 0)
(deflocalvar bdime-preedit-overlays '())

;; enum candidate
(deflocalvar bdime-enum-candidate-p nil)
(deflocalvar bdime-enum-rcandidate-p nil)
(deflocalvar bdime-candidate-minibuffer "")
(deflocalvar bdime-enum-candidate-list '())
(deflocalvar bdime-enable-enum-candidate-p 
  (cons bdime-default-enable-enum-candidate-p nil))
(deflocalvar bdime-current-candidate-index 0)
(deflocalvar bdime-current-candidate-layout-begin-index 0)
(deflocalvar bdime-current-candidate-layout-end-index 0)

;; undo
(deflocalvar anthy-buffer-undo-list-saved nil)

(if (not
     (assq 'bdime-minor-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons
           (cons 'bdime-minor-mode '(bdime-mode-line-string))
           minor-mode-alist)))
;; minor-mode-map-alist
(if (not
     (assq 'bdime-minor-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
          (cons
           (cons 'bdime-minor-mode bdime-mode-map)
           minor-mode-map-alist)))


(defun bdime-query (input bg ed)
  "Query pinyin INPUT result array from BG to ED."
  (with-current-buffer (url-retrieve-synchronously
                        (format bdime-request-format input bg ed))
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (re-search-forward "^$")
    (let* ((json-object-type 'plist)
           (result (json-read))
           (status (plist-get result :status))
           (errno (plist-get result :errno))
           (errmsg (plist-get result :errmsg)))
      (if (and (string= status "T")
               (string= errno "0"))
          (aref (plist-get result :result) 0)
        (message errmsg)))))

(defun bdime-preedit-get-recommend ()
  "Query and parse input pinyin result to set local variables."
  (let* ((result (bdime-query bdime-input 0 1))
         (recommend (aref (aref result 0) 0))
         (py-length (aref (aref result 0) 1))
	 (real-replaced-py-length 0)
	 (left-py ""))
    (if (< py-length (length bdime-input))
	(while (> py-length 0)
	  (let ((current-char (aref bdime-input real-replaced-py-length)))
	    (when (or (and (> current-char ?a)
			   (< current-char ?z))
		      (and (> current-char ?A)
			   (< current-char ?Z)))
	      (setq py-length (1- py-length)))
	    (setq real-replaced-py-length (1+ real-replaced-py-length)))))
    (setq left-py (substring bdime-input 0 real-replaced-py-length))
    (setq bdime-preedit (concat recommend "|" left-py))
    (cons recommend left-py)))

(defun bdime-update-preedit ()
  (let ((start (point))
	end
	ol
	)
    ;; erase old preedit
    (bdime-erase-preedit)
    (let ((recommends (bdime-preedit-get-recommend)))
      ;; insert new preedit
      (setq bdime-preedit-start (point))
      (insert-and-inherit bdime-preedit))))
(defun bdime-erase-preedit ()
  (if (> (string-width bdime-preedit) 0)
      (let* ((str bdime-preedit)
	     (len (length str))
	     (start bdime-preedit-start))
	(delete-region start (+ start len))
	(goto-char start)))
  (setq bdime-preedit "")
  (mapcar 'delete-overlay bdime-preedit-overlays)
  (setq bdime-preedit-overlays nil))
(defun bdime-clean ()
  (setq bdime-preedit "")
  (setq bdime-preedit-start 0)
  (setq bdime-input ""))

(defun bdime-preedit-insert-recommend (recommend recommend-py-length)
  "Update preedit with RECOMMEND string replace RECOMMEND-PY-LENGTH input char."
  (let ((py-length (strnig-to-int recommend-py-length))
	(real-replaced-py-length 0)
	left-py)
    (while (> py-length 0)
      (let ((current-char (aref bdime-input real-replaced-py-length)))
	(when (or (and (> current-char ?a)
		       (< current-char ?z))
		  (and (> current-char ?A)
		       (< current-char ?Z)))
	  (setq py-length (1- py-length)))
	(setq real-replaced-py-length (1+ real-replaced-py-length))))
    (setq left-py (substring 0 real-replaced-py-length))
    (setq bdime-preedit (concat recommend "|" left-py))
    (setq bdime-preedit-start (point))
    (insert-and-inherit bdime-preedit)))

(defun bdime-enum-candidate-list-update ()
  ""
  )

(defun bdime-update-input ()
  ""
  )
;;; newbdime.el ends here





