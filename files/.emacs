;============================
; Emacs customization script
;============================

(normal-erase-is-backspace-mode t)
(setq inhibit-startup-message t)      ; don't show the GNU splash screen
(scroll-bar-mode -1)                  ; no scroll bar
(menu-bar-mode -1)                    ; no menu bar
(tool-bar-mode -1)                    ; no tool bar
(setq frame-title-format "%b")        ; titlebar shows buffer's name
(global-font-lock-mode t)             ; syntax highlighting
(setq font-lock-maximum-decoration t) ; maximum decoration for all modes
(show-paren-mode t)                   ; show opposing paren while hovering
(setq scroll-step 1)                  ; smooth scrolling
(delete-selection-mode t)             ; typing removes highlighted text
(line-number-mode t)                  ; display line number in modeline
(column-number-mode t)                ; display column number in modeline
(auto-compression-mode t)             ; open compressed files
(mouse-wheel-mode t)                  ; enable mouse wheel
(fset 'yes-or-no-p 'y-or-n-p)         ; y or n will do
(setq default-major-mode 'text-mode)  ; change default major mode to text
(setq ring-bell-function 'ignore)     ; turn the alarm totally off
(setq-default indent-tabs-mode nil)   ; spaces instead of tabs
(setq make-backup-files nil)          ; no backupfile
(setq delete-auto-save-files t)       ; delete unnecessary autosave files
(setq delete-old-versions t)          ; delete oldversion file
(setq next-line-add-newlines nil)     ; prevents new line after eof
(setq-default show-trailing-whitespace t)
(setq vc-follow-symlinks t)

; C mode
(c-set-offset 'substatement-open 0)   ; change '{' indentation
(c-set-offset 'case-label '+)         ; make each case line indent from switch
(setq c-font-lock-extra-types
      (append
       '("t_\\sw+")
       '("s_\\sw+")
       '("u_\\sw+")
       '("f_\\sw+")
       '("e_\\sw+")
       c-font-lock-extra-types))

; key bindings
(global-set-key [f1] 'replace-regexp) ; FIXME: change to hide/show trailing
(global-set-key [f2] 'undo)           ; FIXME: change to hide/show tabs
(global-set-key [f3] 'search-selection)
(global-set-key [f4] 'kill-this-buffer)
(global-set-key [f6] 'yic-next-buffer)
(global-set-key [f5] 'yic-prev-buffer)
(global-set-key [f8] 'previous-error)
(global-set-key [f9] 'next-error)
(global-set-key [f10] 'recompile)
(global-set-key [f11] 'compile)
(global-set-key [f12] 'replace-string)
(global-set-key [C-f12] 'replace-regex)

; small compilation window
(setq compilation-window-height 20)
(setq compilation-scroll-output t)

(global-set-key [C-home] 'beginning-of-buffer)          ; go to the beginning of buffer
(global-set-key [C-end] 'end-of-buffer)                 ; go to the end of buffer
(global-set-key [(meta g)] 'goto-line)                  ; goto line #
(global-set-key [M-left] 'windmove-left)                ; move to left windnow
(global-set-key [M-right] 'windmove-right)              ; move to right window
(global-set-key [M-up] 'windmove-up)                    ; move to upper window
(global-set-key [M-down] 'windmove-down)
(global-set-key [(control tab)] 'other-window)          ; Ctrl-Tab = Next buffer
(global-set-key [C-S-iso-lefttab]
                '(lambda () (interactive)
                   (other-window -1)))                  ; Ctrl-Shift-Tab = Previous buffer

; i-search bindings

(global-set-key [(control f)] 'isearch-forward-regexp)  ; search regexp
(define-key
  isearch-mode-map
  [(control n)]
  'isearch-repeat-forward)                              ; next occurence
(define-key
  isearch-mode-map
  [(control p)]
  'isearch-repeat-backward)                             ; previous occurence
(define-key
  isearch-mode-map
  [(control z)]
  'isearch-abort)                                       ; revert ot last valid search
(define-key
  isearch-mode-map
  [(control f)]
  'isearch-exit)                                        ; abort
(define-key
  isearch-mode-map
  [S-insert]
  'isearch-yank-kill)                                   ; paste
(define-key
  isearch-mode-map
  [(control r)]
  'isearch-toggle-regexp)                               ; toggle regexp
(define-key
  isearch-mode-map
  [(control l)]
  'isearch-yank-line)                                   ; yank line from buffer
(define-key
  isearch-mode-map
  [(control w)]
  'isearch-yank-word)                                   ; yank word from buffer

(defun yic-ignore (str)
  (or
   (string-match "\\*Buffer List\\*" str)
   (string-match "\\*scratch\\*" str)
   (string-match "^TAGS" str)
   (string-match "^\\*Messages\\*$" str)
   (string-match "^\\*Completions\\*$" str)
   (string-match "^ " str)
   (memq str
         (mapcar
          (lambda (x)
            (buffer-name
             (window-buffer
              (frame-selected-window x))))
          (visible-frame-list)))
   ))

(defun yic-next (ls)
  (let* ((ptr ls)
	 bf bn go
	 )
    (while (and ptr (null go))
      (setq bf (car ptr)  bn (buffer-name bf))
      (if (null (yic-ignore bn))
	  (setq go bf)
	(setq ptr (cdr ptr))
	)
      )
    (if go
	(switch-to-buffer go))))

(defun yic-prev-buffer ()
  (interactive)
  (yic-next (reverse (buffer-list))))

(defun yic-next-buffer ()
  (interactive)
  (bury-buffer (current-buffer))
  (yic-next (buffer-list)))

; Our own modes repository
(setq load-path (cons "~/.emacs.d/" load-path))

; Flex
(autoload 'flex-mode "flex-mode" "flex editing mode." t)
(add-to-list 'auto-mode-alist '("\\.l$" . flex-mode))
(add-to-list 'auto-mode-alist '("\\.ll$" . flex-mode))

; Bison
(autoload 'bison-mode "bison-mode" "bison editing mode." t)
(add-to-list 'auto-mode-alist '("\\.y$" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.yy$" . bison-mode))

; XML and traces
(setq auto-mode-alist
      (cons '("\\.trace$" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.xml" . sgml-mode) auto-mode-alist))

; C/C++
(defun my-cpp-highlight ()
(setq cpp-known-face '(background-color . "dim gray"))
(setq cpp-unknown-face 'default)
(setq cpp-face-type 'dark)
(setq cpp-known-writable 't)
(setq cpp-unknown-writable 't)
(setq cpp-edit-list
'((#("0" 0 1
(c-in-sws t fontified t))
(background-color . "dim gray")
nil both nil)
(#("1" 0 1
(c-in-sws t fontified t))
nil
(background-color . "dim gray")
both nil)))
(cpp-highlight-buffer t))

(defun my-c-mode-common-hook ()
(my-cpp-highlight)
)

(setq c-basic-offset 4)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-c-mode-recenter ()
"Recenter buffer and refresh highlighting."
(interactive)
(recenter)
(cpp-highlight-buffer t))

(defun my-c-initialization-hook ()
(define-key c-mode-base-map "\C-l" 'my-c-mode-recenter))

(add-hook 'c-initialization-hook 'my-c-initialization-hook)

;; .h -> c++-mode
(push '("\\.h\\'" . c++-mode) auto-mode-alist)
;; .mako -> html-mode
(add-to-list 'auto-mode-alist '("\\.mako$" . html-mode))
;; .less -> css-mode
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

;; insert-BOM
(defun insert-BOM()
  (interactive)
  (goto-char (point-min))
  (ucs-insert (string-to-number "FEFF" 16))
)

;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; rst-mode
(autoload 'rst-mode "rst-mode" "mode for editing reStructuredText documents" t)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))
(put 'upcase-region 'disabled nil)

;; nxHTML
(load "~/.emacs.d/nxhtml/autostart.el")

(custom-set-variables
 '(css-color-global-mode t)
 '(nxhtml-default-encoding (quote utf-8))
 '(tabkey2-mode t))

(custom-set-faces
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#202020"))))
 '(linum ((t (:inherit org-agenda-dimmed-todo-face)))))

(setq mumamo-background-colors nil)

; linum
(global-linum-mode t)
(global-hl-line-mode t)

;; Open files and goto lines like we see from g++ etc. i.e. file:line#
(defadvice find-file (around find-file-line-number
                             (filename &optional wildcards)
                             activate)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename)))
      ad-do-it
      (when line-number
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-number))))))


(defun search-selection (beg end)
  "search for selected text"
  (interactive "r")
  (let (
        (selection (buffer-substring-no-properties beg end))
        )
    (deactivate-mark)
    (isearch-mode t nil nil nil)
    (isearch-yank-string selection)
    )
  )

; elpy python
(package-initialize)
(elpy-enable)
