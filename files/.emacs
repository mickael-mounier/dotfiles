(setq warning-minimum-level :error) ; ewww!

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
; (add-hook 'c-mode-hook 'rm-trailing-spaces-always)
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
(global-set-key [f1] 'replace-regexp)
(global-set-key [f2] 'undo)
(global-set-key [f3] 'eshell)
(global-set-key [f4] 'kill-this-buffer)
(global-set-key [f6] 'yic-next-buffer)
(global-set-key [f5] 'yic-prev-buffer)
(global-set-key [f8] 'previous-error)
(global-set-key [f9] 'next-error)
(global-set-key [f10] 'recompile)
(global-set-key [f11] 'compile)
(global-set-key [f12] 'replace-string)

; small compilation window
(setq compilation-window-height 15)
(setq compilation-scroll-output t)

(if (display-graphic-p)
    (global-set-key [(control z)] 'undo)                ; undo only in graphic mode
)
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

;; Count nb lines
(defun rec-lines (count)
  (forward-line count)
  (when (not (= (char-after) (if (= count 1) ?} ?{)))
    (rec-lines count)
    )
  (point)
  )
(defun lines ()
  "Count number of lines of a C function"
  (interactive)
  (let ((old-pt (point))
        (pt1 (rec-lines 1))
        (pt2 (rec-lines -1)))
    (goto-char old-pt)
    (message (format "Lines : %d" (1- (count-lines pt1 pt2))))
    )
  )
(global-set-key "\C-x\C-l" 'lines)

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


(add-hook 'c-mode-hook 'fp-c-mode-routine)
(defun fp-c-mode-routine ()
  (local-set-key "\M-q" 'rebox-comment))
(autoload 'rebox-comment "rebox" nil t)
(autoload 'rebox-region "rebox" nil t)

; Our own modes repository
;; load files
;(setq load-path (nconc '( "~/.emacs.d/") load-path))
(setq load-path (cons "~/.emacs.d/" load-path))

; Ruby!!
;(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
;(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
;(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))


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


;;;;;;;;;;;;
;; COLORS ;;
;(add-to-list 'load-path "~/.emacs.d/color-theme")
;(autoload 'color-theme "color-theme" "Color themes." t)
;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-deep-blue)

(setq c-basic-offset 4)

;(require 'completion-ui)

;; ------------ C Mode
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

;; lol-mode
(autoload 'lol-mode "lol-mode" "LOL editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lol$" . lol-mode))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(css-color-global-mode t)
 '(nxhtml-default-encoding (quote utf-8))
 '(tabkey2-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#202020"))))
 '(linum ((t (:inherit org-agenda-dimmed-todo-face)))))

(setq mumamo-background-colors nil)

(global-linum-mode t)
(global-hl-line-mode t)

(defun find-file-at-point-with-line()
  "if file has an attached line num goto that line, ie boom.rb:12"
  (interactive)
  (setq line-num 0)
  (save-excursion
    (search-forward-regexp "[^ ]:" (point-max) t)
    (if (looking-at "[0-9]+")
        (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
  (find-file-at-point)
  (if (not (equal line-num 0))
      (goto-line line-num)))
