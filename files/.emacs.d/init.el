;; Use emacs as client/server
(server-start)

(set-foreground-color "#ffffff")
(set-background-color "#000000")
(add-to-list 'default-frame-alist '(foreground-color . "#ffffff"))
(add-to-list 'default-frame-alist '(background-color . "#000000"))

;; Move and resize initial window
;; TODO: adapt to screen resolution!
(setq initial-frame-alist
      '((width . 200) (height . 65)
        (top . 150) (left . 400)))
(setq default-frame-alist
      '((width . 200) (height . 65)
        (top . 150) (left . 400)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

(require 'package)

;; list of the packages
(setq package-list '(
                     airline-themes
                     elpy
                     fill-column-indicator
                     git-gutter
                     helm
                     helm-descbinds
                     helm-ls-git
                     helm-projectile
                     magit
                     magit-gh-pulls
                     move-text
                     multiple-cursors
                     neotree
                     org
                     persp-projectile
                     perspective
                     powerline
                     restclient
                     ))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("elpy" . "https://jorgenschaefer.github.io/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts

(if (eq system-type 'windows-nt)
    (cond
     ((find-font (font-spec :family "Consolas"))
      (custom-set-faces
       '(default ((t (:family "Consolas" :foundry "outline" :slant normal
                              :weight bold :height 98 :width normal))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elpy

(elpy-enable)

;; don't override my bindings!
(eval-after-load "elpy"
  '(cl-dolist (key '("M-<up>" "M-<down>" "M-<left>" "M-<right>" "C-<up>"
                     "C-<down>" "C-<left>" "C-<right>"))
     (define-key elpy-mode-map (kbd key) nil)))

(set-process-coding-system (get-buffer-process (elpy-rpc--get-rpc-buffer))
                           'utf-8-dos 'utf-8-dos)

(set-face-background 'highlight-indentation-face "#0b0b0b")
(set-face-background 'highlight-indentation-current-column-face "#0b0b0b")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move-text

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [C-S-up] 'move-text-up)
(global-set-key [C-S-down] 'move-text-down)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile

(projectile-global-mode)

(if (eq system-type 'windows-nt)
    (setq projectile-indexing-method 'alien))

(setq projectile-enable-caching t)

(require 'helm-projectile)

(setq helm-projectile-fuzzy-match t)
(helm-projectile-on)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline

;; using a forked version
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)

(custom-set-faces
 '(mode-line ((t (:foreground "#000000" :background "SpringGreen1" :box nil))))
 '(mode-line-inactive ((t (:foreground "#ffffff" :background "#333333" :box nil)))))
(setq powerline-color1 "MediumPurple4")
(setq powerline-color2 "#333333")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perspective

(persp-mode)
(require 'persp-projectile)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fill-column-indicator

;; FIXME: this package is not working properly

;; (require 'fill-column-indicator)

;; (setq fci-rule-width 1)
;; (setq fci-rule-color "darkblue")
;; (global-fci-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Neotree

(require 'neotree)

(setq projectile-switch-project-action 'neotree-projectile-action)

(global-set-key [f8] 'neotree-toggle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm

(require 'helm)
(require 'helm-config)
(require 'helm-projectile)
(require 'helm-descbinds)

(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-TAB") 'helm-buffers-list)  ; is helm-mini better?
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-f") 'helm-occur)
(global-set-key (kbd "C-p") 'helm-locate)
(global-set-key (kbd "C-S-f") 'helm-do-grep-ag)
(global-set-key (kbd "M-g g") 'helm-grep-do-git-grep)

;; helm-semantic-or-imenu?

(setq helm-net-prefer-curl t)
(setq helm-buffer-skip-remote-checking t)
(setq helm-boring-file-regexp-list
      '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$"))

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(when (executable-find "ack")
  (setq helm-grep-default-command
        "ack -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command
        "ack -H --no-group --no-color %e %p %f"))

(helm-mode 1)
(helm-descbinds-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit

(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(global-set-key (kbd "C-x g") 'magit-status)

(global-git-gutter-mode t)
(git-gutter:linum-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mics

;; utf-8: disabled for windows (ë encoding issue)
;; (prefer-coding-system 'utf-8)
;; (when (display-graphic-p)
;;   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(global-set-key [M-insert]
                '(lambda() (interactive) (load-file "~/.emacs.d/init.el")))

(global-set-key (kbd "S-C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-M-<down>") 'shrink-window)
(global-set-key (kbd "S-C-M-<up>") 'enlarge-window)

;; linum
(global-linum-mode t)
(global-hl-line-mode t)
(set-face-foreground 'linum "#666666")
(set-face-background 'linum "#000000")
(set-face-background 'hl-line "#333333")
(set-face-foreground 'highlight nil)

;; what-face
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; show-paren
(show-paren-mode t)                       ; show opposing paren while hovering
;; (setq show-paren-style 'expression)       ; highlight the whole paren content
;; (set-face-background 'show-paren-match (face-background 'hl-line))

(normal-erase-is-backspace-mode t)        ; fix delete and backspace keys
(setq inhibit-startup-message t)          ; don't show the GNU splash screen
(scroll-bar-mode -1)                      ; no scroll bar
(menu-bar-mode -1)                        ; no menu bar
(tool-bar-mode -1)                        ; no tool bar
(setq frame-title-format "%b")            ; titlebar shows buffer's name
(global-font-lock-mode t)                 ; syntax highlighting
(setq font-lock-maximum-decoration t)     ; maximum decoration for all modes
(setq scroll-step 1)                      ; smooth scrolling
(delete-selection-mode t)                 ; typing removes highlighted text
(line-number-mode t)                      ; display line number in modeline
(column-number-mode t)                    ; display column number in modeline
(auto-compression-mode t)                 ; open compressed files
(mouse-wheel-mode t)                      ; enable mouse wheel
(fset 'yes-or-no-p 'y-or-n-p)             ; y or n will do
(setq default-major-mode 'text-mode)      ; change default major mode to text
(setq ring-bell-function 'ignore)         ; turn the alarm totally off
(setq-default indent-tabs-mode nil)       ; spaces instead of tabs
(setq make-backup-files nil)              ; no backup files
(setq delete-auto-save-files t)           ; delete unnecessary auto save files
(setq delete-old-versions t)              ; delete oldversion file
(setq next-line-add-newlines nil)         ; prevents new line after eof
(setq-default show-trailing-whitespace t) ; show trailing whitespaces
(setq vc-follow-symlinks t)               ; handle symlinks properly
(setq sentence-end-double-space nil)      ; no double spaces
(setq use-file-dialog nil)

(global-set-key [f1] 'highlight-symbol-at-point)
(global-set-key [C-f1] 'unhighlight-regexp)
(global-set-key [f5] 'yic-prev-buffer)
(global-set-key [f6] 'yic-next-buffer)
(global-set-key [f9] 'sort-lines)
(global-set-key [f12] 'replace-string)
(global-set-key [C-f12] 'replace-regex)

(global-set-key [C-home] 'beginning-of-buffer) ; go to the beginning of buffer
(global-set-key [C-end] 'end-of-buffer)        ; go to the end of buffer
(global-set-key [M-g] 'goto-line)              ; goto line
(global-set-key [M-left] 'windmove-left)       ; move to left windnow
(global-set-key [M-right] 'windmove-right)     ; move to right window
(global-set-key [M-up] 'windmove-up)           ; move to upper window
(global-set-key [M-down] 'windmove-down)       ; move to bottom window


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yic

(defun yic-prev-buffer ()
  (interactive)
  (yic-next (reverse (buffer-list))))

(defun yic-next-buffer ()
  (interactive)
  (bury-buffer (current-buffer))
  (yic-next (buffer-list)))

(defun yic-ignore (str)
  (or
   (string-match "\\*Buffer List\\*" str)
   (string-match "\\*scratch\\*" str)
   (string-match "^TAGS" str)
   (string-match "^\\*Messages\\*$" str)
   (string-match "^\\*Completions\\*$" str)
   (string-match "^\\*ESS\\*$" str)
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
