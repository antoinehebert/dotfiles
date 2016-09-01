;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
'(ansi-color-names-vector
  ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
'(org-export-backends (quote (ascii html icalendar latex md)))
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ahebert variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq c-basic-offset 4)
(setq c-default-style (quote ((c++-mode . "bsd") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
(setq column-number-mode t)
(setq compile-command "make -j5")
(setq css-indent-offset 2)
(setq jsx-indent-level 2)

(delete-selection-mode 1)
(electric-indent-mode 1)
(electric-pair-mode 1)
(setq-default fill-column 80)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
;; note we use setq-default
(setq-default indent-tabs-mode nil)
(setq js-indent-level 4)
(setq mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
(setq python-continuation-offset 4)
(setq python-indent 4)
(setq sgml-basic-offset 2)
(show-paren-mode 1)
(setq standard-indent 4)
(setq tab-width 4)
(setq inhibit-startup-message t) ;; stop showing emacs welcome screen
(setq case-fold-search t)   ; make searches case insensitive
(put 'upcase-region 'disabled nil)

;; frame title
(add-hook 'after-init-hook (lambda ()
                             (setq frame-title-format
                                   '(buffer-file-name "%f" (dired-directory dired-directory "%b")))
                             ))

(tool-bar-mode -1)
(scroll-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spelling
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; org-mode
(defun my-org-mode-hook()
  (org-indent-mode t)
  (visual-line-mode t)
  (org-bullets-mode t)
  )
(add-hook 'org-mode-hook 'my-org-mode-hook)


(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq org-ellipsis "⤵")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defvar user-packages
  '(
    haskell-mode
    wrap-region
    monokai-theme
    multiple-cursors
    ;;auto-complete
    company
    yaml-mode
    projectile
    flx-ido
    linum-relative
    ;; ruby on rails packages
    projectile-rails
    flymake-ruby
    rbenv
    robe
    flycheck
    rubocop
    ;; RoR end
    expand-region
    rainbow-delimiters
    material-theme
    org-bullets
    powerline
    ctags
    fill-column-indicator))

(defun ah/install-packages (packages)
  "You know... install packages."
  (interactive)
  (dolist (p packages)
    (when (not (package-installed-p p))
      (package-install p))))
(ah/install-packages user-packages)

;; Mac specific packages
(when (eq system-type 'darwin)
  (defvar user-packages-mac
    '(
      exec-path-from-shell))

  (ah/install-packages user-packages-mac)

  (exec-path-from-shell-initialize)

  ;; swap cmd and option
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

(wrap-region-global-mode t)

;; theme and font
(load-theme 'solarized-light t)
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Inconsolata" :height 160)
  )
(powerline-default-theme)

(require 'multiple-cursors)
(global-set-key (kbd "C-c c") 'mc/edit-lines)
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c N") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c P") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-c m") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; (require 'auto-complete-config)
;; (ac-config-default)
(add-hook 'after-init-hook 'global-company-mode) ; this or auto-complete?
(setq company-dabbrev-downcase nil)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'ruby-mode-hook 'projectile-mode)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'linum-relative)
;; (linum-on)

;; ruby on rails packages
(setq projectile-rails-keymap-prefix (kbd "C-c C-r")) ; C-c letter is reserve for user!
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'rbenv)
(global-rbenv-mode)
(global-set-key (kbd "C-c r") 'global-rbenv-mode)

(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'ctags)
(setq tags-revert-without-query t)
(global-set-key (kbd "C-c t") 'ctags-create-or-update-tags-table)

(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;; use up and down arrows in vertical mode
(defun ido-define-keys ()
  (define-key ido-completion-map [down] 'ido-next-match)
  (define-key ido-completion-map [up] 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize buffer names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ahebert custom commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun increment-number-at-point ()
  (interactive)
  (let ((col (current-column)))
    (skip-chars-backward "0123456789")
    (or (looking-at "[0123456789]+")
        (error "No number at point"))
    (replace-match (number-to-string (1+ (string-to-number (match-string 0)))))
    (move-to-column col))
  )
;; TODO: refactor to reuse common code...
(defun decrement-number-at-point ()
  (interactive)
  (let ((col (current-column)))
    (skip-chars-backward "0123456789")
    (or (looking-at "[0123456789]+")
        (error "No number at point"))
    (replace-match (number-to-string (1- (string-to-number (match-string 0)))))
    (move-to-column col))
  )
(global-set-key (kbd "C-c a") 'increment-number-at-point)
(global-set-key (kbd "C-c A") 'decrement-number-at-point)

(defun duplicate-line()
  (interactive)
  (let ((col (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank)
    (move-to-column col))
  )
(global-set-key (kbd "C-c d") 'duplicate-line)

(defun move-line-up(&optional arg)
  (interactive "p")
  (let ((col (current-column)))
    (dotimes (_ arg)
      (transpose-lines 1)
      (previous-line 2))
    (move-to-column col))
  )
(defun move-line-down(&optional arg)
  (interactive "p")
  (let ((col (current-column)))
    (dotimes (_ arg)
      (next-line 1)
      (transpose-lines 1)
      (previous-line 1))
    (move-to-column col))
  )
(global-set-key (kbd "C-c k") 'move-line-up)
(global-set-key (kbd "C-c j") 'move-line-down)

(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (previous-line 1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(global-set-key (kbd "C-c o") 'vi-open-line-below)
(global-set-key (kbd "C-c O") 'vi-open-line-above)

(global-set-key (kbd "C-c f") 'ff-find-related-file)

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive "^") ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key [end] 'move-end-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(defun ah/delete-surround ()
  "Delete one char on each side of the region"
  (interactive)
  (kill-region (region-beginning) (region-end))
  (delete-char 1)
  (delete-char -1)
  (yank)
  (set-mark)
  (exchange-point-and-mark)
  )

(global-set-key (kbd "C-c s") 'ah/delete-surround)

(defun ah/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(global-set-key (kbd "M-;") 'ah/comment-or-uncomment-region-or-line)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "2305decca2d6ea63a408edd4701edf5f4f5e19312114c9d1e1d5ffe3112cde58" default)))
 '(safe-local-variable-values
   (quote
    ((flycheck-gcc-language-standard . c++14)
     (flycheck-gcc-language-standard . c++11)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
