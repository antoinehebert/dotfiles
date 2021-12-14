; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
'(ansi-color-names-vector
  ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
'(org-export-backends (quote (ascii html icalendar latex md)))
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq c-basic-offset 4)
(setq c-default-style (quote ((c++-mode . "bsd") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
(setq column-number-mode t)
(setq compile-command "make -j5")
(setq css-indent-offset 4)
(setq jsx-indent-level 4)

(delete-selection-mode 1)
(electric-indent-mode 1)
;; (electric-pair-mode 1) ; auto-pairs, inserting `(` will insert `()`
(setq-default fill-column 120)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
;; note we use setq-default
(setq-default indent-tabs-mode nil)
(setq js-indent-level 4)
(setq mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
(setq python-continuation-offset 4)
(setq python-indent 4)
(setq sgml-basic-offset 4)
(show-paren-mode 1)
(setq standard-indent 4)
(setq tab-width 4)
(setq inhibit-startup-message t) ;; stop showing emacs welcome screen
(setq case-fold-search t)   ; make searches case insensitive
(put 'upcase-region 'disabled nil)

(tool-bar-mode -1)
;; (scroll-bar-mode -1)
(setq initial-scratch-message "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spelling
(setq flyspell-issue-message-flag nil)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(add-hook 'prog-mode-hook 'git-gutter-mode)

;; (setq column-enforce-column 120)
;; (add-hook 'prog-mode-hook 'column-enforce-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode) ;; native alternative to column-enforce.

;; org-mode
(defun my-org-mode-hook()
  (org-indent-mode t)
  (visual-line-mode t)
  (org-bullets-mode t)
  (local-set-key (kbd "M-<return>") 'org-meta-return) ; Disable custom "open line" command
  )
(add-hook 'org-mode-hook 'my-org-mode-hook)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq org-ellipsis "⤵")
(setq org-bullets-bullet-list '("•"))

;; org-agenda command that shows scheduled and closed items.-
(setq org-agenda-custom-commands
      '(("W" "Weekly review"
         agenda ""
         ((org-agenda-span 'week)
          (org-agenda-start-on-weekday 0)
          (org-agenda-start-with-log-mode '(closed))
          ))))

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
    gruvbox-theme
    multiple-cursors
    company ;; auto-complete
    yaml-mode
    flx-ido
    linum-relative
    flycheck
    rbenv
    rubocop
    expand-region
    rainbow-delimiters
    material-theme
    org-bullets
    magit
    git-gutter
    json-mode
    projectile
    projectile-rails
    ;; flymake-ruby ;; not necessary with lsp-mode?
    ;; column-enforce-mode
    idle-highlight-mode
    rg ;; ripgrep
    use-package
    ))

(defun ah/install-packages (packages)
  "You know... install PACKAGES."
  (interactive)
  (dolist (p packages)
    (unless (package-installed-p p)
      (package-refresh-contents)
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

;; themes that are not packages are in this folder.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; theme and font
;; (load-theme 'material-light t)
;; (load-theme 'gruvbox t)
;; (load-theme 'wilmersdorf t) ;; no package for this one, don't push this to the repo unless we add themes folder there too.
;; (load-theme 'naysayer t)

(when (eq system-type 'darwin)
  ;; (set-face-attribute 'default nil :font "source code pro-16")
  (set-face-attribute 'default nil :font "consolas-18")
  ;; (set-face-attribute 'default nil :font "IBM Plex Mono-18")
  )

;;
;; START THEME -- Figure out a way of doing this properly...
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#d3b58d" :background "#0a2626"))))
 ;; '(column-enforce-face ((t (:foreground "IndianRed2"))))
 '(custom-group-tag-face ((t (:underline t :foreground "lightblue"))) t)
 '(custom-variable-tag-face ((t (:underline t :foreground "lightblue"))) t)
 '(font-lock-builtin-face ((t nil)))
 '(font-lock-comment-face ((t (:foreground "#3fdf1f"))))
 '(font-lock-funciton-name-face ((((class color) (background dark)) (:foreground "white"))))
 '(font-lock-keyword-face ((t (:foreground "white"))))
 '(font-lock-string-face ((t (:foreground "#0fdfaf"))))
 '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "#c8d4ec"))))
 '(font-lock-warning-face ((t (:foreground "#504038"))))
 '(highlight ((t (:foreground "navyblue"))))
 '(lsp-face-highlight-read ((t (:background "dimgrey" :foreground "#d3b58d"))))
 '(lsp-face-highlight-textual ((t (:background "dimgrey" :foreground "#d3b58d"))))
 '(lsp-face-highlight-write ((t (:background "dimgrey" :foreground "#d3b58d"))))
 '(mode-line ((t (:inverse-video t))))
 '(region ((t (:background "blue"))))
 '(widget-field-face ((t (:foreground "white"))))
 '(widget-single-line-field-face ((t (:foreground "darkgray"))) t))

(set-cursor-color "lightgreen")  ;; #90ee90
(set-background-color "#0a2626")

(set-face-foreground 'font-lock-builtin-face "lightgreen")
;;
;; END THEME
;;

(require 'multiple-cursors)
(global-set-key (kbd "C-c c") 'mc/edit-lines)
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c N") 'mc/skip-to-next-like-this)
;; (global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c P") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-c m") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

(add-hook 'after-init-hook 'global-company-mode) ; this or auto-complete?
(setq company-dabbrev-downcase nil)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(setq projectile-keymap-prefix (kbd "C-c C-p"))
(add-hook 'ruby-mode-hook 'projectile-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(add-hook 'ruby-mode-hook #'rubocop-mode)
(setq ruby-insert-encoding-magic-comment nil)

(global-set-key (kbd "C-c p") 'projectile-find-file)

;; (require 'flymake-ruby)
;; (add-hook 'ruby-mode-hook 'flymake-ruby-load)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; use the right modes for javascript files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-strict-trailing-comma-warning nil)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config (setq-default flycheck-disabled-checkers '(ruby-reek)))

(use-package try
  :commands try)

(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :config (setq rust-format-on-save t))

;; Run cargo commands in rust buffers, e.g. C-c C-c C-r for cargo-run
(use-package cargo
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'toml-mode-hook 'cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :init
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; Language Server Protocol
(use-package lsp-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'ruby-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp)
  (add-hook 'js2-mode-hook #'lsp)

  ;; perf tweaks based on https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  )
(use-package lsp-ui
  :ensure t)

(use-package buffer-move
  :ensure t
  :init
  (global-set-key (kbd "C-c <S-up>")     'buf-move-up)
  (global-set-key (kbd "C-c <S-down>")   'buf-move-down)
  (global-set-key (kbd "C-c <S-left>")   'buf-move-left)
  (global-set-key (kbd "C-c <S-right>")  'buf-move-right))


(require 'windmove)
;; (windmove-default-keybindings 'shift) ;; make S-<arrow> move windows
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)

;; (use-package dumb-jump
;;   :ensure t
;;   :bind (("C-c j o" . dumb-jump-go-other-window)
;;          ("C-c j j" . dumb-jump-go)
;;          ;; ("M-." . dumb-jump-go)
;;          ("C-c j p" . dumb-jump-go-prompt)
;;          ("C-c j b" . dumb-jump-back)
;;          ;; ("m->" . dumb-jump-back)
;;          )
;;   :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
;;   :ensure)

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))


;; (use-package ivy
;;   :ensure t
;;   :config (setq ivy-use-virtual-buffers t
;;                 ivy-count-format "%d/%d ")
;;   :bind (("C-s" . 'swiper)
;;          ("M-x" . 'counsel-M-x)
;;          ("C-x C-f" . 'counsel-find-file)
;;          ("C-x b" . 'ivy-switch-buffer)
;;          ;; ("<f1> f" . 'counsel-describe-function)
;;          ;; ("<f1> v" . 'counsel-describe-variable)
;;          ;; ("<f1> l" . 'counsel-find-library)
;;          ;; ("<f2> i" . 'counsel-info-lookup-symbol)
;;          ;; ("<f2> u" . 'counsel-unicode-char)
;;          ))

;;
;; Lisp
;;
(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl"))

;; Stolen from https://github.com/codesuki/add-node-modules-path/blob/master/add-node-modules-path.el
(defun my/add-node-modules-path ()
  "Search the current buffer's parent directories for `node_modules/.bin`.
If it's found, then add it to `exec-path`."
  (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (path (and root
                    (expand-file-name "node_modules/.bin/" root))))
    (if root
        (progn
          (make-local-variable 'exec-path)
          (add-to-list 'exec-path path)
          (message (concat "added " path  " to exec-path")))
      (message (concat "node_modules not found in " root)))))

(eval-after-load 'web-mode
  '(add-hook 'web-mode-hook 'my/add-node-modules-path))

(eval-after-load 'js-mode
  '(add-hook 'js-mode-hook 'my/add-node-modules-path))

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook 'my/add-node-modules-path))

;; disable jshint since we prefer eslint checking
;; (setq-default flycheck-disabled-checkers
;;               (append flycheck-disabled-checkers '(javascript-jshint)))

;; use eslint javascript files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'js-mode)

(defun my/eslint-fix ()
  "Format the current file with ESLint."
  (interactive)
  (let ((eslint "eslint"))
    (if (executable-find eslint)
        (progn (call-process eslint nil "*ESLint Errors*" nil "--fix" buffer-file-name)
               (revert-buffer t t t))
      (message (concat eslint " not found.")))))

(defun my/eslint-fix-after-save-hook ()
  "After save hook for my/eslint-fix."
  (add-hook 'after-save-hook 'my/eslint-fix nil t))

(eval-after-load 'js-mode
  '(add-hook 'js-mode-hook 'my/eslint-fix-after-save-hook))

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook 'my/eslint-fix-after-save-hook))

(eval-after-load 'web-mode
  '(add-hook 'web-mode-hook 'my/eslint-fix-after-save-hook))

(eval-after-load 'web2-mode
  '(add-hook 'web2-mode-hook 'my/eslint-fix-after-save-hook))


(require 'rbenv)
(global-rbenv-mode)
(global-set-key (kbd "C-c r") 'global-rbenv-mode)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)

;; (require 'rainbow-delimiters)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(add-hook 'prog-mode-hook 'idle-highlight-mode)

(require 'rg)
;; if you want default ripgrep (rg) keybindings just run this: `(rg-enable-default-bindings)`

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display ido results vertically, rather than horizontally
;; (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;; (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
;; (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;; ;; use up and down arrows in vertical mode
;; (defun ido-define-keys ()
;;   (define-key ido-completion-map [down] 'ido-next-match)
;;   (define-key ido-completion-map [up] 'ido-prev-match))
;; (add-hook 'ido-setup-hook 'ido-define-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize buffer names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; frame title
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/set-frame-title ()
  "Frame title is current buffer full path."
  (setq frame-title-format
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(add-hook 'after-init-hook 'my/set-frame-title)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; don't grep these dirs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories "log")
     (add-to-list 'grep-find-ignored-directories ".git")
     (add-to-list 'grep-find-ignored-directories ".svn")
     (add-to-list 'grep-find-ignored-files ".tags")
     (add-to-list 'grep-find-ignored-files ".tags1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my custom commands
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
(global-set-key (kbd "C-c +") 'increment-number-at-point)
(global-set-key (kbd "C-c -") 'decrement-number-at-point)

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
(global-set-key (kbd "M-S-<down>") 'duplicate-line)

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
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

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

;; (global-set-key (kbd "C-c o") 'vi-open-line-below)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "M-<return>") 'vi-open-line-below)
;; (global-set-key (kbd "C-c O") 'vi-open-line-above)
(global-set-key (kbd "M-S-<return>") 'vi-open-line-above)
(global-set-key (kbd "C-c h") 'ff-find-related-file) ;; h is for header since I used f for rgrep already...

(defun smart-beginning-of-line ()
  "Move point to the first non-whitespace character on this line.
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
  "Delete one char on each side of the region."
  (interactive)
  (kill-region (region-beginning) (region-end))
  (delete-char 1)
  (delete-char -1)
  (yank)
  (set-mark)
  (exchange-point-and-mark)
  )

(global-set-key (kbd "C-c <backspace>") 'ah/delete-surround)

(defun ah/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(global-set-key (kbd "M-;") 'ah/comment-or-uncomment-region-or-line)

(defun my/ticket-description-to-branch-name (start end)
  "Turn START to END selection to git branch name."
  (interactive "r")
  (let ((case-fold-search nil)
        (str (buffer-substring (mark) (point)))
        (ticket ""))
    ;; use tag name for org-mode links
    (setq str (replace-regexp-in-string "^\\[\\[.*\\]\\[\\(.*\\)\\]\\]:" "\\1:" str))
    ;; depending on the selection we sometimes only get the last part of the link "tag_name]]"...\
    (setq str (replace-regexp-in-string "^\\(.*\\)\\]\\]:" "\\1:" str))
    ;; extract ticket number
    (setq ticket (car (split-string str "[: ]")))
    (setq str (mapconcat 'identity (cdr (split-string str " ")) "_"))
    (setq str (replace-regexp-in-string "[^0-9a-zA-Z]+" "_" str))
    ;; (setq str (replace-regexp-in-string "^_*" "" str))
    ;; (setq str (replace-regexp-in-string "_*$" "" str))
    (setq str (string-remove-prefix "_" str))
    (setq str (string-remove-suffix "_" str))
    (setq str (downcase str))
    (kill-new (concat "ticket/" ticket "_" str))))

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.

    Prefixed with negative \\[universal-argument], sorts in
    reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun my/google (arg)
  "Googles a mini-buffer query or region if any active.
   The current language/mode is added to the search unless C-u is used."
  (interactive "P")
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (unless arg
        (concat (car (split-string (format "%s" major-mode) "-")) " "))
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

(defun my/fill-to-end (char)
  "Function my/fill-to-end fills the line with CHAR until FILL-COLUMN."
  (interactive "cFill Character:")
  ;;(save-excursion
    ;;(end-of-line)
    (while (< (current-column) fill-column)
      (insert-char char)));;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (global-set-key (kbd "<f5>") 'sort-lines)
;; (global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c s") 'magit-status)
;; (global-set-key (kbd "C-c g g") 'magit-dispatch-popup)
(global-set-key (kbd "C-c b") 'magit-blame)
;; (global-set-key (kbd "C-c g n") 'git-gutter:next-diff)
(global-set-key (kbd "<f5>") 'git-gutter:next-hunk)
;; (global-set-key (kbd "C-c g p") 'git-gutter:previous-diff)
(global-set-key (kbd "S-<f5>") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-c d") 'git-gutter:popup-hunk)
;; (global-set-key (kbd "C-c f") 'rgrep)
;; (global-set-key (kbd "C-c f") 'ripgrep-regexp)
(global-set-key (kbd "C-c f") 'rg)
(global-set-key (kbd "C-c a") 'align-regexp)
;; (global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-c ?") 'my/google)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

