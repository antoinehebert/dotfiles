;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq c-basic-offset 4)
(setq c-default-style (quote ((c++-mode . "bsd") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
(setq column-number-mode t)
(setq compile-command "make -j5")

(delete-selection-mode 1)
(electric-indent-mode 1)
;; (electric-pair-mode 1) ; auto-pairs, inserting `(` will insert `()`
(setq-default fill-column 120)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
;; note we use setq-default
(setq-default indent-tabs-mode nil)
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq jsx-indent-level 2)
(setq mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
(setq python-continuation-offset 4)
(setq python-indent 4)
(setq sgml-basic-offset 4)
(show-paren-mode 1)
(setq standard-indent 2)
(setq tab-width 2)
(setq inhibit-startup-message t) ;; stop showing emacs welcome screen
(setq case-fold-search t)   ; make searches case insensitive
(put 'upcase-region 'disabled nil)

(tool-bar-mode -1)
;; (scroll-bar-mode -1)
(setq initial-scratch-message "")

;; Prefer horizontal split.
(setq split-height-threshold nil)
(setq split-width-threshold 240) ;; 120 * 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; spelling
(use-package flyspell
  :init
  (setq flyspell-issue-message-flag nil)
  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode))

(add-hook 'prog-mode-hook 'git-gutter-mode)

;; (setq column-enforce-column 120)
;; (add-hook 'prog-mode-hook 'column-enforce-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode) ;; native alternative to column-enforce.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-org-mode-hook()
  (org-indent-mode t)
  (visual-line-mode t)
  (org-bullets-mode t)
  (local-set-key (kbd "M-<return>") 'org-meta-return) ; Disable custom "open line" command
  )
(add-hook 'org-mode-hook 'my-org-mode-hook)
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
    wrap-region
    yaml-mode
    flx-ido
    flycheck
    rubocop
    org-bullets
    git-gutter
    json-mode
    ;; projectile
    ;; projectile-rails
    ;; flymake-ruby ;; not necessary with lsp-mode?
    ;; column-enforce-mode
    idle-highlight-mode
    gruber-darker-theme
    ))

(defun my/install-packages (packages)
  "You know... install PACKAGES."
  (interactive)
  (dolist (p packages)
    (unless (package-installed-p p)
      (package-refresh-contents)
      (package-install p))))
(my/install-packages user-packages)

;; Mac specific packages
(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize))

  ;; swap cmd and option
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

(wrap-region-global-mode t)

;; theme
;; (load-theme 'material-light t)
;; (load-theme 'gruvbox t)
;; (load-theme 'wilmersdorf t) ;; no package for this one, don't push this to the repo unless we add themes folder there too.
;; (load-theme 'naysayer t)

(defun set-font ()
  (let ((font "Iosevka-18"))
    (set-face-attribute 'default nil :font font)
    (set-frame-font font nil t)))
(set-font)

(load-theme 'tone-blow t)

(use-package multiple-cursors
  :ensure t
  :bind (("C-c c" . mc/edit-lines)
         ("C-c n" . mc/mark-next-like-this)
         ("C-c N" . mc/skip-to-next-like-this)
         ("C-c m" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package company
  :ensure t
  ;; removed global mode in favor of using use-package :hook section...
  ;; :config (global-company-mode t)
  )
(add-hook 'prog-mode-hook 'company-mode)



(use-package treesit
  ;; :ensure t
  ;; :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
        '(
          ;; (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          ;; (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          ;; (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          ;; (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          ;; (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          ;; (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          ;; (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          ;; (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          ;; (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          ;; (julia . ("https://github.com/tree-sitter/tree-sitter-julia"))
          ;; (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          ;; (make . ("https://github.com/alemuller/tree-sitter-make"))
          ;; (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src"))
          ;; (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          ;; (php . ("https://github.com/tree-sitter/tree-sitter-php"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
          ;; (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          ;; (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          ;; ;; (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          ;; (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          ;; (zig . ("https://github.com/GrayJack/tree-sitter-zig"))
          ))
  ;; :config
  ;; (defun nf/treesit-install-all-languages ()
  ;;   "Install all languages specified by `treesit-language-source-alist'."
  ;;   (interactive)
  ;;   (let ((languages (mapcar 'car treesit-language-source-alist)))
  ;;     (dolist (lang languages)
  ;;       (treesit-install-language-grammar lang)
  ;;       (message "`%s' parser was installed." lang)
  ;;       (sit-for 0.75)))))
  )

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'ruby-mode-hook #'rubocop-mode)
(setq ruby-insert-encoding-magic-comment nil)

;; projectile
;; (setq projectile-keymap-prefix (kbd "C-c C-p"))
;; (add-hook 'ruby-mode-hook 'projectile-mode)
;; (add-hook 'projectile-mode-hook 'projectile-rails-on)
;; (setq ruby-insert-encoding-magic-comment nil)
;; (global-set-key (kbd "C-c p") 'projectile-find-file)

;; (require 'flymake-ruby)
;; (add-hook 'ruby-mode-hook 'flymake-ruby-load)

(use-package fzf
  :ensure t
  :bind (("C-c p" . fzf-git-files)))



(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;
;; rust
;;
(use-package rust-mode
  :ensure t
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

;;
;; typescript
;;
(use-package typescript-ts-mode
  :ensure t
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         ;; (typescript-ts-mode . company-mode))
         )
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :init
  (setq typescript-indent-level 2))

(defun my/eslint-fix ()
  "Format the current file with ESLint."
  (interactive)
  ;; (let ((eslint "eslint"))
  ;;   (if (executable-find eslint)
  ;;       (progn
  (message (concat "eslint --fix the file " (buffer-file-name)))
  ;; (call-process eslint nil "*ESLint Errors*" nil "--fix" buffer-file-name)
  (call-process "npx" nil "*ESLint Errors*" nil "eslint" "--fix" (buffer-file-name))
  ;; (shell-command (concat "npx eslint --fix " (buffer-file-name)) nil "*eslint errors*")
  (revert-buffer t t t))
;; (message (concat eslint " not found.")))))

;; (defun my/eslint-fix-after-save-hook ()
;;   "After save hook for my/eslint-fix."
;;   (add-hook 'after-save-hook 'my/eslint-fix nil t))

;; (eval-after-load 'js-mode
;;   '(add-hook 'js-mode-hook 'my/eslint-fix-after-save-hook))

;; (eval-after-load 'js2-mode
;;   '(add-hook 'js2-mode-hook 'my/eslint-fix-after-save-hook))

;; (eval-after-load 'web-mode
;;   '(add-hook 'web-mode-hook 'my/eslint-fix-after-save-hook))

;; (eval-after-load 'web2-mode
;;   '(add-hook 'web2-mode-hook 'my/eslint-fix-after-save-hook))

;; (eval-after-load 'typescript-mode
;;   '(add-hook 'typescript-mode-hook 'my/eslint-fix-after-save-hook))

;; (eval-after-load 'typescript-ts-mode
;;   '(add-hook 'typescript-ts-mode-hook 'my/eslint-fix-after-save-hook))

;; Shows available keybindings in context
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package prettier-js
  :ensure t)
  ;; :hook ((js2-mode . prettier-js-mode)
  ;;        (web-mode . prettier-js-mode))
  ;; :commands prettier-js-mode)
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))
(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode))
                            (enable-minor-mode
                             '("\\.tsx?\\'" . prettier-js-mode))))



(use-package buffer-move
  :ensure t
  :bind (
  ("C-c <S-up>"    . buf-move-up)
  ("C-c <S-down>"  . buf-move-down)
  ("C-c <S-left>"  . buf-move-left)
  ("C-c <S-right>" . buf-move-right)))


(use-package windmove
  :ensure t
  :bind (
         ("C-c <up>"    . windmove-up)
         ("C-c <down>"  . windmove-down)
         ("C-c <left>"  . windmove-left)
         ("C-c <right>" . windmove-right)))


(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

;;
;; Lisp
;;
(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package add-node-modules-path
  :ensure t
  :config
  '(add-hook 'js-mode-hook #'add-node-modules-path)
  '(add-hook 'js2-mode-hook #'add-node-modules-path)
  '(add-hook 'web-mode-hook #'add-node-modules-path)
  '(add-hook 'web2-mode-hook #'add-node-modules-path))

;; expand-region
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; (use-package treemacs
;;   :ensure t)

(add-hook 'prog-mode-hook 'idle-highlight-mode)

(use-package rg
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-c s" . magit-status)
         ("C-c b" . magit-blame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Display ido results vertically, rather than horizontally
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

(defun my/delete-surround ()
  "Delete one char on each side of the region."
  (interactive)
  (kill-region (region-beginning) (region-end))
  (delete-char 1)
  (delete-char -1)
  (yank)
  (set-mark)
  (exchange-point-and-mark)
  )
(global-set-key (kbd "C-c <backspace>") 'my/delete-surround)

(defun my/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(global-set-key (kbd "M-;") 'my/comment-or-uncomment-region-or-line)

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
    (setq str (mapconcat 'identity (cdr (split-string str " ")) "-"))
    (setq str (replace-regexp-in-string "[^0-9a-zA-Z]+" "-" str))
    ;; (setq str (replace-regexp-in-string "^_*" "" str))
    ;; (setq str (replace-regexp-in-string "_*$" "" str))
    (setq str (string-remove-prefix "-" str))
    (setq str (string-remove-suffix "-" str))
    (setq str (downcase str))
    (kill-new (concat "tony/" ticket "_" str))))

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


(defun my/insert-date ()
  "Insert current date at point."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (global-set-key (kbd "<f5>") 'sort-lines)
;; (global-set-key (kbd "C-c g n") 'git-gutter:next-diff)
(global-set-key (kbd "M-<f5>") 'git-gutter:next-hunk)
;; (global-set-key (kbd "C-c g p") 'git-gutter:previous-diff)
(global-set-key (kbd "M-S-<f5>") 'git-gutter:previous-hunk)
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
