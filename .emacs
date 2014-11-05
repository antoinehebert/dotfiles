(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(org-export-backends (quote (ascii html icalendar latex md))))
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ahebert variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq c-basic-offset 3)
(setq c-default-style (quote ((c++-mode . "bsd") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
(setq column-number-mode t)
(setq compile-command "make -C /home/ahebert/projects/MDI/builddev/AMD64_RELEASE -j3")
(setq css-indent-offset 3)
(delete-selection-mode 1)
(electric-indent-mode 1)
(electric-pair-mode 1)
(setq-default fill-column 80)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
;; note we use setq-default
(setq-default indent-tabs-mode nil)
(setq js-indent-level 3)
(setq mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
(setq python-continuation-offset 4)
(setq python-indent 4)
(setq sgml-basic-offset 3)
(show-paren-mode 1)
(setq standard-indent 3)
(setq tab-width 3)
(tool-bar-mode -1)
(setq inhibit-startup-message t) ;; stop showing emacs welcome screen
(setq case-fold-search t)   ; make searches case insensitive

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ahebert custom commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0)))))
  )
(global-set-key (kbd "C-+") 'increment-number-at-point)

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
(global-set-key (kbd "C-S-d") 'duplicate-line)

(defun move-line-up()
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (previous-line 2)
    (move-to-column col))
  )
(defun move-line-down()
  (interactive)
    (let ((col (current-column)))
      (next-line 1)
      (transpose-lines 1)
      (previous-line 1)
      (move-to-column col))
  )
(global-set-key (kbd "C-S-<up>") 'move-line-up)
(global-set-key (kbd "C-S-<down>") 'move-line-down)

(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(global-set-key (kbd "C-<return>") 'vi-open-line-below)
(global-set-key (kbd "C-S-<return>") 'vi-open-line-above)

(global-set-key (kbd "C-<tab>") 'ff-find-related-file)

(defun my-org-mode-hook()
  (org-indent-mode 1)
  (visual-line-mode 1)
  )
(add-hook 'org-mode-hook 'my-org-mode-hook)
(put 'upcase-region 'disabled nil)

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
