;; Skip the default splash screen
(setq inhibit-startup-message t)

;; straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'exec-path-from-shell)

(when (memq window-system '(x mac ns))
  (exec-path-from-shell-initialize))

;; Intergate it with use-package
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(when (not (eq window-system nil))
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(blink-cursor-mode 0)

(set-face-attribute 'default (selected-frame) :height 140)

(setq spacemacs-theme-comment-bg nil
      spacemacs-theme-comment-italic t)

(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

(use-package projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Use ⌘ as meta, ⌥ as always
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(use-package company
  :demand t
  :commands company-mode
  :config
  ;; Enable company-mode globally.
  (global-company-mode)
  ;; Except when you're in term-mode.
  (setq company-global-modes '(not term-mode))
  ;; Give Company a decent default configuration.
  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-require-match nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  ;; Sort completion candidates that already occur in the current
  ;; buffer at the top of the candidate list.
  (setq company-transformers '(company-sort-by-occurrence))
  ;; Show documentation where available for selected completion
  ;; after a short delay.
  (use-package company-quickhelp
    :config
    (setq company-quickhelp-delay 1)
    (company-quickhelp-mode 1))
  ;; Add a completion source for emoji. 😸
  (use-package company-emoji
    :config
    (company-emoji-init))
  ;; Use C-\ to activate the Company autocompleter.
  ;; We invoke company-try-hard to gather completion candidates from multiple
  ;; sources if the active source isn't being very forthcoming.
  (use-package company-try-hard
    :commands company-try-hard
    :bind ("C-\\" . company-try-hard)
    :config
    (bind-keys :map company-active-map
               ("C-\\" . company-try-hard)))
  :diminish company-mode)

;; Install improved JavaScript editing mode.

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
   ("\\.es6\\'" . js2-mode)
   ("\\.ejs\\'" . js2-mode))
  :interpreter "node"
  :commands js2-mode
  :config
  ;; Enable js2-refactor to enable refactoring support.
  (use-package js2-refactor
    :commands (js2r-add-keybindings-with-prefix)
    :init
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (js2r-add-keybindings-with-prefix "C-c C-m"))
  (setq-default
   js2-mode-indent-ignore-first-tab t
   js2-strict-inconsistent-return-warning nil
   js2-global-externs
   '("module" "require" "__dirname" "process"
     "console" "JSON" "$" "_"))
  )

;; Install Flycheck.
(use-package flycheck
  :config
  ;; Start it automatically for all modes except ELisp mode,
  ;; where the linter is just designed to make you mad.
  (add-hook 'find-file-hook
            (lambda ()
              (when (not (equal 'emacs-lisp-mode major-mode))
                (flycheck-mode)))))

;; Bind M-n and M-p to navigate to the next/previous errors.
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

;; Use web-mode for all JS files.
(use-package web-mode
  :mode (("\\.tsx?$" . web-mode)
         ("\\.jsx?$" . web-mode)
         ("\\.es6\\'" . web-mode)
         ("\\.ejs\\'" . web-mode))
  :config
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.jsx?$")))
  ;; Stop web-mode from using block comments in comment-dwim.
  (setq web-mode-comment-formats
        (-map-when (lambda (i) (equal (car i) "javascript"))
                   (lambda (i) (cons (car i) "//"))
                   web-mode-comment-formats))
  (add-to-list 'web-mode-comment-formats `("jsx" . "//"))

  ;; Let Flycheck know that we're using web-mode for JS.
  (with-eval-after-load "flycheck"
    (flycheck-add-mode 'javascript-eslint 'web-mode))
  )

(use-package tide
  :after (web-mode company flycheck)
  :hook ((web-mode . tide-setup)
         (web-mode . tide-hl-identifier-mode))
  :mode (("\\.tsx?$" . web-mode)
         ("\\.ts\\'" . web-mode))
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t))

(use-package helm
  :demand t
  :config
  (require 'helm-config)
  (require 'helm)
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (defun grep-do-git-grep-root ()
    (interactive)
    (helm-grep-do-git-grep (vc-root-dir)))
  (bind-key "C-x C-M-g" 'grep-do-git-grep-root)
  :bind (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list))
  :diminish helm-mode)

(use-package helm-projectile
  :demand t
  :bind (("C-c C-f" . helm-projectile-find-file-dwim)
         ("C-x C-g" . helm-grep-do-git-grep))
  :config (helm-projectile-on))

(use-package prettier-js
  :commands prettier-js
  :bind ("C-c C-p" . prettier-js)
  )

;; EDITING

;; Make the selection work like most people expect.
(delete-selection-mode t)
(transient-mark-mode t)

;; Disable the auto-save feature
(setq auto-save-default nil)

;; Automatically insert matching braces and do other clever
;; things pertaining to braces and such.
(electric-pair-mode 1)

(set-default 'indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq-default tab-width 2)

;; A key for intelligently shrinking whitespace.
;; See https://github.com/jcpetkovich/shrink-whitespace.el for details.
(use-package shrink-whitespace
  :commands shrink-whitespace
  :bind ("C-c DEL" . shrink-whitespace))

;; Highlight changed areas with certain operations, such as undo, kill, yank.
(use-package volatile-highlights
  :commands volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

;; JavaScript
(setq-default js2-basic-offset 2)

;; TypeScript
(setq typescript-indent-level 2
      typescript-expr-indent-offset 2)

;; HTML etc with web-mode
(setq-default web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-style-padding 2
              web-mode-script-padding 2)

;; supercharge undo
(use-package undo-tree
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :diminish undo-tree-mode)

(setq sentence-end-double-space nil)

;; Always indent after a newline.
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Strict whitespace with ethan-wspace: highlight bad habits,
;; and automatically clean up your code when saving.
;; Use C-c c to instantly clean up your file.
;; Read more about ethan-wspace: https://github.com/glasserc/ethan-wspace
(use-package ethan-wspace
  :demand t
  :commands global-ethan-wspace-mode
  :config
  (global-ethan-wspace-mode 1)
  :bind ("C-c c" . ethan-wspace-clean-all)
  :diminish ethan-wspace-mode)

(set-default 'indent-tabs-mode nil)
(set-default 'mode-require-final-newline nil)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; C-c <left/right> to manage windows history
(winner-mode 1)
;; M-<number> to switch windows
(use-package winum
  :init
  (progn
    (setq winum-keymap
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-`") 'winum-select-window-by-number)
            (define-key map (kbd "C-²") 'winum-select-window-by-number)
            (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
            (define-key map (kbd "M-1") 'winum-select-window-1)
            (define-key map (kbd "M-2") 'winum-select-window-2)
            (define-key map (kbd "M-3") 'winum-select-window-3)
            (define-key map (kbd "M-4") 'winum-select-window-4)
            (define-key map (kbd "M-5") 'winum-select-window-5)
            (define-key map (kbd "M-6") 'winum-select-window-6)
            (define-key map (kbd "M-7") 'winum-select-window-7)
            (define-key map (kbd "M-8") 'winum-select-window-8)
            map))
    (winum-mode)))

(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(global-set-key (kbd "C-c k o") 'kill-other-buffers)

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-c k a") 'kill-all-buffers)

;; GIT

;; Invoke Magit by typing C-x g.
;; See http://magit.github.io/ for instructions.
(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-mode-line-process ((t (:inherit mode-line-emphasis :foreground "medium purple")))))

;; Ensure linum-mode is disabled in certain major modes.
(setq linum-disabled-modes
      '(term-mode slime-repl-mode magit-status-mode help-mode nrepl-mode
        mu4e-main-mode mu4e-headers-mode mu4e-view-mode
        mu4e-compose-mode))
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes))
    (linum-mode 1)))

;; Highlight the line number of the current line.
(use-package hlinum
  :config
  (hlinum-activate))

;; Show column numbers in modeline.
(setq column-number-mode t)

;; Show current function in modeline.
(which-function-mode)

;; Highlight matching braces.
(show-paren-mode 1)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(setq doom-modeline-buffer-file-name-style 'file-name)
