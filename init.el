(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq use-dialog-box nil
      inhibit-startup-screen t)

(show-paren-mode 1)
(size-indication-mode 1)

(blink-cursor-mode -1)
(setq make-backup-files nil
      auto-save-default nil
      ring-bell-function 'ignore
      indent-tabs-mode nil
      display-line-numbers-type 'relative
      magit-last-seen-setup-instructions "1.4.0"
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(global-display-line-numbers-mode)
(set-frame-font "monospace 9" nil t)

(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font
    t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil
      straight-use-package-by-default t)

(eval-and-compile
  (require 'straight))

(straight-use-package 'use-package)

(eval-and-compile (require 'use-package))

(use-package use-package
  :custom
  (straight-check-for-modifications 'at-startup)
  (use-package-verbose t))

(use-package which-key
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.1)
  (which-key-setup-minibuffer))

(use-package arc-dark-theme
  :straight (:host github :repo "cfraz89/arc-dark-theme")
  :config (load-theme 'arc-dark t))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil))

(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "tb" 'buffer-menu)
  (evil-mode t))

(use-package evil-collection
  :after evil
  :config
  (add-to-list 'evil-collection-mode-list 'help)
  (evil-collection-init))

(use-package scala-mode
  :interpreter ("scala3" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition
    'minibuffer-complete-word
    'self-insert-command
    minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package rustic
  :config (setq rustic-format-on-save t
                rustic-lsp-client 'lsp-mode
                rustic-lsp-server 'rust-analyzer))

(use-package lsp-mode
  :hook (scala-mode . lsp-deferred)
        (lsp-mode . lsp-lens-mode)
        (lsp-mode . lsp-ui-mode)
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-prefer-flymake nil
        lsp-enable-suggest-server-download t
        ;; Rust
        lsp-rust-analyzer-inlay-hints-mode t
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-cargo-watch-command "clippy"))

(use-package lsp-metals)

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t))

(use-package yasnippet
  :hook (lsp-mode . yas-minor-mode)
        (prog-mode . yas-minor-mode)
        (text-mode . yas-minor-mode))

(use-package company
  :hook (scala-mode . company-mode)
        (rustic-mode . company-mode)
  :config (setq lsp-completion-provider :capf
                company-selection-wrap-around t)
  :bind (:map company-active-map
              ("<tab>" . 'company-select-next)
              ("<backtab>" . 'company-select-previous)
         :map company-mode-map
              ("C-SPC" . 'company-indent-or-complete-common)))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package vterm)
