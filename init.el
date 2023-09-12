(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-unix* (not *is-windows*))
(defconst *project-dir* (expand-file-name "~/git"))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq use-dialog-box nil
      inhibit-startup-screen t
      completion-cycle-threshold t)

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
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      package-enable-at-startup nil
      straight-use-package-by-default t)

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
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t))

(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-mode t))

(use-package evil-collection
  :after evil-leader
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (add-to-list 'evil-collection-mode-list 'help)
  (add-to-list 'evil-collection-mode-list 'vertico)
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil-leader
  :config
  (global-evil-surround-mode t))

(use-package evil-numbers
  :ensure t
  :after evil-leader
  :config
  (evil-define-key '(normal visual) 'global (kbd "C-a") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C-x") 'evil-numbers/dec-at-pt))

(use-package evil-textobj-tree-sitter
  :ensure t
  :after evil-leader
  :config
  (define-key evil-outer-text-objects-map "f"
            (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f"
            (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "c"
            (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c"
            (evil-textobj-tree-sitter-get-textobj "class.inner")))

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

(defun me/expand-git-project-dirs (root)
  "Return a list of all project directories 2 levels deep in ROOT.

Given my git projects directory ROOT, with a layout like =git/{hub,lab}/<user>/project=, return a list of 'user' directories that are part of the ROOT."
  (mapcan #'(lambda (d) (cddr (directory-files d t)))
          (cddr (directory-files root t))))

(use-package projectile
  :demand t
  :custom
  (projectile-completion-system 'default)
  (projectile-enable-caching t)
  (projectile-sort-order 'recently-active)
  (projectile-indexing-method (if *is-unix* 'hybrid 'native))
  (projectile-project-search-path `((,*project-dir* . 3)))
  :config
  (projectile-mode +1))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package savehist
  :demand t
  :config
  (savehist-mode))

(use-package vertico
  :demand t
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  :config
  (vertico-mode))

(use-package icomplete
  :custom
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)

  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion))))

  (completion-group t)
  (completions-group-format
   (concat
    (propertize "    " 'face 'completions-group-separator)
    (propertize " %s " 'face 'completions-group-title)
    (propertize " " 'face 'completions-group-separator
                'display '(space :align-to right)))))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless))
  :config
  (defun prefix-if-tilde (pattern _index _total)
   (when (string-suffix-p "~" pattern)
    `(orderless-prefixes . ,(substring pattern 0 -1))))

  (defun regexp-if-slash (pattern _index _total)
   (when (string-prefix-p "/" pattern)
    `(orderless-regexp . ,(substring pattern 1))))

  (defun literal-if-equal (pattern _index _total)
   (when (string-suffix-p "=" pattern)
     `(orderless-literal . ,(substring pattern 0 -1))))

  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-matching-styles '(orderless-flex))
  (setq orderless-style-dispatchers
        '(prefix-if-tilde
          regexp-if-slash
          literal-if-equal
          without-if-bang)))

(use-package marginalia
  :defer t
  :config
  (marginalia-mode t))

(use-package consult
  :ensure t
  :after evil-leader projectile
  :config
  (evil-leader/set-key
    "ff" 'consult-find
    "fb" 'consult-buffer
    "fg" 'consult-ripgrep)
  :custom
  (consult-project-root-function #'projectile-project-root)
  (consult-narrow-key "<"))

(use-package neotree
  :ensure t
  :config
  (evil-define-key '(normal visual) 'global (kbd "SPC e") 'neotree-toggle))

(use-package vterm)
