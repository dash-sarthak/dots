;;; init.el --- lexical-binding: t;
;;; Commentary:

;;; Code:
;; Don't load default emacs config
(setq inhibit-default-init 1)

;; Write customizations to custom.el
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)


;; UI Cleanup
(setq inhibit-startup-message t)    ; No splash screen
(scroll-bar-mode -1)                ; Bye bye scrollbar
(tool-bar-mode -1)                  ; Adios toolbar
(tooltip-mode -1)                   ; No tooltips, thank you
(set-fringe-mode 10)                ; Give some breathing room
(menu-bar-mode -1)                  ; No menu bar
(setq ring-bell-function 'ignore)   ; No bell

(setq-default fill-column 80)
(set-face-attribute 'fill-column-indicator nil
                    :foreground "#717C7C" ; katana-gray
                    :background "transparent")
(global-display-fill-column-indicator-mode 1)

;; Best monospace font
(set-face-attribute 'default nil
		    :font "JetBrainsMono Nerd Font"
		    :weight 'regular
		    :height 132)
(set-face-attribute 'mode-line nil :weight 'medium :height 120)

;; Theme
;; (load-theme 'modus-vivendi t)
;; (load-file "~/.config/emacs/modus-colors.el")

;; Line and column numbers
(column-number-mode)                ; Show column numbers
(global-display-line-numbers-mode t) ; Show line numbers globally

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)))
  (add-hook mode (lambda () (set-face-attribute 'fill-column-indicator nil
                    :foreground "transparent"
                    :background "transparent"))))

;; PACKAGE MANAGER
;; Initialize package sources

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/")
                         ("elpa" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; Add packages
;; Catppuccin theme
(use-package catppuccin-theme
             :init (load-theme 'catppuccin t))

;; Non-intrusive garbage collection
(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

;; Ivy - Swanky search and completion framework
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Ivy-rich - Make Ivy even richer
(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

;; Counsel - More Ivy magic
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-c b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

;; WhichKey - Never forget your keybindings
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0))

;; Helpful - A better *help* buffer
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Icons - Pretty icons for a prettier Emacs
;; Run M-x all-the-icons-install-fonts on new machines
(use-package all-the-icons)

;; General - Easier keybindings
(use-package general
  :config
  (general-evil-setup)
  (general-create-definer dash/leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; Evil - Vim emulation
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-undo-system 'undo-redo))

;; Evil everywhere
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Teminal emulation
(use-package vterm)

;; Toggle terminal
(use-package vterm-toggle)

;; Comments
(use-package evil-nerd-commenter
  :general
  (general-nvmap
    "gc" 'evilnc-comment-operator))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode 1)
  :custom ((projectile-completion-system 'ivy))
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Development")
    (setq projectile-project-search-path '("~/Development")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Counsel-projectile
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Magit
(use-package magit)


;; Keybindings
(dash/leader-key
  ;; Projects
  "p" 'projectile-command-map

  "f"  '(:ignore t :which-key "find")
  "f <escape>" '(keyboard-escape-quit :which-key t)
  "ff" '(find-file :which-key "find a file")
  "fp" '(lambda() (interactive)(find-file "~/.config/emacs/init.el") :which-key "find private config")
 
  "t"  '(:ignore t :which-key "toggles")
  "t <escape>" '(keyboard-escape-quit :which-key t)
  "tt" '(dash/modus-themes-toggle-and-update :which-key "toggle between light and dark themes")

  "o"  '(:ignore t :which-key "open")
  "o <escape>" '(keyboard-escape-quit :which-key t)
  "ot" '(vterm-toggle :which-key "terminal")
  "o-" '(dired-jump :which-key "dired")

  "w"  '(:ignore t :which-key "window")
  "w <escape>" '(keyboard-escape-quit :which-key t)
  "wv" '(split-window-right :which-key "split window to right")
  "ws" '(split-window-below :which-key "split window to bottom"))


;; Org-mode
(defun dash/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; (use-package org
;;   :hook (org-mode . dash/org-src-block-background))

(use-package visual-fill-column
  :hook (org-mode . dash/org-mode-visual-fill))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))


;; LSP
(defun dash/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . dash/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))


;; Languages
(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred))

(use-package go-mode
  :ensure t
  :hook (python-mode . lsp-deferred))


;; Niceties
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; init.el ends here
