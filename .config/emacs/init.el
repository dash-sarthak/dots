;; Don't load default emacs config
(setq inhibit-default-init 1)

;; Write customizations to custom.el
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)


;; UI Cleanup (setq inhibit-startup-message)
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
(set-face-attribute 'fixed-pitch nil
		    :font "JetBrainsMono Nerd Font"
		    :weight 'regular
		    :height 132)

(set-face-attribute 'variable-pitch nil
		    :font "Inter"
		    :weight 'regular
		    :height 144)

(set-face-attribute 'mode-line nil :weight 'medium :height 120)

;; Theme
;; Configure the Modus Themes' appearance
(setq modus-themes-mode-line '(borderless)
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-fringes 'subtle
      modus-themes-paren-match '(bold intense)
      modus-themes-prompts '(bold intense)
      modus-themes-completions '(extrabold intense)
      modus-themes-scale-headings t
      ;; modus-themes-region '(bg-only)
      modus-themes-headings
      '((1 . (rainbow underline))
        (2 . (rainbow))
        (3 . (rainbow))))
;; Load the dark theme
(load-theme 'modus-vivendi t)

;; Line and column numbers
(column-number-mode)                ; Show column numbers
(global-display-line-numbers-mode t) ; Show line numbers globally

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Initialize package sources --------------------------------------------------
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/")
                         ("elpa" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t)


;; Essential packages ----------------------------------------------------------
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
(use-package vterm-toggle)
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


;; Leader keybindings ----------------------------------------------------------
(dash/leader-key
  "p" 'projectile-command-map
  "t"  '(:ignore t :which-key "toggles")
  "t <escape>" '(keyboard-escape-quit :which-key t)
  "tt" '(counsel-load-theme :which-key "choose theme")
  "RET" '(vterm-toggle :which-key "terminal")
  "bb" '(counsel-switch-buffer :which-key "active buffers")
  )


;; Org Mode Configuration ------------------------------------------------------
(defun dash/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . dash/org-mode-setup)
  :config
  (setq org-ellipsis " … ")

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "●" "○" "●" "○" "●" "○")))

(defun dash/org-mode-visual-fill ()
  (setq visual-fill-column-width 80
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . dash/org-mode-visual-fill))


;; LSP -------------------------------------------------------------------------
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))


