;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; -----------------------------------------------------------------------------
;; User configuration
;; -----------------------------------------------------------------------------
(setq user-full-name "Cristiano Carvalho"
      user-mail-address "cristiano.dev@icloud.com")

;; Projectile configuration
(use-package! projectile
  :config
  (setq projectile-project-search-path '("~/Projects")
        projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-sort-order 'recentf
        projectile-completion-system 'ivy)
  (projectile-mode +1))

;; Recent files configuration
(use-package! recentf
  :config
  (setq recentf-max-saved-items 50
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never
        recentf-exclude '(".gz" ".xz" ".zip" ".zst"))
  (recentf-mode +1))

;; Savehist configuration
(use-package! savehist
  :config
  (setq savehist-file (concat doom-cache-dir "savehist")
        savehist-additional-variables '(search-ring regexp-search-ring)
        savehist-autosave-interval 60)
  (savehist-mode +1))

;; -----------------------------------------------------------------------------
;; Fonts
;; -----------------------------------------------------------------------------
(setq doom-font (font-spec :family "MonacoB" :size 10 :weight 'bold))

;; -----------------------------------------------------------------------------
;; Theme
;; -----------------------------------------------------------------------------
(defvar my-current-theme 'doom-one
  "Current theme being used in Emacs.")

(defvar my-light-theme 'solo-jazz
  "Light theme to use during daytime.")

(defvar my-dark-theme 'doom-one
  "Dark theme to use during nighttime.")

(defun my-set-theme (theme)
  "Set the current THEME in Emacs."
  (when (member theme '(doom-one solo-jazz))
    (disable-theme my-current-theme)
    (load-theme theme t)
    (setq my-current-theme theme)
    (message "Switched to theme: %s" theme)))

(defun switch-dark-mode ()
  "Toggle between light and dark themes."
  (interactive)
  (if (eq my-current-theme my-light-theme)
      (my-set-theme my-dark-theme)
    (my-set-theme my-light-theme)))

(defun my-set-theme-for-time-of-day ()
  "Set the theme based on the time of day."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (if (and (>= hour 6) (<= hour 18))
        (my-set-theme my-light-theme)
      (my-set-theme my-dark-theme))))

(my-set-theme-for-time-of-day)

;; -----------------------------------------------------------------------------
;; Org configuration
;; -----------------------------------------------------------------------------
(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Documents")

;; -----------------------------------------------------------------------------
;; Line numbers
;; -----------------------------------------------------------------------------
(setq display-line-numbers-type t)

;; -----------------------------------------------------------------------------
;; Frame configuration
;; -----------------------------------------------------------------------------
(add-to-list 'default-frame-alist '(width . 210))
(add-to-list 'default-frame-alist '(height . 68))

;; -----------------------------------------------------------------------------
;; Keybindings
;; -----------------------------------------------------------------------------
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "s-s") 'save-buffer)

;; -----------------------------------------------------------------------------
;; macOS configuration
;; -----------------------------------------------------------------------------
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; -----------------------------------------------------------------------------
;; Elixir configuration
;; -----------------------------------------------------------------------------
(use-package! exunit)

;; -----------------------------------------------------------------------------
;; LSP configuration
;; -----------------------------------------------------------------------------
(use-package! lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l") ; Set prefix for lsp-command-keymap
  :config
  ;; Elixir LSP
  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-stdio-connection
                     (expand-file-name
                      "~/.elixir-ls/release/language_server.sh"))
                    :major-modes '(elixir-mode)
                    :priority -1
                    :server-id 'elixir-ls
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                        (let ((config `(:elixirLS
                                                        (:mixEnv "dev"
                                                                 :dialyzerEnabled
                                                                 :json-false))))
                                          (lsp--set-configuration config)))))))

  (setq lsp-enable-file-watchers nil
        lsp-enable-text-document-color nil
        lsp-enable-on-type-formatting nil
        lsp-enable-symbol-highlighting nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-lens-enable nil)

(use-package! lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-max-height 20
        lsp-ui-doc-max-width 80
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-use-webkit nil
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-kind-position 'left
        lsp-ui-sideline-code-actions-prefix "💡"
        company-lsp-match-candidate-predicate #'company-lsp-match-candidate-prefix))

(use-package! lsp-elixir
  :defer t
  :hook (elixir-mode . lsp))

;; -----------------------------------------------------------------------------
;; Optional: lsp-treemacs integration
;; -----------------------------------------------------------------------------
(use-package! lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

;; -----------------------------------------------------------------------------
;; Folding configuration
;; -----------------------------------------------------------------------------
(setq lsp-enable-folding t)
(use-package! lsp-origami)
(add-hook! 'lsp-after-open-hook #'lsp-origami-try-enable)

;; -----------------------------------------------------------------------------
;; Custom functions
;; -----------------------------------------------------------------------------
(defun elixir-append-inspect()
  (interactive)
  (evil-append-line nil)
  (insert " |> IO.inspect")
  (evil-normal-state))

(defun my-elixir-mix-credo ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix credo")))

(defun my-elixir-mix-dialyzer ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix dialyzer")))

;; -----------------------------------------------------------------------------
;; Elixir keybindings
;; -----------------------------------------------------------------------------
(map! :mode elixir-mode
      :leader
      :desc "Sort Lines" :nve "l" #'sort-lines
      :desc "iMenu" :nve "c/" #'lsp-ui-imenu
      :desc "Toggle Test" :nve "cT" #'exunit-toggle-file-and-test
      :desc "Inspect" :nve "cI" #'elixir-append-inspect
      :desc "Mix Credo" :nve "mc" #'my-elixir-mix-credo
      :desc "Mix Dialyzer" :nve "md" #'my-elixir-mix-dialyzer)

;; -----------------------------------------------------------------------------
;; LSP mode file watch ignored
;; -----------------------------------------------------------------------------
(after! lsp-mode
  (dolist (match
           '("[/\\\\].direnv$"
             "[/\\\\]node_modules$"
             "[/\\\\]deps"
             "[/\\\\]priv"
             "[/\\\\]build"
             "[/\\\\]_build"))
    (add-to-list 'lsp-file-watch-ignored match)))

;; -----------------------------------------------------------------------------
;; Keychain refresh
;; -----------------------------------------------------------------------------
(keychain-refresh-environment)

;; -----------------------------------------------------------------------------
;; Neotree configuration
;; -----------------------------------------------------------------------------
(use-package! neotree
  :defer t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (setq neo-window-width 30)
  (setq neo-window-fixed-size nil)
  (setq neo-mode-line-type 'neotree)
  (setq neo-show-hidden-files t)
  (setq neo-create-file-auto-open t)
  (setq neo-banner-message nil)
  (setq neo-auto-indent-point t)
  (setq neo-keymap-style 'concise)
  (doom-themes-neotree-config)
  (setq doom-themes-neotree-file-icons t)
  (setq neo-show-updir-line nil))

;; Automatically resize Neotree window
(add-hook 'neo-after-create-hook
          (lambda (_)
            (let ((fit-window-to-buffer-horizontally t))
              (neo-buffer--with-resizable-window
               (fit-window-to-buffer)))))

;; Neotree keybindings
(map! :n "<f8>" #'neotree-toggle)
