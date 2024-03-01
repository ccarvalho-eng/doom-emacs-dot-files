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
(setq doom-font (font-spec :family "MonacoB2" :size 10 :weight 'bold))

;; Set the default font size for Org mode with smaller sizes
(custom-set-faces!
  '(org-level-1 :height 1.1)
  '(org-level-2 :height 1.05)
  '(org-level-3 :height 1.0)
  ;; Add more lines for other heading levels if needed
  )

;; -----------------------------------------------------------------------------
;; Theme
;; -----------------------------------------------------------------------------
(setq doom-theme 'doom-one)

;; Set transparency to 95 percent
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

;; -----------------------------------------------------------------------------
;; Org configuration
;; -----------------------------------------------------------------------------
;; Define a variable to store the directory path for the journal files
(defvar my-journal-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Base Camp/org/journal/"
  "The directory where I store my journal files.")

;; Set the date format for org-journal entries
(setq org-journal-date-format "%a %b %e, %Y")

;; Set the file format for org-journal entries
(setq org-journal-file-format "%Y-%m-%d.org")

;; Set the default org directory
(setq org-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Base Camp/org/")

;; Set the org-journal directory to the journal directory
(after! org-journal
  (setq org-journal-dir my-journal-directory))

;; Set the default notes file to the journal directory
(setq org-default-notes-file my-journal-directory)

;; Set the agenda files to the journal directory
(setq org-agenda-files `(,my-journal-directory))

;; -----------------------------------------------------------------------------
;; Line numbers
;; -----------------------------------------------------------------------------
(setq display-line-numbers-type t)

;; -----------------------------------------------------------------------------
;; Frame configuration
;; -----------------------------------------------------------------------------
(add-to-list 'default-frame-alist '(width . 180))
(add-to-list 'default-frame-alist '(height . 63))

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

(defun elixir-mix-credo ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix credo")))

(defun elixir-mix-dialyzer ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix dialyzer")))

(defun elixir-mix-deps-compile ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix deps.compile")))

(defun elixir-mix-deps-get ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix deps.get")))

(defun elixir-mix-ecto-create ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix ecto.create")))

(defun elixir-mix-ecto-migrate ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix ecto.migrate")))

(defun elixir-mix-ecto-rollback ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix ecto.rollback")))

;; -----------------------------------------------------------------------------
;; Elixir keybindings
;; -----------------------------------------------------------------------------
(map! :mode elixir-mode
      :leader
      :desc "Sort Lines" :nve "l" #'sort-lines
      :desc "iMenu" :nve "c/" #'lsp-ui-imenu
      :desc "Toggle Test" :nve "cT" #'exunit-toggle-file-and-test
      :desc "IO.inspect/1" :nve "cI" #'elixir-append-inspect
      :desc "mix credo" :nve "mc" #'elixir-mix-credo
      :desc "mix dialyzer" :nve "md" #'elixir-mix-dialyzer
      :desc "mix deps.compile" :nve "mDc" #'elixir-mix-deps-compile
      :desc "mix deps.get" :nve "mDg" #'elixir-mix-deps-get
      :desc "mix ecto.create" :nve "meC" #'elixir-mix-ecto-create
      :desc "mix ecto.migrate" :nve "meM" #'elixir-mix-ecto-migrate
      :desc "mix ecto.rollback" :nve "meR" #'elixir-mix-ecto-rollback)

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
  (setq neo-theme (if (display-graphic-p) 'nerd 'arrow))
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
  (setq neo-show-updir-line nil)
  ;; Set folder color to purple
  (set-face-attribute 'neo-dir-link-face nil :foreground "#7c6d91")
  ;; Prevent text wrapping within Neotree
  (add-hook 'neo-after-create-hook
            (lambda (_)
              (setq truncate-lines t))))

;; Automatically resize Neotree window
(add-hook 'neo-after-create-hook
          (lambda (_)
            (let ((fit-window-to-buffer-horizontally t))
              (neo-buffer--with-resizable-window
               (fit-window-to-buffer)))))

;; Neotree keybindings
(map! :n "<f8>" #'neotree-toggle)

;; -----------------------------------------------------------------------------
;; Text wrapping
;; -----------------------------------------------------------------------------

;; Set fill-column to a reasonable value
(setq-default fill-column 80)

;; Enable auto-fill-mode to automatically wrap text at `fill-column`
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Enable visual line mode for visual line wrapping
(global-visual-line-mode 1)
