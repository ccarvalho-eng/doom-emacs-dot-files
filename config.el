;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; -----------------------------------------------------------------------------
;; User configuration
;; -----------------------------------------------------------------------------
(setq user-full-name "Cristiano Carvalho"
      user-mail-address "ccarvalho.infosec@icloud.com")

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
(setq doom-font (font-spec :family "MonacoB2" :size 9.5 :weight 'bold))

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

(custom-set-faces!
  `(font-lock-comment-face :slant italic)
  `(font-lock-doc-face :slant italic))

;; -----------------------------------------------------------------------------
;; Org configuration
;; -----------------------------------------------------------------------------
;; Define a variable to store the directory path for org-mode files
(defvar my-org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/org-notes/"
  "The directory where I store my org files.")

;; Define a variable to store the directory path for the journal files
(defvar my-journal-directory (concat my-org-directory "journal/")
  "The directory where I store my journal files.")

;; Define a variable to store the directory path for the templates
(defvar my-templates-directory (concat my-org-directory "templates/")
  "The directory where I store my templates.")

;; Set the date format for org-journal entries
(setq org-journal-date-format "%a %e %b, %Y")

;; Set the default org directory
(setq org-directory my-org-directory)

;; Set the org-journal directory to the journal directory
(after! org-journal
  (setq org-journal-dir my-journal-directory))

;; Set the default notes file to the journal directory
(setq org-default-notes-file my-org-directory)

;; Define a function to recursively find org files in a directory
(defun find-org-files-recursively (directory)
  "Find all .org files recursively within DIRECTORY."
  (directory-files-recursively directory "\\.org$"))

;; Set the agenda files to include both org and journal directories and subdirectories
(setq org-agenda-files
      (append
       (find-org-files-recursively my-org-directory)))

(setq org-roam-directory (concat my-org-directory "train-of-thought"))

;; Include org agenda diary
(setq org-agenda-include-diary t)

(defun org-journal-daily-entry ()
  "Create a new daily journal entry."
  (interactive)
  (setq org-journal-dir (concat my-journal-directory "daily/"))
  (setq org-journal-date-format "%a %e %b, %Y")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-file-type 'daily)
  (org-journal-new-entry nil)
  (org-journal-load-template "daily"))

(defun org-journal-weekly-entry ()
  "Create a new weekly journal entry."
  (interactive)
  (setq org-journal-dir (concat my-journal-directory "weekly/"))
  (setq org-journal-date-format "Week %V, %Y")
  (setq org-journal-file-format "%Y-W%V.org")
  (setq org-journal-file-type 'weekly)
  (org-journal-new-entry nil)
  (org-journal-load-template "weekly"))

(defun org-journal-monthly-entry ()
  "Create a new monthly journal entry."
  (interactive)
  (setq org-journal-dir (concat my-journal-directory "monthly/"))
  (setq org-journal-date-format "%B %Y")
  (setq org-journal-file-format "%Y-%m.org")
  (setq org-journal-file-type 'monthly)
  (org-journal-new-entry nil)
  (org-journal-load-template "monthly"))

(defun org-journal-quarterly-entry ()
  "Create a new quarterly journal entry."
  (interactive)
  (setq org-journal-dir (concat my-journal-directory "quarterly/"))
  (setq org-journal-date-format "Quarter %q, %Y")
  (setq org-journal-file-format "%Y-Q%q.org")
  (setq org-journal-file-type 'quarterly)
  (org-journal-new-entry nil)
  (org-journal-load-template "quarterly"))

(defun org-journal-yearly-entry ()
  "Create a new yearly journal entry."
  (interactive)
  (setq org-journal-dir (concat my-journal-directory "yearly/"))
  (setq org-journal-date-format "%Y")
  (setq org-journal-file-format "%Y.org")
  (setq org-journal-file-type 'yearly)
  (org-journal-new-entry nil)
  (org-journal-load-template "yearly"))

(defun org-journal-load-template (type)
  "Load a template for the journal entry of TYPE."
  (let ((template-file (concat my-templates-directory type ".org")))
    (when (file-exists-p template-file)
      (insert-file-contents template-file))))

(map! :leader
      :desc "Daily journal entry" "n j d" #'org-journal-daily-entry
      :desc "Weekly journal entry" "n j w" #'org-journal-weekly-entry
      :desc "Monthly journal entry" "n j m" #'org-journal-monthly-entry
      :desc "Quarterly journal entry" "n j q" #'org-journal-quarterly-entry
      :desc "Yearly journal entry" "n j y" #'org-journal-yearly-entry)

(global-set-key (kbd "C-c r u") 'org-roam-ui-open)

;; -----------------------------------------------------------------------------
;; Line numbers
;; -----------------------------------------------------------------------------
(setq display-line-numbers-type t)

;; -----------------------------------------------------------------------------
;; Frame configuration
;; -----------------------------------------------------------------------------
(add-to-list 'default-frame-alist '(width . 220))
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
      :desc "mix dialyzer" :nve "mdy" #'elixir-mix-dialyzer
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
  ;; Theme and appearance settings
  (setq neo-theme (if (display-graphic-p) 'nerd 'arrow))
  (setq neo-window-width 30)
  (setq neo-window-fixed-size nil)
  (setq neo-mode-line-type 'neotree)
  (setq neo-show-hidden-files t)
  (setq neo-create-file-auto-open t)
  (setq neo-banner-message nil)
  (setq neo-auto-indent-point t)
  (setq neo-keymap-style 'concise)
  (setq neo-window-position 'left)

  ;; Auto-reveal current file
  (setq neo-smart-open t)

  ;; Custom icons from Doom themes
  (doom-themes-neotree-config)
  (setq doom-themes-neotree-file-icons t)

  ;; Prevent text wrapping within NeoTree
  (add-hook 'neo-after-create-hook
            (lambda (_)
              (setq truncate-lines t)))

  ;; Ignore files/directories
  (setq neo-hidden-regexp-list
        '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$"
          "__pycache__" ".vscode" ".git" ".DS_Store"))

  ;; Auto-resize NeoTree window
  (add-hook 'neo-after-create-hook
            (lambda (_)
              (let ((fit-window-to-buffer-horizontally t))
                (neo-buffer--with-resizable-window
                 (fit-window-to-buffer)))))

  ;; Keybindings
  (map! :leader
        :desc "Toggle NeoTree" :n "<f8>" #'neotree-toggle))

;; Automatically toggle NeoTree when opening a new frame
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (if (display-graphic-p frame)
                (neotree-toggle))))

;; -----------------------------------------------------------------------------
;; Text wrapping
;; -----------------------------------------------------------------------------
;; Set fill-column to a reasonable value
(setq-default fill-column 80)

;; Enable auto-fill-mode to automatically wrap text at `fill-column`
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Enable visual line mode for visual line wrapping
(global-visual-line-mode 1)


;; -----------------------------------------------------------------------------
;; Multiple cursors keybindings
;; -----------------------------------------------------------------------------
(map! :leader
      (:prefix "m"
        :desc "Edit lines" "l" #'mc/edit-lines
        :desc "Add cursor to all" "a" #'mc/mark-all-like-this))

;; -----------------------------------------------------------------------------
;; Permanently display workspace tabs
;; -----------------------------------------------------------------------------
(after! persp-mode
  ;; alternative, non-fancy version which only centers the output of +workspace--tabline
  (defun workspaces-formatted ()
    (+doom-dashboard--center (frame-width) (+workspace--tabline)))

  (defun hy/invisible-current-workspace ()
    "The tab bar doesn't update when only faces change (i.e. the
current workspace), so we invisibly print the current workspace
name as well to trigger updates"
    (propertize (safe-persp-name (get-current-persp)) 'invisible t))

  (customize-set-variable 'tab-bar-format '(workspaces-formatted tab-bar-format-align-right hy/invisible-current-workspace))

  ;; don't show current workspaces when we switch, since we always see them
  (advice-add #'+workspace/display :override #'ignore)
  ;; same for renaming and deleting (and saving, but oh well)
  (advice-add #'+workspace-message :override #'ignore))

;; need to run this later for it to not break frame size for some reason
(run-at-time nil nil (cmd! (tab-bar-mode +1)))

;; -----------------------------------------------------------------------------
;; Major mode for the hurl restclient
;; -----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.hurl\\'" . hurl-mode))

;; -----------------------------------------------------------------------------
;; Path to ob-mermaid package
;; -----------------------------------------------------------------------------
(setq ob-mermaid-cli-path "/Users/cristianocarvahlo/.asdf/shims/mmdc")
