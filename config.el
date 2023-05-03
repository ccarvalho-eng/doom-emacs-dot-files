;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; -----------------------------------------------------------------------------
;; User configuration
;; -----------------------------------------------------------------------------
(setq user-full-name "Cristiano Carvalho"
      user-mail-address "cristiano.dev@icloud.com"
      projectile-project-search-path '("~/Projects"))

;; -----------------------------------------------------------------------------
;; Fonts
;; -----------------------------------------------------------------------------
(setq doom-font (font-spec :family "MonacoB" :size 10 :weight 'bold))

;; -----------------------------------------------------------------------------
;; Theme
;; -----------------------------------------------------------------------------
(setq doom-theme 'doom-one)

;; -----------------------------------------------------------------------------
;; Org configuration
;; -----------------------------------------------------------------------------
(setq org-directory "~/org/")

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
(after! lsp-clients
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
  ;; LSP UI
  (after! lsp-ui
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
(doom-themes-neotree-config)
(setq doom-themes-neotree-file-icons t)
