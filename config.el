;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;
;;

;; Setup ENV for MAC
(when (memq window-system '(mac ns x))
 (exec-path-from-shell-initialize))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Cristiano Carvalho"
      user-mail-address "cristiano.dev@icloud.com"
      projectile-project-search-path '("~/Projects")
      )

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "MonacoB" :size 10 :weight 'bold))

;; Elixir LSP
(after! lsp-clients
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

;; Enable LSP UI documentation
(after! lsp-ui
  (setq lsp-ui-doc-enable t))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'solo-jazz)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq display-line-numbers-type nil)
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Sort keys
(setq which-key-sort-order 'which-key-key-order-alpha)

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(width . 210))
(add-to-list 'default-frame-alist '(height . 68))

(defun elixir-append-inspect()
  (interactive)
  (evil-append-line nil)
  (insert " |> IO.inspect")
  (evil-normal-state)
)

(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "s-s") 'save-buffer)

;; Configure exunit
(use-package! exunit)

;; Setup some keybindings for exunit and lsp-ui
(map! :mode elixir-mode
      :leader
      :desc "Sort Lines" :nve  "l"    #'sort-lines
      :desc "iMenu" :nve  "c/"    #'lsp-ui-imenu
      :desc "Toggle Test" :nve  "cT"    #'exunit-toggle-file-and-test
      :desc "Inspect" :nve  "cI"    #'elixir-append-inspect)

(after! lsp-mode
  (dolist (match
           '("[/\\\\].direnv$"
             "[/\\\\]node_modules$"
             "[/\\\\]deps"
             "[/\\\\]priv"
             "[/\\\\]build"
             "[/\\\\]_build"))
    (add-to-list 'lsp-file-watch-ignored match)))

(keychain-refresh-environment)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(doom-themes-neotree-config)
(setq doom-themes-neotree-file-icons t)

;; Enable folding
(setq lsp-enable-folding t)

;; Add origami and LSP integration
(use-package! lsp-origami)
(add-hook! 'lsp-after-open-hook #'lsp-origami-try-enable)

(use-package! chatgpt
  :defer t
  :config
  (unless (boundp 'python-interpreter)
    (defvaralias 'python-interpreter 'python-shell-interpreter))
  (setq chatgpt-repo-path (expand-file-name "straight/repos/ChatGPT.el/" doom-local-dir))
  (set-popup-rule! (regexp-quote "*ChatGPT*")
    :side 'bottom :size .5 :ttl nil :quit t :modeline nil)
  :bind ("C-c q" . chatgpt-query))

(setq chatgpt-query-format-string-map '(
                                        ;; ChatGPT.el defaults
                                        ("doc" . "Please write the documentation for the following function.\n\n%s")
                                        ("bug" . "There is a bug in the following function, please help me fix it.\n\n%s")
                                        ("understand" . "What does the following function do?\n\n%s")
                                        ("improve" . "Please improve the following code.\n\n%s")))
