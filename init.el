(setq straight-use-package-by-default t) ;; have use-package use straight.el by default.

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

(straight-use-package 'use-package) ;; install use-package via straight

(use-package emacs
  :init
  ;; my details
  (setq user-full-name "Patrick D. Elliott") 
  (setq user-mail-address "patrick.d.elliott@gmail.com")

  (defalias 'yes-or-no-p 'y-or-n-p) ;; life is too short

  ;; stop emacs from littering
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq create-lockfiles nil)

  ;; saves customizations made via the Customize mode in a different file.
  ;; (borrowed from Pat Mike's config)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file)

  ;; utf-8 everywhere
  ;; (borrowed from Pat Mike's config)
  (set-language-environment "UTF-8")
  (setq locale-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  (setq delete-by-moving-to-trash t) ;; use trash-cli rather than rm when deleting files.

  ;; ignores warnings during native compilation
  ;; (borrowed from Pat Mike's config)
  (setq warning-minimum-level :error)
  )

(use-package electric
  :straight (:type built-in)
  :init
  (electric-pair-mode +1) ;; automatically insert closing parens 
  (setq electric-pair-preserve-balance nil)
  )

(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer patrl/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC"
    )

  ;; set up ',' as the local leader key
  (general-create-definer patrl/local-leader-keys
    :states '(normal visual)
    :keymaps 'override
    :prefix ","
    :global-prefix "SPC m"
    )

  (patrl/leader-keys
    "SPC" '(execute-extended-command :wk "execute command")
    "." '(find-file :wk "find file")
    "TAB" '(:keymap tab-prefix-map :wk "tab") ;; remap tab bindings
    "h" '(:keymap help-map :wk "help") ;; remap help bindings
    )

  ;; file bindings
  (patrl/leader-keys
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find file") ;; gets overridden by consult
    "fd" '(find-file :wk "dired")
    )

  ;; buffer bindings
  (patrl/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "switch buffer") ;; gets overridden by consult
    "bk" '(kill-this-buffer :wk "kill this buffer")
    "br" '(revert-buffer :wk "reload buffer")
    )

  (patrl/leader-keys
    "u" '(universal-argument :wk "universal prefix")
    )

  ;; notes bindings
  (patrl/leader-keys
   "n" '(:ignore t :wk "notes") ;; see org-roam and citar sections
   )

  ;; open bindings
  (patrl/leader-keys
   "o" '(:ignore t :wk "open")
   )

  ;; search bindings
  (patrl/leader-keys
   "s" '(:ignore t :wk "search")
   )
  )

(use-package evil
  :general
  (patrl/leader-keys
   "w" '(:keymap evil-window-map :wk "window") ;; window bindings
   )
  :init
  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'

  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  (setq evil-split-window-below t)
  (setq evil-split-window-right t)
  :config
  (evil-mode t)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t) ;; '<TAB>' cycles visibility in 'outline-minor-mode'
  ;; (setq evil-collection-mode-list nil) ;; I don't like surprises
  ;; (add-to-list 'evil-collection-mode-list 'magit) ;; evilify magit
  ;; (add-to-list 'evil-collection-mode-list '(pdf pdf-view)) ;; evilify pdf-view
  :config
  (evil-collection-init))

;; port of Tim Pope's commentary package
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; port of Tim Pope's surround package
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; show visual hints for evil motions
(use-package evil-goggles
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package which-key
  :after evil
  :init (which-key-mode)
  :config
  (which-key-setup-minibuffer))

(use-package olivetti
  :init
  (setq olivetti-body-width .67))

(use-package mood-line
  :config (mood-line-mode))

(use-package emacs
  :init
  (set-face-attribute 'default nil :font "Cascadia Code-12")
  (add-to-list 'default-frame-alist '(font . "Cascadia Code-12"))
  )

(use-package solaire-mode
       :config
       (solaire-global-mode +1))

(use-package tron-legacy-theme
       :config
       (setq tron-legacy-theme-vivid-cursor t))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

(use-package hl-todo
  :init
  (global-hl-todo-mode))

(use-package tab-bar
  :init (tab-bar-mode)
  :straight (:type built-in))

;; let's see how long I can go without projectile
(use-package project
  :general
  (patrl/leader-keys
    "p" '(:keymap project-prefix-map :wk "project")
    )
  :straight (:type built-in))

;; automatically organize projects
(use-package project-tab-groups
  :after (project tab-bar)
  :config
  (project-tab-groups-mode 1))

(use-package dired
  :straight (:type built-in))

;; FIXME using the latest version of org results in an error
(use-package org
  :straight (:type built-in)
  :init
  (setq org-src-fontify-natively t) ;; fontify code in src blocks
  (setq org-adapt-indentation nil) ;; interacts poorly with 'evil-open-below'
  :custom
  (org-agenda-files '("~/Dropbox (MIT)/org/agenda" "~/notes/daily"))
  :general
  (patrl/local-leader-keys
    :keymaps 'org-mode-map
    "l" '(:ignore t :wk "link")
    "ll" '(org-insert-link t :wk "link")
    "s" '(consult-org-heading :wk "consult heading")
    "b" '(:keymap org-babel-map :wk "babel")
    "t" '(org-insert-structure-template :wk "template")
    "e" '(org-edit-special :wk "edit")
    "i" '(:ignore t :wk "insert")
    "ih" '(org-insert-heading :wk "insert heading")
    "is" '(org-insert-subheading :wk "insert heading")
    )
  :hook
  (org-mode . visual-line-mode)
  (org-mode . (lambda () (electric-indent-local-mode -1))) ;; disable electric indentation
  ;; :config
  ;; (add-to-list 'org-modules 'org-tempo t) ;; enables auto-expansion for templates
  )

(use-package org-cliplink
  :after org
  :general
  (patrl/local-leader-keys
    :keymaps 'org-mode-map 
    "lc" '(org-cliplink :wk "cliplink")
    )
  )

(use-package org-superstar
  :after org
  :hook
  (org-mode . (lambda () (org-superstar-mode 1))))

(use-package org-roam
  :general
  (patrl/leader-keys
    "nr" '(:ignore t :wk "roam")
    "nrf" '(org-roam-node-find :wk "find")
    "nrd" '(:ignore t :wk "dailies")
    "nrdt" '(org-roam-dailies-goto-today :wk "today")
    "nrdt" '(org-roam-dailies-goto-yesterday :wk "today")
    "nrdT" '(org-roam-dailies-goto-tomorrow :wk "today")
    )
  :init
  (setq org-roam-v2-ack t) ;; disables v2 warning
  :config
  (setq org-roam-directory (file-truename "~/notes"))
  (org-roam-db-autosync-enable)
  )

(use-package haskell-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package auctex-latexmk
  :after latex
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  )

(use-package latex
  :straight auctex ;; if this isn't set to true, error!
  :init
  ;; automatically enables outline mode
  ;; this means I can use '<TAB>' to cycle visibility
  ;; just like in org-mode
  (add-hook 'LaTeX-mode-hook #'outline-minor-mode)
  (add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode) ;; necessary for synctex
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :general
  (patrl/local-leader-keys
    :keymaps 'LaTeX-mode-map
    "i" '(:ignore t :wk "insert")
    "ie" '(LaTeX-environment :wk "insert environment")
    "im" '(LaTeX-macro :wk "insert macro")
    "is" '(LaTeX-section :wk "insert section header")
    "p" '(:ignore t :wk "preview")
    "ps" '(preview-section :wk "preview section")
    )
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
  )

(use-package cdlatex)

(use-package pdf-tools
  :config
  (pdf-tools-install)
  )

(use-package citar
  :general
  (patrl/leader-keys
    "nb" '(citar-insert-citation :wk "citar")
    )
  :custom
  (citar-library-paths '("~/Dropbox (MIT)/library"))
  (citar-bibliography '("~/repos/bibliography/master.bib"))
  )

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode))
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package vertico
  :init (vertico-mode)
  (setq vertico-cycle t) ;; enable cycling for 'vertico-next' and 'vertico-prev'
  :general
  (:keymaps 'vertico-map
	    ;; keybindings to cycle through vertico results.
	    "C-j" 'vertico-next
	    "C-k" 'vertico-previous
	    "C-f" 'vertico-exit)
  (:keymaps 'minibuffer-local-map
	    "M-h" 'backward-kill-word)
  )

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :general
  (patrl/leader-keys
    "bb" '(consult-buffer :wk "consult buffer")
    "ht" '(consult-theme :wk "consult theme")
    "sr" '(consult-ripgrep :wk "consult rg")
    "sg" '(consult-grep :wk "consult grep")
    "sG" '(consult-git-grep :wk "consult git grep")
    "sf" '(consult-find :wk "consult find")
    "sF" '(consult-locate :wk "consult locate")
    "sl" '(consult-line :wk "consult line")
    )
  )

(use-package embark
  :general
  (
   "C-." 'embark-act
   "C-;" 'embark-dwim
   )
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  )

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package company
  :custom
  (company-idle-delay nil) ;; turn off auto-completion
  :general
  (:keymap 'company-mode-map
	    "C-SPC" 'company-complete) ;; hit TAB to trigger company completion
  :hook
  (prog-mode . company-mode)
  (LaTeX-mode . company-mode)
  )

(use-package company-bibtex
  :init
  (setq company-bibtex-bibliography
	'("/home/patrl/repos/bibliography/master.bib"))
  :after company
  :config
  (add-to-list 'company-backends 'company-bibtex)
  )

;; (use-package bufler
;;   :general
;;   (patrl/leader-keys
;;     "bB" '(bufler-switch-buffer :wk "bufler switch") 
;;     "bw" '(bufler-workspace-frame-set :wk "bufler workspace focus") 
;;     )
;;   :config
;;   (bufler-mode)
;;   (bufler-tabs-mode))

(use-package magit
  :general
  (patrl/leader-keys
    "g" '(:ignore t :wk "git")
    "gg" '(magit-status :wk "status")
    )
)

(use-package eshell
  :straight (:type built-in)
  :general
  (patrl/leader-keys
    "oe" '(eshell :wk "eshell")
    )
  )

(use-package flymake
  :straight (:type built-in)

  :hook
  (emacs-lisp-mode . flymake-mode)
  (LaTeX-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout nil)
  :general
  (general-nmap "] !" 'flymake-goto-next-error)
  (general-nmap "[ !" 'flymake-goto-prev-error)
  )

(use-package lsp-mode
  :hook
  (haskell-mode . lsp)
  (haskell-literate-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands
  lsp
)

(use-package lsp-ui
  :commands lsp-ui-mode
  )

(use-package direnv
  :config
  (direnv-mode))

(use-package deadgrep
  :general
  (patrl/leader-keys
    "sd" '(deadgrep :wk "deadgrep")
    )
  )

;;   (use-package aas
;;     :hook (LaTeX-mode . aas-activate-for-major-mode)
;;     :hook (org-mode . aas-activate-for-major-mode)
;;     :config
;;     (aas-set-snippets 'latex-mode
;;       ;; set condition!
;;       :cond #'texmathp ; expand only while in math
;;       ";l" "λ"
;;       ";a" "α"
;;       ";b" "β"
;;       "\\rr" "→"
;;       "\\lr" "←"
;;       "\\all" "∀"
;;       "\\ex" "∃"
;;     ;; disable snippets by redefining them with a nil expansion
;;   )
;; )
