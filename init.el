;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-
;; NOTE: init.el is now generated from readme.org.  Please edit that file instead

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

  (setq indent-tabs-mode nil) ;; tabs are evil

  ;; stop emacs from littering
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq create-lockfiles nil)

  ;; utf-8 everywhere
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; escape everywhere
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; Don't persist a custom file
  (setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
  (setq custom-safe-themes t)            ; mark all themes as safe, since we can't persist now
  (setq enable-local-variables :all)     ; fix =defvar= warnings

  (setq delete-by-moving-to-trash t) ;; use trash-cli rather than rm when deleting files.

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)

  (show-paren-mode t)
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
    :global-prefix "M-SPC" ;; access leader in insert mode
    )

  ;; set up ',' as the local leader key
  (general-create-definer patrl/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix ","
    :global-prefix "M-SPC m" ;; access local leader in insert mode
    )

  ;; unbind some useless bindings
  (general-unbind
    "C-x C-r" ;; unbind find file read only
    "C-x C-z" ;; unbind suspend frame
    "C-x C-d" ;; unbind list directory
    "<mouse-2>" ;; pasting with mouse wheel click
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
    "na" '(org-todo-list :wk "agenda todos") ;; agenda
    ;; TODO hack on agenda bindings
    )

  ;; code bindings
  (patrl/leader-keys
    "c" '(:ignore t :wk "code") ;; see flymake
    )

  ;; open bindings
  (patrl/leader-keys
    "o" '(:ignore t :wk "open")
    "os" '(speedbar t :wk "speedbar")
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

(use-package all-the-icons
  :if (display-graphic-p))


(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode)
  )

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
  :general
  (patrl/leader-keys
    "fd" '(dired :wk "dired")
    "fj" '(dired-jump :wk "dired jump"))
  ;; ranger like navigation
  (:keymaps 'dired-mode-map
            :states 'normal
            "h" 'dired-up-directory
            "q" 'kill-current-buffer
            "l" 'dired-find-file
            )
  :hook
  (dired-mode . dired-hide-details-mode) ;; no thanks
  :straight (:type built-in))

;; toggle subtree visibility with 'TAB'
;; makes dired a much more pleasant file manager
(use-package dired-subtree)

;; FIXME using the latest version of org results in an error
(use-package org
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
    :keymaps 'org-agenda-mode-map
    "j" '(org-agenda-next-line)
    "h" '(org-agenda-previous-line)
    )
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode)
  (org-mode . (lambda () (electric-indent-local-mode -1))) ;; disable electric indentation
  :config
  ;; hack until straight.el builds org-mode properly
  (defun org-git-version () "9.5")
  (defun org-release () "9.5")
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

(use-package racket-mode
  :hook (racket-mode . racket-xp-mode) ;; n.b. this requires Dr. Racket to be installed as a backend
  :general
  (patrl/leader-keys
    :keymaps 'racket-mode-map
    "cr" 'racket-run-and-switch-to-repl  
    )
  )

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package auctex
  :no-require t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (setq TeX-parse-self t ; parse on load
      TeX-auto-save t  ; parse on save
      TeX-source-correlate-mode t
      TeX-source-correlate-method 'synctex
      TeX-source-correlate-start-server nil
      TeX-electric-sub-and-superscript t
      TeX-save-query nil) 
      )

(use-package tex
  :straight auctex
  :general
  (patrl/local-leader-keys
    :keymaps 'LaTeX-mode-map
    "i" '(:ignore t :wk "insert")
    "ie" '(LaTeX-environment :wk "insert environment")
    "im" '(LaTeX-macro :wk "insert macro")
    "is" '(LaTeX-section :wk "insert section header")
    "p" '(:ignore t :wk "preview")
    "ps" '(preview-section :wk "preview section")
    "f" '(TeX-font :wk "font")
    )
  :config
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  (add-hook 'TeX-mode-hook #'prettify-symbols-mode)
  (add-hook 'TeX-after-compilation-finished-functions
              #'TeX-revert-document-buffer)
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
  (add-hook 'TeX-mode-hook #'outline-minor-mode)
  )

(use-package auctex-latexmk
  :after latex 
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup)
  )

(use-package pdf-tools
  :config
  (pdf-tools-install)
  )

(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode))

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
  :init (setq markdown-command "pandoc"))

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
	   "C-SPC" 'company-complete) ;; keybinding to trigger company completion
  :hook
  (prog-mode . company-mode)
  (LaTeX-mode . company-mode)
  :config
  ;; the following stops company from using the orderless completion style
  ;; makes company much more useful
  (define-advice company-capf
      (:around (orig-fun &rest args) set-completion-styles)
    (let ((completion-styles '(basic partial-completion)))
      (apply orig-fun args)))
  )

(use-package company-bibtex
  :init
  (setq company-bibtex-bibliography
	'("/home/patrl/repos/bibliography/master.bib"))
  :after company
  :config
  (add-to-list 'company-backends 'company-bibtex)
  )

(use-package flymake
  :straight (:type built-in)
  :general
  (patrl/leader-keys
    :keymaps 'flymake-mode-map
    "cf" '(consult-flymake :wk "consult flymake") ;; depends on consult
    )
  ;; :hook
  ;; (emacs-lisp-mode . flymake-mode)
  ;; (LaTeX-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout nil)
  :general
  (general-nmap "] !" 'flymake-goto-next-error)
  (general-nmap "[ !" 'flymake-goto-prev-error)
  )

(use-package flymake-aspell
  :hook
  (text-mode-hook . flymake-aspell-setup)
  (prog-mode-hook . flymake-aspell-setup))

(use-package ispell
  :straight (:type built-in)
  :init
  (setq ispell-dictionary "en_US")
  (setq ispell-program-name "aspell")
  (setq ispell-silently-savep t)
)

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

(use-package lsp-mode
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands
  lsp
)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  )

(use-package lsp-haskell
  :after lsp-mode
  :init
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)
  :config
  (setq lsp-haskell-server-path "haskell-language-server") ;; for some reason this doesn't get found automatically
  ;; (setq lsp-haskell-formatting-provider "brittany")
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

;; (use-package bufler
;;   :general
;;   (patrl/leader-keys
;;     "bB" '(bufler-switch-buffer :wk "bufler switch") 
;;     "bw" '(bufler-workspace-frame-set :wk "bufler workspace focus") 
;;     )
;;   :config
;;   (bufler-mode)
;;   (bufler-tabs-mode))

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'LaTeX-mode-hook #'yas-minor-mode)
)

(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :hook (org-mode . aas-activate-for-major-mode)
  )

(use-package laas
  :hook (LaTeX-mode . laas-mode)
  :config
  (aas-set-snippets 'laas-mode
    "mk" (lambda () (interactive)
            (yas-expand-snippet "\\\\($0\\\\)"))
    "dm" (lambda () (interactive)
            (yas-expand-snippet "\\[\n$0\n\\]"))
    "ii" (lambda () (interactive)
            (yas-expand-snippet "\\begin{itemize}\n\\item $0\n\\end{itemize}"))
    "iee" (lambda () (interactive)
            (yas-expand-snippet "\\begin{enumerate}\n\\item $0\n\\end{enumerate}"))
    "exex" (lambda () (interactive)
            (yas-expand-snippet "\\ex\n$0\n\\xe"))
    "forfor" (lambda () (interactive)
            (yas-expand-snippet "\\begin{forest}\n[{$1}\n[{$2}]\n[{$0}]\n]\n\\end{forest}"))
    :cond #'texmathp ; expand only while in math 
    "Olon" "O(n \\log n)"
    ;; bind to functions!
    "sum" (lambda () (interactive)
            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
    "Span" (lambda () (interactive)
             (yas-expand-snippet "\\Span($1)$0"))
    "lam" (lambda () (interactive)
            (yas-expand-snippet "\\lambda $1_{$2}\\,.\\,$0"))
    "set" (lambda () (interactive)
              (yas-expand-snippet "\\set{ $1 | $2} $0"))
    "ml" (lambda () (interactive)
              (yas-expand-snippet "\\text{$1} $0"))
    "ev" (lambda () (interactive)
                (yas-expand-snippet "\\left\\llbracket$3\\right\\rrbracket^$1_$2 $3"))
    ;; add accent snippets
    :cond #'laas-object-on-left-condition
    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
    ))

(use-package tree-sitter)

(use-package tree-sitter-langs)

;; (use-package cdlatex)
