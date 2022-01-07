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
  (setq user-full-name "Patrick D. Elliott") ;; my details
  (setq user-mail-address "patrick.d.elliott@gmail.com")

  (defalias 'yes-or-no-p 'y-or-n-p) ;; life is too short

  (setq indent-tabs-mode nil) ;; tabs are evil

  (setq make-backup-files nil) ;; no littering
  (setq auto-save-default nil)
  (setq create-lockfiles nil)

  (set-charset-priority 'unicode) ;; utf8 in every nook and cranny
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; escape quits everything

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

  ;; Hide commands in M-x which don't work in the current mode
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  )

(use-package electric
  :straight (:type built-in)
  :init
  (electric-pair-mode +1) ;; automatically insert closing parens 
  (setq electric-pair-preserve-balance nil) ;; more annoying than useful
  )

(use-package general
  :config
  (general-evil-setup) ;; integrate general with evil

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

  (general-define-key
   :states 'insert
   "C-g" 'evil-normal-state ;; don't stretch for ESC
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
    )

  ;; help
  (patrl/leader-keys
    "h" '(:ignore t :wk "help")
                )

  ;; file
  (patrl/leader-keys
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find file") ;; gets overridden by consult
    "fs" '(save-buffer :wk "save file")
    )

  ;; buffer 
  (patrl/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "switch buffer") ;; gets overridden by consult
    "bk" '(kill-this-buffer :wk "kill this buffer")
    "br" '(revert-buffer :wk "reload buffer")
    )

  ;; universal argument
  (patrl/leader-keys
    "u" '(universal-argument :wk "universal prefix")
    )

  ;; notes
  (patrl/leader-keys
    "n" '(:ignore t :wk "notes") ;; see org-roam and citar sections
    "na" '(org-todo-list :wk "agenda todos") ;; agenda
    )

  ;; code
  (patrl/leader-keys
    "c" '(:ignore t :wk "code") ;; see flymake
    )

  ;; open
  (patrl/leader-keys
    "o" '(:ignore t :wk "open")
    "os" '(speedbar t :wk "speedbar") ;; FIXME I never use this
    )

  ;; search
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
  (setq evil-search-module 'evil-search) ;; makes evil search more like vim

  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'

  (setq evil-want-integration t) ;; necessary for evil collection
  (setq evil-want-keybinding nil)

  (setq evil-split-window-below t)
  (setq evil-split-window-right t)

  (setq evil-undo-system 'undo-redo) ;; undo via 'u', and redo the undone change via 'C-r'; only available in emacs 28+.
  :config
  (evil-mode t) ;; globally enable evil mode
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'eshell-mode 'insert)
  )

(use-package evil-collection ;; evilifies a bunch of things
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t) ;; '<TAB>' cycles visibility in 'outline-minor-mode'
  ;; If I want to incrementally enable evil-collection mode-by-mode, I can do something like the following:
  ;; (setq evil-collection-mode-list nil) ;; I don't like surprises
  ;; (add-to-list 'evil-collection-mode-list 'magit) ;; evilify magit
  ;; (add-to-list 'evil-collection-mode-list '(pdf pdf-view)) ;; evilify pdf-view
  :config
  (evil-collection-init)
  )

;; port of Tim Pope's commentary package
;; 'g c c' in normal mode to comment a line.
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode) ;; globally enable evil-commentary
  )

;; port of Tim Pope's surround package
(use-package evil-surround
  :after evil
  :hook (
         (org-mode . (lambda () (push '(?~ . ("~" . "~")) evil-surround-pairs-alist)))
         )
  :config
  (global-evil-surround-mode 1) ;; globally enable evil-surround
  )

;; show visual hints for evil motions
(use-package evil-goggles
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package avy
  :general
  (general-def '(normal motion)
    "s" 'evil-avy-goto-char-timer
    "f" 'evil-avy-goto-char-in-line
    "gl" 'evil-avy-goto-line ;; this rules
    ;; TODO incorporate avy-resume (maybe ";")
    )
  )

(use-package which-key
  :after evil
  :init (which-key-mode)
  :config
  (which-key-setup-minibuffer))

(use-package all-the-icons
  :if (display-graphic-p))


;; prettify dired with icons
(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode)
  )

(use-package olivetti
  :init
  (setq olivetti-body-width 80))

(use-package mood-line
  :config (mood-line-mode))

(defun patrl/setup-font-faces ()
  (set-face-attribute 'default nil :font (font-spec :family "Blex Mono Nerd Font" :size 30 :weight 'medium))
  (set-face-attribute 'fixed-pitch nil :font (font-spec :family "Blex Mono Nerd Font" :size 30 :weight 'medium))
  (set-face-attribute 'variable-pitch nil :font (font-spec :family "iA Writer Duospace" :size 30 :weight 'medium))
  (set-fontset-font t 'unicode "DeJa Vu Sans Mono")
  )

;; run this hook after we have initialized the first time
(add-hook 'after-init-hook 'patrl/setup-font-faces)
;; re-run this hook if we create a new frame from daemonized Emacs
(add-hook 'server-after-make-frame-hook 'patrl/setup-font-faces)

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
  (load-theme 'doom-nord t)

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

;; use emacs' built-in 'project.el'
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
    "fd" '(dired :wk "dired") ;; open dired (in a directory)
    "fj" '(dired-jump :wk "dired jump")) ;; open direct in the current directory
  ;; ranger like navigation
  (:keymaps 'dired-mode-map
            :states 'normal
            "h" 'dired-up-directory
            "q" 'kill-current-buffer
            "l" 'dired-find-file
            )
  :hook
  (dired-mode . dired-hide-details-mode) ;; make dired prettier
  :straight (:type built-in))

;; toggle subtree visibility with 'TAB'
;; makes dired a much more pleasant file manager
(use-package dired-subtree)

(use-package org
  ;; :straight (:type built-in)
  :init
  (setq org-todo-keywords
        ;; it's extremely useful to distinguish between short-term goals and long-term projects
        '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d)")))
  (setq org-src-fontify-natively t) ;; fontify code in src blocks
  (setq org-highlight-latex-and-related '(native))
  (setq org-adapt-indentation nil) ;; interacts poorly with 'evil-open-below'
  :custom
  (org-agenda-files '("~/Dropbox (MIT)/org/agenda" "~/notes/daily"))
  :general
  (patrl/local-leader-keys
    :keymaps 'org-mode-map
    "l" '(:ignore t :wk "link")
    "ll" '(org-insert-link t :wk "link")
    "s" '(consult-org-heading :wk "consult heading")
    "d" '(org-cut-special :wk "org cut special")
    "y" '(org-copy-special :wk "org copy special")
    "p" '(org-paste-special :wk "org paste special")
    "b" '(:keymap org-babel-map :wk "babel")
    "t" '(org-insert-structure-template :wk "template")
    "e" '(org-edit-special :wk "edit")
    "i" '(:ignore t :wk "insert")
    "ih" '(org-insert-heading :wk "insert heading")
    "is" '(org-insert-subheading :wk "insert heading")
    )
  (:keymaps 'org-agenda-mode-map
            "j" '(org-agenda-next-line)
            "h" '(org-agenda-previous-line))

  :hook
  (org-mode . olivetti-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode)
  (org-mode . org-num-mode)
  (org-mode . (lambda () (electric-indent-local-mode -1))) ;; disable electric indentation
  ;; :config
  ;; FIXME this turns out to be a bad idea, since the symbols conflict
  ;; (defvar org-electric-pairs '((?\* . ?\*)\ (?/ . ?/) (?= . ?=) (?\_ . ?\_) (?~ . ?~) (?+ . ?+) (?$ . ?$))) ;; electric pairs for org-mode
  ;; (defun org-add-electric-pairs ()
  ;;   (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  ;;   (setq-local electric-pair-text-pairs electric-pair-pairs))
  ;; (add-hook 'org-mode-hook 'org-add-electric-pairs)
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

(use-package citeproc
  :after org)

(with-eval-after-load 'ox-latex
   (add-to-list 'org-latex-classes
                '("scrartcl"
                  "\\documentclass[letterpaper]{scrartcl}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                  )))

(use-package haskell-mode)

(use-package racket-mode
  :hook (racket-mode . racket-xp-mode) ;; n.b. this requires Dr. Racket to be installed as a backend
  :general
  (patrl/local-leader-keys
    :keymaps 'racket-mode-map
    "r" '(racket-run-and-switch-to-repl :wk "run")
    "e" '(racket-eval-last-sexp :wk "eval last sexp")
    :keymaps 'racket-xp-mode-map
    "xr" '(racket-xp-rename :wk "rename")
    ))

(use-package nix-mode
  ;; There's no `nix-mode-map`, so not currently possible to set local bindings.
  :mode "\\.nix\\'")

(use-package nix-update
  :commands
  nix-update-fetch
)

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
      TeX-engine 'luatex ;; use lualatex by default
      TeX-save-query nil) 
      )

(use-package tex
  :straight auctex
  :general
  (patrl/local-leader-keys
    :keymaps 'LaTeX-mode-map
    ;; "TAB" 'TeX-complete-symbol ;; FIXME let's 'TAB' do autocompletion (but it's kind of useless to be honest)
    "m" '(LaTeX-macro :wk "insert macro")
    "s" '(LaTeX-section :wk "insert section header")
    "e" '(LaTeX-environment :wk "insert environment")
    "p" '(preview-at-point :wk "preview at point")
    "f" '(TeX-font :wk "font")
    "c" '(TeX-command-run-all :wk "compile")
    )
  :init
  (setq TeX-electric-math (cons "\\(" "\\)")) ;; '$' inserts an in-line equation '\(...\)'
  :config
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  (add-hook 'TeX-mode-hook #'prettify-symbols-mode)
  (add-hook 'TeX-after-compilation-finished-functions
              #'TeX-revert-document-buffer)
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
  (add-hook 'TeX-mode-hook #'outline-minor-mode)
  (add-hook 'TeX-mode-hook #'flymake-aspell-setup)
  )

;; (use-package auctex-latexmk
;;   :after latex 
;;   :init
;;   (setq auctex-latexmk-inherit-TeX-PDF-mode t)
;;   :config
;;   (auctex-latexmk-setup)
;;   )

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

(use-package laas
  :hook ((LaTeX-mode . laas-mode)
         (org-mode . laas-mode))
  :config
  (aas-set-snippets 'laas-mode
    ;; I need to make sure not to accidentally trigger the following, so I should only use impossible (or extremely rare) bigrams/trigrams.
    "mx" (lambda () (interactive)
            (yas-expand-snippet "\\\\($0\\\\)"))
    "mq" (lambda () (interactive)
            (yas-expand-snippet "\\[\n$0\n\\]"))
    "*i" (lambda () (interactive)
            (yas-expand-snippet "\\begin{itemize}\n$>\\item $0\n\\end{itemize}"))
    "*I" (lambda () (interactive)
            (yas-expand-snippet "\\begin{enumerate}\n$>\\item $0\n\\end{enumerate}"))
    "*e" (lambda () (interactive)
            (yas-expand-snippet "\\begin{exe}\n$>\\ex $0\n\\end{exe}"))
    "*f" (lambda () (interactive)
            (yas-expand-snippet "\\begin{forest}\n[{$1}\n[{$2}]\n[{$0}]\n]\n\\end{forest}"))
    :cond #'texmathp ; expand only while in math 
    "Olon" "O(n \\log n)"
    ";:" "\\coloneq"
    ";;N" "\\mathbb{N}"
    ";T" "\\top"
    ";B" "\\bot"
    ";;x" "\\times"
    ";;v" "\\veebar"
    ;; bind to functions!
    "sum" (lambda () (interactive)
            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
    "Span" (lambda () (interactive)
             (yas-expand-snippet "\\Span($1)$0"))
    "lam" (lambda () (interactive)
            (yas-expand-snippet "\\lambda $1_{$2}\\,.\\,$0"))
    ;; "set" (lambda () (interactive)
    ;;           (yas-expand-snippet "\\set{ $1 | $2} $0"))
    "txt" (lambda () (interactive)
              (yas-expand-snippet "\\text{$1} $0"))
    ";;o" (lambda () (interactive)
              (yas-expand-snippet "\\oplus"))
    ;; "ev" (lambda () (interactive)
    ;;             (yas-expand-snippet "\\left\\llbracket$3\\right\\rrbracket^$1_$2 $3"))
    ;; clash with event type sigs
    ;; add accent snippets
    :cond #'laas-object-on-left-condition
    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
    ))

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . olivetti-mode)
         (markdown-mode . variable-pitch-mode))
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")
  (setq markdown-header-scaling t))

(use-package pandoc-mode
  :after markdown-mode
  :hook (markdown-mode . pandoc-mode))

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

(use-package corfu
  :custom
  (corfu-cycle t) ;; allows cycling through candidates
  (corfu-auto nil) ;; disables auto-completion
  (corfu-quit-at-boundary nil) ;; needed to use orderless completion with corfu
  :bind
  :general
  (:keymaps 'corfu-map
            "C-j" 'corfu-next
            "C-k" 'corfu-previous
            )
  :init
  (corfu-global-mode)
  )

(general-unbind
  :states '(insert)
  "C-k" ;; this was interfering with corfu completion
  )

(use-package emacs
  :init
  (setq tab-always-indent 'complete)
  )

(use-package flymake
  :straight (:type built-in)
  :general
  (patrl/leader-keys
    :keymaps 'flymake-mode-map
    "cf" '(consult-flymake :wk "consult flymake") ;; depends on consult
    "cc" '(flymake-mode :wk "toggle flymake") ;; depends on consult
    )
  :hook
  (TeX-mode . flymake-mode) ;; this is now working
  (emacs-lisp-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout nil)
  :general
  (general-nmap "] !" 'flymake-goto-next-error)
  (general-nmap "[ !" 'flymake-goto-prev-error)
  )

(use-package flymake-aspell
  :after flymake)

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
  :custom
  (lsp-completion-provider :none) ;; probably want to delete this if I reenable company
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

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package numbex
  :straight (:type git :host github :repo "enricoflor/numbex"))

(use-package elfeed
  :commands elfeed)

(use-package elfeed-org
  :init
  (setq rmh-elfeed-org-files (list "~/Dropbox (MIT)/org/elfeed.org"))
  :config
  (elfeed-org))

(use-package simple-httpd
  :commands httpd-serve-directory)

(use-package notmuch
  :init
  (defvar +notmuch-delete-tags '("+trash" "-inbox" "-unread" "-new"))
  (setq notmuch-archive-tags '("-inbox" "-new"))
  :config
  (add-to-list 'notmuch-saved-searches '(:name "new" :query "tag:new" :key "n"))
  (defun +notmuch/search-delete ()
    (interactive)
    (notmuch-search-add-tag +notmuch-delete-tags)
    (notmuch-tree-next-message))
  (defun +notmuch/tree-delete ()
    (interactive)
    (notmuch-tree-add-tag +notmuch-delete-tags)
    (notmuch-tree-next-message))
  :general
  (patrl/leader-keys
    "on" '(notmuch :wk "notmuch")) 
  (:keymaps 'notmuch-search-mode-map
            :states 'normal
            "S" '+notmuch/search-delete
            )
  (:keymaps 'notmuch-tree-mode-map
            :states 'normal
            "S" '+notmuch/tree-delete)
  )

(use-package sly)

(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode)
  (racket-mode . lispy-mode))

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  ;; the following is necessary to retain tab completion in lispy mode
  :general
  ;; (:keymaps 'evil-collection-lispy-mode-map-special
  ;;           "TAB" 'indent-for-tab-command)
  (:keymaps 'lispy-mode-map 
            "TAB" 'indent-for-tab-command)
  :config
  ;; TODO play around with keythemes 
  (lispyville-set-key-theme '(operators c-w additional)))

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

(use-package tree-sitter)

(use-package tree-sitter-langs)

(use-package helpful
  :general
  (patrl/leader-keys
    "hc" '(helpful-command :wk "helpful command")
    "hf" '(helpful-callable :wk "helpful callable")
    "hh" '(helpful-at-point :wk "helpful at point")
    "hF" '(helpful-function :wk "helpful function")
    "hv" '(helpful-variable :wk "helpful variable")
    "hk" '(helpful-key :wk "helpful key")))
