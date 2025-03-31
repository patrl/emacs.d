;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-
;; NOTE: init.el is now generated from readme.org.  Please edit that file instead

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))
(setq use-package-always-ensure t)

(defvar patrl/library-path "~/MEGA/library/"
  "Directory .pdf collection lives.")

(defvar patrl/notes-path "~/MEGA/org/roam/"
  "Notes.")

(defvar patrl/journal-path (concat patrl/notes-path "daily/")
  "Journal entries.")

(defvar patrl/global-bib-file "~/texmf/bibtex/bib/master.bib"
  "Bibliography.")

(defvar patrl/org-path "~/MEGA/org/"
  "Org path.")

(use-package emacs
  :demand t
  :ensure nil
  :init

  (setq enable-recursive-minibuffers t)

  (setq backup-by-copying t)

  (setq sentence-end-double-space nil)

  (setq frame-inhibit-implied-resize t) ;; useless for a tiling window manager

  (setq show-trailing-whitespace t) ;; self-explanatory

  (setq user-full-name "Patrick D. Elliott") ;; my details
  (setq user-mail-address "patrick.d.elliott@gmail.com")

  (defalias 'yes-or-no-p 'y-or-n-p) ;; life is too short

  (setq indent-tabs-mode nil) ;; no tabs

  ;; keep backup and save files in a dedicated directory
  (setq backup-directory-alist
	  `((".*" . ,(concat user-emacs-directory "backups")))
	  auto-save-file-name-transforms
	  `((".*" ,(concat user-emacs-directory "backups") t)))

  (setq create-lockfiles nil) ;; no need to create lockfiles

  (set-charset-priority 'unicode) ;; utf8 everywhere
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
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package browse-url
  :demand t
  :ensure nil
  :init
  (setq browse-url-firefox-program "firefox-beta"))

(use-package electric
  :demand t
  :ensure nil
  :init
  (electric-pair-mode +1) ;; automatically insert closing parens
  (setq electric-pair-preserve-balance nil)) ;; more annoying than useful

(use-package ediff
  :demand t
  :ensure nil
)

(use-package evil
  :demand t
  :init
  (setq evil-respect-visual-line-mode t) ;; respect visual lines

  (setq evil-search-module 'isearch) ;; use emacs' built-in search functionality.

  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll t) ;; allow scroll down with 'C-d'

  (setq evil-want-integration t) ;; necessary for evil collection
  (setq evil-want-keybinding nil)

  (setq evil-split-window-below t) ;; split windows created below
  (setq evil-vsplit-window-right t) ;; vertically split windows created to the right

  (setq evil-want-C-i-jump nil) ;; hopefully this will fix weird tab behaviour

  (setq evil-undo-system 'undo-redo) ;; undo via 'u', and redo the undone change via 'C-r'; only available in emacs 28+.
  :config
  (evil-mode t) ;; globally enable evil mode
  ;; set the initial state for some kinds of buffers.
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; buffers in which I want to immediately start typing should be in 'insert' state by default.
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'magit-diff-mode 'insert))

(use-package evil-collection ;; evilifies a bunch of things
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t) ;; '<TAB>' cycles visibility in 'outline-minor-mode'
  ;; If I want to incrementally enable evil-collection mode-by-mode, I can do something like the following:
  ;; (setq evil-collection-mode-list nil) ;; I don't like surprises
  ;; (add-to-list 'evil-collection-mode-list 'magit) ;; evilify magit
  ;; (add-to-list 'evil-collection-mode-list '(pdf pdf-view)) ;; evilify pdf-view
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode)) ;; globally enable evil-commentary

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)) ;; globally enable evil-surround

(use-package evil-goggles
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-evil-setup)
  ;; integrate general with evil

  ;; set up 'SPC' as the global leader key
  (general-create-definer patrl/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  ;; set up ',' as the local leader key
  (general-create-definer patrl/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "," ;; set local leader
    :global-prefix "M-,") ;; access local leader in insert mode

  (general-define-key
   :states 'insert
   "C-g" 'evil-normal-state) ;; don't stretch for ESC

  ;; unbind some annoying default bindings
  (general-unbind
    "C-x C-r"	;; unbind find file read only
    "C-x C-z"	;; unbind suspend frame
    "C-x C-d"	;; unbind list directory
    "<mouse-2>") ;; pasting with mouse wheel click


  (patrl/leader-keys
    "SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
    "TAB" '(:keymap tab-prefix-map :wk "tab")) ;; remap tab bindings

  (patrl/leader-keys
  "w" '(:keymap evil-window-map :wk "window")) ;; window bindings

  (patrl/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; help
  ;; namespace mostly used by 'helpful'
  (patrl/leader-keys
    "h" '(:ignore t :wk "help"))

  ;; file
  (patrl/leader-keys
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find file") ;; gets overridden by consult
    "fs" '(save-buffer :wk "save file"))

  ;; buffer
  ;; see 'bufler' and 'popper'
  (patrl/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "switch buffer") ;; gets overridden by consult
    "bk" '(kill-this-buffer :wk "kill this buffer")
    "br" '(revert-buffer :wk "reload buffer"))

  ;; bookmark
  (patrl/leader-keys
    "B" '(:ignore t :wk "bookmark")
    "Bs" '(bookmark-set :wk "set bookmark")
    "Bj" '(bookmark-jump :wk "jump to bookmark"))

  ;; universal argument
  (patrl/leader-keys
    "u" '(universal-argument :wk "universal prefix"))

  ;; notes
  ;; see 'citar' and 'org-roam'
  (patrl/leader-keys
    "n" '(:ignore t :wk "notes")
    ;; see org-roam and citar sections
    "na" '(org-todo-list :wk "agenda todos")) ;; agenda

  ;; code
  ;; see 'flymake'
  (patrl/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; open
  (patrl/leader-keys
    "o" '(:ignore t :wk "open")
    "os" '(speedbar t :wk "speedbar")
    "op" '(elpaca-log t :wk "elpaca"))


  ;; search
  ;; see 'consult'
  (patrl/leader-keys
    "s" '(:ignore t :wk "search"))

  ;; templating
  ;; see 'tempel'
  (patrl/leader-keys
    "t" '(:ignore t :wk "template")))

;; "c" '(org-capture :wk "capture")))

(use-package org
  :ensure (:wait t)
  :demand t
  :init
  ;; edit settings (recommended by org-modern)
  (setq org-auto-align-tags nil
	    org-tags-column 0
	    org-catch-invisible-edits 'show-and-error
	    org-special-ctrl-a/e t ;; special navigation behaviour in headlines
	    org-insert-heading-respect-content t)

  ;; styling, hide markup, etc. (recommended by org-modern)
  (setq org-hide-emphasis-markers t
	    org-src-fontify-natively t ;; fontify source blocks natively
	    org-highlight-latex-and-related '(native) ;; fontify latex blocks natively
	    org-pretty-entities t)

  ;; agenda styling (recommended by org-modern)
  (setq org-agenda-tags-column 0
	    org-agenda-block-separator ?─
	    org-agenda-time-grid
	    '((daily today require-timed)
	      (800 1000 1200 1400 1600 1800 2000)
	      " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
	    org-agenda-current-time-string
	    "⭠ now ─────────────────────────────────────────────────")

  (setq org-ellipsis "...")

  ;; todo setup
  (setq org-todo-keywords
	    ;; it's extremely useful to distinguish between short-term goals and long-term projects
	    '((sequence "TODO(t)" "SOMEDAY(s)" "|" "DONE(d)")
	      (sequence "TO-READ(r)" "READING(R)" "|" "HAVE-READ(d)")
	      (sequence "PROJ(p)" "|" "COMPLETED(c)")))


  (setq org-adapt-indentation nil) ;; interacts poorly with 'evil-open-below'

  :custom
  (org-agenda-files '((concat patrl/notes-path "todo.org") (concat patrl/notes-path "teaching.org") (concat patrl/notes-path "projects.org")))
  (org-cite-global-bibliography (list patrl/global-bib-file))
  ;; handle citations using citar
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :general
  (patrl/local-leader-keys
	:keymaps 'org-mode-map
	"a" '(org-archive-subtree :wk "archive")
	"c" '(org-cite-insert :wk "insert citation")
	"l" '(:ignore t :wk "link")
	"ll" '(org-insert-link t :wk "link")
	"lp" '(org-latex-preview t :wk "prev latex")
	"h" '(consult-org-heading :wk "consult heading")
	"d" '(org-cut-special :wk "org cut special")
	"y" '(org-copy-special :wk "org copy special")
	"p" '(org-paste-special :wk "org paste special")
	"b" '(:keymap org-babel-map :wk "babel")
	"t" '(org-todo :wk "todo")
	"s" '(org-insert-structure-template :wk "template")
	"e" '(org-edit-special :wk "edit")
	"i" '(:ignore t :wk "insert")
	"ih" '(org-insert-heading :wk "insert heading")
	"is" '(org-insert-subheading :wk "insert heading")
	"f" '(org-footnote-action :wk "footnote action")
	">" '(org-demote-subtree :wk "demote subtree")
	"<" '(org-promote-subtree :wk "demote subtree"))
  (:keymaps 'org-agenda-mode-map
		"j" '(org-agenda-next-line)
		"h" '(org-agenda-previous-line))

  :hook
  (org-mode . olivetti-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . (lambda () (electric-indent-local-mode -1))) ;; disable electric indentation

  :config
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  (add-to-list 'org-latex-packages-alist '("" "braket" t))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
	 (emacs-lisp . t)
	 (awk . t)))
  ;; set up org paths
  (setq org-directory "~/MEGA/org/agenda")
  (setq org-default-notes-file (concat org-directory "/notes.org")))

(use-package avy
    :demand t
    :init
(defun patrl/avy-action-insert-newline (pt)
      (save-excursion
        (goto-char pt)
        (newline))
      (select-window
       (cdr
        (ring-ref avy-ring 0))))
    (defun patrl/avy-action-kill-whole-line (pt)
      (save-excursion
        (goto-char pt)
        (kill-whole-line))
      (select-window
       (cdr
        (ring-ref avy-ring 0))))
    (defun patrl/avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t) ;; adds an avy action for embark
    :general
    (general-def '(normal motion)
      "s" 'evil-avy-goto-char-timer
      "f" 'evil-avy-goto-char-in-line
      "gl" 'evil-avy-goto-line ;; this rules
      ";" 'avy-resume)
    :config
    (setf (alist-get ?. avy-dispatch-alist) 'patrl/avy-action-embark ;; embark integration
          (alist-get ?i avy-dispatch-alist) 'patrl/avy-action-insert-newline
          (alist-get ?K avy-dispatch-alist) 'patrl/avy-action-kill-whole-line)) ;; kill lines with avy

(use-package link-hint
  :demand t
  :general
  (patrl/leader-keys
    "l" '(link-hint-open-link :wk "open link"))
  :config
  (setq browse-url-browser-function 'browse-url-firefox)
  (setq link-hint-avy-style 'pre))

(use-package which-key
  :after evil
  :demand t
  :init (which-key-mode)
  :config
  (which-key-setup-minibuffer))

(use-package mood-line
  :disabled
  :demand t
  :config (mood-line-mode))

(use-package minions
  :demand t
  :config
  (minions-mode 1))

(use-package all-the-icons
  :demand t)


;; prettify dired with icons
(use-package all-the-icons-dired
  :demand t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :demand t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

(use-package olivetti
  :demand t
  :init
  (setq olivetti-body-width 80)
  (setq olivetti-style 'fancy)
  (setq olivetti-minimum-body-width 50))

(defun patrl/setup-font-wolfe ()
    (set-face-attribute 'default nil :font (font-spec :family "Iosevka Comfy Motion" :size 10.0 :weight 'regular))
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family "Iosevka Comfy Motion" :size 10.0 :weight 'regular))
    (set-face-attribute 'variable-pitch nil :font (font-spec :family "Iosevka Etoile" :size 10.0 :weight 'medium))
    (set-fontset-font t 'unicode "JuliaMono"))

(defun patrl/setup-font-vivacia ()
  (set-face-attribute 'default nil :font (font-spec :family "Iosevka Comfy Motion" :size 10.0 :weight 'regular))
  (set-face-attribute 'fixed-pitch nil :font (font-spec :family "Iosevka Comfy Motion" :size 10.0 :weight 'regular))
  (set-face-attribute 'variable-pitch nil :font (font-spec :family "Iosevka Etoile" :size 10.0 :weight 'medium))
  (set-fontset-font t 'unicode "JuliaMono"))

(when (string= (system-name) "wolfe")
  (add-hook 'after-init-hook 'patrl/setup-font-wolfe)
  (add-hook 'server-after-make-frame-hook 'patrl/setup-font-wolfe))

(when (string= (system-name) "vivacia")
  (add-hook 'after-init-hook 'patrl/setup-font-vivacia)
  (add-hook 'server-after-make-frame-hook 'patrl/setup-font-vivacia))

(use-package solaire-mode
  :demand t
  :config
  (solaire-global-mode +1))

(use-package catppuccin-theme
  :demand t
  :config
  (setq catppuccin-height-title1 1.5)
  (load-theme 'catppuccin t))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t)
                                        ; if nil, italics is universally disabled
  ;; (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package hl-todo
  :demand t
  :init
  (global-hl-todo-mode))

(use-package popper
  :demand t
  :general
  (patrl/leader-keys
        "bp" '(:ignore t :wk "popper")
        "bpc" '(popper-cycle t :wk "cycle")
        "bpt" '(popper-toggle-latest t :wk "toggle latest")
        "bpb" '(popper-toggle-type t :wk "toggle type")
        "bpk" '(popper-kill-latest-popup t :wk "kill latest"))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*helpful"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
          magit-process-mode
          "^\\*eshell.*\\*" eshell-mode
          "\\*direnv\\*"
          "\\*elfeed-log\\*"
          "\\*Async-native-compile-log\\*"
          "\\*TeX Help\\*"
          "\\*Embark Collect Live\\*"))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package bufler
  :demand t
  :custom
  (bufler-workspace-mode t)
  ;; (bufler-workspace-tabs-mode t)
  :general
  (patrl/leader-keys
    "bB" '(bufler :wk "bufler") ;; overrides consult
    "bf" '(bufler-workspace-frame-set :wk "bufler workspace frame set")
    "bl" '(bufler-list :wk "bufler list"))
  (:keymaps 'bufler-list-mode-map
            :states 'normal
            "?" 'hydra:bufler/body
            "RET" 'bufler-list-buffer-switch
            "SPC" 'bufler-list-buffer-peek
            "d" 'bufler-list-buffer-kill))

(use-package project
  :ensure nil
  :general
  ;; assign built-in project.el bindings a new prefix
  (patrl/leader-keys "p" '(:keymap project-prefix-map :wk "project")))

(use-package dired
    :ensure nil
    :general
    (patrl/leader-keys
      "fd" '(dired :wk "dired") ;; open dired (in a directory)
      "fj" '(dired-jump :wk "dired jump")) ;; open direct in the current directory
    ;; ranger like navigation
    (:keymaps 'dired-mode-map
              :states 'normal
              "h" 'dired-up-directory
              "q" 'kill-current-buffer
              "l" 'dired-find-file)
    :hook
    (dired-mode . dired-hide-details-mode))

  ;; toggle subtree visibility with 'TAB'
  ;; makes dired a much more pleasant file manager
(use-package dired-subtree
  :demand t)

(use-package lua-mode
  :mode "\\.lua\\'")

;; FIXME - compatibility with corfu
(use-package sly)

(use-package lispy
  :general
  (:keymaps 'lispy-mode-map
	      "TAB" 'indent-for-tab-command) ;; necessary for 'corfu'
  :hook
  (reb-lisp-mode . lispy-mode)
  (emacs-lisp-mode . lispy-mode)
  (racket-mode . lispy-mode)
  (fennel-mode . lispy-mode))

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :general
  (:keymaps 'lispyville-mode-map
	      "TAB" 'indent-for-tab-command) ;; necessary for 'corfu'
  ;; the following is necessary to retain tab completion in lispy mode
  :config
  ;; TODO play around with keythemes
  (lispyville-set-key-theme '(operators c-w additional)))

(use-package js2-mode)

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config (setq rustic-lsp-client 'eglot))

(use-package haskell-mode)

(use-package dante
  :after haskell-mode cape
  :init
  (defun dante-setup-capf ()
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'dante-company)))
  :commands 'dante-mode
  :config (dante-setup-capf))

(use-package racket-mode
  :hook (racket-mode . racket-xp-mode) ;; n.b. this requires Dr. Racket to be installed as a backend
  :general
  (patrl/local-leader-keys
    :keymaps 'racket-mode-map
    "r" '(racket-run-and-switch-to-repl :wk "run")
    "e" '(racket-eval-last-sexp :wk "eval last sexp")
    :keymaps 'racket-xp-mode-map
    "xr" '(racket-xp-rename :wk "rename")))

(use-package nix-mode
  ;; There's no `nix-mode-map`, so not currently possible to set local bindings.
  :mode "\\.nix\\'")

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . olivetti-mode))
         (markdown-mode . variable-pitch-mode)
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

(use-package tex 
  :ensure (auctex :pre-build (("./autogen.sh")
			      ("./configure"
			       "--without-texmf-dir"
			       "--with-packagelispdir=./"
			       "--with-packagedatadir=./")
			      ("make"))
		  :build (:not elpaca--compile-info) ;; Make will take care of this step
		  :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
		  :version (lambda (_) (require 'tex-site) AUCTeX-version))
  :init
  (setq TeX-parse-self t ; parse on load
	reftex-plug-into-AUCTeX t
	TeX-auto-save t  ; parse on save
	TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-source-correlate-mode t
	TeX-source-correlate-method 'synctex
	TeX-source-correlate-start-server t
	TeX-electric-sub-and-superscript t
	TeX-engine 'luatex ;; use lualatex by default
	TeX-save-query nil
	TeX-electric-math (cons "\\(" "\\)")) ;; '$' inserts an in-line equation '\(...\)'

  (add-hook 'TeX-mode-hook #'reftex-mode)
  (add-hook 'TeX-mode-hook #'olivetti-mode)
  (add-hook 'TeX-mode-hook #'turn-on-auto-fill)
  (add-hook 'TeX-mode-hook #'prettify-symbols-mode)
  (add-hook 'TeX-after-compilation-finished-functions
	    #'TeX-revert-document-buffer)
  (add-hook 'TeX-mode-hook #'outline-minor-mode)
  :general 
  (patrl/local-leader-keys
    :keymaps 'LaTeX-mode-map
    ;; "TAB" 'TeX-complete-symbol ;; FIXME let's 'TAB' do autocompletion (but it's kind of useless to be honest)
    "=" '(reftex-toc :wk "reftex toc")
    "(" '(reftex-latex :wk "reftex label")
    ")" '(reftex-reference :wk "reftex ref")
    "m" '(LaTeX-macro :wk "insert macro")
    "s" '(LaTeX-section :wk "insert section header")
    "e" '(LaTeX-environment :wk "insert environment")
    "p" '(preview-at-point :wk "preview at point")
    "f" '(TeX-font :wk "font")
    "c" '(TeX-command-run-all :wk "compile")))

(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode)
  :general
  (:keymaps 'evil-tex-mode-map
	    "M-]" 'evil-tex-brace-movement)
  :config
  (unbind-key "M-n" 'evil-tex-mode-map)) ;; interfering with jinx

(use-package citar
  :demand t
  :after all-the-icons
  :init
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon
	      "file-o"
	      :face 'all-the-icons-green
	      :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))

  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (all-the-icons-octicon
              "link"
              :face 'all-the-icons-orange
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))

  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (all-the-icons-material
              "speaker_notes"
              :face 'all-the-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))

  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon
              "circle-o"
              :face 'all-the-icon-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))
  :hook
  ;; set up citation completion for latex, org-mode, and markdown
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  (markdown-mode . citar-capf-setup)
  :config
  (setq citar-indicators
	(list citar-indicator-files-icons
	      citar-indicator-links-icons
	      citar-indicator-notes-icons
	      citar-indicator-cited-icons))
  :general
  (patrl/leader-keys
    "nb" '(citar-open :wk "citar"))
  :init
  (setq citar-notes-paths (list patrl/notes-path))
  (setq citar-library-paths (list patrl/library-path))
  (setq citar-bibliography (list patrl/global-bib-file)))

(use-package citar-embark
  :after citar embark
  :config (citar-embark-mode))

(use-package engrave-faces)

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-auctex
  :ensure (:type git :host github :repo
                   "karthink/org-auctex")
  :hook (org-mode . org-auctex-mode))

(use-package org-transclusion
  :after org
  :general
  (patrl/leader-keys
    "nt" '(org-transclusion-mode :wk "transclusion mode")))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

(use-package org-cliplink
  :after org
  :general
  (patrl/local-leader-keys
    :keymaps 'org-mode-map
    "lc" '(org-cliplink :wk "cliplink")))

(use-package org-modern
  :after org
  :config (global-org-modern-mode))

(use-package org-roam
  :demand t
  :general
  (patrl/leader-keys
    "nr" '(:ignore t :wk "roam")
    "nri" '(org-roam-node-insert t :wk "insert node")
    "nrt" '(org-roam-buffer-toggle t :wk "roam buffer toggle")
    "nrc" '(org-roam-capture t :wk "roam capture")
    "nrf" '(org-roam-node-find :wk "find node")
    "nrd" '(:ignore t :wk "dailies")
    "nrdt" '(org-roam-dailies-goto-today :wk "today")
    "nrdt" '(org-roam-dailies-goto-yesterday :wk "today")
    "nrdT" '(org-roam-dailies-goto-tomorrow :wk "today")
    "nrdd" '(org-roam-dailies-goto-date :wk "goto date"))
  :config
  ;; org-roam-buffer
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  ;; get tags to show up in 'org-roam-node-find':
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-completion-everywhere t) ;; roam completion anywhere
  (setq org-roam-directory patrl/notes-path)
  (setq org-roam-db-location (concat org-roam-directory "/.database/org-roam.db"))
  (unless (< emacs-major-version 29)
    (setq org-roam-database-connector 'sqlite-builtin))
  (org-roam-db-autosync-mode) ;; ensures that org-roam is available on startup


  ;; dailies config
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: daily\n")))))

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
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
             '("memoir"
               "\\documentclass{memoir}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(use-package htmlize)

(use-package org-noter
  :commands
  org-noter
  :general
  (patrl/local-leader-keys
    :keymaps 'org-noter-doc-mode-map
    "i" 'org-noter-insert-note)
  :config
  (setq org-noter-notes-search-path (list patrl/notes-path))
  (setq org-noter-default-notes-file-names '("literature-notes.org"))
  (setq org-noter-hide-other nil)
  (setq org-noter-always-create-frame nil))

(use-package vertico
  :demand t
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  (setq vertico-multiform-categories
	'((file grid)
	  (jinx grid (vertico-grid-annotate . 20))
	  (citar buffer)))
  (setq vertico-cycle t) ;; enable cycling for 'vertico-next' and 'vertico-prev'
  :general
  (:keymaps 'vertico-map
            ;; keybindings to cycle through vertico results.
            "C-j" 'vertico-next
            "C-k" 'vertico-previous
            "C-f" 'vertico-exit
	    "<backspace>" 'vertico-directory-delete-char
	    "C-<backspace>" 'vertico-directory-delete-word
	    "C-w" 'vertico-directory-delete-word
	    "RET" 'vertico-directory-enter)
  (:keymaps 'minibuffer-local-map
            "M-h" 'backward-kill-word))

(use-package orderless
  :demand t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package marginalia
  :demand t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :demand t
  :general
  (patrl/leader-keys
    "bb" '(consult-buffer :wk "consult buffer")
    "Bb" '(consult-bookmark :wk "consult bookmark")
    "ht" '(consult-theme :wk "consult theme")
    "sr" '(consult-ripgrep :wk "consult rg")
    "sg" '(consult-grep :wk "consult grep")
    "sG" '(consult-git-grep :wk "consult git grep")
    "sf" '(consult-find :wk "consult find")
    "sF" '(consult-locate :wk "consult locate")
    "sl" '(consult-line :wk "consult line")
    "sy" '(consult-yank-from-kill-ring :wk "consult yank from kill ring")
    "i" '(consult-imenu :wk "consult imenu"))
  :config
  ;; use project.el to retrieve the project root
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package affe
  :demand t
  :after orderless
  :general
  (patrl/leader-keys
    "sa" '(affe-grep :wk "affe grep")
    "sw" '(affe-find :wk "affe find"))
  :init
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  :config
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

(use-package embark
  :demand t
  :general
  (patrl/leader-keys
     "." 'embark-act) ;; easily accessible 'embark-act' binding.
  ("C-." 'embark-act) ;; overlaps with evil-repeat 
  ("C-;" 'embark-dwim) ;; overlaps with IEdit
  (:keymaps 'vertico-map
            "C-." 'embark-act) ;; embark on completion candidates
  (:keymaps 'embark-heading-map
            "l" 'org-id-store-link)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :demand t
  :hook
  (eval-expression-minibuffer-setup . corfu-mode)
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t) ;; allows cycling through candidates
  (corfu-auto nil) ;; disables auto-completion
  :bind
  :general
  (:keymaps 'corfu-map
            "SPC" 'corfu-insert-separator)) ;; for compatibility with orderless

(general-unbind
  :states '(insert)
  "C-k" ;; this was interfering with corfu completion
  :states '(normal)
  "C-;") 

(use-package emacs
  :ensure nil
  :init
  (setq tab-always-indent 'complete)) ;; enable tab completion

(use-package cape
  :demand t
  ;; bindings for dedicated completion commands
  :general
  ("M-p p" 'completion-at-point ;; capf
   "M-p t" 'complete-tag ;; etags
   "M-p d" 'cape-dabbrev ;; dabbrev
   "M-p h" 'cape-history
   "M-p f" 'cape-file
   "M-p k" 'cape-keyword
   "M-p s" 'cape-symbol
   "M-p a" 'cape-abbrev
   "M-p i" 'cape-ispell
   "M-p l" 'cape-line
   "M-p w" 'cape-dict
   "M-p \\" 'cape-tex
   "M-p &" 'cape-sgml
   "M-p r" 'cape-rfc1345)
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package company-reftex
  :after cape
  :init
  (defun reftex-setup-capf ()
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-reftex-labels)))
  :hook
  (LaTeX-mode . reftex-setup-capf))

(use-package flymake
  :ensure nil
  :general
  (patrl/leader-keys
    :keymaps 'flymake-mode-map
    "cf" '(consult-flymake :wk "consult flymake") ;; depends on consult
    "cc" '(flymake-mode :wk "toggle flymake")) ;; depends on consult
  :hook
  (TeX-mode . flymake-mode) ;; this is now working
  (emacs-lisp-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout nil)
  :general
  (general-nmap "] !" 'flymake-goto-next-error)
  (general-nmap "[ !" 'flymake-goto-prev-error))

(use-package re-builder
  :ensure nil
  :general (patrl/leader-keys
	     "se" '(regexp-builder :wk "regex builder"))
  :config (setq reb-re-syntax 'rx))

(use-package pdf-tools
  :demand t
  :ensure nil
  :hook (TeX-after-compilation-finished . TeX-revert-document-buffer)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (require 'pdf-tools)
  (require 'pdf-view)
  (require 'pdf-misc)
  (require 'pdf-occur)
  (require 'pdf-util)
  (require 'pdf-annot)
  (require 'pdf-info)
  (require 'pdf-isearch)
  (require 'pdf-history)
  (require 'pdf-links)
  (require 'pdf-outline)
  (require 'pdf-sync)
  (pdf-tools-install :no-query))

(use-package jinx
  :demand t
  :ensure nil
  :hook (emacs-startup . global-jinx-mode)
  :general
  ("M-$" 'jinx-correct
   "C-M-$" 'jinx-languages))

(use-package helpful
  :demand t
  :general
  (patrl/leader-keys
    "hc" '(helpful-command :wk "helpful command")
    "hf" '(helpful-callable :wk "helpful callable")
    "hh" '(helpful-at-point :wk "helpful at point")
    "hF" '(helpful-function :wk "helpful function")
    "hv" '(helpful-variable :wk "helpful variable")
    "hk" '(helpful-key :wk "helpful key")))

(use-package deadgrep
  :demand t
  :general
  (patrl/leader-keys
    "sd" '(deadgrep :wk "deadgrep")))

(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :hook (org-mode . aas-activate-for-major-mode)
  :config
  ;; easy emoji entry in text mode.
  (aas-set-snippets 'text-mode
    ":-)" "🙂"
    "8-)" "😎"
    ":rofl" "🤣"
    ":lol" "😂"
    "<3" "❤️"
    ":eyes" "👀"
    ":dragon" "🐉"
    ":fire" "🔥"
    ":hole" "🕳️"
    ":flush" "😳"
    ":wow" "😮"))

(use-package laas
  ;; disables accent snippets - things like 'l (which expands to \textsl{}) end up being very disruptive in practice.
  :init (setq laas-accent-snippets nil)
  :hook ((LaTeX-mode . laas-mode)
	   (org-mode . laas-mode))
  :config
  (aas-set-snippets 'laas-mode
    ;; I need to make sure not to accidentally trigger the following, so I should only use impossible (or extremely rare) bigrams/trigrams.
    ;; "*b" (lambda () (interactive)
    ;;        (yas-expand-snippet "\\textbf{$1}$0"))
    ;; "*i" (lambda () (interactive)
    ;; 	   (yas-expand-snippet "\\textit{$1}$0"))
    "mx" (lambda () (interactive)
	      (yas-expand-snippet "\\\\($1\\\\)$0"))
    "mq" (lambda () (interactive)
	      (yas-expand-snippet "\\[$1\\]$0"))
    ;; "*I" (lambda () (interactive)
    ;; 	    (yas-expand-snippet "\\begin{enumerate}\n$>\\item $0\n\\end{enumerate}"))
    ;; "*e" (lambda () (interactive)
    ;; 	    (yas-expand-snippet "\\begin{exe}\n$>\\ex $0\n\\end{exe}"))
    ;; "*f" (lambda () (interactive)
    ;; 	    (yas-expand-snippet "\\begin{forest}\n[{$1}\n[{$2}]\n[{$0}]\n]\n\\end{forest}"))
    "*\"" (lambda () (interactive)
	      (yas-expand-snippet "\\enquote{$1}$0"))
    :cond #'texmathp ; expand only while in math
    "Olon" "O(n \\log n)"
    ";:" "\\coloneq"
    ";;N" "\\mathbb{N}"
    ";T" "\\top"
    ";B" "\\bot"
    ";;x" "\\times"
    ";;v" "\\veebar"
    ";;u" "\\cup"
    ";;{" "\\subseteq"
    ";D" "\\Diamond"
    ";;b" "\\Box"
    ;; bind to functions!
    "sum" (lambda () (interactive)
	      (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
    "grandu" (lambda () (interactive)
	      (yas-expand-snippet "\\bigcup\limits_{$1} $0"))
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
    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-to-list 'yas-snippet-dirs "~/.config/emacs/snippets")
  (yas-global-mode 1))

(use-package tempel
  :demand t
  :general
  ("M-p +" 'tempel-complete) ;; M-p completion prefix; see `cape'
  (patrl/leader-keys
    "ti" '(tempel-insert :wk "tempel insert"))
  (:keymaps 'tempel-map
            "TAB" 'tempel-next) ;; progress through fields via `TAB'
  :init
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package simple-httpd
  :commands httpd-serve-directory)

(use-package transient)

(use-package magit
  :after transient
  :general
  (patrl/leader-keys
    "g" '(:ignore t :wk "git")
    "gg" '(magit-status :wk "status")))

(use-package eshell
  :ensure nil
  :general
  (patrl/leader-keys
    "oe" '(eshell :wk "eshell")))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use corfu!
  :init
  (defun patrl/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-completion-mode . patrl/lsp-mode-setup-completion) ;; setup orderless completion style.
  :commands
  lsp)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package lsp-haskell
  :after lsp-mode
  :config
  (setq lsp-haskell-server-path "haskell-language-server") ;; for some reason this doesn't get found automatically
  ;; (setq lsp-haskell-formatting-provider "brittany")
  )

(use-package eglot
  :ensure nil
  :init (setq completion-category-overrides '((eglot (styles orderless))))
  :commands eglot
  :config
  (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server" "--lsp"))))

(use-package direnv
  :config
  (direnv-mode))

(use-package kbd-mode
  :ensure (:type git :host github :repo
                   "kmonad/kbd-mode"))

;; We write a function to determine how we want elfeed to display the buffer with the current entry.
(defun patrl/elfeed-display-buffer (buf &optional act)
  (pop-to-buffer buf)
  (set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height)))))

(defun patrl/elfeed-search-show-entry-pre (&optional lines)
  "Returns a function to scroll forward or back in the Elfeed search results, displaying entries without switching to them."
  (lambda (times)
    (interactive "p")
    (forward-line (* times (or lines 0)))
    (recenter)
    (call-interactively #'elfeed-search-show-entry)
    (select-window (previous-window))
    (unless elfeed-search-remain-on-entry (forward-line -1))))

(use-package elfeed
  :commands elfeed
  :general
  (patrl/leader-keys
    "oE" '(elfeed :wk "elfeed"))
  ;; :config
  (setq elfeed-show-entry-switch #'patrl/elfeed-display-buffer))

(use-package elfeed-org
  :init
 (setq rmh-elfeed-org-files (list "~/MEGA/org/elfeed.org"))
  :config
  (elfeed-org))

(use-package notmuch
  :init
  ;; in gmail, messages are trashed by removing the 'inbox' tag, and adding the 'trash' tag. This will move messages to the gmail trash folder, but won't permnanently delete them.
  (defvar +notmuch-delete-tags '("+trash" "-inbox" "-unread" "-new"))
  ;; in gmail, messages are archived simply by removing the 'inbox' tag.
  (setq notmuch-archive-tags '("-inbox" "-new"))
  ;; show new mail first
  :config
  ;; the 'new' tag isn't synced to the gmail server; messages new to notmuch aquire this tag.
  (add-to-list 'notmuch-saved-searches '(:name "new" :query "tag:new" :key "n"))
  (defun +notmuch/search-delete ()
    (interactive)
    (notmuch-search-add-tag +notmuch-delete-tags)
    (notmuch-tree-next-message))
  (defun +notmuch/tree-delete ()
    (interactive)
    (notmuch-tree-add-tag +notmuch-delete-tags)
    (notmuch-tree-next-message))
  (setq-default notmuch-search-oldest-first nil)
  ;; sending mail using lieer
  (setq message-kill-buffer-on-exit t)
  (setq sendmail-program "gmi")
  (setq send-mail-function 'sendmail-send-it)
  (setq message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/.mail/personal"))
  (setq notmuch-fcc-dirs nil) ;; let gmail take care of sent mail
  :general
  (patrl/leader-keys
    "on" '(notmuch :wk "notmuch"))
  ;; single key deletion.
  (:keymaps 'notmuch-search-mode-map
            :states 'normal
            "D" '+notmuch/search-delete)
  (:keymaps 'notmuch-tree-mode-map
            :states 'normal
            "D" '+notmuch/tree-delete))

(use-package consult-notmuch
  :disabled
  :general
  (patrl/leader-keys
    "nmm" '(consult-notmuch t :wk "consult notmuch")
    "nmt" '(consult-notmuch-tree t :wk "consult notmuch tree")
    "nma" '(consult-notmuch-address t :wk "consult notmuch address"))
  :after notmuch)

(use-package denote
  :general
  (patrl/leader-keys
    "nd" '(:ignore t :wk "denote")
    "ndd" '(denote t :wk "denote")
    "ndr" '(denote-rename-file t :wk "rename file")
    "ndl" '(denote-link t :wk "link")
    "ndb" '(denote-backlinks t :wk "backlinks")
    "ndf" '(denote-sort-dired t :wk "sort dired"))
  :hook
  (text-mode . denote-fontify-links-mode-maybe)
  (dired-mode . denote-dired-mode)
  :config
  (setq denote-save-buffers nil)
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-backlinks-show-context t)
  (setq denote-directory (expand-file-name "~/MEGA/denote/"))
  (denote-rename-buffer-mode 1))

(use-package consult-notes
  :general
  (patrl/leader-keys
    "nn" '(consult-notes t :wk "consult notes"))
  :commands (consult-notes
	     consult-notes-search-in-all-notes)
  :config
(when (locate-library "denote")
    (consult-notes-denote-mode))
  ;; search only for text files in denote dir
(setq consult-notes-denote-files-function (lambda () (denote-directory-files nil t t))))

(use-package ace-window
  :demand t
  :general
  ("M-o" 'ace-window) ;; global keybinding for ace-window (there's some lispy interference)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))) ;; use the homerow to jump

(use-package rainbow-mode)

(use-package burly
  :init
  (burly-tabs-mode +1)
  :general
  (patrl/leader-keys
    "Bf" '(burly-bookmark-frames t :wk "bookmark frames")
    "Bw" '(burly-bookmark-windows t :wk "bookmark windows")
    "Bo" '(burly-open-bookmark t :wk "open bookmark")
    "Bl" '(burly-open-last-bookmark t :wk "open last bookmark")))

(use-package ebib
  :config
  (setq ebib-bibtex-dialect 'biblatex)
  (setq ebib-preload-bib-files (list patrl/global-bib-file))
  :general
  (patrl/leader-keys
    "ob" '(ebib :wk "ebib")))

(use-package tree-sitter :disabled)
(use-package tree-sitter-langs :disabled)
