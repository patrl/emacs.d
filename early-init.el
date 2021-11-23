;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-

(setq package-enable-at-startup nil)

(menu-bar-mode -1) ;; disables menubar
(tool-bar-mode -1) ;; disables toolbar
(scroll-bar-mode -1) ;; disables scrollbar

(setq inhibit-splash-screen t) ;; no thanks
(setq use-file-dialog nil) ;; don't use system file dialog

(setq tab-bar-new-button-show nil) ;; don't show new tab button
(setq tab-bar-close-button-show nil) ;; don't show tab close button
;;; early-init.el ends here
