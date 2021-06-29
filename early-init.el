;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
   ;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Ensure Doom is running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name))

(add-to-list 'load-path (concat user-emacs-directory "lib"))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
(setq custom-safe-themes t)

;; From: https://github.com/hartzell/straight.el/commit/882649137f73998d60741c7c8c993c7ebbe0f77a#diff-b335630551682c19a781afebcf4d07bf978fb1f8ac04c6bf87428ed5106870f5R1649
(setq straight-disable-byte-compilation (member "--no-byte-compilation" command-line-args))
(unless straight-disable-byte-compilation
    (byte-compile-file (concat user-emacs-directory "init.el") t)
    (byte-recompile-directory (concat user-emacs-directory "lib") nil t)
    (byte-recompile-directory (concat user-emacs-directory "themes") nil t))
