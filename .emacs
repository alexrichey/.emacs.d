(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(require 'use-package)
(use-package bm
  :ensure t)
(use-package markdown-mode
  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)

  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)

  ;; where to store persistant files
  (setq bm-repository-file "~/.emacs.d/bm-repository")

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (add-hook' after-init-hook 'bm-repository-load)

  ;; Restoring bookmarks when on file find.
  (add-hook 'find-file-hooks 'bm-buffer-restore)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  (setq bm-cycle-all-buffers t))

(use-package evil
  :ensure t)
(use-package package
  :ensure t)
(use-package flyspell-correct-helm
  :ensure t)
(use-package magit
  :ensure t)
(use-package cc-mode
  :ensure t)
(use-package helm
  :ensure t)
(use-package cc-mode
  :ensure t)
(use-package package
  :ensure t)
(use-package web-mode
  :ensure t)
(use-package yasnippet
  :ensure t)
(use-package js-comint
  :ensure t)
(use-package tagedit
  :ensure t)
(use-package js-comint
  :ensure t)
(use-package expand-region
  :ensure t)
(use-package multiple-cursors
  :ensure t)
(use-package ace-jump-mode
  :ensure t)
(use-package projectile
  :ensure t)
(use-package exec-path-from-shell
  :ensure t)
(use-package go-mode
  :ensure t)
(use-package jedi
  :ensure t)
(use-package js2-mode
  :ensure t)
(use-package company
  :ensure t)
(use-package key-chord
  :ensure t)
(use-package helm-projectile
  :ensure t)
(use-package emmet-mode
  :ensure t)
(use-package cider
  :ensure t)
(use-package ac-cider
  :ensure t)
(use-package clj-mode
  :ensure t)
(use-package clojurescript-mode
  :ensure t)
(use-package http
  :ensure t)

;; Trying this out
(use-package js2-highlight-vars
  :ensure t)

(use-package js2-refactor
  :ensure t)
(use-package json-mode
  :ensure t)
(use-package eclim
  :ensure t)
(use-package neotree
  :ensure t)
(use-package json-mode
  :ensure t)
(use-package olivetti
  :ensure t)
(use-package paredit
  :ensure t)

;;Trying this out
(use-package password-vault
  :ensure t)

(use-package pretty-lambdada
  :ensure t)
(use-package requirejs
  :ensure t)
(use-package salesforce-utils
  :ensure t)
(use-package scala-mode
  :ensure t)
;; (use-package slime
;;   :ensure t)
(use-package skewer-mode
  :ensure t)
(use-package auto-indent-mode
  :ensure t)
(use-package clj-refactor
  :ensure t)
(use-package cljr-helm
  :ensure t)
(use-package yafolding
  :ensure t)
(use-package angular-snippets
  :ensure t)
(use-package angular-mode
  :ensure t)
(use-package pony-mode
  :ensure t)
(use-package python-pep8
  :ensure t)
(use-package pretty-mode-plus
  :ensure t)
(use-package highlight-symbol
  :ensure t)
(use-package undo-tree
  :ensure t)

;; basic config
(linum-mode 1)
(setq-default indent-tabs-mode nil)
(scroll-bar-mode 0)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(show-paren-mode 1)
(load-theme 'tango-dark)
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(setq mac-command-modifier 'control)
(yas-global-mode 1)
(undo-tree-mode 1)
(put 'set-goal-column 'disabled nil)
(exec-path-from-shell-copy-env "PATH")
(global-company-mode)
(electric-pair-mode 1)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)
                                        ;keys
;; basic
(global-set-key		(kbd "<C-tab>")		'other-window)
(global-set-key		(kbd "C-x C-p")		'other-window-backwards)
(global-set-key		(kbd "C-x C-x")		'mark-page)
(global-set-key		(kbd "C-c C-m")		'execute-extended-command)
(global-set-key		(kbd "C-w")		'kill-word)
(global-set-key		(kbd "C-x C-r")		'kill-region)
(global-set-key		(kbd "C-x C-o")	'other-window)
(define-key global-map	(kbd "<f9>")	'compile)
(global-set-key		(kbd "C-=")		'er/expand-region)
(global-set-key		(kbd "C-x <C-return>")	'eval-buffer)
(global-set-key		(kbd "C-c t")	'neotree)
(global-set-key		(kbd "C-x m")	'comment-dwim)
(global-set-key		(kbd "C-x g")	'magit-status)
(global-set-key		(kbd "<f7>")	'other-frame)
(global-set-key		(kbd "C-x b")	'helm-buffers-list)
(global-set-key		(kbd "C-x C-f")	'helm-find-files)
(global-set-key		(kbd "M-y")	'helm-show-kill-ring)
(global-set-key		(kbd "M-x")	'helm-M-x)
(global-set-key		(kbd "C-c SPC")	'ace-jump-word-mode)

;; multiple cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)

;;Chord Definitions
(key-chord-mode 1)
(key-chord-define-global "df" 'other-window)
(key-chord-define-global "jd" 'helm-buffers-list)
(key-chord-define-global ",," 'evil-mode)
(key-chord-define-global "jk" 'evil-force-normal-state)
(key-chord-define-global "qw" 'ace-jump-char-mode)

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)


                                        ;Appearance
(setq default-directory "~/Dev/")
;;(neotree-dir "~/Dev")


                                        ; Helm
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(helm-mode 1)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(helm-projectile-on)

;; (add-hook 'before-save-hook 'whitespace-cleanup)

                                        ; Apex
(add-to-list 'auto-mode-alist '("\\.trigger\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.cls\\'" . java-mode))
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 4)))

                                        ; Clojure

(defun clojure-mode-hooks ()
  (progn
    (paredit-mode 1)
    (auto-indent-mode 1)
    (toggle-truncate-lines 1)
    (show-paren-mode 1)
    (clj-refactor-mode 1)
    (clj-refactor-mode 1)
    (undo-tree-mode 1)
    (eldoc-mode 1)
    (yafolding-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m")
    (print "It's Clojure time, brosef!")))

(add-hook 'clojure-mode-hook 'clojure-mode-hooks)
(put 'upcase-region 'disabled nil)

                                        ; HTML

(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

(defun html-mode-hooks ()
  (progn
    (auto-indent-mode 1)
    (toggle-truncate-lines 1)
    (show-paren-mode 1)
    (emmet-mode 1)
    (undo-tree-mode 1)
    (eldoc-mode 1)
    (yafolding-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (print "It's HTML time, my man!")))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . html-mode))
(add-hook 'html-mode-hook 'html-mode-hooks)

                                        ; js-comint
;; (setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")
;; (add-hook 'js2-mode-hook '(lambda ()
;;                          (local-set-key "\C-x\C-e" 'js-send-last-sexp)
;;                          (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
;;                          (local-set-key "\C-cb" 'js-send-buffer)
;;                          (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
;;                          (local-set-key "\C-cl" 'js-load-file-and-go)
;;                          (lambda ()
;;                            (slime-js-minor-mode 1))))

(setq inferior-lisp-program "/usr/local/bin/sbcl")
;; (require 'slime)

;; make js2 the default mode for js
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun my-js2-mode-hooks ()
  (progn
    (setq js-indent-level 2)
    (auto-indent-mode 1)
    (toggle-truncate-lines 1)
    (show-paren-mode 1)
    (undo-tree-mode 1)
    (yas-minor-mode 1)
    (print "It's js2 time, my funky friend!")))
(add-hook 'js2-mode-hook 'my-js2-mode-hooks)


(defun my-json-mode-hooks ()
  (progn
    (setq js-indent-level 2)
    (auto-indent-mode 1)
    (toggle-truncate-lines 1)
    (show-paren-mode 1)
    (undo-tree-mode 1)
    (yas-minor-mode 1)
    (print "It's JSON time, drop the beat!")))
(add-hook 'json-mode-hook 'my-json-mode-hooks)

(defun current-file-path ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))


;; Go Mode
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

;; For Easy JS Editing - double tap to add semicolon to end of line
(defun insert-semicolon-at-end ()
  (interactive)
  (save-excursion
    (move-end-of-line 1)
    (insert-string ";")))
(key-chord-define-global ";;" 'insert-semicolon-at-end)


;; Experimental Python
(package-initialize)
(pyde-enable)
(setq python-check-command "pyflakes")
(use-package pyde
  :ensure t)
(use-package flymake
  :ensure t)
(use-package pyflakes
  :ensure t)
(use-package highlight-indentation
  :ensure t)

;; Python Hooks
(add-hook 'python-mode-hook
          (lambda ()
            (progn
              ;; (jedi-mode 1)
              (print "Python, huh? Cool, dude."))))

(setq jedi:complete-on-dot t)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" default)))
 '(package-selected-packages
   (quote
    (bm flyspell-correct-helm elpy pyfslakes pyflakes highlight-indentation pyde helm-cider ac-helm psvn ssh flymake-go go-snippets company-go scala-mode salesforce-utils requirejs pretty-lambdada password-vault eclim js2-highlight-vars http clojurescript-mode clj-mode ac-cider use-package helm-company company-jedi yafolding xah-elisp-mode web-mode web tagedit sublimity sublime-themes slime-js skewer-mode rvm rinari restclient olivetti nodejs-repl neotree mocha minimap markdown-mode magit-svn key-chord json-mode js3-mode js2-refactor js-comint jedi jade-mode icicles heroku-theme helm-rails helm-projectile helm-emmet go-mode foggy-night-theme expand-region exec-path-from-shell evil-paredit eruby-mode emacs-eclim elisp-slime-nav company coffee-mode cljr-helm auto-indent-mode ace-jump-mode ac-slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-persistent-face ((t (:background "selectedKnobColor" :foreground "White")))))

                                        ; The force is with you
(load-file "~/.emacs.d/site-lisp/force-mode/force-mode.el")

                                        ; Bookmarking
(global-set-key (kbd "C-c b b") 'bm-toggle)
(global-set-key (kbd "C-c b a") 'bm-show-all)
;; (key-chord-define-global "qe" 'bm-next)
(key-chord-define-global "qw" 'bm-previous)

;; Make the color not SO jarring...
(defface bm-persistent-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:foreground "White" :background "selectedKnobColor"))
    (((class color)
      (background dark))  (:foreground "White" :background "selectedKnobColor")))
  "Face used to highlight current line if bookmark is persistent."
  :group 'bm)
