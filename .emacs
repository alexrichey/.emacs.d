(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(require 'use-package)
(use-package package
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
(use-package slime
  :ensure t)
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


;; basic config
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

;; multiple cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)

;;Chord Definitions
(key-chord-mode 1)
(key-chord-define-global "jk" 'ace-jump-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
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
;; 			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
;; 			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
;; 			    (local-set-key "\C-cb" 'js-send-buffer)
;; 			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
;; 			    (local-set-key "\C-cl" 'js-load-file-and-go)
;; 			    (lambda ()
;; 			      (slime-js-minor-mode 1))))

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


;; Python Jedi Hook
;; (add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)   
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (highlight-symbol pretty-mode-plus pretty-mode python-pep8 pony-mode angular-snippets angular-mode yafolding web-mode use-package tagedit slime skewer-mode scala-mode salesforce-utils requirejs pretty-lambdada password-vault olivetti neotree markdown-mode magit key-chord json-mode js2-refactor js2-highlight-vars js-comint jedi http helm-projectile go-mode expand-region exec-path-from-shell evil emmet-mode eclim company clojurescript-mode cljr-helm clj-mode auto-indent-mode ace-jump-mode ac-cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )