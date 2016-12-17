(defun force-cli-command (command)
  (shell-command (concat "force " command)))

(defun force-cli-login ()
  (interactive)
  (shell-command "force login"))

(defun force-cli-list-logins ()
  (interactive)
  (shell-command "force logins"))

(defun force-cli-push-aura-file ()
  (interactive)
  (progn
    (print "Pushing Aura File")
    (let ((path (current-file-path)))
     (force-cli-command (concat "aura push -f " path)))))

(defvar force-cli-keymap nil "Keymap for Force-cli mode")
(progn
  (setq force-cli-keymap (make-sparse-keymap))
  (define-key force-cli-keymap (kbd "C-c f p") 'force-cli-push-aura-file)
  (define-key force-cli-keymap (kbd "C-c f l") 'force-cli-login)
  )

;; Add mode hooks
(add-to-list 'auto-mode-alist '("\\.app\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.cmp\\'" . html-mode))


(define-minor-mode force-mode
  "A minor mode for interacting with the Force CLI, and other goodies."
  :lighter " force-cli"
  :keymap force-cli-keymap)

;; (provide 'force-mode)
