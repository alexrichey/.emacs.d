(defun force-cli-command (command)
  (shell-command (concat "force " command)))

(defun force-cli-login ()
  (interactive)
  (shell-command "force login"))

(defun force-cli-fetch-classes ()
  "fetches classes"
  (interactive)
  (force-cli-command "fetch -t ApexClass"))

(defun force-cli-fetch-aura ()
  "fetches aura"
  (interactive)
  (force-cli-command "fetch -t aura"))

(defun force-cli-create-apex-class (class-name)
  "docstring"
  (interactive "sClass Name: ")
  (force-cli-command (concat "create -w apexclass -n " class-name)))

(defun force-cli-pull-package ()
  (interactive)
  (progn
    (print "exporting standard objects")
    (force-cli-command "export")
    (print "fetching aura")
    (force-cli-fetch-aura "fetch -t ")))

(defun force-cli-list-logins ()
  (interactive)
  (shell-command "force logins"))

(defun force-cli-apex-class ()
  (interactive)
  (progn
    (print "Pushing Apex Class")
    (let ((path (current-file-path)))
      (force-cli-command (concat "force push -t ApexClass -f " path)))))

(defun force-cli-push-aura-file ()
  (interactive)
  (progn
    (print "Pushing Aura File")
    (let ((path (current-file-path)))
      (force-cli-command (concat "aura push -f " path)))))

(defun force-cli-push-apex-class ()
  (interactive)
  (progn
    (print "Pushing Apex Class")
    (let ((path (current-file-path)))
      (force-cli-command (concat "push -t ApexClass " path)))))

(defun force-cli-complete-objects (data)
  (interactive)
  (print (helm :sources (helm-build-sync-source "test"
                    :candidates data
                    :fuzzy-match t)
         :buffer "*helm test*")))

(defvar force-cli-keymap nil "Keymap for Force-cli mode")
(progn
  (setq force-cli-keymap (make-sparse-keymap))
  (define-key force-cli-keymap (kbd "C-c f p") 'force-cli-push-aura-file)
  (define-key force-cli-keymap (kbd "C-c f c") 'force-cli-push-apex-class)
  (define-key force-cli-keymap (kbd "C-c f l") 'force-cli-login))

;; Add mode hooks
(add-to-list 'auto-mode-alist '("\\.app\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.app\\'" . force-mode))

(add-to-list 'auto-mode-alist '("\\.cmp\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.cmp\\'" . force-mode))

(define-minor-mode force-mode
  "A minor mode for interacting with the Force CLI, and other goodies."
  :lighter " force-cli"
  :keymap force-cli-keymap)

(provide 'force-mode)

(defun force-cli--parse-objectnames-from-response (response)
  (mapcar (lambda (x) (print (plist-get x :Name))) response))

(defun force-cli-get-objects ()
  (interactive)
  (request
   "localhost:8080/objects"

   :parser
   (lambda ()
     (let ((json-object-type 'plist))
       (json-read)))

   :success
   (function* (lambda (&key data &allow-other-keys)
                (progn
                  (print (plist-get data :objects))
                  (setq objects (plist-get data :objects))
                  (force-cli-complete-objects (force-cli--parse-objectnames-from-response (plist-get data :objects)))
                  (force-cli-complete-objects (plist-get data :objects)))))))
