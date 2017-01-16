(defun force-cli-command (command)
  (shell-command (concat "force " command)))

(defun force-cli-current-dir ()
  (let* ((current-file (current-file-path))
         (current-dir (f-dirname current-file)))
    (print current-dir)))

(defun force-cli--file-is-controller? (file)
  (s-suffix? "controller.js" file))

(defun force-cli--file-is-helper? (file)
  (s-suffix? "helper.js" file))

;; (defun force-cli-find-controller ()
;;   "docstring"
;;   (interactive)
;;   (let* ((dir (force-cli-current-dir))
;;          (files (f-files dir))
;;          (isController (map #s-suffix? files))))
;;   (print files))

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

(defun force-cli-helm-complete (data)
  (interactive)
  (print (helm :sources (helm-build-sync-source "objects"
                          :candidates data
                          :fuzzy-match t)
               :buffer "* Force cli completions *")))

(defun force-cli--parse-objectnames-from-response (response)
  (mapcar (lambda (x) (print (plist-get x :Name))) response))

(defun onSuccessCB (args)
  )

(defun force-cli-complete (params cb)
  (request
   (concat "localhost:8080/complete")
   :params params

   :parser
   (lambda ()
     (let ((json-object-type 'plist))
       (json-read)))

   :success
   (function* (lambda (&key data &allow-other-keys)
                (progn
                  (setq results (append data '()))
                  (cb results))))))

;; completions
(defun force-cli-complete-ui ()
  (interactive)
  (force-cli-complete '(("type" . "ui"))
                      (lambda (results) (progn
                                          (print "hi")
                                          (insert (force-cli-helm-complete results))))))

(defun force-cli-complete-vf ()
  (interactive)
  (let ((results (force-cli-complete '(("type" . "vf")))))
    (insert (force-cli-helm-complete results))))

(defun force-cli-complete-classes ()
  (interactive)
  (force-cli-complete '(("type" . "classes"))
                      (insert (force-cli-helm-complete results))))

(defun force-cli-complete-class-methods (className)
  (interactive "sClassName: ")
  (force-cli-complete `(("type" . "classes")
                        ("className" . ,className))
                      (insert (force-cli-helm-complete results))))

(define-minor-mode force-mode
  "A minor mode for interacting with the Force CLI, and other goodies."
  :lighter " force-cli"
  :keymap force-cli-keymap)

(defvar force-cli-keymap nil "Keymap for Force-cli mode")
(progn
  (setq force-cli-keymap (make-sparse-keymap))
  (define-key force-cli-keymap (kbd "C-c f p") 'force-cli-push-aura-file)
  (define-key force-cli-keymap (kbd "C-c f l") 'force-cli-login))

;; Add mode hooks
(add-to-list 'auto-mode-alist '("\\.app\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.app\\'" . force-mode))

(add-to-list 'auto-mode-alist '("\\.cmp\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.cmp\\'" . force-mode))


(provide 'force-mode)
