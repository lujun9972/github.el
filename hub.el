(require 'gh)

(defun hub-git-do (&rest args)
  ""
  (shell-command-to-string (format "git %s" (string-join args " "))))

(defun hub (op &rest args)
  ""
  (let ((op-fn (intern-soft (format "hub-%s" op))))
    (if (functionp  op-fn)
        (apply op-fn args)
      (apply #'hub-git-do args))))

(defun hub-parse (url)
  (let ((case-fold-search t))
    (when (string-match "^https?://github.com/\\(.+\\)$" url)
      (setq url (match-string 1 url))))
  )


(defun hub-clone (user-and-repo)
  ""
  (let ((api (gh-repos-api "api"))
        user repo)
    (if (string-match-p "/" user-and-repo)
        (let ((user-and-repo (split-string user-and-repo "/")))
          (setq user (nth 0 user-and-repo))
          (setq repo (nth 1 user-and-repo)))
      (setq user (gh-api-get-username api))
      (setq repo user-and-repo))
    (let ((repo-url (format "git://github.com/%s/%s.git" user repo)))
      (hub-git-do "clone" repo-url))))

(defun hub--current-repo-name ()
  (file-name-nondirectory (directory-file-name default-directory)))

(defun hub-create ()
  ""
  (let* ((api (gh-repos-api "api"))
        (repo-name (hub--current-repo-name))
        (git-url (format "git@github.com:%s/%s.git" (gh-api-get-username api) repo-name)))
    (gh-repos-repo-new api (gh-repos-repo-stub :name repo-name))
    (hub-git-do "remote" "add" "origin" git-url)))
