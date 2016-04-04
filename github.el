(require 'gh)
(require 'vc-git)

(defun github-git-do (&rest args)
  ""
  (shell-command-to-string (format "git %s" (string-join args " "))))

(defun github (op &rest args)
  ""
  (let ((op-fn (intern-soft (format "github-%s" op))))
    (if (functionp  op-fn)
        (apply op-fn args)
      (apply #'github-git-do args))))

(defun github-parse (url)
  (let ((case-fold-search t))
    (when (string-match "^https?://github.com/\\(.+\\)$" url)
      (setq url (match-string 1 url))))
  )


(defun github-clone (user-and-repo)
  ""
  (let ((api (gh-repos-api "api"))
        user repo)
    (if (string-match-p "/" user-and-repo)
        (let ((user-and-repo (split-string user-and-repo "/")))
          (setq user (nth 0 user-and-repo))
          (setq repo (nth 1 user-and-repo)))
      (setq user (gh-api-get-username api))
      (setq repo user-and-repo))
    (let ((repo-url (format "git@github.com:%s/%s.git" user repo)))
      (github-git-do "clone" repo-url))))

(defun github--current-repo-name ()
  (file-name-nondirectory (directory-file-name default-directory)))

(defun github-create ()
  ""
  (let* ((api (gh-repos-api "api"))
        (repo-name (github--current-repo-name))
        (git-url (format "git@github.com:%s/%s.git" (gh-api-get-username api) repo-name)))
    (unless (vc-git-registered repo-name)
      (vc-git-register (list repo-)))
    (gh-repos-repo-new api (gh-repos-repo :name repo-name))
    (github-git-do "remote" "add" "origin" git-url)))

(defun github-remote-add (user)
  ""
  (let ((repo-name (github--current-repo-name))
        (git-url (format "git@github.com:%s/%s.git" user repo-name)))
    (github-git-do "remote" "add" user git-url)))

(defun github-pull (user)
  ""
  (github-remote-add user)
  (github-git-do "pull" user))
