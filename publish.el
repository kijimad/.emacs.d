;; Initialize package sources
(require 'package)

(defun yes-or-no-p (dummy) t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.
(setq package-user-dir (expand-file-name "./.packages"))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(use-package htmlize
  :ensure t)

(require 'ox-publish)

(setq org-publish-project-alist
  `(
     ("kijima:readme"
      :recursive t
      :base-extension "org"
      :base-directory "./conf"
      :exclude "./.packages"
      :publishing-function org-html-publish-to-html
      :publishing-directory "./public"
      :html-link-home "/"
      :html-head nil ;; cleans up anything that would have been in there.
      )))

(defun kd/publish ()
  (org-publish-all t))

(defun kd/gen-el ()
  (org-babel-tangle-file "~/.emacs.d/conf/index.org" "~/.emacs.d/conf/index.el" "emacs-lisp"))
