(defvar straight-process-buffer)
(setq-default straight-process-buffer "*straight-process*")

(defvar straight-build-dir)
(setq straight-build-dir (format "build-%s" emacs-version))

(setq straight-check-for-modifications '(check-on-save find-when-checking))

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

;; package.el initialization is expensive, thus should be disabled before loading main init file:
(setq package-enable-at-startup nil)

(setq straight-repository-branch "develop")

(straight-use-package 'use-package)
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)

(if (file-exists-p (concat user-emacs-directory "eww-bookmarks/eww-bookmarks"))
    (message "file exists")
  (progn
    (make-directory "~/.emacs.d/eww-bookmarks/")
    (make-empty-file "~/.emacs.d/eww-bookmarks/eww-bookmarks")))

(if (file-exists-p (concat user-emacs-directory "prot-eww-visited-history"))
    (message "file exists")
  (progn
    (make-empty-file "~/.emacs.d/prot-eww-visited-history")))

(if (file-exists-p (concat user-emacs-directory "prot-bongo-last-inserted"))
    (message "file exists")
  (progn
    (make-empty-file "~/.emacs.d/prot-bongo-last-inserted")
    (write-region ";; Auto-generated file; don't edit -*- mode: lisp-data -*-" nil "~/.emacs.d/prot-bongo-last-inserted" 'append)
    (write-region "\n" nil "~/.emacs.d/prot-bongo-last-inserted" 'append)
    (write-region "(())" nil "~/.emacs.d/prot-bongo-last-inserted" 'append)))
