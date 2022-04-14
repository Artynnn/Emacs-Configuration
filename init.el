;;; defaults
(use-package emacs
  :init
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (menu-bar-mode -1)
  :custom
  (global-auto-revert-mode t)
  (global-visual-line-mode 1)
  (sentence-end-double-space nil)
  (auto-save-default nil)
  (ring-bell-function (quote ignore))
  (use-short-answers)
  ;; (initial-buffer-choice t)
  (make-backup-files nil)
  (recentf-max-menu-items 25)
  (user-full-name "Artyn Karadak")
  ;; (user-mail-address "GarethPrichett124115@protonmail.com")
  (user-mail-address "AmaziahBender49964@cock.li")
  (blink-cursor-mode 0)
  (set-fringe-mode 0)
  (custom-file "~/.emacs.d/custom.el")
  :config
  ;; (load-theme 'modus-operandi t)
  (put 'dired-find-alternate-file 'disabled nil)
  (put 'overwrite-mode 'disabled t)
  (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))
  (load custom-file 'noerror)
  (recentf-mode 1)
  (set-face-attribute 'default nil
		      :font "IBM Plex Mono"
		      :weight 'regular
		      :height 150))

(use-package repeat
  :straight nil
  :custom
  (repeat-on-final-keystroke t)
  (set-mark-command-repeat-pop t)
  :config
  (repeat-mode 1))

;;; completion
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package marginalia
  :straight t
  :config
  (marginalia-mode))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
		     :includes (vertico-directory))
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  :config
  (define-key vertico-map (kbd "C-j") 'vertico-directory-enter)
  (define-key vertico-map (kbd "C-l") 'vertico-directory-up))

(use-package consult
  :straight t
  :bind (("s-r" . consult-recent-file)
         ("C-x r b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          
         ("C-M-#" . consult-register)
         ("C-c C-r" . consult-recent-file)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)
         ("M-s e" . consult-isearch)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
  :custom
  ;; improves register preview
  (register-preview-delay 0)
  (register-preview-function #'consult-register-format)
  ;; go to xref
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  :config
  ;; tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

;;; general
(use-package general
  :straight t
  :config
  (global-unset-key (kbd "M-SPC"))
  (general-create-definer artyn-vc
    :prefix "C-x v"))

;;; window management
(use-package windmove
  :straight nil
  :config
  (windmove-default-keybindings)
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  :bind(("s-<right>" . windmove-right)
	("s-<left>" . windmove-left)
	("s-<up>" . windmove-up)
	("s-<down>" . windmove-down)))

(use-package ace-window
  :straight t
  :bind (("C-c o" . ace-window)))

;;; modeline
(use-package clean-modeline
  :straight (clean-modeline
	     :type git
	     :host github
	     :repo "Artynnn/clean-modeline"))

;;; themes
(use-package stimmung-themes
  :straight t)

;;; search
(use-package isearch
  :straight nil
  :custom
  (search-highlight t)
  (search-whitespace-regexp ".*?")
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace nil)
  (isearch-lazy-highlight t)
  (isearch-lazy-count t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format " (%s/%s)")
  (isearch-yank-on-move 'shift)
  (isearch-allow-scroll 'unlimited)
  ;; These variables are from Emacs 28
  (isearch-repeat-on-direction-change t)
  (lazy-highlight-initial-delay 0.5)
  (lazy-highlight-no-delay-length 3)
  (isearch-wrap-pause t)
  :bind (:map isearch-mode-map
	      ("C-g" . isearch-cancel)
	      ("M-/" . isearch-complete)))

;;; better help
(use-package helpful
  :straight t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h C" . helpful-command)))

;;; embark
(use-package embark
  :straight t
  :bind
  ;; "actually...", "yes but first" commmand
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '(
		 "\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;; avy
(use-package avy
  :straight t
  :bind (("C-c j" . avy-goto-char-timer)))

;; broken
(use-package artyn-avy
  :after avy
  :straight (artyn-avy
	     :type git
	     :host github
	     :repo "Artynnn/artyn-avy-extensions"))

;;; programming
;;;; indentation
(use-package aggressive-indent
  :straight t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)))

;;;; project management
(use-package projectile
  :straight t
  :init
  (setq projectile-project-search-path '(
					 ;; non elisp projects
					 "~/Sync/Projects/"
					 ;; elisp projects
					 "~/Sync/Projects/Emacs-Configuration/straight/repos/"
					 ;; my blog / digital garden
					 "~/Sync/Writing/roamNotes"
					 ))
  :config
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (global-set-key (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1))


;;;; language specific
(use-package nix-mode
  :straight t)

(use-package lua-mode
  :straight t)

;; supports common HTML templating languages
(use-package web-mode
  :straight t
  :mode
  (("\\.html?\\'" . web-mode)))

;; tag expansion for HTML
(use-package emmet-mode
  :straight t
  :hook (emmet-mode . web-mode)
  :bind (("C-j" . emmet-expand-yas)
	 ;; alternatively C-y or C-c w
	 ("C-c %" . emmet-wrap-with-markup)))

;; only use for emacs lisp
(use-package xref
  :straight nil
  :custom
  ;; All those have been changed for Emacs 28
  (xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (xref-file-name-display 'project-relative)
  (xref-search-program 'grep))

;;;; linting
(use-package flycheck
  :straight t
  :commands flycheck-mode)

;;;; LSP
(use-package lsp-mode
  :straight t
  :custom
  (lsp-keymap-prefix "C-c l")
  :hook ((LaTeX-mode . lsp-mode))
  :commands lsp)

;;;; in line completion
(use-package company
  :straight t
  :bind (("s-/" . company-complete))
  :config
  (global-company-mode))

(use-package dabbrev
  :straight nil
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand)))

;;;; pastebin
(use-package 0x0
  :straight t)

;;;; structured editing
(use-package outline
  :straight nil
  :custom
  (outline-minor-mode-highlight 'override) ; emacs28
  (outline-minor-mode-cycle t)             ; emacs28
  :bind (:map outline-minor-mode-map
	      ;; this works with repeat mode!
	      ("C-c C-n" . outline-next-visible-heading)
	      ("C-c C-p" . outline-previous-visible-heading)
	      ("C-c C-f" . outline-forward-same-level)
	      ("C-c C-b" . outline-backward-same-level)
	      ("C-c C-a" . outline-show-all)
	      ("C-c C-o" . outline-hide-other)
	      ("C-c C-u" . outline-up-heading)))

(use-package prot-outline
  :straight (:host gitlab :repo "protesilaos/dotfiles" :branch "master"
                   :files ("emacs/.emacs.d/prot-lisp/prot-common.el"
                           "emacs/.emacs.d/prot-lisp/prot-outline.el"))
  :hook ((emacs-lisp-mode . prot-outline-minor-mode-safe))
  :bind (("<f8>" . prot-outline-minor-mode-safe)
	 :map outline-minor-mode-map
	 ("C-c C-v" . prot-outline-move-major-heading-down)
	 ("M-<down>" . prot-outline-move-major-heading-down)
	 ("C-c M-v" . prot-outline-move-major-heading-up)
	 ("M-<up>" . prot-outline-move-major-heading-up)
	 ("C-x n s" . prot-outline-narrow-to-subtree)))

;;; writing
;;;; org mode
(use-package org
  :straight t
  :bind (("C-c c" . org-capture))
  :custom
  (org-use-speed-commands t)
  (org-imenu-depth 10)
  (org-goto-interface 'outline-path-completion)
  (org-adapt-indentation nil)
  (org-outline-path-complete-in-steps nil)
  (org-edit-src-content-indentation 0)
  (org-edit-src-persistent-message nil)
  (org-capture-templates '(
			   ("j" "Journal" entry (file+olp+datetree "~/Sync/Writing/private/cpb.org") "* %?\n")
			   ("f" "Elfeed" entry (file+olp+datetree "~/Sync/Writing/private/cpb.org") "* %? :feed:\n")
			   ("m" "Mastodon" entry (file+olp+datetree "~/Sync/Writing/roamNotes/20220314195005-mastodon_toots.org") "* %U :tobesent:\n%?")
			   )))
;;;; spellcheck
(use-package recomplete
  :straight t
  :custom
  (ispell-program-name "hunspell")
  :bind (("M-p" . recomplete-ispell-word)
	 ("M-P" . (lambda ()
		    (interactive)
		    (let ((current-prefix-arg '(-1)))
		      (call-interactively 'recomplete-ispell-word))))))

;;;; markdown
(use-package markdown-mode
  :straight t)

;;;; dictionary and thesaurus
(use-package synosaurus
  :straight t
  :custom
  (synosaurus-choose-method 'Completing read)
  (synosaurus-backend 'synosaurus-backend-wordnet)
  :bind (("s-|" . synosaurus-choose-and-replace)))

(use-package wordnut
  :straight t
  :bind(;; ("M-\\" . wordnut-lookup-current-word)
	("s-\\" . wordnut-lookup-current-word)))

;;;; visual environment for writing
(use-package olivetti
  :straight t
  :custom
  (olivetti-body-width 0.5)
  (olivetti-minimum-body-width 65)
  (olivetti-recall-visual-line-mode-entry-state t))

(use-package logos
  :straight (logos
	     :type git
	     :host github
	     :repo "protesilaos/logos")
  :config
  (let ((map global-map))
    (define-key map (kbd "C-x n n") #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)
    (define-key map (kbd "<f7>") #'logos-focus-mode))
  (defun logos--reveal-entry ()
    "Reveal Org or Outline entry."
    (cond
     ((and (eq major-mode 'org-mode)
           (org-at-heading-p))
      (org-show-entry))
     ((or (eq major-mode 'outline-mode)
          (bound-and-true-p outline-minor-mode))
      (outline-show-entry))))
  (add-hook 'logos-page-motion-hook #'logos--reveal-entry)
  :custom
  ;; If you want to use outlines instead of page breaks (the ^L)
  (logos-outlines-are-pages t)
  (logos-outline-regexp-alist
   `((emacs-lisp-mode . "^;;;+ ")
     (org-mode . "^\\*+ +")
     (t . ,(or outline-regexp logos--page-delimiter))))
  ;; These apply when `logos-focus-mode' is enabled.
  (logos-hide-mode-line nil)
  (logos-scroll-lock nil)
  (logos-variable-pitch nil)
  (logos-indicate-buffer-boundaries nil)
  (logos-buffer-read-only nil)
  (logos-olivetti t))

;;;; emacs everywhere
(use-package emacs-everywhere
  :straight (emacs-everywhere
	     :type git
	     :host github
	     :repo "Artynnn/emacs-everywhere"))

;;; version control
(use-package magit
  :straight t
  :custom
  ;; useful for prose
  (magit-diff-refine-hunk t)
  (vc-follow-symlinks nil)
  :config
  ;; banish rebase
  (with-eval-after-load 'git-rebase
    (setq auto-mode-alist
          (delete (cons git-rebase-filename-regexp 'git-rebase-mode)
                  auto-mode-alist))))

(use-package vc
  :straight nil
  :general
  (artyn-vc
    "b" #'vc-retrieve-tag ; "branch" switch
    "t" #'vc-create-tag
    "f" #'vc-log-incoming  ; the actual git fetch
    "o" #'vc-log-outgoing
    "F" #'vc-update        ; "F" because "P" is push
    "d" #'vc-diff)
  :config
  ;; Those offer various types of functionality, such as blaming,
  ;; viewing logs, showing a dedicated buffer with changes to affected
  ;; files.
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)

  ;; This one is for editing commit messages.
  (require 'log-edit)
  :custom
  (log-edit-confirm 'changed)
  (log-edit-keep-buffer nil)
  (log-edit-require-final-newline t)
  (log-edit-setup-add-author nil)
  (vc-find-revision-no-save t)
  (vc-annotate-display-mode 'scale) ; scale to oldest
  (add-log-mailing-address "example@example.com")
  (add-log-keep-changes-together t)
  (vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (vc-git-print-log-follow t)
  (vc-git-revision-complete-only-branches nil)
  (vc-git-root-log-format
   '("%d %h %ad %an: %s"
     ;; The first shy group matches the characters drawn by --graph.
     ;; We use numbered groups because `log-view-message-re' wants the
     ;; revision number to be group 1.
     "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?\
\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) \
\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \
\\(?3:.*?\\):"
     ((1 'log-view-message)
      (2 'change-log-list nil lax)
      (3 'change-log-name)
      (4 'change-log-date))))
  :hook ((log-view-mode . hl-line-mode)) ; -hook?
  :bind (
	 :map vc-dir-mode-map
	 ("b" . vc-retrieve-tag)
	 ("t" . vc-create-tag)
	 ("O" . vc-log-outgoing)
	 ("o" . vc-dir-find-file-other-window)
	 ("f" . vc-log-incoming) ; replaces `vc-dir-find-file' (use RET)
	 ("F" . vc-update)       ; symmetric with P: `vc-push'
	 ("d" . vc-diff)         ; parallel to D: `vc-root-diff'
	 ("k" . vc-dir-clean-files)
	 ("G" . vc-revert)
	 :map vc-git-stash-shared-map
	 ("a" . vc-git-stash-apply-at-point)
	 ("c" . vc-git-stash) ; "create" named stash
	 ("D" . vc-git-stash-delete-at-point)
	 ("p" . vc-git-stash-pop-at-point)
	 ("s" . vc-git-stash-snapshot)
	 :map vc-annotate-mode-map
	 ("M-q" . vc-annotate-toggle-annotation-visibility)
	 ("C-c C-c" . vc-annotate-goto-line)
	 ("<return>" . vc-annotate-find-revision-at-line)
	 :map log-view-mode-map
	 ("<tab>" . log-view-toggle-entry-display)
	 ("<return>" . log-view-find-revision)
	 ("s" . vc-log-search)
	 ("o" . vc-log-outgoing)
	 ("f" . vc-log-incoming)
	 ("F" . vc-update)
	 ("P" . vc-push)))

(use-package prot-vc
  :straight (:host gitlab :repo "protesilaos/dotfiles" :branch "master"
                   :files ("emacs/.emacs.d/prot-lisp/prot-common.el"
                           "emacs/.emacs.d/prot-lisp/prot-vc.el"))
  :general
  (artyn-vc
    "i" #'prot-vc-git-log-insert-commits
    "p" #'prot-vc-project-or-dir
    "SPC" #'prot-vc-custom-log
    "g" #'prot-vc-git-grep
    "G" #'prot-vc-git-log-grep
    "a" #'prot-vc-git-patch-apply
    "c" #'prot-vc-git-patch-create-dwim
    "s" #'prot-vc-git-show
    "r" #'prot-vc-git-find-revision
    "B" #'prot-vc-git-blame-region-or-file
    "R" #'prot-vc-git-reset)
  :custom
  (prot-vc-log-limit 100)
  (prot-vc-log-bulk-action-limit 50)
  (prot-vc-git-log-edit-show-commits t)
  (prot-vc-git-log-edit-show-commit-count 10)
  (prot-vc-shell-output "*prot-vc-output*")
  (prot-vc-patch-output-dirs (list "~/" "~/Sync/Projects/patches"))
  :config
  (prot-vc-git-setup-mode 1)
  :bind (
	 :map vc-git-log-edit-mode-map
	 ("C-c C-n" . prot-vc-git-log-edit-extract-file-name)
	 ("C-c C-i" . prot-vc-git-log-insert-commits)
	 ;; Also done by `prot-vc-git-setup-mode', but I am putting it here
	 ;; as well for visibility.
	 ("C-c C-c" . prot-vc-git-log-edit-done)
	 ("C-c C-a" . prot-vc-git-log-edit-toggle-amend)
	 ("M-p" . prot-vc-git-log-edit-previous-comment)
	 ("M-n" . prot-vc-git-log-edit-next-comment)
	 ("M-s" . prot-vc-git-log-edit-complete-comment)
	 ("M-r" . prot-vc-git-log-edit-complete-comment)
	 :map log-view-mode-map
	 ("<C-tab>" . prot-vc-log-view-toggle-entry-all)
	 ("a" . prot-vc-git-patch-apply)
	 ("c" . prot-vc-git-patch-create-dwim)
	 ("R" . prot-vc-git-log-reset)
	 ("w" . prot-vc-log-kill-hash)))

;;; web browsing
(use-package eww
  :straight nil
  :custom
  (eww-restore-desktop t)
  (eww-desktop-remove-duplicates t)
  (eww-auto-rename-buffer t)
  (eww-header-line-format nil)
  (eww-search-prefix "https://duckduckgo.com/html/?q=")
  (eww-download-directory (expand-file-name "~/Downloads/eww-downloads"))
  (eww-suggest-uris
   '(eww-links-at-point
     thing-at-point-url-at-point))
  (eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/"))
  (eww-history-limit 150)
  (eww-use-external-browser-for-content-type
   "\\`\\(video/\\|audio\\)") ; On GNU/Linux check your mimeapps.list
  (eww-browse-url-new-window-is-tab nil)
  (eww-form-checkbox-selected-symbol "[X]")
  (eww-form-checkbox-symbol "[ ]")
  ;; NOTE `eww-retrieve-command' is for Emacs28.  I tried the following
  ;; two values.  The first would not render properly some plain text
  ;; pages, such as by messing up the spacing between paragraphs.  The
  ;; second is more reliable but feels slower.  So I just use the
  ;; default (nil), though I find wget to be a bit faster.  In that case
  ;; one could live with the occasional errors by using `eww-download'
  ;; on the offending page, but I prefer consistency.
  ;;
  ;; '("wget" "--quiet" "--output-document=-")
  ;; '("chromium" "--headless" "--dump-dom")
  (eww-retrieve-command nil)
  :bind (:map eww-link-keymap
	      ("v" . nil)
	      :map eww-mode-map
	      ("L" . eww-list-bookmarks)
	      ("{" . backward-paragraph)
              ("}" . forward-paragraph)
	      :map dired-mode-map
	      ("E" . eww-open-file)
	      :map eww-buffers-mode-map
              ("d" . eww-bookmark-kill)
	      :map eww-bookmark-mode-map
	      ("d" . eww-bookmark-kill)))

(use-package prot-eww
  :straight (:host gitlab :repo "protesilaos/dotfiles" :branch "master"
                   :files ("emacs/.emacs.d/prot-lisp/prot-common.el"
                           "emacs/.emacs.d/prot-lisp/prot-eww.el"))
  :custom
  ;; (prot-eww-save-history-file "~/.emacs.d/prot-eww-visited-history")
  (prot-eww-save-visited-history t)
  (prot-eww-save-history-file
   (locate-user-emacs-file "prot-eww-visited-history"))
  ;; (prot-eww-save-visited-history nil)
  (prot-eww-bookmark-link nil)
  :hook ((prot-eww-history-mode . hl-line-mode))
  ;; ERROR no history is defined?
  :bind (("C-c w b" . prot-eww-visit-bookmark)
	 ("C-c w e" . prot-eww-browse-dwim)
	 ("C-c w s" . prot-eww-search-engine)
	 :map eww-mode-map
	 ("B" . prot-eww-bookmark-page)
	 ("D" . prot-eww-download-html)
         ("F" . prot-eww-find-feed)
	 ("H" . prot-eww-list-history)
	 ("b" . prot-eww-visit-bookmark)
	 ("e" . prot-eww-browse-dwim)
	 ("o" . prot-eww-open-in-other-window)
	 ("E" . prot-eww-visit-url-on-page)
	 ("J" . prot-eww-jump-to-url-on-page)
	 ("R" . prot-eww-readable)
	 ("Q" . prot-eww-quit)))

;; https://github.com/oantolin/emacs-config/blob/8a948477181ae35f2e35f44246787a51748a47ae/my-lisp/shr-heading.el
(use-package shr-heading
  :straight (:host github :repo "oantolin/emacs-config" :branch "master"
                   :files ("my-lisp/shr-heading.el"))
  :bind (("C-c C-p" . shr-heading-previous)
	 ("C-c C-n" . shr-heading-next)))

;; https://github.com/alphapapa/unpackaged.el#eww-imenu-support
(defun unpackaged/imenu-eww-headings ()
  "Return alist of HTML headings in current EWW buffer for Imenu.
Suitable for `imenu-create-index-function'."
  (let ((faces '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6 shr-heading)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (cl-loop for next-pos = (next-single-property-change (point) 'face)
                 while next-pos
                 do (goto-char next-pos)
                 for face = (get-text-property (point) 'face)
                 when (cl-typecase face
                        (list (cl-intersection face faces))
                        (symbol (member face faces)))
                 collect (cons (buffer-substring (point-at-bol) (point-at-eol)) (point))
                 and do (forward-line 1))))))

(add-hook 'eww-mode-hook
          (lambda ()
            (setq-local imenu-create-index-function #'unpackaged/imenu-eww-headings)))



;;; file management
(use-package dired
  :straight nil
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-listing-switches
   "-AGFhlv --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  ;; also see `dired-do-revert-buffer'
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  :hook ((dired-mode . dired-hide-details-mode)
	 (dired-mode . hl-line-mode)))

(use-package prot-dired
  :straight (:host gitlab :repo "protesilaos/dotfiles" :branch "master"
                   :files ("emacs/.emacs.d/prot-lisp/prot-common.el"
                           "emacs/.emacs.d/prot-lisp/prot-dired.el"))
  :custom
  (prot-dired-image-viewers '("feh" "sxiv"))
  (prot-dired-media-players '("mpv" "vlc"))
  (prot-dired-media-extensions
   "\\.\\(mp[34]\\|ogg\\|flac\\|webm\\|mkv\\)")
  (prot-dired-image-extensions
   "\\.\\(png\\|jpe?g\\|tiff\\)")
  (dired-guess-shell-alist-user ; those are the defaults for ! and & in Dired
   `((,prot-dired-image-extensions (prot-dired-image-viewer))
     (,prot-dired-media-extensions (prot-dired-media-player))))
  :hook ((dired-mode .  prot-dired-setup-imenu))
  :bind (:map dired-mode-map
              ("i" . prot-dired-insert-subdir)
              ("/" . prot-dired-limit-regexp)
              ("C-c C-l" . prot-dired-limit-regexp)
              ("M-n" . prot-dired-subdirectory-next)
              ("C-c C-n" . prot-dired-subdirectory-next)
              ("M-p" . prot-dired-subdirectory-previous)
              ("C-c C-p" . prot-dired-subdirectory-previous)))

(use-package dired-aux
  :straight nil
  :custom
  (dired-isearch-filenames 'dwim "search matches file names when initial point position is on a file name")
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t "this causes less conflict version contro systems")
  (dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))
  :bind (:map dired-mode-map
              ("C-+" . dired-create-empty-file)
              ;; better search dired tools available like `consult-find', `consult-grep', `project-find-file',
              ;; `project-find-regexp', `prot-vc-git-grep'
              ("M-s f" . nil)
              ;; note this command in position and context sensitive
              ("C-x v v" . dired-vc-next-action)))

;; more dired extensions
(use-package dired-x
  :straight nil
  :custom
  (dired-clean-up-buffers-too t)
  (dired-clean-confirm-killing-deleted-buffers t)
  (dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (dired-bind-man nil)
  (dired-bind-info nil)
  :bind (:map dired-mode-map
              ("I" . dired-info)))

(use-package dired-subtree
  :defer 10
  :straight t
  :custom
  (dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ;; shift tab?
              ("<backtab>" . dired-subtree-remove)
	      ("C-c C-u" . dired-subtree-up)
	      ("C-c C-d" . dired-subtree-down)))

;; dired write-mode
(use-package wdired
  :straight nil
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(use-package image-dired
  :straight nil
  :custom
  (image-dired-external-viewer "xdg-open")
  (image-dired-thumb-size 80)
  (image-dired-thumb-margin 2)
  (image-dired-thumb-relief 0)
  (image-dired-thumbs-per-row 4)
  :bind (:map image-dired-thumbnail-mode-map
              ("<return>" . image-dired-thumbnail-display-external)))

(use-package dired-hist
  :straight
  (dired-hist
   :type git
   :host github
   :repo "karthink/dired-hist")
  :config
  (define-key dired-mode-map "l" #'dired-hist-go-back)
  (define-key dired-mode-map "r" #'dired-hist-go-forward)
  (with-eval-after-load 'dired (dired-hist-mode 1)))

(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;; music management
(use-package bongo
  :straight t
  :custom
  (bongo-default-directory "~/Sync/Music")
  (bongo-prefer-library-buffers nil)
  (bongo-insert-whole-directory-trees t)
  (bongo-logo nil)
  (bongo-display-track-icons nil)
  (bongo-display-track-lengths nil)
  (bongo-display-header-icons nil)
  (bongo-display-playback-mode-indicator t)
  (bongo-display-inline-playback-progress nil) ; t slows down the playlist buffer
  (bongo-join-inserted-tracks nil)
  (bongo-field-separator (propertize " · " 'face 'shadow))
  (bongo-mark-played-tracks t)
  (bongo-mpv-program-name "vlc")
  :config
  (bongo-mode-line-indicator-mode -1)
  (bongo-header-line-mode -1)
  :bind (("C-c b" . bongo)
	 :map bongo-playlist-mode-map
	 ("n" . bongo-next-object)
	 ("p" . bongo-previous-object)
	 ("R" . bongo-rename-line)
	 ("j" . bongo-dired-line) ; Jump to dir of file at point
	 ("J" . dired-jump) ; Jump to library buffer
	 ("I" . bongo-insert-special)))

(use-package prot-bongo
  :straight (:host gitlab :repo "protesilaos/dotfiles" :branch "master"
                   :files ("emacs/.emacs.d/prot-lisp/prot-common.el"
                           "emacs/.emacs.d/prot-lisp/prot-bongo.el"))
  :custom
  (prot-bongo-enabled-backends '(vlc)) ;; originally vlc
  (prot-bongo-playlist-section-delimiter (make-string 30 ?*))
  (prot-bongo-playlist-heading-delimiter "§")
  ;; maybe remove this?
  (prot-bongo-playlist-directory
   (concat
    (file-name-as-directory bongo-default-directory)
    (file-name-as-directory "playlists")))
  :config
  (prot-bongo-enabled-backends)
  (prot-bongo-remove-headers)
  (prot-bongo-imenu-setup)
  :hook ((dired-mode . prot-bongo-dired-library-enable)
	 (wdired-mode . prot-bongo-dired-library-disable)
	 (prot-bongo-playlist-change-track-hook . prot-bongo-playlist-recenter))
  :bind (:map bongo-playlist-mode-map
	      ("C-c C-n" . prot-bongo-playlist-heading-next)
	      ("C-c C-p" . prot-bongo-playlist-heading-previous)
	      ("M-n" . prot-bongo-playlist-section-next)
	      ("M-p" . prot-bongo-playlist-section-previous)
	      ("M-h" . prot-bongo-playlist-mark-section)
	      ("M-d" . prot-bongo-playlist-kill-section)
	      ("g" . prot-bongo-playlist-reset)
	      ("D" . prot-bongo-playlist-reset)
	      ("r" . prot-bongo-playlist-random-toggle)
	      ("i" . prot-bongo-playlist-insert-playlist-file)
	      :map bongo-dired-library-mode-map
	      ("<C-return>" . prot-bongo-dired-insert)
	      ("C-c SPC" . prot-bongo-dired-insert)
	      ("C-c +" . prot-bongo-dired-make-playlist-file)))

;;; feed management
(use-package elfeed
  :straight t
  :custom
  (elfeed-search-clipboard-type 'CLIPBOARD)
  (elfeed-search-title-max-width 100)
  (elfeed-search-title-min-width  30)
  (elfeed-search-trailing-width  25)
  (elfeed-use-curl t)
  (elfeed-curl-max-connections 10)
  (elfeed-db-directory (concat user-emacs-directory "elfeed/"))
  (elfeed-enclosure-default-dir "~/Downloads/")
  (elfeed-search-filter "@4-months-ago +unread")
  (elfeed-sort-order 'descending)
  (elfeed-search-clipboard-type 'CLIPBOARD)
  (elfeed-search-title-max-width 100)
  (elfeed-search-title-min-width 30)
  (elfeed-search-trailing-width 25)
  (elfeed-show-truncate-long-urls t)
  (elfeed-show-unique-buffers t)
  (elfeed-search-date-format '("%F %R" 16 :left))
  :config
  ;; iffy
  (add-hook 'elfeed-show-mode-hook
            (lambda () (setq-local shr-width (current-fill-column))))
  (add-hook 'elfeed-show-mode-hook
	    (lambda () (text-scale-decrease 1)))
  :bind (("C-c e" . elfeed)
	 :map elfeed-search-mode-map
	 ("g" . elfeed-update)
	 ("f" . elfeed-search-untag-all-unread)
	 ("j" . elfeed-search-untag-all-unread)
	 ("b" . nil)
	 ("h" . nil)
	 ;; ("o" . elfeed-search-browse-url)
	 ;;	 ("o" . elfeed-search-show-entry)
	 ("w" . elfeed-search-yank)
	 ("G" . elfeed-search-update--force)
	 ;; ("b" . prot-elfeed-bongo-insert-item)
	 ;; ("h" . prot-elfeed-bongo-switch-to-playlist)
	 :map elfeed-show-mode-map
	 ("w" . elfeed-show-yank)
	 ;; ("b" . prot-elfeed-bongo-insert-item)
	 ))

(use-package prot-elfeed
  :straight (:host gitlab :repo "protesilaos/dotfiles" :branch "master"
                   :files ("emacs/.emacs.d/prot-lisp/prot-common.el"
                           "emacs/.emacs.d/prot-lisp/prot-elfeed.el"))
  :custom
  (prot-elfeed-tag-faces t)
  :bind (:map elfeed-search-mode-map
	      ;; ("s" . prot-elfeed-search-tag-filter)
	      ;; iffy
	      ("o" . prot-elfeed-search-open-other-window)
	      ("q" . prot-elfeed-kill-buffer-close-window-dwim)
	      ("v" . prot-elfeed-mpv-dwim)
	      ("+" . prot-elfeed-toggle-tag)
	      :map elfeed-show-mode-map
	      ("a" . prot-elfeed-show-archive-entry)
	      ("e" . prot-elfeed-show-eww)
	      ("q" . prot-elfeed-kill-buffer-close-window-dwim)
	      ("v" . prot-elfeed-mpv-dwim)
	      ("+" . prot-elfeed-toggle-tag)
	      ("b" . prot-elfeed-bongo-insert-item)))

(use-package prot-elfeed-bongo
  :defer 10
  :straight (:host gitlab :repo "protesilaos/dotfiles" :branch "master"
                   :files ("emacs/.emacs.d/prot-lisp/prot-elfeed-bongo.el"))
  :bind (:map elfeed-search-mode-map
	      ("b" . prot-elfeed-bongo-insert-item)
	      ("h" . prot-elfeed-bongo-switch-to-playlist)
	      :map elfeed-show-mode-map
	      ("b" . prot-elfeed-bongo-insert-item)))

(use-package elfeed-org
  :straight t
  :custom
  (rmh-elfeed-org-files (list "~/Sync/Writing/elfeed.org"))
  :init
  (elfeed-org))

;;; mpv management
(use-package empv
  :straight (empv
	     :type git
	     :host github
	     :repo "isamert/empv.el")
  :custom
  ;; https://y.com.sb/
  ;; https://invidious.lunar.icu/
  (empv-invidious-instance "https://vid.puffyan.us/api/v1")
  :config
  (add-to-list 'empv-mpv-args "--ytdl-format=bestvideo[height<=720]+bestaudio/best[height<=720]")
  ;; I chose Z as a prefix since it is uncommon that I will call it
  ;; all the time. I am not confident on it however.
  :bind (("C-x Z y" . empv-youtube)
	 ("C-x Z t" . empv-toggle-video)
	 ("C-x Z p" . empv-pause)
	 ("C-x Z r" . empv-resume)
	 ("C-x Z m" . empv-play-radio)
	 ("C-x Z v" . empv-play-video)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((org-hugo-section . "posts")
     (org-hugo-base-dir . "/home/green/Sync/Projects/simplest")
     (eval progn
	   (org-remove-from-invisibility-spec
	    '(org-link))
	   (org-restart-font-lock)
	   (setq org-descriptive-links nil)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; password management
(use-package pass
  :straight t
  :custom
  (epa-pinentry-mode 'loopback)
  ;; experimental and probably not a good idea:
  :hook((pass-view-mode . org-mode)))

;;; email
;;;; notmuch
(use-package notmuch
  :straight (:host github :repo "notmuch/notmuch" :branch "master"
                   :files ("emacs/*")))

;; MSMTP setting for multi-smtp sending
(setq message-send-mail-function 'message-send-mail-with-sendmail)

;;;; msmtp
(setq user-full-name "Artyn")
(setq user-mail-adress "AmaziahBender49964@cock.li")
(setq mail-user-agent 'message-user-agent)
(setq mail-specify-envelope-from t)
(setq sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)

