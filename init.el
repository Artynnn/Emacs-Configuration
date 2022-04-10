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
  (user-mail-address "GarethPrichett124115@protonmail.com")
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

;;; programming
;;;; indentation
(use-package aggressive-indent
  :straight t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)))

;;; structured editing
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

;;; org mode
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
  (prot-eww-save-history-file "~/.emacs.d/prot-eww-visited-history")
  (prot-eww-save-visited-history t)
  ;; (prot-eww-save-history-file
  ;;       (locate-user-emacs-file "prot-eww-visited-history"))
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
