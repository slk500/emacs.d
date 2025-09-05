;;; ...  -*- lexical-binding: t -*-

;; https://github.com/vberezhnev/better-org-habit.el?tab=readme-ov-file

;;; buffer box

(use-package buffer-box
  :straight (:host github :repo "rougier/buffer-box"))

;;; ibuffer

(use-package all-the-icons-ibuffer :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package ibuffer :ensure nil
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-title-face 'font-lock-doc-face)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (setq ibuffer-saved-filter-groups
        '(("Main"
           ("Directories" (mode . dired-mode))
           ("C++" (or
                   (mode . c++-mode)
                   (mode . c++-ts-mode)
                   (mode . c-mode)
                   (mode . c-ts-mode)
                   (mode . c-or-c++-ts-mode)))
           ("Python" (or
                      (mode . python-ts-mode)
                      (mode . c-mode)
                      (mode . python-mode)))
           ("Build" (or
                     (mode . make-mode)
                     (mode . makefile-gmake-mode)
                     (name . "^Makefile$")
                     (mode . change-log-mode)))
           ("Scripts" (or
                       (mode . shell-script-mode)
                       (mode . shell-mode)
                       (mode . sh-mode)
                       (mode . lua-mode)
                       (mode . bat-mode)))
           ("Config" (or
                      (mode . conf-mode)
                      (mode . conf-toml-mode)
                      (mode . toml-ts-mode)
                      (mode . conf-windows-mode)
                      (name . "^\\.clangd$")
                      (name . "^\\.gitignore$")
                      (name . "^Doxyfile$")
                      (name . "^config\\.toml$")
                      (mode . yaml-mode)))
           ("Web" (or
                   (mode . mhtml-mode)
                   (mode . html-mode)
                   (mode . web-mode)
                   (mode . nxml-mode)))
           ("CSS" (or
                   (mode . css-mode)
                   (mode . sass-mode)))
           ("JS" (or
                  (mode . js-mode)
                  (mode . rjsx-mode)))
           ("Markup" (or
                   (mode . markdown-mode)
                   (mode . adoc-mode)))
           ("Org" (mode . org-mode))
           ("LaTeX" (name . "\.tex$"))
           ("Magit" (or
                     (mode . magit-blame-mode)
                     (mode . magit-cherry-mode)
                     (mode . magit-diff-mode)
                     (mode . magit-log-mode)
                     (mode . magit-process-mode)
                     (mode . magit-status-mode)))
           ("Apps" (or
                    (mode . elfeed-search-mode)
                    (mode . elfeed-show-mode)))
           ("Fundamental" (or
                           (mode . fundamental-mode)
                           (mode . text-mode)))
           ("Emacs" (or
                     (mode . emacs-lisp-mode)
                     (name . "^\\*Help\\*$")
                     (name . "^\\*Custom.*")
                     (name . "^\\*Org Agenda\\*$")
                     (name . "^\\*info\\*$")
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Backtrace\\*$")
                     (name . "^\\*Messages\\*$"))))))
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-switch-to-saved-filter-groups "Main")))
)

;;; view mode

(setq view-read-only t)

(with-eval-after-load 'view

  (define-key view-mode-map (kbd "a") 'beginning-of-line)
  (define-key view-mode-map (kbd "e") 'end-of-line)
  (define-key view-mode-map (kbd "<return>") 'View-exit)
  (define-key view-mode-map (kbd "u") '(lambda()
                                         (interactive)
                                         (View-scroll-page-backward 3)))
  (define-key view-mode-map (kbd "d") '(lambda()
                                         (interactive)
                                         (View-scroll-page-forward 3)))
  (define-key view-mode-map (kbd "0") 'beginning-of-line)
  (define-key view-mode-map (kbd "$") 'end-of-line)
  (define-key view-mode-map (kbd "g") 'beginning-of-buffer)
  (define-key view-mode-map (kbd "G") 'end-of-buffer)
  (define-key view-mode-map (kbd ";") 'other-window))

(global-set-key (kbd "<escape>") 'view-mode)

(add-hook 'after-save-hook
          (lambda ()
            (when (and buffer-file-name (not view-mode))
              (view-mode 1))))

(add-hook 'view-mode-hook
          (defun view-mode-hookee+ ()
            (setq cursor-type (if view-mode 'hollowe 'bar))))

;; (add-hook 'find-file-hook
;;           (lambda ()
;;             (unless (or (derived-mode-p 'dired-mode))
;;               (view-mode 1))))

;;; desktop
 (desktop-save-mode 1)

;;; backward-forward

(use-package backward-forward
  :demand t
  :config
  (backward-forward-mode t)
  :bind
  (:map backward-forward-mode-map
   ("M-C-<left>" . backward-forward-previous-location)
   ("M-C-<right>" . backward-forward-next-location)))

;;; org-super-agenda

;(use-package org-super-agenda)

;; https://github.com/alphapapa/org-super-agenda/blob/master/examples.org

;;; vundo

(use-package vundo)

;;; habits

(use-package org-habit-plus
  :straight (:host github :repo "myshevchuk/org-habit-plus"))

;; https://github.com/colonelpanic8/org-window-habit

;;; repeat todo

(use-package repeat-todo
  :straight (:host github :repo "cashpw/repeat-todo"))

;;; hide lines

(use-package hide-lines)

;;; highlight-indent-guides

(use-package highlight-indent-guides)

;;; toggle monocle

  (defun my/toggle-maximize-buffer ()
    "Maximize current buffer"
    (interactive)
    (if (one-window-p)
        (jump-to-register '_)
      (window-configuration-to-register '_)
      (delete-other-windows)))

  (keymap-global-set "M-[" #'my/toggle-maximize-buffer)

;;; hide show

    (defun hs-cycle (&optional level)
              (interactive "p")
              (let (message-log-max
                    (inhibit-message t))
                (if (= level 1)
                    (pcase last-command
                      ('hs-cycle
                       (hs-hide-level 1)
                       (setq this-command 'hs-cycle-children))
                      ('hs-cycle-children
                       ;; TODO: Fix this case. `hs-show-block' needs to be
                       ;; called twice to open all folds of the parent
                       ;; block.
                       (save-excursion (hs-show-block))
                       (hs-show-block)
                       (setq this-command 'hs-cycle-subtree))
                      ('hs-cycle-subtree
                       (hs-hide-block))
                      (_
                       (if (not (hs-already-hidden-p))
                           (hs-hide-block)
                         (hs-hide-level 1)
                         (setq this-command 'hs-cycle-children))))
                  (hs-hide-level level)
                  (setq this-command 'hs-hide-level))))

            (defun hs-global-cycle ()
                (interactive)
                (pcase last-command
                  ('hs-global-cycle
                   (save-excursion (hs-show-all))
                   (setq this-command 'hs-global-show))
                  (_ (hs-hide-all))))

    (with-eval-after-load 'hideshow (keymap-set hs-minor-mode-map "C-<tab>" #'hs-cycle)
                          (keymap-set hs-minor-mode-map "C-S-<iso-lefttab>" #'hs-global-cycle))

  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;;; legacy define-key and global-set-key in Emacs
;;;; https://drshapeless.com/blog/posts/legacy-define-key-and-global-set-key-in-emacs.html
;;; long lines, truncate, wrap

  (setq-default truncate-lines t)

;;; sudo

  (use-package sudo-edit)

;;; regexp

  (use-package visual-regexp)

;;; wrap text in parenthesis

 (electric-pair-mode -1)

;;; sort words

(defun sort-words (reverse beg end)
      "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
      (interactive ";;;P\nr")
      (sort-regexp-fields reverse "\\w+" "\\&" beg end))

;;; space

    (defun remove-multiple-spaces-in-region (start end)
      "Replace multiple spaces in the selected region with a single space."
      (interactive "r")
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (goto-char (point-min))
          (while (re-search-forward "  +" nil t)
            (replace-match " ")))))

;;; replace+

(use-package replace+)

;;; windows resizing

(use-package windsize)
(windsize-default-keybindings)
;;https://github.com/grammati/windsize

;;; sum numbers in region

(defun sum-numbers-in-region (start end)
  (interactive "r")
  (message "%s"
           (cl-reduce #'+
                      (split-string (buffer-substring start
                                                      end))
                      :key #'string-to-number)))

;;; non brake space

(setq nobreak-char-display nil)

;;; compare windows

  (keymap-global-set "M-p" #'ace-swap-window)

;;; indent

  (setq electric-indent-mode nil)

;;; babel
;;;; php

  (use-package php-mode)
  (defun org-babel-execute:php (body params)
  "Orgmode Babel PHP evaluate function for `BODY' with `PARAMS'."
  (let;;; ((cmd "php")
         (body (concat "<?php\n" body "\n?>")))
    (org-babel-eval cmd body))

;;;; path

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; [[https://emacs.stackexchange.com/questions/39478/emacs-not-loading-org-agenda-files-on-startup][Emacs not loading org-agenda-files on startup]]
;;; display ^l glyphs as horizontal lines

  (use-package form-feed
    :delight)
  (global-form-feed-mode)

;;; music

    (use-package bongo
    :ensure t :defer t
    :init (progn
            (setq bongo-default-directory "~/Music"
                  bongo-confirm-flush-playlist nil
                  bongo-insert-whole-directory-trees t
                  bongo-action-track-icon nil
                  bongo-display-header-icons nil
                  bongo-logo nil
                  bongo-display-track-icons nil))
    :bind (:map bongo-mode-map ("<delete>" . bongo-kill-line)))

  (setq-default bongo-next-action 'bongo-play-next)

;;; cursor

(setq-default cursor-type 'hollowe)
(set-face-attribute 'cursor nil :background "#FDDA0D")

;;; copy/paste

  (defun my-copy-to-next-window (b e)
  "Copy text in the region to next window."
  (interactive "r")
  (pcase (window-list)
    (`(,w0 ,w1)
     (with-selected-window w1
       (insert-buffer-substring (window-buffer w0) b e)))
    (t (user-error "Only works with 2 windows"))))

;;; whole line

  (use-package whole-line-or-region)

;;; irc

  (require 'erc-services)
  (erc-services-mode 1)
  (setq erc-prompt-for-nickserv-password nil)

  (setq erc-autojoin-channels-alist
	'(( "#emacs" "#systemcrafters")))

  (setq erc-hide-list '("JOIN" "PART" "QUIT"))

  (setq rcirc-default-nick "slk500")
  (setq rcirc-server-alist '((
				   "irc.libera.chat"
				   :channels ("#emacs")
				   :port 6697
				   :encryption tls)))

;;; treemacs

  (use-package treemacs
    :defer t
    :init
    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
    :config
    (progn
      (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
            treemacs-deferred-git-apply-delay   0.5
            treemacs-display-in-side-window     t
            treemacs-file-event-delay           5000
            treemacs-file-follow-delay          0.2
            treemacs-follow-after-init          t
            treemacs-follow-recenter-distance   0.1
            treemacs-git-command-pipe           ""
            treemacs-goto-tag-strategy          'refetch-index
            treemacs-indentation                2
            treemacs-indentation-string         " "
            treemacs-is-never-other-window      nil
            treemacs-max-git-entries            5000
            treemacs-no-png-images              nil
            treemacs-project-follow-cleanup     nil
            treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
            treemacs-recenter-after-file-follow nil
            treemacs-recenter-after-tag-follow  nil
            treemacs-show-cursor                nil
            treemacs-show-hidden-files          t
            treemacs-silent-filewatch           nil
            treemacs-silent-refresh             nil
            treemacs-sorting                    'alphabetic-asc
            treemacs-space-between-root-nodes   t
            treemacs-tag-follow-cleanup         t
            treemacs-tag-follow-delay           1.5
            treemacs-width                      35)

      ;; The default width and height of the icons is 22 pixels. If you are
      ;; using a Hi-DPI display, uncomment this to double the icon size.
      ;;(treemacs-resize-icons 44)

      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t)
      (treemacs-fringe-indicator-mode t)
      (pcase (cons (not (null (executable-find "git")))
                   (not (null (executable-find "python3"))))
        (`(t . t)
         (treemacs-git-mode 'deferred))
        (`(t . _)
         (treemacs-git-mode 'simple))))
    :bind
    (:map global-map
          ("M-0"       . treemacs-select-window)
          ("C-x t 1"   . treemacs-delete-other-windows)
          ("M-1"   . treemacs)
          ("C-x t B"   . treemacs-bookmark)
          ("C-x t C-t" . treemacs-find-file)
          ("C-x t M-t" . treemacs-find-tag)))

;;; ledger

(use-package ledger-mode)

;;; clock

(use-package org-multi-clock
  :straight (org-multi-clock :type git :host gitlab :repo "OlMon/org-multi-clock" :branch "master"))

; org-clock-in into any task in your org files. To create a parallel clock use the omc-make-new-parallel-clock. This clock will be the active clock.

;;; diff

; https://difftastic.wilfred.me.uk/

(setq
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain)

;;; htmlize

(use-package htmlize)

;;; info+

(use-package info+)

;;; org-ql

(use-package org-ql)

;;; casual

(use-package casual
  :ensure nil
  :bind 
  (:map org-agenda-mode-map ("?" . casual-agenda-tmenu))
  (:map dired-mode-map ("?" . casual-dired-tmenu)))

;;; healh-template

(use-package gnuplot)

(use-package health-template
  :straight (:host gitlab :repo "dto/health-template"))

;;; centered-window-mode

(use-package centered-window)

;;; no-littering

(use-package no-littering)

;;; visual-fill-column-mode

(use-package visual-fill-column)

;;; eshell

;; https://www.youtube.com/watch?v=__f9A9uYJkE

(use-package eshell
  :ensure nil)

(use-package eshell-syntax-highlighting
  :after eshell-mode
  :ensure t ;; Install if not already installed.
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

(defun eshell/cat-with-syntax-highlighting (filename)
  "Like cat(1) but with syntax highlighting.
Stole from aweshell"
  (let ((existing-buffer (get-file-buffer filename))
        (buffer (find-file-noselect filename)))
    (eshell-print
     (with-current-buffer buffer
       (if (fboundp 'font-lock-ensure)
           (font-lock-ensure)
         (with-no-warnings
           (font-lock-fontify-buffer)))
       (let ((contents (buffer-string)))
         (remove-text-properties 0 (length contents) '(read-only nil) contents)
         contents)))
    (unless existing-buffer
      (kill-buffer buffer))
    nil))
(advice-add 'eshell/cat :override #'eshell/cat-with-syntax-highlighting)

;;; projectile

(use-package projectile
  :config (projectile-mode))

(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
;(setq projectile-project-search-path '(("~/builds/emacs-projects/" )))


;;; speedup on large file after reconfiguring whitespace-mode

(setq whitespace-style '(space-mark tab-mark))

;;; color

(use-package colorful-mode
  :straight
  (:host github :repo "DevelopmentCool2449/colorful-mode")
  :config
  (global-colorful-mode t))

;;; divider

(setq window-divider-default-bottom-width 8)
(setq window-divider-default-right-width 8)
(setq window-divider-default-places t)

(window-divider-mode 1)

;;; youtube

(use-package yeetube)

(setf yeetube-display-thumbnails nil)

;;; fill-mode

(setq-default fill-column 60)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; palimpset

(use-package palimpsest
  :bind
  ("M-w" . palimpsest-move-region-to-bottom))

;;; theme

(use-package doom-themes
  :config
  (load-theme 'doom-ayu-dark t))

(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)


;;; toggle-fill-paragraph

(defun my-fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'my-fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively 'fill-paragraph nil (vector nil t))))

(global-set-key [remap org-fill-paragraph]
                'my-fill-or-unfill)

;;; latitude

(setq calendar-latitude 52.237049
      calendar-longitude 21.017532)

;;; elfeed, rss

(use-package elfeed)
(use-package elfeed-org)

(elfeed-org)

;; Optionally specify a number of files containing elfeed
;; configuration. If not set then the location below is used.
;; Note: The customize interface is also supported.
(setq rmh-elfeed-org-files (list "~/aamystuff/rss.org"))

;;; gpt ai

(use-package gptel
  :defer t ;; because .auth should be decrypt first (gptel-api-key-from-auth-source)
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-api-key (gptel-api-key-from-auth-source)
	gptel-model 'gpt-4o))

;;; tetris

(setq gamegrid-glyph-height-mm 10.0)

;;; publish

(require 'ox-publish)

(defun m/org-publish-org-sitemap-format-entry (entry style project)
  (cond ((not (directory-name-p entry))
         (let* ((date (org-publish-find-date entry project)))
           (format "%s - [[file:%s][%s]]"
                   (format-time-string "%F" date) entry
                   (org-publish-find-title entry project))))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(setq org-publish-project-alist
      `(("pages"
	 :auto-sitemap t
	 :sitemap-filename "index.org"
	 :sitemap-title "Slawomir Grochowski homepage"
	 :sitemap-style tree
	 :sitemap-sort-files anti-chronologically
	 :sitemap-format-entry m/org-publish-org-sitemap-format-entry
         :base-directory "~/aamystuff/slawomir-grochowski.com/org"
         :base-extension "org"
         :recursive t
         :publishing-directory "/ssh:root@51.178.48.169:/var/www/slawomir-grochowski.com"
;;         :publishing-directory "~/aamystuff/slawomir-grochowski.com/html"
	 :html-doctype "html5"
	 :html-html5-fancy t
	 :html-head-include-scripts nil
	 :html-head-include-default-style t
	 :html-head "<link href= \"../static/style.css\" rel=\"stylesheet\" type=\"text/css\" />"
         :publishing-function org-html-publish-to-html)
        ("static"
         :base-directory "~/aamystuff/slawomir-grochowski.com/static"
         :base-extension "css\\|txt\\|jpg\\|jpeg\\|gif\\|png"
         :recursive t
;;         :publishing-directory "~/aamystuff/slawomir-grochowski.com/static"
         :publishing-directory  "/ssh:root@51.178.48.169:/var/www/slawomir-grochowski.com/static/"
         :publishing-function org-publish-attachment)

        ("slawomir-grochowski.com" :components ("pages" "static"))))

;;; modeline

(setq column-number-mode nil)
(setq-default mode-line-format (delq 'mode-line-modes mode-line-format))

;; remove Git:master from modeline
(setq-default mode-line-format (delete '(vc-mode vc-mode) mode-line-format))

;; add point display to mode-line construct
(add-to-list 'mode-line-position
	     '(show-point-mode (5 (:eval (format "(%d)" (point)))))
	     'append)

(define-minor-mode show-point-mode
  "Toggle show-point mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, the value of `point' is displayed in the
mode-line (after the line and column numbers, if those are being
displayed too).")



;;; help

(use-package elisp-demos
  :straight (:host github :repo "xuchunyang/elisp-demos")
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package helpful
;;  :commands (helpful-callable helpful-variable helpful-command helpful-key) - to chyba nie ma sensu
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-command] . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   ("C-h l" . find-library)
   :map emacs-lisp-mode-map
   ("C-c C-d" . helpful-at-point)
   :map helpful-mode-map
   ("C-c C-d" . helpful-at-point)))

(setq helpful-switch-buffer-function #'+helpful-switch-to-buffer)

(defun +helpful-switch-to-buffer (buffer-or-name)
  "Switch to helpful BUFFER-OR-NAME.

The logic is simple, if we are currently in the helpful buffer,
reuse it's window, otherwise create new one."
  (if (eq major-mode 'helpful-mode)
      (switch-to-buffer buffer-or-name)
    (pop-to-buffer buffer-or-name)))


;;; hide copyright

(add-hook 'emacs-lisp-mode-hook 'elide-head-mode)

;;; search web

(use-package engine-mode
  :config
  (engine-mode t)
  (engine/set-keymap-prefix (kbd "C-c e"))
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g"))

;;; coding system
(set-language-environment "UTF-8")

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

;;; which-key

(which-key-mode 1)

;; (setq which-key-persistent-popup t)

;;; text language
;;;; translate
; https://www.reddit.com/r/emacs/comments/1b1s7wk/grammarly_in_emacs/ TODO
; https://github.com/emacs-languagetool

(use-package gt
  :config
  (setq gt-langs '(pl en)
	gt-default-translator (gt-translator
			       :taker   (gt-taker :text 'buffer :pick 'paragraph)
			       :engines (list (gt-bing-engine) (gt-google-engine) (gt-chatgpt-engine))
			       :render  (gt-buffer-render))
 	gt-chatgpt-key (auth-source-pick-first-password :host "api.openai.com")))


(add-hook 'gt-buffer-render-output-hook 'visual-line-mode)

;;;; langtool

(use-package langtool)
(setq langtool-language-tool-jar "~/apps/LanguageTool-6.4/languagetool-commandline.jar")

;;; moves the point to the newly created window after splitting

;; (defadvice split-window (after move-point-to-new-window activate)
;;   "Moves the point to the newly created window after splitting."
;;   (other-window 1))

(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

;;; hungry delete

(use-package hungry-delete)
(setq hungry-delete-join-reluctantly t
      backward-delete-char-untabify-method 'all) ; to work with paredit https://emacs.stackexchange.com/questions/33734/how-to-get-hungry-delete-working-in-paredit-mode
(global-hungry-delete-mode)

;;; inhibit startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(defun display-startup-echo-area-message ()
  (message ""))

;;; magit

;;Symbolic link to Git-controlled source file; follow link?
(setq vc-follow-symlinks t)

(use-package magit
:config
(setq magit-branch-read-upstream-first 'fallback
      magit-log-section-commit-count 50)
(dolist (m (list magit-diff-mode-map
                 magit-file-section-map
                 magit-hunk-section-map
                 magit-unstaged-section-map
                 magit-staged-section-map))
  (define-key m (kbd "C-c") 'cua-copy-region))
:bind
(("C-x g" . magit-status)))


(defun copy-diff-region ()
  "Copy diff region without + or - markers."
  (interactive)
  (deactivate-mark)
  (let ((text (buffer-substring-no-properties
               (region-beginning) (region-end))))
    (kill-new (replace-regexp-in-string "^[\\+\\-]" "" text))))

;;; pomodoro

(use-package hammy)

(use-package svg-lib)

(setq image-types '(svg png gif tiff jpeg xpm xbm pbm))

(defun my/org-pomodoro-clock-in ()
  (interactive)
  (if (derived-mode-p 'org-agenda-mode)
      ;; For org-agenda buffers
      (let ((marker (or (org-get-at-bol 'org-hd-marker)
                        (org-get-at-bol 'org-marker))))
        (when marker
          (with-current-buffer (marker-buffer marker)
            (goto-char marker)
            (if (org-entry-get nil "Effort")
                (org-clock-in)
              (org-pomodoro)))))
    ;; For org-mode buffers
    (if (org-entry-get nil "Effort")
        (org-clock-in)
      (org-pomodoro))))

(use-package org-pomodoro
  :config
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))
  (setq org-pomodoro-format "%s")
  :bind
  ("<f6>" . my/org-pomodoro-clock-in))

;;; savehist

(use-package savehist
  :init
  (savehist-mode))

;;; mail, email, notmuch
;; https://myaccount.google.com/apppasswords

(setq display-time-mail-string "") ;; remove "Mail" in mode line

(use-package notmuch
  :bind
  (:map global-map
	("C-c m" . notmuch-hello)
	:map notmuch-message-mode-map
	("C-s" . notmuch-draft-save)
	:map notmuch-show-mode-map
	("r" . notmuch-show-reply)
	("t" . capture-mail))
  :init
  (setq-default notmuch-search-oldest-first nil)
  (setq notmuch-show-logo nil)
  :config
  (keymap-set notmuch-search-mode-map "<delete>"
	      (lambda (&optional beg end)
		"Mark thread as spam"
		(interactive (notmuch-interactive-region))
		(notmuch-search-tag (list "+deleted" "-inbox") beg end)))

  (define-key notmuch-show-mode-map "b"
	      (lambda (&optional address)
		"Bounce the current message."
		(interactive "sBounce To: ")
		(notmuch-show-view-raw-message)
		(message-resend address))))

(setq notmuch-fcc-dirs "sent +sent -unread")

 (setq notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key
	    "i")
     (:name "todo" :query "tag:todo" :key
	    "t")
     (:name "emacs" :query "tag:emacs" :key
	    "e")
     (:name "emacs-org" :query "tag:emacs-org" :key
	    "o")
     (:name "flagged" :query "tag:flagged" :key
	    "f")
     (:name "sent" :query "tag:sent" :key
	    "s")
     (:name "drafts" :query "tag:draft" :key
	    "d")
     (:name "work" :query "tag:work" :key
	    "w")
     (:name "all mail" :query "*" :key
	    "a")))

(setq message-send-mail-function 'smtpmail-send-it
      user-mail-address "slawomir.grochowski@gmail.com"
      user-full-name "Slawomir Grochowski"
      smtpmail-stream-type 'ssl
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      smtpmail-debug-verb t
      mail-user-agent 'sendmail-user-agent
      smtpmail-debug-info t
      message-signature "Slawomir Grochowski")

(setq auth-sources
      '((:source "~/aamystuff/.authinfo.gpg"))
      auth-source-debug t)
(setq auth-source-do-cache nil)

;;;; jump to latest

(defun notmuch-show-jump-to-latest ()
  "Jump to the message in the current thread with the latest
timestamp."
  (let ((timestamp 0)
	    latest)
    (notmuch-show-mapc
     (lambda () (let ((ts (notmuch-show-get-prop :timestamp)))
		      (when (> ts timestamp)
			(setq timestamp ts
			      latest (point))))))
    (if latest
	    (goto-char latest)
      (error "Cannot find latest message."))))

;;;; store link
;; (org-add-link-type "notmuch" 'org-notmuch-open)
;; (add-hook 'org-store-link-functions 'org-notmuch-store-link)

(org-link-set-parameters "notmuch"
			 :follow 'org-notmuch-open
			 :store 'org-notmuch-store-link)

(defun org-notmuch-open (id)
  "Visit the notmuch message or thread with id ID."
  (notmuch-show id))

(defun org-notmuch-store-link ()
  "Store a link to a notmuch mail message."
  (pcase major-mode
    ('notmuch-show-mode
     ;; Store link to the current message
     (let* ((id (notmuch-show-get-message-id))
	    (link (concat "notmuch:" id))
	    (description (format "Mail: %s" (notmuch-show-get-subject))))
       (org-store-link-props
	:type "notmuch"
	:link link
	:description description)))
    ('notmuch-search-mode
     ;; Store link to the thread on the current line
     (let* ((id (notmuch-search-find-thread-id))
	    (link (concat "notmuch:" id))
	    (description (format "Mail: %s" (notmuch-search-find-subject))))
       (org-store-link-props
	:type "notmuch"
	:link link
	:description description)))))

;;;; date display

;; (defun notmuch-show/format-date (&rest args)
;;   (let* ((args (car args))
;;          (headers (plist-get (nth 0 args) :headers))
;; 	 (date (plist-get headers :Date))
;;          (parsed-date (parse-time-string date))
;;          (fmt-date (format-time-string
;; 		    "%a, %d-%m-%Y %H:%M"
;;                     (apply #'encode-time (parse-time-string date)))))
;;     (plist-put headers :Date fmt-date)
;;     args))

;; (advice-add #'notmuch-show-insert-headerline :filter-args #'notmuch-show/format-date)

;;;; hide patches

  (advice-add 'notmuch-show-insert-bodypart :filter-args 'my/notmuch-hide-content)

  (defvar my/notmuch-hide-content-types '("text/x-patch" "text/x-diff"))

  (defun my/notmuch-hide-content (args)
    (cl-destructuring-bind (msg part depth . hide) args
      (list msg part depth
            (if-let ((mime-type (notmuch-show-mime-type part))
                     (_ (seq-some (lambda (type) (notmuch-match-content-type mime-type type))
                                  my/notmuch-hide-content-types)))
                t (car hide)))))

;;;; org-capture

(defun capture-mail()
  "Capture mail to org mode."
  (interactive)
  (org-store-link nil)
  (org-capture nil "r"))

;;;; using org-mode in composing an email

(use-package org-mime)

;;;; see the recipient address instead of your address when listing sent messages

(defun my/notmuch-unthreaded-show-recipient-if-sent (format-string result)
(let* ((headers (plist-get result :headers))
       (to (plist-get headers :To))
       (author (plist-get headers :From))
       (face (if (plist-get result :match)
                 'notmuch-tree-match-author-face
               'notmuch-tree-no-match-author-face)))
  (propertize
   (format format-string
           (if (string-match "slawomir.grochowski@gmail.com" author)
               (concat "↦ " (notmuch-tree-clean-address to))
               (notmuch-tree-clean-address to)
             author))
   'face face)))

(setq notmuch-unthreaded-result-format
      '(("date" . "%12s  ")
        (my/notmuch-unthreaded-show-recipient-if-sent . "%-20.20s")
        ((("subject" . "%s"))
         . " %-54s ")
        ("tags" . "(%s)")))

;;;; avoid forgetting the subject

(defun my-notmuch-mua-empty-subject-check ()
  "Request confirmation before sending a message with empty subject"
  (when (and (null (message-field-value "Subject"))
             (not (y-or-n-p "Subject is empty, send anyway? ")))
    (error "Sending message cancelled: empty subject.")))
(add-hook 'message-send-hook 'my-notmuch-mua-empty-subject-check)

;;; table

(defun org-table-strip-table-at-point ()
  (interactive)
  (let* ((table (delete 'hline (org-table-to-lisp)))
     (contents (orgtbl-to-generic
            table '(:sep "\t"))))
    (goto-char (org-table-begin))
    (re-search-forward "|")
    (backward-char)
    (delete-region (point) (org-table-end))
    (insert contents)))

;;; debug on error

(setq debug-on-error t)

;;; hl-line-mode

(defun my/column-view ()
  (interactive)
  (hl-line-mode -1)  ;; Disable first, so the setting can be made before it starts
  (setq-local hl-line-overlay-priority +50)  ;; this overlay needs higher priority than column-view’s
  (hl-line-mode)
  (org-columns))

(global-set-key [remap org-columns]
                'my/column-view)

(global-hl-line-mode t)
(set-face-background 'hl-line "grey13")

(defun show-overlay-priorities ()
  "Show the priorities of overlays at the current point."
  (interactive)
  (let ((overlays (overlays-at (point))))
    (if overlays
        (message "Overlays at point:\n%s"
                 (mapconcat (lambda (ov)
                              (format "Overlay: %s, Priority: %s"
                                      ov
                                      (overlay-get ov 'priority)))
                            overlays "\n"))
      (message "No overlays at point."))))

;;; ui

(setq-default
 cursor-in-non-selected-windows nil) ; Hide the cursor in inactive windows

;;;; unbind commands

(global-unset-key (kbd "C-h <RET>")) ; view-order-manuals
(global-unset-key (kbd "C-h g")) ; describe-gnu-project

;;; kill ring

(use-package browse-kill-ring)

;;; tempel

;; Configure Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

;;  (setq tempel-path)
  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)

;;; narrow

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; (define-key endless/toggle-map "n"
;;   #'narrow-or-widen-dwim)
;; ;; This line actually replaces Emacs' entire narrowing
;; ;; keymap, that's how much I like this command. Only
;; ;; copy it if that's what you want.
;; (define-key ctl-x-map "n" #'narrow-or-widen-dwim)
;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (define-key LaTeX-mode-map "\C-xn"
;;               nil)))

;;; clojure

; commeted out - bc - errors after update to master emacs 30 with theme doom

;  (require 'ob-clojure)
 ; (use-package cider)
 ; (require 'cider)
;  (setq org-babel-clojure-backend 'cider)

;  (define-key clojure-mode-map (kbd "\C-x \C-e") 'cider-eval-last-sexp)
;  (global-set-key (kbd "\C-x \C-e") 'cider-eval-last-sexp)

;  (define-key clojure-mode-map (kbd "M-RET") 'lsp-execute-code-action)

  ;; (add-hook 'cider-repl-mode-hook #'paredit-mode)
  ;; (add-hook 'cider-mode-hook #'paredit-mode)
  ;; (add-hook 'clojure-mode-hook #'paredit-mode)
  ;; (add-hook 'clojure-mode-hook #'lsp-mode)

;;; treesitter

;;(require 'treesit)

;; (add-to-list
;;   'treesit-language-source-alist
;;   '(php "https://github.com/tree-sitter/tree-sitter-php.git")
;;   '(clojure "http://github.com/sogaiu/tree-sitter-clojure.git")
;;   '(bash "https://github.com/tree-sitter/tree-sitter-bash.git"))

(setq treesit-load-name-override-list '((clojure "libtree-sitter-clojure" "libtree-sitter-clojure")))
							
(add-to-list 'major-mode-remap-alist
	     '(php-mode . php-ts-mode)
	     '(sh-mode . bash-ts-mode))

;; (use-package clojure-ts-mode
;;     :straight (:host github :repo "clojure-emacs/clojure-ts-mode"))

(use-package php-ts-mode
    :straight (:host github :repo "emacs-php/php-ts-mode"))

;;; org-mode

;https://stackoverflow.com/questions/75900632/filter-custom-org-agenda-view-to-see-done-items-in-past-week TODO

(use-package org-view-mode)

(use-package org-menu
  :straight (:host github :repo "sheijk/org-menu"))

(keymap-global-set "C-c !" #'org-timestamp-inactive)

(use-package org
  :config
  (setq-default org-fold-catch-invisible-edits 'error) ;; dosent work with hungry delete!!!!
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-log-buffer-setup-hook #'auto-fill-mode)
  (use-package org-contrib)
  (setq org-M-RET-may-split-line '((default . nil)) ;; don't split line, just create the new heading
	org-insert-heading-respect-content nil
	org-adapt-indentation t
	org-startup-folded t
	org-hide-emphasis-markers t
	org-log-done 'time
	org-log-reschudle 'time
	org-log-redeadline 'time
	org-log-into-drawer t
	org-use-fast-todo-selection 'expert ;; todo selection appear in the smaller via minibuffer
	org-special-ctrl-a/e t ;; ctrl a move to begining of heading not line, w lispie powinień na koniec wyrażenie bo
	                       ;; jak jest komentarz to idzie na koniec komentarza
	org-treat-insert-todo-heading-as-state-change t
	initial-major-mode 'org-mode
	org-ellipsis "⤵"
	org-agenda-sticky t
	org-checkbox-hierarchical-statistics nil
	bookmark-set-fringe-mark nil
	org-log-reschedule 'note
	org-log-redeadline 'note
	org-log-note-purpose '(deldeadline delschedule)
	org-use-speed-commands t
	org-src-tab-acts-natively t
	org-cycle-level-after-item/entry-creation nil) ;; that default behaviour is extremally confusing, especially for beginers
  (setq org-log-note-headings (assq-delete-all 'note org-log-note-headings))
  (add-to-list 'org-log-note-headings '(note . "%t"))
    (require 'org-tempo)
  (require 'org-eldoc)
  (global-eldoc-mode 1))

;;;; org-note

;; remove Insert note for this entry.
;; Znalazłem funkcje ale jest ona wpisana na sztywno
;; więc musze skopiować całą funkcję i zmienić to jedno miejsce

(defun org-add-log-note (&optional _purpose)
  "Pop up a window for taking a note, and add this note later."
  (when (and (equal org-log-note-this-command this-command)
             (= org-log-note-recursion-depth (recursion-depth)))
    (remove-hook 'post-command-hook 'org-add-log-note)
    (setq org-log-setup nil)
    (setq org-log-note-window-configuration (current-window-configuration))
    (move-marker org-log-note-return-to (point))
    (pop-to-buffer (marker-buffer org-log-note-marker) '(org-display-buffer-full-frame))
    (goto-char org-log-note-marker)
    (pop-to-buffer "*Org Note*" '(org-display-buffer-split))
    (erase-buffer)
    (if (memq org-log-note-how '(time state))
        (org-store-log-note)
      (let ((org-inhibit-startup t)) (org-mode))
      (insert (format "# Insert note for %s.\n\n"
                      (cl-case org-log-note-purpose
                        (clock-out "stopped clock")
                        (done  "closed todo item")
                        (reschedule "rescheduling")
                        (delschedule "no longer scheduled")
                        (redeadline "changing deadline")
                        (deldeadline "removing deadline")
                        (refile "refiling")
                        (note "this entry")
                        (state
                         (format "state change from \"%s\" to \"%s\""
                                 (or org-log-note-previous-state "")
                                 (or org-log-note-state "")))
                        (t (error "This should not happen")))))
      (when org-log-note-extra (insert org-log-note-extra))
      (setq-local org-finish-function 'org-store-log-note)
      (run-hooks 'org-log-buffer-setup-hook))))


;;;; org-tidy

(use-package org-tidy)

;;;; org-table

(with-eval-after-load 'org-table
  (org-defkey org-table-fedit-map [return] #'org-table-insert-row))

;;;; export

(setq org-export-with-email t)
(setq org-html-validation-link nil)
(setq org-export-allow-bind-keywords t)
(setq org-image-actual-width nil)

;;;;; export underscore as underscore instead of highlight in HTML

(setq org-use-sub-superscripts nil)
(setq org-export-with-sub-superscripts nil)

;;;; do not insert line between headers

(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

;;;; http

(use-package ob-http
  :straight (:host github :repo "zweifisch/ob-http"))

;;;; babel

   (setq org-confirm-babel-evaluate nil)
   (setq org-babel-default-header-args
         (cons '(:results . "output replace")
               (assq-delete-all :results org-babel-default-header-args)))
   (setq org-structure-template-alist (assoc-delete-all "e" org-structure-template-alist))
   (setq org-structure-template-alist (assoc-delete-all "c" org-structure-template-alist))
   (setq org-structure-template-alist (assoc-delete-all "s" org-structure-template-alist))
   (add-to-list 'org-structure-template-alist '("e" . "src elisp"))
   (add-to-list 'org-structure-template-alist '("c" . "src clojure"))
   (add-to-list 'org-structure-template-alist '("b" . "src bash"))
   (add-to-list 'org-structure-template-alist '("s" . "src sql"))
   (add-to-list 'org-structure-template-alist '("t" . "src text"))

  (org-babel-do-load-languages
   'org-babel-load-languages 
   '((shell . t)
     (sql . t)
     (http . t)
     (clojure . t)
     (gnuplot . t)))

;;;; tags

(setq org-tag-alist '(("book" . ?b)
		      ("emacs" . ?e)
		      ("email" . ?m)
		      ("sms" . ?s)
		      ("video" . ?v)
		      ("noexport" . ?n)))

;;;; todo keywords

(setq org-todo-keywords
      '((sequence "TODO(t!)" "STUCK(k!)" "DOING(o!)" "NEXT(n!)" "WAITING(w@)" "SOMEDAY(s@)"
                  "|" "CANCELED(c@)" "DONE(d!)"))) ;; WAIT not WAITING
 
;;;; org-agenda

;; TODO https://youtu.be/a_WNtuefREM Making Org Agenda Look Beautiful

;; Zaznaczenie danego taska jako DONE - analogicznie jak się zaznacza checkboxa.
;; Wcześniej był skrót to tagowania: ale przecież juz jest skrót C-c C-q
(defun sacha/org-agenda-done (&optional arg)
  "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE")
  (org-agenda-redo-all))
;; Override the key definition for org-exit
;(define-key org-agenda-mode-map (kbd "C-c c") nil)
(define-key org-agenda-mode-map (kbd "C-c C-c") 'sacha/org-agenda-done)

;; wyświetla start od dzisiejszego dnia, a nie jak wcześniej od poniedziałku
(setq org-agenda-start-on-weekday nil)

;; org mode - prevent future repetitive entries from showing up in agenda view
;; https://emacs.stackexchange.com/questions/12609/org-mode-prevent-future-repetitive-entries-from-showing-up-in-agenda-view
;; natomiast nie znalazłem ustawienia które nie wyświetlało by entries from the past
;; ale może rozsądniej jest po prostu zaczynać agenda view od tego dnia co jest teraz czyli tak
;; jakbym nacisnąc 'D' na danym aktualnie dniu - chociaż chciałbym jednak zobaczyć też jakie są taski na następne dni
;; Bo teraz jest tak, że pokazuje się cały tydzień czyli jeśli jest niedziela to widzę wszystkie dni od poniedziałku do niedzieli
;; raczej powinno to się samo przesuwąć żeby zawsze widział parę dni do przodu, a dopiero czekam na poniedziałek i nagle przewracasz stronę
;; a tam masa tasków o których nie byłeś do końca świadomy.
(setq org-agenda-show-future-repeats nil)

(defun my-gtd ()  
  (interactive)
  (org-agenda nil "g"))

(defun my-org-agenda-a ()
  (interactive)
  (org-agenda nil "a"))

(global-set-key (kbd "<f9>") (lambda () (interactive) (org-agenda nil "g")))

(global-set-key (kbd "C-c l") 'org-store-link)

(defadvice org-agenda (around split-vertically activate)
  (let ((split-width-threshold 80))  ; or whatever width makes sense for you
    ad-do-it))

(defun my-signum (x)
  (cond
   ((> x 0) 1)
   ((< x 0) -1)
   (t 0)))

(defun cmp-date-property (prop)
  "Compare two `org-mode' agenda entries, `A' and `B', by some date property.
		If a is before b, return -1. If a is after b, return 1. If they
		are equal return nil.
	      https://emacs.stackexchange.com/questions/26351/custom-sorting-for-agenda"
  (let ((prop prop))
    #'(lambda (a b)
	(let* ((a-pos (get-text-property 0 'org-marker a))
	       (b-pos (get-text-property 0 'org-marker b))
	       (a-date (or (org-entry-get a-pos prop)
			   (format "[%s]" (org-read-date t nil "1986-01-01"))))
	       (b-date (or (org-entry-get b-pos prop)
			   (format "[%s]" (org-read-date t nil "1986-01-01"))))
	       (cmp (compare-strings a-date nil nil b-date nil nil))
	       )
	  (if (eq cmp t) nil (my-signum cmp))))))

(setq org-agenda-custom-commands
      '(("a" "default" agenda "" ((org-scheduled-past-days 1)
				  (org-deadline-past-days 1)
				  (org-habit-scheduled-past-days 10000)
				  (org-deadline-warning-days 0)))
	("Z" "org-ql test"
	 ((org-ql-block '(or (ts-active :on today) (and (habit) (scheduled :to today))))))
	;; use org-ql

	("b" "List of read books" tags "book/DONE|DOING|CANCELED|STUCK|LOOKINGFOR"
	 ((org-agenda-files (append org-agenda-files (directory-files-recursively "~/aamystuff/mystuff/" "\\.org$")))
	  (org-agenda-cmp-user-defined (cmp-date-property "CLOSED"))
	  (org-agenda-sorting-strategy '(user-defined-down todo-state-down priority-down))
	  (org-agenda-todo-keyword-format "%-2s")
	  (org-agenda-prefix-format "%(if (org-entry-get nil \"CLOSED\") (format \"%s \"(truncate-string-to-width (org-entry-get nil \"CLOSED\") 11 1)) \"\")")
	  ) nil ("~/aamystuff/books.html"))
	("s" "Someday" tags-todo "-book-video/SOMEDAY")
	("e" "Emacs" tags-todo "+emacs/-SOMEDAY"
	 ((org-agenda-overriding-header
	   (format "EMACSs (%s)" (org-agenda-count "elo")))))
	("g" "Get Things Done (GTD)"
	 ((agenda "")
	  (tags-todo "-book-video-emacs/TODO"
		     ((org-agenda-overriding-header
		       (format "TODOs (%s)" (org-agenda-count "bar")))))
	  (tags-todo "-emacs/WAITING"
		((org-agenda-overriding-header
		  (format "WAITINGs (%s)" (org-agenda-count "foo")))))
	  (tags-todo "book/DOING"
		     ((org-agenda-overriding-header
		       (format "BOOKs (%s)" (org-agenda-count "book")))))))))

(setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
				 (todo . " ")
				 (tags . " ")
				 (search . " %i %-12:c")))

(setq org-agenda-hide-tags-regexp (regexp-opt '("book")))

(keymap-global-set "C-c a" #'org-agenda)
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-deadline-warning-days 14
      org-agenda-show-future-repeats nil)

(when (string= 'slk user-login-name)
  (setq org-default-notes-file "~/aamystuff/life/todos.org.gpg")
  (setq org-agenda-files (append '("~/aamystuff/life/life.org.gpg"
				   "~/aamystuff/life/todos.org.gpg"
;				   "~/aamystuff/job/job.org"
				   "~/aamystuff/phprefactor/phprefactor.org"
				   "~/aamystuff/emacs/emacs.org"
		;;		   "~/aamystuff/clojure/clojure-examples.org"
				   "~/aamystuff/mystuff/books.org"
		;;		   "~/aamystuff/mystuff/psychology.org"
				   )
					; (directory-files-recursively "~/aamystuff/slawomir-grochowski.com/" "\\.org$")
				 )))

(add-hook 'org-agenda-finalize-hook
	  (lambda ()
	    (save-excursion
	      (goto-char (point-min))
	      (when (or (re-search-forward "Global list of TODO items of type: [[:upper:]]*" nil t)
			(re-search-forward "Headlines with TAGS match: [[:upper:]]*" nil t))
		(insert (format " (%s remaining)"
				(- (count-lines (point-min) (point-max)) 2)))))))

(add-hook 'org-agenda-finalize-hook
	  (lambda ()
	    (save-excursion
	      (goto-char (point-min))
	      (when (re-search-forward "Tasks*" nil t)
		(insert (format " (%d remaining)" (- (count-lines (point) (point-max) t) 2)))))))

(defun my/org-agenda-adjust-text-size ()
  (if (= text-scale-mode-amount 0)
      (text-scale-adjust -2)))

(add-hook 'org-agenda-finalize-hook #'my/org-agenda-adjust-text-size)

(setq org-agenda-sorting-strategy '((todo todo-state-up priority-down)
				    (tags todo-state-up priority-down)))

;;;; org-agenda-count

(use-package org-agenda-count
  :straight (:host github :repo "sid-kurias/org-agenda-count"))

;;;; day counter
;; https://www.reddit.com/r/orgmode/comments/13fgc09/orgagenda_elapsed_dayscounter_of_days/

;;;; imenu

(setq org-imenu-depth 6)
(setq org-goto-interface 'outline-path-completionp)
(setq org-outline-path-complete-in-steps nil)

;;;; org-capture

(keymap-global-set "<f10>" #'org-capture)

(when (string= 'slk user-login-name)
  (setq org-capture-templates
	'(("r" "Email reply" entry
	   (file "~/aamystuff/life/todos.org.gpg")
	   "* TODO %a :email:" :prepend t)
	  ("m" "Meeting" entry
           (file "~/aamystuff/life/todos.org.gpg")
           "* TODO %? :meeting:" :prepend t)
	  ("t" "Task" entry
           (file "~/aamystuff/life/todos.org.gpg")
           "* TODO %?" :prepend t)
	  ("v" "Event" entry
           (file "~/aamystuff/life/todos.org.gpg")
           "* %? :event:" :prepend t)
	  ("e" "Emacs Task" entry
           (file "~/aamystuff/life/todos.org.gpg")
           "* TODO %? :emacs:" :prepend t))))

;;; shell here

(use-package shell-here)
;(define-key (current-global-map) "\C-c!" 'shell-here)

;;; spelling

;; https://sjp.pl/sl/en/ - ściągnąć polski słownik
;; musiałem też zainstalować sudo apt-get install hunspell-pl

(use-package jinx
  :straight (:host github :repo "minad/jinx")
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(setq jinx-languages "pl_PL en_US")

;;; skip system buffers when cycling

;; (set-frame-parameter (selected-frame) 'buffer-predicate
;;              (lambda (buf) (not (string-match-p "^*" (buffer-name buf)))))

;;; snippets

(defun my/copy-current-buffer-file-name ()
  (interactive)
  (kill-new (buffer-file-name)))

;;; long lines

(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;;; smooth scroll

(use-package ultra-scroll
  :straight (:host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
    scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(setq scroll-conservatively 101
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t
      scroll-margin 0
      scroll-preserve-screen-position t)

;;; text properties

(defun remove-display-text-property (start end)
  "Remote all text properties from START to END.
This is useful when copying stuff with a display property set
from elsewhere."
  (interactive "r")
  (set-text-properties start end nil))

(defun my-make-word-red (begin end)
  "make current region colored red, using text properties"
  (interactive (list (region-beginning) (region-end)))
  (put-text-property begin end 'font-lock-face '(:foreground "red")))

(defun my-make-word-green (begin end)
  "make current region colored red, using text properties"
  (interactive (list (region-beginning) (region-end)))
  (put-text-property begin end 'font-lock-face '(:foreground "green")))
;;; org-web-tools

(use-package org-web-tools)

;;; edebug

(setq edebug-print-level 100
      edebug-print-length 1000
      edebug-print-circle nil
      eval-expression-print-level 50
      eval-expression-print-length 1000)

;;;; eros
; https://xenodium.com/inline-previous-result-and-why-you-should-edebug/

(defun adviced:edebug-compute-previous-result (_ &rest r)
  "Adviced `edebug-compute-previous-result'."
  (let ((previous-value (nth 0 r)))
    (if edebug-unwrap-results
        (setq previous-value
              (edebug-unwrap* previous-value)))
    (setq edebug-previous-result
          (edebug-safe-prin1-to-string previous-value))))

(advice-add #'edebug-compute-previous-result
            :around
            #'adviced:edebug-compute-previous-result)

(defun adviced:edebug-previous-result (_ &rest r)
  "Adviced `edebug-previous-result'."
  (eros--make-result-overlay edebug-previous-result
    :where (point)
    :duration eros-eval-result-duration))

(advice-add #'edebug-previous-result
            :around
            #'adviced:edebug-previous-result)

;;; savehist

(savehist-mode 1)

;;; point mode

(use-package show-point-mode)

;;; ws-butler

(use-package ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

;;; copy word

(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "Copy thing between beg & end into kill ring."
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
	  (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end))))

(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg))

(defun beginning-of-string (&optional arg)
  (when (re-search-backward "[ \t]" (line-beginning-position) :noerror 1)
    (forward-char 1)))

(defun end-of-string (&optional arg)
  (when (re-search-forward "[ \t]" (line-end-position) :noerror arg)
    (backward-char 1)))

(defun thing-copy-string-to-mark(&optional arg)
  " Try to copy a string and paste it to the mark
     When used in shell-mode, it will paste string on shell prompt by default "
  (interactive "P")
  (copy-thing 'beginning-of-string 'end-of-string arg))

(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "Copy thing between beg & end into kill ring."
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
	  (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end))))

(defun paste-to-mark (&optional arg)
  "Paste things to mark, or to the prompt in shell-mode."
  (unless (eq arg 1)
    (if (string= "shell-mode" major-mode)
	(comint-next-prompt 25535)
      (goto-char (mark)))
    (yank)))

(keymap-global-set "C-c w" #'copy-word)

(global-set-key (kbd "C-c s")
   (lambda ()
      (interactive)
      (kill-new (thing-at-point 'symbol))))

;;; pulsar

  (use-package pulsar
    :config
    (setq pulsar-pulse t
	  pulsar-delay 0.2
	  pulsar-iterations 10
	  pulsar-face 'pulsar-cyan
	  pulsar-highlight-face 'pulsar-yellow))

  (pulsar-global-mode 1)

;;; crux

(use-package crux)

;;; cape

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
	 ("C-c p t" . complete-tag)        ;; etags
	 ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
	 ("C-c p h" . cape-history)
	 ("C-c p f" . cape-file)
	 ("C-c p k" . cape-keyword)
	 ("C-c p s" . cape-symbol)
	 ("C-c p a" . cape-abbrev)
	 ("C-c p l" . cape-line)
	 ("C-c p w" . cape-dict)
	 ("C-c p \\" . cape-tex)
	 ("C-c p _" . cape-tex)
	 ("C-c p ^" . cape-tex)
	 ("C-c p &" . cape-sgml)
	 ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  ;;(add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

;;; corfu

(use-package corfu
  :init
  (global-corfu-mode))

;;; orderless

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; vertico, consult

(setq read-extended-command-predicate
	  #'command-completion-default-include-p)

(use-package vertico
  :demand t                             ; Otherwise won't get loaded immediately
  :straight (vertico :files (:defaults "extensions/*") ; Special recipe to load extensions conveniently
		                          :includes (vertico-multiform))
  :config
  (vertico-mode)
  (vertico-multiform-mode) ;; Extensions
  (setq vertico-multiform-categories
	'((imenu buffer)))
  :bind (:map vertico-map
    ;; Use page-up/down to scroll vertico buffer, like ivy does by default.
    ("<prior>" . 'vertico-scroll-down)
    ("<next>"  . 'vertico-scroll-up)))


;;;; function to highlight enabled modes similar to counsel-M-x
(defvar +vertico-transform-functions nil)

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
  (dolist (fun (ensure-list +vertico-transform-functions))
    (setq cand (funcall fun cand)))
  (cl-call-next-method cand prefix suffix index start))


(defun +vertico-highlight-enabled-mode (cmd)
  "If MODE is enabled, highlight it as font-lock-constant-face."
  (let ((sym (intern cmd)))
    (if (or (eq sym major-mode)
            (and
             (memq sym minor-mode-list)
             (boundp sym)))
      (propertize cmd 'face 'font-lock-constant-face)
      cmd)))

(add-to-list 'vertico-multiform-commands
             '(execute-extended-command
               (+vertico-transform-functions . +vertico-highlight-enabled-mode)))

;;;; consult

  (use-package consult
    :bind (;; C-c bindings in `mode-specific-map'
	   ("C-c M-x" . consult-mode-command)
	   ("C-c h" . consult-history)
	   ("C-c k" . consult-kmacro)
	   ([remap Info-search] . consult-info)
	   ;; C-x bindings in `ctl-x-map'
	   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ;  ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	   ;; Custom M-# bindings for fast register access
	  ;; ("M-#" . consult-register-load)
	   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	   ("C-M-#" . consult-register)
	   ;; Other custom bindings
	   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	   ;; M-g bindings in `goto-map'
	   ("M-g e" . consult-compile-error)
	   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	   ("M-g g" . consult-goto-line)             ;; orig. goto-line
	   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	   ("M-g m" . consult-mark)
	   ("M-g k" . consult-global-mark)
	   ("M-i" . consult-imenu)
	   ("M-g I" . consult-imenu-multi)
	   ;; M-s bindings in `search-map'
	   ("M-s d" . consult-find)
	   ("M-s D" . consult-locate)
	   ("M-s g" . consult-grep)
	   ("M-s G" . consult-git-grep)
	   ("M-s r" . consult-ripgrep)
	   ("C-f" . consult-line)
	   ("M-s L" . consult-line-multi)
	   ("M-s k" . consult-keep-lines)
	   ("M-s u" . consult-focus-lines)
	   ;; Isearch integration
	   ("M-s e" . consult-isearch-history)
	   :map isearch-mode-map
	   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	   ;; Minibuffer history
	   :map minibuffer-local-map
	   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	   ("M-r" . consult-history))                ;; orig. previous-matching-history-element

    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
	  register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
	  xref-show-definitions-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key "M-.")
    ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-buffer :keymap my/consult-buffer-map
     consult-theme :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-file-register
     consult--source-recent-file consult--source-project-recent-file
     ;; :preview-key "M-."
     :preview-key '(:debounce 0.4 any))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; "C-+"
  )

;;;;;; Previewing files in find-file

;; sometimes errro
;; (setq read-file-name-function #'consult-find-file-with-preview)

;; (defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
;;   (interactive)
;;   (let ((default-directory (or dir default-directory))
;;         (minibuffer-completing-file-name t))
;;     (consult--read #'read-file-name-internal :state (consult--file-preview)
;;                    :prompt prompt
;;                    :initial initial
;;                    :require-match mustmatch
;;                    :predicate pred)))

;;; embark

(use-package iedit
  :disabled t) ;; because use C-; from embark-dwim

(use-package embark
  :ensure t

  :bind
  (("C-'" . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; kill buffer in `consult-buffer

(defun my-embark-M-k (&optional arg)
  (interactive "P")
  (require 'embark)
  (if-let ((targets (embark--targets)))
      (let* ((target
              (or (nth
                  (if (or (null arg) (minibufferp))
                      0
                    (mod (prefix-numeric-value arg) (length targets)))
                  targets)))
            (type (plist-get target :type)))
        (cond
         ((eq type 'buffer)
          (let ((embark-pre-action-hooks))
            (embark--act 'kill-buffer target)))))))

(defvar my/consult-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-k" #'my-embark-M-k)
    map))

;;; marginalia

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
	 :map minibuffer-local-map
	 ("M-A" . marginalia-cycle))

  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;;; horizontal split

(defun my-display-buffer-pop-up-same-width-window (buffer alist)
  "A `display-buffer' ACTION forcing a vertical window split.
    See `split-window-sensibly' and `display-buffer-pop-up-window'."
  (let ((split-width-threshold nil)
	(split-height-threshold 0))
    (display-buffer-pop-up-window buffer alist)))

(add-to-list 'display-buffer-alist
	     '("\\*cider-repl\\*" my-display-buffer-pop-up-same-width-window))

(add-to-list 'display-buffer-alist
	     '("\\*Help\\*" (display-buffer-reuse-window)))


;;; dictionary

; https://www.emacs.dyerdwelling.family/emacs/20240712082430-emacs--spelling-powerthesaurus/
; https://www.masteringemacs.org/article/wordsmithing-in-emacs
; sudo apt-get install dictd dict dict-{wn,vera,jargon,devil,gcide,foldoc}
; https://github.com/agzam/mw-thesaurus.el

(setq dictionary-server "localhost")
(keymap-global-set "M-#" #'dictionary-lookup-definition)

;;; elisp

(show-paren-mode 1)

(defun endless/locally-disable-show-paren ()
  (interactive)
  (setq-local show-paren-mode nil))

(add-hook 'org-mode-hook
          #'endless/locally-disable-show-paren)

(setq show-paren-when-point-inside-paren t)

;;;; outli

(use-package outli
  :straight (:host github :repo "jdtsmith/outli")
  :bind (:map outli-mode-map ; convenience key to get back to containing heading
	      ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
  :hook ((prog-mode text-mode) . outli-mode))

;;;; rainbow

(use-package rainbow-delimiters)

;;;; highlight

(use-package idle-highlight-mode
  :config (setq idle-highlight-idle-time 0.2)

  :hook ((emacs-lisp-mode) . idle-highlight-mode))

;;;; refactor

(use-package emr)
(keymap-set emacs-lisp-mode-map "M-RET" 'emr-show-refactor-menu)

;;;; others

(use-package paredit)
(defun my-paredit-mode-hook ()
  (define-key paredit-mode-map (kbd "\C-c c") 'paredit-copy-as-kill)
  (keymap-set paredit-mode-map "M-/" #'xref-find-references))

(add-hook 'paredit-mode-hook 'my-paredit-mode-hook)

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)

(use-package highlight-defined
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(use-package eros)
(eros-mode 1)

;;;; ert

; emacs -batch -l ert -l *-test.el -f ert-run-tests-batch-and-exit

(defun my-eval-and-run-all-tests-in-buffer ()
  "Delete all loaded tests from the runtime, evaluate the current buffer and run all loaded tests with ert."
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert 't))

(defun ert-run-all-tests ()
  (interactive)
  (ert "t")
  (other-window -1))

;(keymap-global-set "<f7>" #'ert-run-all-tests)

;;;; nameless

(use-package nameless
    :hook
    (emacs-lisp-mode . nameless-mode)
    :config
    (setq nameless-prefix "-"))

;;; moving around code
;;;; smart scan

(use-package smartscan
  :bind (:map smartscan-map
	      ("M-n" . nil)
	      ("M-p" . nil)
	      ("C-." . 'smartscan-symbol-go-forward)
	      ("C-," . 'smartscan-symbol-go-backward)))

(global-smartscan-mode 1)

;;; toggle window split

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)

;;; paste youtube link

(defun youtube-link-insert ()
  (interactive)
  (let* ((link (or (current-kill 0) (read-from-minibuffer "Youtube-Link:")))
         (title (string-trim (shell-command-to-string (format "yt-dlp --get-title '%s' 2>/dev/null" link)))))
    (save-excursion
      (insert (format "[[%s][%s]]" link title)))))

;;; gpg

(setq epg-gpg-home-directory "~/.gnupg")

;; gpg --gen-key
;; -*- mode: org -*- -*- epa-file-encrypt-to: ("slawomir.grochowski@gmail.com") -*-

;;; backup

;; https://blog.sumtypeofway.com/posts/emacs-config.html
;; Emacs is super fond of littering filesystems with backups and autosaves,
;; since it was built with the assumption that multiple users could be using the same Emacs instance on the same filesystem.
;; This was valid in 1980. It is no longer the case.
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

;;; dired

(use-package dired
  :ensure nil
  :straight (:type built-in)
  :custom ((dired-listing-switches "-alFh --group-directories-first")
	   (dired-dwim-target t)
	   (delete-by-moving-to-trash t)
	   (dired-free-space nil)))

;; Auto refresh buffers
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(use-package dired-du)

(use-package peep-dired
  :ensure t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :config
  (setq peep-dired-cleanup-eagerly t)
  :bind (:map dired-mode-map
	      ("P" . peep-dired)))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
	      ("<tab>" . dired-subtree-toggle)
	      ("<C-tab>" . dired-subtree-cycle)
	      ("<S-iso-lefttab>" . dired-subtree-remove)))

(use-package dired-filter)

;; Colourful columns
(use-package diredfl
  :config
  (diredfl-global-mode 1))

(require 'dired-x)
(add-hook 'dired-mode-hook  #'dired-omit-mode)

(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")     ;; emacs autosave files
	      (seq bol "." (not (any "."))) ;; dot-files
	      (seq "~" eol)                 ;; backup-files
	      (seq bol "CVS" eol)           ;; CVS dirs
	      )))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-details-mode) ; make dired use the same buffer for viewing directory
	    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
	    (define-key dired-mode-map (kbd ".") 'dired-omit-mode) ; was dired-advertised-find-file
	    (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))))

;;; calendar

(setq calendar-week-start-day 1)
(defalias 'cc 'calendar)

(setq calendar-today-marker 'calendar-month-header)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

(copy-face font-lock-constant-face 'calendar-iso-week-face)

(set-face-attribute 'calendar-iso-week-face nil
		    :height 1.0 :foreground "salmon")

(setq calendar-intermonth-text
      '(propertize
	(format "W%02d "
		(car
		 (calendar-iso-from-absolute
		  (calendar-absolute-from-gregorian (list month day year)))))
	'font-lock-face 'calendar-iso-week-face))

;;; colview

(defun org-hide-all-drawers ()
  "Hide all drawers in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*:.*:" nil t)
      (org-flag-drawer t))))


;;;; org-agenda

(setq org-overriding-columns-format "%60ITEM(item) %CLOCKSUM(time) %DEADLINE(deadline)")

;;;; excercise

(defun my/string-in-brackets-to-number (string)
  "Zamienia ciąg w formacie '[X]' na liczbę całkowitą."
  (let* ((trimmed-string (substring string 1 -1)) ; Usuń nawiasy
         (number (if (string= trimmed-string "X")
		     1
		     (string-to-number trimmed-string)))) ; Konwertuj na liczbę
    number))

(defun my/org-columns-excercise-update (pom key nval)
  (when (and nval (string-equal key "EXCERCISE") (< 0 (my/string-in-brackets-to-number nval)))
    (org-entry-put pom "PLANK-SHOULDER" "20")
    (org-entry-put pom "CRUNCH" "20")
    (org-entry-put pom "BIRDIE" "20")
    (org-entry-put pom "LYING-LEG-RAISE" "15")
    (org-entry-put pom "PLANK" "1")
    (org-entry-put pom "PUSHUP" "15")))

(with-eval-after-load 'org
   (advice-add 'org-columns-edit-value :override 'my/org-columns-edit-value))

(defun my/org-columns-done-update (pom key nval)
  (when (and nval (string-equal key "DONE") (< 0 (my/string-in-brackets-to-number nval)))
    (org-entry-put pom "NOCOFFE" "[X]")
    (org-entry-put pom "SX" "[X]")
    (org-entry-put pom "S-FOOD-CHECK" "[X]")))

(defun my/org-columns-edit-value (&optional key)
  "Edit the value of the property at point in column view.
Where possible, use the standard interface for changing this line."
  (interactive)
  (org-columns-check-computed)
  (let* ((col (current-column))
	 (bol (line-beginning-position))
	 (eol (line-end-position))
	 (pom (or (get-text-property bol 'org-hd-marker) (point)))
	 (key (or key (get-char-property (point) 'org-columns-key)))
	 (org-columns--time (float-time))
	 (action
	  (pcase key
	    ("CLOCKSUM"
	     (user-error "This special column cannot be edited"))
	    ("ITEM"
	     (lambda () (org-with-point-at pom (org-edit-headline))))
	    ("TODO"
	     (lambda ()
	       (org-with-point-at pom (call-interactively #'org-todo))))
	    ("PRIORITY"
	     (lambda ()
	       (org-with-point-at pom
		 (call-interactively #'org-priority))))
	    ("TAGS"
	     (lambda ()
	       (org-with-point-at pom
		 (let ((org-fast-tag-selection-single-key
			(if (eq org-fast-tag-selection-single-key 'expert)
			    t
			  org-fast-tag-selection-single-key)))
		   (call-interactively #'org-set-tags-command)))))
	    ("DEADLINE"
	     (lambda ()
	       (org-with-point-at pom (call-interactively #'org-deadline))))
	    ("SCHEDULED"
	     (lambda ()
	       (org-with-point-at pom (call-interactively #'org-schedule))))
	    ("BEAMER_ENV"
	     (lambda ()
	       (org-with-point-at pom
		 (call-interactively #'org-beamer-select-environment))))
	    (_
	     (let* ((allowed (org-property-get-allowed-values pom key 'table))
		    (value (get-char-property (point) 'org-columns-value))
		    (nval (org-trim
			   (if (null allowed) (read-string "Edit: " value)
			     (completing-read
			      "Value: " allowed nil
			      (not (get-text-property
				    0 'org-unrestricted (caar allowed))))))))
	       (and (not (equal nval value))
		    (lambda () (org-entry-put pom key nval)
		      (my/org-columns-excercise-update pom key nval)
		      (my/org-columns-done-update pom key nval))))))))
    (cond
     ((null action))
     ((eq major-mode 'org-agenda-mode)
      (org-columns--call action)
      ;; The following let preserves the current format, and makes
      ;; sure that in only a single file things need to be updated.
      (let* ((org-overriding-columns-format org-columns-current-fmt)
	     (buffer (marker-buffer pom))
	     (org-agenda-contributing-files
	      (list (with-current-buffer buffer
		      (buffer-file-name (buffer-base-buffer))))))
	(org-agenda-columns)))
     (t
      (let ((inhibit-read-only t))
	(with-silent-modifications
	  (remove-text-properties (max (point-min) (1- bol)) eol '(read-only t)))
	(org-columns--call action))
      ;; Some properties can modify headline (e.g., "TODO"), and
      ;; possible shuffle overlays.  Make sure they are still all at
      ;; the right place on the current line.
      (let ((org-columns-inhibit-recalculation)) (org-columns-redo))
      (org-columns-update key)
      (org-move-to-column col)))))

;;;; rest
(setq org-columns-checkbox-allowed-values '("[X]" "[-]" "[ ]" "" "[^]"))

(defun org-columns-switch-columns ()
  (interactive)
  (save-excursion
    (org-columns-goto-top-level)
    (re-search-forward ":PROPERTIES:")
    (let* ((folded-p (org-fold-folded-p))
	   (beg (re-search-forward ":COLUMNS:"))
	   (end (re-search-forward ":END:"))
	   (num-of-columns (count-matches ":COLUMNS:" beg end)))
      (when folded-p
	(org-fold-hide-drawer-toggle))
      (goto-char beg)
      (dotimes (_ num-of-columns)
	(org-metadown))
      (re-search-backward ":PROPERTIES:")
      (when folded-p
	(org-fold-hide-drawer-toggle))
      (org-columns))))

(with-eval-after-load 'org-colview
  (org-defkey org-columns-map "x" #'org-columns-switch-columns))

(defun my/org-columns-get-format (&optional fmt-string)
  "Return columns format specifications.
When optional argument FMT-STRING is non-nil, use it as the
current specifications.  This function also sets
`org-columns-current-fmt-compiled' and
`org-columns-current-fmt'."
  (interactive)
  (let ((format
	 (or fmt-string
             (progn
               (save-excursion (re-search-forward ":COLUMNS:\\s-*.*" nil t)
                               (replace-regexp-in-string ":COLUMNS:\\s-*" ""
                                                         (buffer-substring-no-properties
                                                          (line-beginning-position) (line-end-position)))))
	     (org-with-wide-buffer
	      (goto-char (point-min))
	      (catch :found
		(let ((case-fold-search t))
		  (while (re-search-forward "^[ \t]*#\\+COLUMNS: .+$" nil t)
		    (let ((element (org-element-at-point)))
		      (when (org-element-type-p element 'keyword)
			(throw :found (org-element-property :value element)))))
		  nil)))
	     org-columns-default-format)))
    (setq org-columns-current-fmt format)
    (org-columns-compile-format format)
    format))

(with-eval-after-load 'org
  (advice-add 'org-columns-get-format :override 'my/org-columns-get-format))

(defun my/org-columns--summary-checkbox-count (check-boxes _)
  "Summarize CHECK-BOXES with a check-box cookie."
  (let ((completed 0)
	(total 0))
    (dolist (b check-boxes)
      (cond
       ((or (equal b "[X]") (equal b "[X]*") (equal b "[X]!") (equal b "[X]^"))
	(setq completed (+ completed 1))
	(setq total (+ total 1)))
       ((or (equal b "[^]") (equal b "[-]^")) nil)
       ((string-match (rx "[" (one-or-more digit) "]") b)
	(setq completed (+ completed 1))
	(setq total (+ total 1)))
       ((string-match (rx "[" (group (one-or-more digit))
                          "/"
                          (group (or (one-or-more digit) "?")) "]") b)
	(setq completed (+ completed (string-to-number (match-string 1 b))))
	(setq total (+ total (let ((total (string-to-number (match-string 2 b)))) ;; "?" is counted as one
			       (if (= 0 total) 1 total) ))))
       (t (setq total (+ total 1)))))
    (format "[%d/%d]" completed total)))

(with-eval-after-load 'org
  (advice-add 'org-columns--summary-checkbox-count :override 'my/org-columns--summary-checkbox-count))

(with-eval-after-load 'org-colview
  (org-defkey org-columns-map [return] #'org-columns-edit-value))

(defun my/org-columns--overlay-text (value fmt width property original)
  "Return decorated VALUE string for columns overlay display.
FMT is a format string.  WIDTH is the width of the column, as an
integer.  PROPERTY is the property being displayed, as a string.
ORIGINAL is the real string, i.e., before it is modified by
`org-columns--displayed-value'."
  (format fmt
	  (let ((v (org-columns-add-ellipses value width)))
	    (pcase property
	      ("PRIORITY"
	       (propertize v 'face (org-get-priority-face original)))
	      ("TAGS"
	       (if (not org-tags-special-faces-re)
		   (propertize v 'face 'org-tag)
		 (replace-regexp-in-string
		  org-tags-special-faces-re
		  (lambda (m) (propertize m 'face (org-get-tag-face m)))
		  v nil nil 1)))
	      ("TODO" (propertize v 'face (org-get-todo-face original)))
	      (_ (if (string-match (rx (and "["
					    (group (one-or-more digit))
					    "/"
					    (group (or (one-or-more digit) "?"))
					    "]")) v)
		     (let* ((first-number (string-to-number (match-string 1 v)))
			    (second-number (string-to-number (match-string 2 v))))
		       (cond
			((and (eq first-number 0) (eq second-number 0))
			 (propertize v 'face '(:foreground "white")))
			((eq first-number 0)
			 (propertize v 'face 'error))
			((or (> first-number second-number) (= first-number second-number) (= 0 second-number))
			 (propertize v 'face (org-get-todo-face original))) ;; 0 because (string-to-number "?") => 0
			(t
			 (let* ((color (nth (- first-number 1) (color-gradient
								(color-values (face-foreground 'error))
								(color-values (face-foreground 'org-todo))
								(- second-number 1))))
				(hex-color (apply #'format "#%02x%02x%02x"
						  (mapcar (lambda (c) (/ c 256)) color))))
			   (propertize v 'face `(:foreground ,hex-color ))))
			))
		   (if (string-match (rx "[" (or (one-or-more digit) "X") "]") v)
		       (propertize v 'face (org-get-todo-face original))
		     v)))))))

(defun my/org-columns--displayed-value (spec value &optional no-star)
  "Return displayed value for specification SPEC in current entry.

SPEC is a column format specification as stored in
`org-columns-current-fmt-compiled'.  VALUE is the real value to
display, as a string.

When NO-STAR is non-nil, do not add asterisks before displayed
value for ITEM property."
  (or (and (functionp org-columns-modify-value-for-display-function)
	   (funcall org-columns-modify-value-for-display-function
		    (nth 1 spec)	;column name
		    value))
      (pcase spec
	(`("ITEM" . ,_)
	 (let ((stars
		(and (not no-star)
		     (concat (make-string (1- (org-current-level))
					  (if org-hide-leading-stars ?\s ?*))
			     "* ")))
	       ;; usuwa rok z timestamp
	       (value-without-year (replace-regexp-in-string "^\\[\\([0-9]\\{4\\}\\)-\\|\\[" ""
                            (replace-regexp-in-string "\\]" "" value))))
	   (concat stars (org-link-display-format value-without-year))))
	(`(,(or "DEADLINE" "SCHEDULED" "TIMESTAMP") . ,_)
	 (replace-regexp-in-string org-ts-regexp "[\\1]" value))
	(`(,_ ,_ ,_ ,_ nil) value)
	;; If PRINTF is set, assume we are displaying a number and
	;; obey to the format string.
	(`(,_ ,_ ,_ ,_ ,printf)
         (if (string-empty-p value) ;; empty string if empty value - aby nie wyświetlał zer w kolumnie z wagą
             ""
           (format printf (string-to-number value))))
	(_ (error "Invalid column specification format: %S" spec)))))

(with-eval-after-load 'org
  (advice-add 'org-columns--displayed-value :override 'my/org-columns--displayed-value))

(with-eval-after-load 'org
  (advice-add 'org-columns--overlay-text :override 'my/org-columns--overlay-text))

(defun my/org-columns-toggle-or-columns-quit ()
  "Quit column view."
  (interactive)
  (org-columns--toggle))

(with-eval-after-load 'org
  (advice-add 'org-columns-toggle-or-columns-quit :override 'my/org-columns-toggle-or-columns-quit))

(defun my/org-ctrl-c-ctrl-c (&optional arg)
  "Set tags in headline, or update according to changed information at point.

This command does many different things, depending on context:

- If there are highlights, remove them.

- If a function in `org-ctrl-c-ctrl-c-hook' recognizes this location,
  this is what we do.

- If the cursor is on a statistics cookie, update it.

- If the cursor is in a headline, in an agenda or an org buffer,
  prompt for tags and insert them into the current line, aligned
  to `org-tags-column'.  When called with prefix arg, realign all
  tags in the current buffer.

- If the cursor is in one of the special #+KEYWORD lines, this
  triggers scanning the buffer for these lines and updating the
  information.

- If the cursor is inside a table, realign the table.  This command
  works even if the automatic table editor has been turned off.

- If the cursor is on a #+TBLFM line, re-apply the formulas to
  the entire table.

- If the cursor is at a footnote reference or definition, jump to
  the corresponding definition or references, respectively.

- If the cursor is a the beginning of a dynamic block, update it.

- If the current buffer is a capture buffer, close note and file it.

- If the cursor is on a <<<target>>>, update radio targets and
  corresponding links in this buffer.

- If the cursor is on a numbered item in a plain list, renumber the
  ordered list.

- If the cursor is on a checkbox, toggle it.

- If the cursor is on a code block, evaluate it.  The variable
  `org-confirm-babel-evaluate' can be used to control prompting
  before code block evaluation, by default every code block
  evaluation requires confirmation.  Code block evaluation can be
  inhibited by setting `org-babel-no-eval-on-ctrl-c-ctrl-c'."
  (interactive "P")
  (cond
    ((or (bound-and-true-p org-clock-overlays) org-occur-highlights)
    (when (boundp 'org-clock-overlays) (org-clock-remove-overlays))
    (org-remove-occur-highlights)
    (message "Temporary highlights/overlays removed from current buffer"))
   ((and (local-variable-p 'org-finish-function)
	 (fboundp org-finish-function))
    (funcall org-finish-function))
   ((org-babel-hash-at-point))
   ((run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-hook))
   (t
    (let* ((context
	    (org-element-lineage
	     (org-element-context)
	     ;; Limit to supported contexts.
	     '(babel-call clock dynamic-block footnote-definition
			  footnote-reference inline-babel-call inline-src-block
			  inlinetask item keyword node-property paragraph
			  plain-list planning property-drawer radio-target
			  src-block statistics-cookie table table-cell table-row
			  timestamp)
	     t))
	   (radio-list-p (org-at-radio-list-p))
	   (type (org-element-type context)))
      ;; For convenience: at the first line of a paragraph on the same
      ;; line as an item, apply function on that item instead.
      (when (eq type 'paragraph)
	(let ((parent (org-element-parent context)))
	  (when (and (org-element-type-p parent 'item)
		     (= (line-beginning-position)
			(org-element-begin parent)))
	    (setq context parent)
	    (setq type 'item))))
      ;; Act according to type of element or object at point.
      ;;
      ;; Do nothing on a blank line, except if it is contained in
      ;; a source block.  Hence, we first check if point is in such
      ;; a block and then if it is at a blank line.
      (pcase type
	((or `inline-src-block `src-block)
	 (unless org-babel-no-eval-on-ctrl-c-ctrl-c
	   (org-babel-eval-wipe-error-buffer)
	   (org-babel-execute-src-block
	    current-prefix-arg (org-babel-get-src-block-info nil context))))
	((guard (org-match-line "[ \t]*$"))
	 (or (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)
	     (user-error
	      (substitute-command-keys
	       "`\\[org-ctrl-c-ctrl-c]' can do nothing useful here"))))
	((or `babel-call `inline-babel-call)
	 (let ((info (org-babel-lob-get-info context)))
	   (when info (org-babel-execute-src-block nil info nil type))))
	(`clock
         (if (org-at-timestamp-p 'lax)
             ;; Update the timestamp as well.  `org-timestamp-change'
             ;; will call `org-clock-update-time-maybe'.
             (org-timestamp-change 0 'day)
           (org-clock-update-time-maybe)))
	(`dynamic-block
	 (save-excursion
	   (goto-char (org-element-post-affiliated context))
	   (org-update-dblock)))
	(`footnote-definition
	 (goto-char (org-element-post-affiliated context))
	 (call-interactively 'org-footnote-action))
	(`footnote-reference (call-interactively #'org-footnote-action))
	((or `headline `inlinetask)
	 (save-excursion (goto-char (org-element-begin context))
			 (org-todo "DONE")))
	(`item
	 ;; At an item: `C-u C-u' sets checkbox to "[-]"
	 ;; unconditionally, whereas `C-u' will toggle its presence.
	 ;; Without a universal argument, if the item has a checkbox,
	 ;; toggle it.  Otherwise repair the list.
	 (if (or radio-list-p
		 (and (boundp org-list-checkbox-radio-mode)
		      org-list-checkbox-radio-mode))
	     (org-toggle-radio-button arg)
	   (let* ((box (org-element-property :checkbox context))
		  (struct (org-element-property :structure context))
		  (old-struct (copy-tree struct))
		  (parents (org-list-parents-alist struct))
		  (prevs (org-list-prevs-alist struct))
		  (orderedp (org-not-nil (org-entry-get nil "ORDERED"))))
	     (org-list-set-checkbox
	      (org-element-begin context) struct
	      (cond ((equal arg '(16)) "[-]")
		    ((and (not box) (equal arg '(4))) "[ ]")
		    ((or (not box) (equal arg '(4))) nil)
		    ((eq box 'on) "[ ]")
		    (t "[X]")))
	     ;; Mimic `org-list-write-struct' but with grabbing a return
	     ;; value from `org-list-struct-fix-box'.
	     (org-list-struct-fix-ind struct parents 2)
	     (org-list-struct-fix-item-end struct)
	     (org-list-struct-fix-bul struct prevs)
	     (org-list-struct-fix-ind struct parents)
	     (let ((block-item
		    (org-list-struct-fix-box struct parents prevs orderedp)))
	       (if (and box (equal struct old-struct))
		   (if (equal arg '(16))
		       (message "Checkboxes already reset")
		     (user-error "Cannot toggle this checkbox: %s"
				 (if (eq box 'on)
				     "all subitems checked"
				   "unchecked subitems")))
		 (org-list-struct-apply-struct struct old-struct)
		 (org-update-checkbox-count-maybe))
	       (when block-item
		 (message "Checkboxes were removed due to empty box at line %d"
			  (org-current-line block-item)))))))
	(`plain-list
	 ;; At a plain list, with a double C-u argument, set
	 ;; checkboxes of each item to "[-]", whereas a single one
	 ;; will toggle their presence according to the state of the
	 ;; first item in the list.  Without an argument, repair the
	 ;; list.
	 (if (or radio-list-p
		 (and (boundp org-list-checkbox-radio-mode)
		      org-list-checkbox-radio-mode))
	     (org-toggle-radio-button arg)
	   (let* ((begin (org-element-contents-begin context))
		  (struct (org-element-property :structure context))
		  (old-struct (copy-tree struct))
		  (first-box (save-excursion
			       (goto-char begin)
			       (looking-at org-list-full-item-re)
			       (match-string-no-properties 3)))
		  (new-box (cond ((equal arg '(16)) "[-]")
				 ((equal arg '(4)) (unless first-box "[ ]"))
				 ((equal first-box "[X]") "[ ]")
				 (t "[X]"))))
	     (cond
	      (arg
	       (dolist (pos
			(org-list-get-all-items
			 begin struct (org-list-prevs-alist struct)))
		 (org-list-set-checkbox pos struct new-box)))
	      ((and first-box (eq (point) begin))
	       ;; For convenience, when point is at bol on the first
	       ;; item of the list and no argument is provided, simply
	       ;; toggle checkbox of that item, if any.
	       (org-list-set-checkbox begin struct new-box)))
	     (when (equal
		    (org-list-write-struct
		     struct (org-list-parents-alist struct) old-struct)
		    old-struct)
	       (message "Cannot update this checkbox"))
	     (org-update-checkbox-count-maybe))))
	(`keyword
	 (let ((org-inhibit-startup-visibility-stuff t)
	       (org-startup-align-all-tables nil))
	   (when (boundp 'org-table-coordinate-overlays)
	     (mapc #'delete-overlay org-table-coordinate-overlays)
	     (setq org-table-coordinate-overlays nil))
	   (org-save-outline-visibility 'use-markers (org-mode-restart)))
	 (message "Local setup has been refreshed"))
	((or `property-drawer `node-property)
	 (call-interactively #'org-property-action))
	(`radio-target
	 (call-interactively #'org-update-radio-target-regexp))
	(`statistics-cookie
	 (call-interactively #'org-update-statistics-cookies))
	((or `table `table-cell `table-row)
	 ;; At a table, generate a plot if on the #+plot line,
         ;; recalculate every field and align it otherwise.  Also
	 ;; send the table if necessary.
         (cond
          ((and (org-match-line "[ \t]*#\\+plot:")
                (< (point) (org-element-post-affiliated context)))
           (org-plot/gnuplot))
          ;; If the table has a `table.el' type, just give up.
          ((eq (org-element-property :type context) 'table.el)
           (message "%s" (substitute-command-keys "\\<org-mode-map>\
Use `\\[org-edit-special]' to edit table.el tables")))
          ;; At a table row or cell, maybe recalculate line but always
	  ;; align table.
          ((or (eq type 'table)
               ;; Check if point is at a TBLFM line.
               (and (eq type 'table-row)
                    (= (point) (org-element-end context))))
           (save-excursion
             (if (org-at-TBLFM-p)
                 (progn (require 'org-table)
                        (org-table-calc-current-TBLFM))
               (goto-char (org-element-contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))
               (orgtbl-send-table 'maybe))))
          (t
           (org-table-maybe-eval-formula)
           (cond (arg (call-interactively #'org-table-recalculate))
                 ((org-table-maybe-recalculate-line))
                 (t (org-table-align))))))
	((or `timestamp (and `planning (guard (org-at-timestamp-p 'lax))))
	 (org-timestamp-change 0 'day))
	((and `nil (guard (org-at-heading-p)))
	 ;; When point is on an unsupported object type, we can miss
	 ;; the fact that it also is at a heading.  Handle it here.
	 (org-todo "DONE"))
	((guard
	  (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)))
	(_
	 (user-error
	  (substitute-command-keys
	   "`\\[org-ctrl-c-ctrl-c]' can do nothing useful here")))))))

  (set-face-attribute 'org-column-title nil
		      :inherit 'default))

(with-eval-after-load 'org
  (advice-add 'org-ctrl-c-ctrl-c :override 'my/org-ctrl-c-ctrl-c))
