
;;; Speed up line movement

;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)

;;; edebug
(setq edebug-print-level 100
      edebug-print-length 1000
      edebug-print-circle nil
      eval-expression-print-level 50
      eval-expression-print-length 1000)

;;; https://github.com/joaotavora/breadcrumb
;;; expand region

(use-package expand-region
  :bind ("C-=" . er/expand-region))

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

;;; centered-cursor-mode

;; (use-package centered-cursor-mode)

;; (define-global-minor-mode my-global-centered-cursor-mode centered-cursor-mode
;;   (lambda ()
;;     (when (not (memq major-mode
;; 		     (list 'Info-mode 'term-mode 'eshell-mode 'shell-mode 'erc-mode)))
;;       (centered-cursor-mode))))
;; (my-global-centered-cursor-mode -1)

;;; crux

(use-package crux)

;;; openwith

(use-package openwith)

(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "evince" (file))))

;;; whitespace

(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode +1))

					;(add-hook 'before-save-hook 'whitespace-cleanup)

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
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
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

(use-package paredit)
(defun my-paredit-mode-hook ()
  (define-key paredit-mode-map (kbd "\C-c c") 'paredit-copy-as-kill)
  (keymap-set paredit-mode-map "M-/" #'xref-find-references))

(add-hook 'paredit-mode-hook 'my-paredit-mode-hook)

(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
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
	 ("M-g i" . consult-imenu)
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

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
    ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
    ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
    ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
    ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
    ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

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

;;; focus

(use-package focus)

;;; dictionary

					; https://github.com/SqrtMinusOne/reverso.el
					; https://www.masteringemacs.org/article/wordsmithing-in-emacs
					; https://github.com/agzam/mw-thesaurus.el
(use-package reverso
  :straight (:host github :repo "SqrtMinusOne/reverso.el"))
(setq reverso-languages '(english polish))

(keymap-global-set "M-#" #'dictionary-lookup-definition)
(setq dictionary-server "localhost")

;;; elisp
;;;; outshine

(use-package outshine
  :straight
  (:host github :repo "alphapapa/outshine")
  :bind (:map outshine-mode-map
	      ("<backtab>" . outshine-cycle-buffer)
	      ("M-<up>" . nil)
	      ("M-<down>" . nil)))

(add-hook 'emacs-lisp-mode-hook 'outshine-mode)

;;;; highlight

(use-package idle-highlight-mode
  :config (setq idle-highlight-idle-time 0.2)

  :hook ((emacs-lisp-mode) . idle-highlight-mode))

;;;; refactor

(use-package emr)
(keymap-set emacs-lisp-mode-map "M-RET" 'emr-show-refactor-menu)

;;;; others

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (add-hook 'before-save-hook 'eval-buffer nil 'make-it-local)))

(defun mp-elisp-mode-eval-buffer ()
  (interactive)
  (message "Evaluated buffer")
  (eval-buffer))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)

(use-package highlight-defined
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(use-package eros)
(eros-mode 1)

;;; moving around code
;;;; smart scan

(use-package smartscan
  :bind (:map smartscan-map
	      ("M-n" . nil)
	      ("M-p" . nil)
	      ("C-." . 'smartscan-symbol-go-forward)
	      ("C-," . 'smartscan-symbol-go-backward)))

(global-smartscan-mode 1)

;;;; which function cursor is at?

(which-function-mode 1)

;;; overlay

(defun highlight-line ()
  (interactive)
  (overlay-put
   (make-overlay (line-beginning-position) (line-end-position)) 'face '(:background "gray20")))

(defun highlight-line-remove ()
  (interactive)
  (remove-overlays (line-beginning-position) (line-end-position)))

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
  (let* ((link (read-from-minibuffer "Youtube-Link:"))
	 (title (string-trim (shell-command-to-string (format "yt-dlp --get-title '%s' 2>/dev/null" link))))
	 (save-excursion
	   (insert (format "[[%s][%s]]" link title))))))

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
	    (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))))

;;; calendar

(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
		    :height 0.7)
(set-face-attribute 'calendar-iso-week-face nil
		    :height 1.0 :foreground "salmon")

(setq calendar-intermonth-text
      '(propertize
	(format "W%02d "
		(car
		 (calendar-iso-from-absolute
		  (calendar-absolute-from-gregorian (list month day year)))))
	'font-lock-face 'calendar-iso-week-face))

;;; org
;;;; org

(use-package org
  :straight
  (:type built-in)
  :config
  (setq-default org-fold-catch-invisible-edits 'error) ;; dosent work with hungry delete!!!!
  (use-package org-bullets)
  ;;(add-hook 'org-mode-hook 'org-bullets-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (use-package org-contrib)
  (setq org-startup-folded t
	org-hide-emphasis-markers t
	org-log-done 'time
	org-log-into-drawer t
	org-special-ctrl-a/e t ;; ctrl a move to beginig of headline not line
	org-treat-insert-todo-heading-as-state-change t
	initial-major-mode 'org-mode
	org-ellipsis "⤵"
	org-agenda-span 14
	org-src-tab-acts-natively t)
  (require 'org-tempo)
  (require 'org-expiry)
  (require 'org-eldoc)
  (global-eldoc-mode 1))

;;;; agenda

(eval-when-compile (require 'cl)) ;; adds lexical-let
(defadvice org-agenda (around split-vertically activate)
  (let ((split-width-threshold 80))  ; or whatever width makes sense for you
    ad-do-it))

(defun cmp-date-property (prop)
  "Compare two `org-mode' agenda entries, `A' and `B', by some date property.
		If a is before b, return -1. If a is after b, return 1. If they
		are equal return nil.
	      https://emacs.stackexchange.com/questions/26351/custom-sorting-for-agenda"
  (lexical-let ((prop prop))
    #'(lambda (a b)

	(let* ((a-pos (get-text-property 0 'org-marker a))
	       (b-pos (get-text-property 0 'org-marker b))
	       (a-date (or (org-entry-get a-pos prop)
			   (format "[%s]" (org-read-date t nil "1986-01-01"))))
	       (b-date (or (org-entry-get b-pos prop)
			   (format "[%s]" (org-read-date t nil "1986-01-01"))))
	       (cmp (compare-strings a-date nil nil b-date nil nil))
	       )
	  (if (eq cmp t) nil (signum cmp))
	  ))))

(setq org-agenda-custom-commands
      '(
	("b" "List of read books" tags "book/DONE|DOING|CANCELED|STUCK"
	 ((org-agenda-cmp-user-defined (cmp-date-property
					"CLOSED"))
	  (org-agenda-sorting-strategy '(todo-state-down user-defined-down priority-down))
	  (org-agenda-todo-keyword-format "%-2s")
	  (org-agenda-prefix-format "%(if (org-entry-get nil \"CLOSED\") (format \"%s \"(truncate-string-to-width (org-entry-get nil \"CLOSED\") 11 1)) \"\")")
	  )nil
	 ("~/aamystuff/books.html"))
	("g" "Get Things Done (GTD)"
	 ((agenda ""
		  ((org-agenda-skip-function
		    '(org-agenda-skip-entry-if 'deadline))
		   (org-deadline-warning-days 0)))
	  (tags-todo "-book-video/DOING"
		     ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
		      (org-agenda-overriding-header "\nTasks\n")))
	  ))))

(setq org-agenda-hide-tags-regexp (regexp-opt '("book")))

(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-skip-scheduled-if-done t)

(setq org-agenda-files (append (directory-files-recursively "~/aamystuff/mystuff/" "\\.org$")
			       '(
				 "~/aamystuff/life/life.org.gpg"
				 "~/aamystuff/phprefactor/phprefactor.org"
				 "~/aamystuff/emacs/emacs.org"
				 "~/aamystuff/clojure/clojure-examples.org"
				 )))

(setq org-agenda-prefix-format "%t %s")

(add-hook 'org-agenda-finalize-hook
	  (lambda ()
	    (save-excursion
	      (goto-char (point-min))
	      (when (or (re-search-forward "Global list of TODO items of type: [[:upper:]]*" nil t)
			(re-search-forward "Headlines with TAGS match: [[:upper:]]*" nil t))
		(insert (propertize
			 (format " (%s remaining)"
				 (- (count-lines (point-min) (point-max)) 2))
			 'face 'font-lock-comment-face))))))

(add-hook 'org-agenda-finalize-hook
	  (lambda ()
	    (save-excursion
	      (goto-char (point-min))
	      (when (re-search-forward "Tasks*" nil t)
		(insert (propertize
			 (format " (%s remaining)" (- (count-lines (point) (point-max) t) 2)))
			'face 'font-lock-comment-face)))))

(setq org-agenda-show-future-repeats nil)
(defun my/org-agenda-adjust-text-size ()
  (if (= text-scale-mode-amount 0)
      (text-scale-adjust -2)))

(add-hook 'org-agenda-finalize-hook #'my/org-agenda-adjust-text-size)

(setq org-agenda-sorting-strategy '((todo todo-state-up priority-down)
				    (tags todo-state-up priority-down)))
