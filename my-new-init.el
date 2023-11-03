;;; kill ring

(use-package browse-kill-ring
  :ensure t)

;;; sql

(add-hook 'sql-mode-hook 'lsp)
(setq lsp-sqls-workspace-config-path nil)
(setq lsp-sqls-connections
    '(((driver . "mysql") (dataSourceName . "test:test@tcp(localhost:3306)/test"))
      ((driver . "mssql") (dataSourceName . "Server=localhost;Database=sammy;User Id=yyoncho;Password=hunter2;"))
      ))

;;; tempo 

(defun tempo-complete (prompt completions require-match &optional save-name no-insert)
      "Like `tempo-insert-prompt', but use completing-read."
      (cl-flet ((read-string (prompt)
               (completing-read prompt completions (lambda (s) t) require-match)))
        (tempo-insert-prompt prompt save-name no-insert)))

 ;; (tempo-define-template "begin-environment"
 ;;    '("* "
 ;;      '(tempo-complete "Day: " (org-timestamp-inactive) 'environment) > n
 ;;      (s environment)))

(require 'tempo)
    (setq tempo-interactive t)

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

;;; code compas
(use-package async)
(use-package dash)
(use-package f)
(use-package s)
(use-package simple-httpd)

(use-package code-compass
  :load-path "~/.emacs.d/lisp")
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

  (require 'ob-clojure)
  (use-package cider)
  (require 'cider)  
  (setq org-babel-clojure-backend 'cider)

;  (define-key clojure-mode-map (kbd "\C-x \C-e") 'cider-eval-last-sexp)
;  (global-set-key (kbd "\C-x \C-e") 'cider-eval-last-sexp)

  (define-key clojure-mode-map (kbd "M-RET") 'lsp-execute-code-action)

  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'lsp-mode)

;;; time
(setq display-time-format "%H:%M")
(setq display-time-default-load-average nil)
(display-time-mode 1)   
;;; treesitter

(require 'treesit)

(add-to-list
  'treesit-language-source-alist
  '(php "https://github.com/tree-sitter/tree-sitter-php.git")
  '(clojure "http://github.com/sogaiu/tree-sitter-clojure.git") 
  '(bash "https://github.com/tree-sitter/tree-sitter-bash.git"))

(setq treesit-load-name-override-list '((clojure "libtree-sitter-clojure" "libtree-sitter-clojure")))
							
(add-to-list 'major-mode-remap-alist
	     '(php-mode . php-ts-mode)
	     '(sh-mode . bash-ts-mode))

(use-package clojure-ts-mode
    :straight (:host github :repo "clojure-emacs/clojure-ts-mode"))

(use-package php-ts-mode
    :straight (:host github :repo "emacs-php/php-ts-mode"))

;;; org
;;;; org

(use-package org
  :config
  (setq-default org-fold-catch-invisible-edits 'error) ;; dosent work with hungry delete!!!!
  ;; (use-package org-bullets)
  ;; (add-hook 'org-mode-hook 'org-bullets-mode)
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
	org-M-RET-may-split-line t
	org-checkbox-hierarchical-statistics nil
	org-src-tab-acts-natively t)
  (require 'org-tempo)
  (require 'org-expiry)
;;  (require 'org-eldoc) turned off after update to emacs 29 - error 
;;  (global-eldoc-mode 1)
  )
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
      '(("w" "work"
	 ((todo "TODO" ((org-agenda-files '("~/aamystuff/job/molecular.gpg"))))))
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
	  (tags "nullo"
		     ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
		      (org-agenda-overriding-header "\nSince\n")))
	  (tags-todo "-book-video/DOING"
		     ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
		      (org-agenda-overriding-header "\nTasks\n")))
	  ))))

(setq org-agenda-hide-tags-regexp (regexp-opt '("book")))

(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-skip-scheduled-if-done t)

(setq org-agenda-files (append (directory-files-recursively "~/aamystuff/mystuff/" "\\.org$")
			       '("~/aamystuff/life/life.org.gpg"
				 "~/aamystuff/life/job.org"
				 "~/aamystuff/phprefactor/phprefactor.org"
				 "~/aamystuff/emacs/emacs.org"
				 "~/aamystuff/clojure/clojure-examples.org")))

(setq org-agenda-prefix-format "%t %s")

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

(add-hook 'org-agenda-finalize-hook
	  (lambda ()
	    (save-excursion
	      (goto-char (point-min))
	      (when (re-search-forward "Since*" nil t)
		(insert "\n")
		(insert "\n")
		(insert (format "free lungs %d" (* -1 (org-time-stamp-to-now "2023-05-09"))))))))


(setq org-agenda-show-future-repeats nil)
(defun my/org-agenda-adjust-text-size ()
  (if (= text-scale-mode-amount 0)
      (text-scale-adjust -2)))

(add-hook 'org-agenda-finalize-hook #'my/org-agenda-adjust-text-size)

(setq org-agenda-sorting-strategy '((todo todo-state-up priority-down)
				    (tags todo-state-up priority-down)))

;;;; day counter
;; https://www.reddit.com/r/orgmode/comments/13fgc09/orgagenda_elapsed_dayscounter_of_days/
;;;; created 

;(require 'org-expiry)
;(org-expiry-insinuate)
;(setq org-expiry-created-property-name "CREATED")

;;;; imenu

(setq org-imenu-depth 6)
(setq org-goto-interface 'outline-path-completionp)
(setq org-outline-path-complete-in-steps nil)

;;;; export to markdown

(use-package ox-gfm)

(eval-after-load "org"
  '(require 'ox-gfm nil t))

;;;; org-capture

(global-set-key (kbd "<f6>") 'org-capture)



;;; shell here

(use-package shell-here)
(define-key (current-global-map) "\C-c!" 'shell-here)

;;; spelling

(use-package jinx
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(setq jinx-languages "en_US pl_PL")

;;; skip system buffers when cycling

(set-frame-parameter (selected-frame) 'buffer-predicate
             (lambda (buf) (not (string-match-p "^*" (buffer-name buf)))))

;;; drag stuff

(use-package drag-stuff
    :straight (:host github :repo "rejeep/drag-stuff.el"))

;; (drag-stuff-mode t)
;; (drag-stuff-define-keys)

;;; snippets

(defun my/copy-current-buffer-file-name ()
  (interactive)
  (kill-new (buffer-file-name)))

;;; long lines

(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;;; smooth scroll
(setq scroll-conservatively 101
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t
      scroll-margin 0
      scroll-preserve-screen-position t)

;;; keys

;(use-package user-keys
;  :straight (user-keys :type git :host github :repo "positron-solutions/user-keys"))

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
;;; org-blk-uri

(use-package org-web-tools)

(use-package org-blk-uri
    :straight (:host github :repo "ag91/org-blk-uri"))

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

(recentf-mode 1)
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

(show-paren-mode 1)

(defun endless/locally-disable-show-paren ()
  (interactive)
  (setq-local show-paren-mode nil))

(add-hook 'org-mode-hook
          #'endless/locally-disable-show-paren)

(setq show-paren-when-point-inside-paren t)

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
  (eval-buffer)
  (ert-run-all-tests))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)

(use-package highlight-defined
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(use-package eros)
(eros-mode 1)

;;;; ert

(defun ert-run-all-tests ()
  (interactive)
  (ert "t")
  (other-window -1))

(keymap-global-set "<f7>" #'ert-run-all-tests)

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

;;; colview
(defun my/org-columns--summary-checkbox-count (check-boxes _)
  "Summarize CHECK-BOXES with a check-box cookie."
  (let ((completed 0)
	(total 0))
    (dolist (b check-boxes)
      (cond
       ((equal b "[X]")
	(setq completed (+ completed 1))
	(setq total (+ total 1)))
       ((string-match "\\[\\([0-9]+\\)/\\([0-9]+\\)\\]" b)
	(setq completed (+ completed (string-to-number (match-string 1 b))))
	(setq total (+ total (string-to-number (match-string 2 b)))))
       (t (setq total (+ total 1)))))
    (format "[%d/%d]" completed total)))

(with-eval-after-load 'org
  (advice-add 'org-columns--summary-checkbox-count :override 'my/org-columns--summary-checkbox-count))


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
	      (_ (if (string-match "\\[\\([1-9][0-9]*\\)/\\([1-9][0-9]*\\)\\]" v)
		     (let* ((first-number (string-to-number (match-string 1 v)))
			    (second-number (string-to-number (match-string 2 v))))
		       (if (= first-number second-number)
			   (propertize v 'face (org-get-todo-face original))
			 v))
		   (if (string-match "\\[X\\]" v)
		       (propertize v 'face (org-get-todo-face original))
		     v)))))))

(with-eval-after-load 'org
  (advice-add 'org-columns--overlay-text :override 'my/org-columns--overlay-text))

(defun my/org-columns-toggle-or-columns-quit ()
  "Toggle checkbox at point, or quit column view."
  (interactive)
  (org-columns--toggle))

(with-eval-after-load 'org
  (advice-add 'org-columns-toggle-or-columns-quit :override 'my/org-columns-toggle-or-columns-quit))


(defun my/org-ctrl-c-ctrl-c (&optional arg)
  "Set tags in headline, or update according to changed information at point.

This command does many different things, depending on context:

- If column view is active, in agenda or org buffers, quit it.

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
			 (call-interactively #'org-set-tags-command)))
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
	 (call-interactively #'org-set-tags-command))
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
