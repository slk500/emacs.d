;;; whitespace
    (use-package whitespace-cleanup-mode
      :config
      (global-whitespace-cleanup-mode +1))
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
           ("C-c m" . consult-man)
;;           ("C-c i" . consult-info)
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
;;; column view
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
;;; outshine
(use-package outshine
  :straight 
  (:host github :repo "alphapapa/outshine")
  :config
  (setq outshine-startup-folded-p t))

(keymap-set outshine-mode-map "M-<up>" nil)
(keymap-set outshine-mode-map "M-<down>" nil)
(add-hook 'emacs-lisp-mode-hook 'outshine-mode)
;;; dictionary
; https://github.com/SqrtMinusOne/reverso.el
; https://www.masteringemacs.org/article/wordsmithing-in-emacs
; https://github.com/agzam/mw-thesaurus.el
(use-package reverso
    :straight (:host github :repo "SqrtMinusOne/reverso.el"))
(setq reverso-languages '(english polish))

(keymap-global-set "M-#" #'dictionary-lookup-definition)
(setq dictionary-server "localhost")

;;; imenu
(keymap-global-set "M-g i" #'counsel-imenu)
;;; elisp
  ;(require 'hi-var)
  ;(add-hook 'emacs-lisp-mode-hook 'hi-var-mode 'APPEND)

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)

(defun mp-elisp-mode-eval-buffer ()
  (interactive)
  (message "Evaluated buffer")
  (eval-buffer))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)

(use-package highlight-defined
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(use-package eros)
(eros-mode 1)
;;; smart scan
(use-package smartscan)
;(global-smartscan-mode 1)
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
	   (insert (format "[[%s][%s]]" link title))
	   ))))
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
                                   (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
                                   ))
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
        org-src-tab-acts-natively t)
  (require 'org-tempo)
  (require 'org-expiry)
  (require 'org-eldoc)
  (global-eldoc-mode 1))
