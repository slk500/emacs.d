;;; ...  -*- lexical-binding: t -*-

(setq native-comp-speed -1)
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation-deny-list nil)
(setq straight-disable-native-compile t)

;; completion
(setq completion-styles '(substring))
(setq tab-always-indent 'complete)

(cua-mode t)

(define-key cua-global-keymap [C-return] nil)
(global-unset-key (kbd "C-x C-s"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x C-n"))
;(global-unset-key (kbd "C-x h")) ;=> mark whole buffer, easy to make mistake with C-h
(bind-key* "C-w" 'kill-current-buffer)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-s") 'save-buffer)
(bind-key* "M-o" 'other-window) ;; overwrite M-o in html mode
(global-set-key (kbd "C-<delete>") 'org-kill-line)
(global-set-key (kbd "<f7>") 'visual-fill-column-mode)
(global-set-key (kbd "<f8>") 'visual-line-mode)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-x 5") 'toggle-frame-split)
(keymap-global-set "C-z" #'undo-only)
(keymap-global-set "M-p" #'ace-swap-window)
(keymap-global-set "C--" #'text-scale-decrease)
(keymap-global-unset "C-=")
(keymap-global-set "C-=" #'text-scale-increase)
(keymap-global-set "M-=" #'count-words)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; escape quits everything

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(load-theme 'modus-vivendi t)
(setq text-scale-mode-step 1.05)

(repeat-mode t)

;; (add-variable-watcher 'smtpmail-smtp-service (lambda (s n o w)
;;                                                      (debug)))

;; quit Emacs directly even if there are running processes
(setopt confirm-kill-processes nil)

(setq custom-theme-allow-multiple-selections nil)
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

(global-set-key (kbd "C-c 2") (lambda() (interactive)(find-file "~/aamystuff/slawomir-grochowski.com/articles.org")))
(global-set-key (kbd "C-c 3") (lambda() (interactive)(find-file "~/aamystuff/phprefactor/phprefactor.org")))
(global-set-key (kbd "C-c 1") (lambda() (interactive)(find-file "~/aamystuff/life/life.org.gpg")))

(global-set-key (kbd "C-c 4") (lambda() (interactive)(find-file "~/aamystuff/emacs/emacs.org")))
(global-set-key (kbd "C-c 5") (lambda() (interactive)(find-file "~/aamystuff/mystuff/software.org")))
(global-set-key (kbd "C-c i") (lambda() (interactive)(find-file (expand-file-name "my-new-init.el" user-emacs-directory))))
(global-set-key (kbd "C-c r") (lambda() (interactive)(find-file (expand-file-name "init.el" user-emacs-directory))))

(blink-cursor-mode 0)
(setq org-hide-leading-stars t)

;; do not show the startup screen.
(setq inhibit-startup-message t
      initial-scratch-message nil
      use-file-dialog nil)

;; always select the help window
(setq help-window-select t)

;; disable
(tool-bar-mode  0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq 
 ;; needed for hotkey startup
 frame-title-format "emacs"
 ring-bell-function 'ignore
 use-short-answers t
 ;; prefer newer elisp files
 load-prefer-newer t)
