;;; ...  -*- lexical-binding: t -*-

(require 'package)
(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(defvar comp-deferred-compilation-deny-list ())
(setq straight-disable-native-compile t)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'org)

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono 14" ))

(load (expand-file-name "my-new-init.el" user-emacs-directory))

(mapc 'load (file-expand-wildcards "~/.emacs.d/lisp/*.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application tramp)
      tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
					"pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
					"-o" "state=abcde"
					"-o"
					"ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
					  (euid . number)
					  (user . string)
					  (egid . number)
					  (comm . 52)
					  (state . 5)
					  (ppid . number)
					  (pgrp . number)
					  (sess . number)
					  (ttname . string)
					  (tpgid . number)
					  (minflt . number)
					  (majflt . number)
					  (time
					   . tramp-ps-time)
					  (pri . number)
					  (nice . number)
					  (vsize . number)
					  (rss . number)
					  (etime
					   . tramp-ps-time)
					  (pcpu . number)
					  (pmem . number)
					  (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
					"pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
					"-o" "stat=abcde"
					"-o"
					"ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number)
					  (user . string)
					  (group . string)
					  (comm . 52)
					  (state . 5)
					  (ppid . number)
					  (pgrp . number)
					  (ttname . string)
					  (time
					   . tramp-ps-time)
					  (nice . number)
					  (etime
					   . tramp-ps-time)
					  (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
					"pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
					"-o"
					"state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
					  (euid . number)
					  (user . string)
					  (egid . number)
					  (group . string)
					  (comm . 52)
					  (state . string)
					  (ppid . number)
					  (pgrp . number)
					  (sess . number)
					  (ttname . string)
					  (tpgid . number)
					  (minflt . number)
					  (majflt . number)
					  (time
					   . tramp-ps-time)
					  (pri . number)
					  (nice . number)
					  (vsize . number)
					  (rss . number)
					  (etime . number)
					  (pcpu . number)
					  (pmem . number)
					  (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":") (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294"
     "e7d20d1848605f119537171fa445c5bb51a6b152de4af46e571a7a2a61a15ee5"
     "30ff9fb125a91788b693b53968cd661355f9fd77c057ffa160e17770aa26bae6"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce"
     "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948"
     "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b"
     "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2"
     "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9"
     "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4"
     "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde"
     "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525"
     "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644"
     default))
 '(dabbrev-ignored-buffer-modes '(archive-mode image-mode notmuch-message-mode))
 '(frame-resize-pixelwise t)
 '(ignored-local-variable-values '((Coding . utf-8)))
 '(marginalia-annotators
   '((command marginalia-annotate-command
	      marginalia-annotate-binding builtin none)
     (embark-keybinding
      marginalia-annotate-embark-keybinding builtin none)
     (customize-group marginalia-annotate-customize-group
		      builtin none)
     (variable marginalia-annotate-variable builtin none)
     (function marginalia-annotate-function builtin none)
     (face marginalia-annotate-face builtin none)
     (color marginalia-annotate-color builtin none)
     (unicode-name marginalia-annotate-char builtin none)
     (minor-mode marginalia-annotate-minor-mode builtin none)
     (symbol marginalia-annotate-symbol builtin none)
     (environment-variable
      marginalia-annotate-environment-variable builtin none)
     (input-method marginalia-annotate-input-method builtin
		   none)
     (coding-system marginalia-annotate-coding-system
		    builtin none)
     (charset marginalia-annotate-charset builtin none)
     (package marginalia-annotate-package builtin none)
     (imenu marginalia-annotate-imenu builtin none)
     (bookmark marginalia-annotate-bookmark builtin none)
     (project-file marginalia-annotate-project-file builtin
		   none)
     (buffer marginalia-annotate-buffer builtin none)
     (library marginalia-annotate-library builtin none)
     (tab marginalia-annotate-tab builtin none)
     (multi-category marginalia-annotate-multi-category
		     builtin none)))
 '(marginalia-mode t)
 '(org-agenda-files
   '("/home/slk/aamystuff/life/life.org.gpg"
     "/home/slk/aamystuff/life/todos.org.gpg"
     "/home/slk/aamystuff/phprefactor/phprefactor.org"
     "/home/slk/aamystuff/emacs/emacs.org"
     "/home/slk/aamystuff/clojure/clojure-examples.org"
     "/home/slk/aamystuff/mystuff/books.org"
     "/home/slk/aamystuff/mystuff/psychology.org"))
 '(org-ellipsis nil)
 '(org-fold-catch-invisible-edits 'error)
 '(org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"
     "\\`https://github\\.com/fniessen/org-html-themes/blob/master/org/theme-bigblow-local\\.setup\\'"
     "\\`https://github\\.com/fniessen/org-html-themes/blob/master/org/theme-bigblow\\.setup\\'"))
 '(package-selected-packages
   '(eev debbugs centered-window bongo diredfl simpleclip
	 dired-subtree php-mode ivy-rich which-key counsel
	 conunsel swiper org-plus-contrib use-package))
 '(pulsar-pulse-functions
   '(smartscan-symbol-go-forward smartscan-symbol-go-backward
				 recenter-top-bottom
				 move-to-window-line-top-bottom
				 reposition-window
				 bookmark-jump other-window
				 delete-window
				 delete-other-windows
				 forward-page backward-page
				 scroll-up-command
				 scroll-down-command
				 next-buffer previous-buffer
				 windmove-right
				 windmove-left windmove-up
				 windmove-down
				 windmove-swap-states-right
				 windmove-swap-states-left
				 windmove-swap-states-up
				 windmove-swap-states-down
				 tab-new tab-close tab-next
				 org-next-visible-heading
				 org-previous-visible-heading
				 org-forward-heading-same-level
				 org-backward-heading-same-level
				 outline-backward-same-level
				 outline-forward-same-level
				 outline-next-visible-heading
				 outline-previous-visible-heading
				 outline-up-heading
				 xref-pulse-momentarily
				 pop-global-mark))
 '(safe-local-variable-values
   '((etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc")
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/"
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))
     (nameless-separator . "/")
     (nameless-current-name . "org-columns")))
 '(send-mail-function 'sendmail-send-it)
 '(treemacs-no-png-images t)
 '(warning-suppress-types '((org)))
 '(xref-after-jump-hook '(recenter xref-pulse-momentarily pulsar-pulse-line)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-annotations ((t (:inherit marginalia-documentation))))
 '(font-lock-comment-face ((t (:foreground "dim gray"))))
 '(font-lock-string-face ((t (:foreground "#6C8A56"))))
 '(fringe ((t (:background "#0d1017"))))
 '(ledger-font-payee-uncleared-face ((t (:foreground "dim gray" :weight bold))))
 '(ledger-font-posting-account-face ((t (:foreground "dark gray"))))
 '(message-mml ((t (:foreground "dim gray" :slant italic))))
 '(notmuch-message-summary-face ((t (:extend t :foreground "dim gray"))))
 '(notmuch-search-count ((t (:foreground "dim gray"))))
 '(org-agenda-done ((t (:inherit org-special-keyword))))
 '(org-block-begin-line ((t (:foreground "#5c626b" :extend t :inherit org-block))))
 '(org-checkbox-statistics-done ((t (:inherit org-todo))))
 '(org-checkbox-statistics-todo ((t (:inherit org-done))))
 '(org-column ((t (:background "black" :strike-through nil :underline nil :slant normal :weight normal))))
 '(org-column-title ((t (:inherit default :weight bold))))
 '(org-drawer ((t (:foreground "#696c71"))))
 '(org-habit-ready-face ((t (:background "dark green" :weight bold))))
 '(org-level-3 ((t (:extend nil :foreground "white"))))
 '(org-mode-line-clock-overrun ((t (:inherit mode-line))))
 '(org-scheduled-previously ((t (:foreground "dim gray"))))
 '(org-scheduled-today ((t (:foreground "white smoke"))))
 '(org-time-grid ((t (:foreground "dim gray"))))
 '(shadow ((t (:foreground "dim gray"))))
 '(swiper-match-face-1 ((t (:foreground "#7fd962")))))
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
