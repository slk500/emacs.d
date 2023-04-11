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

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono 14" ))

(org-babel-load-file (expand-file-name "myinit.org" user-emacs-directory))

(mapc 'load (file-expand-wildcards "~/.emacs.d/lisp/*.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e7d20d1848605f119537171fa445c5bb51a6b152de4af46e571a7a2a61a15ee5" "30ff9fb125a91788b693b53968cd661355f9fd77c057ffa160e17770aa26bae6" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default))
 '(org-fold-catch-invisible-edits 'error)
 '(package-selected-packages
   '(eev debbugs centered-window bongo diredfl simpleclip dired-subtree php-mode ivy-rich which-key counsel conunsel swiper org-plus-contrib use-package))
 '(smtpmail-smtp-server "smtp.gmail.com" t)
 '(smtpmail-smtp-service 587 t)
 '(treemacs-no-png-images t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "dim gray"))))
 '(font-lock-string-face ((t (:foreground "#6C8A56"))))
 '(fringe ((t (:background "#0d1017"))))
 '(lsp-face-highlight-textual ((t (:background "SeaGreen1" :foreground "#565b66"))))
 '(org-agenda-done ((t (:inherit org-special-keyword))))
 '(org-checkbox-statistics-done ((t (:inherit org-todo))))
 '(org-checkbox-statistics-todo ((t (:inherit org-done))))
 '(org-column-title ((t (:inherit default :weight bold))))
 '(org-scheduled-previously ((t (:foreground "dim gray"))))
 '(org-scheduled-today ((t (:foreground "white smoke"))))
 '(swiper-match-face-1 ((t (:foreground "#7fd962")))))
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
