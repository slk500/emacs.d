;; completion
(setq completion-styles '(substring))
(setq tab-always-indent 'complete)

(cua-mode t)

(define-key cua-global-keymap [C-return] nil)
(global-unset-key (kbd "C-x C-s"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-f") 'isearch)
(global-set-key (kbd "C-s") 'save-buffer)

(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(load-theme 'modus-vivendi t)

(setq custom-theme-allow-multiple-selections nil)
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

(global-set-key (kbd "C-c 1") (lambda() (interactive)(find-file "~/aamystuff/mystuff/index.org")))
(global-set-key (kbd "C-c 2") (lambda() (interactive)(find-file "~/aamystuff/phprefactor/phprefactor.org")))
(global-set-key (kbd "C-c 3") (lambda() (interactive)(find-file "~/aamystuff/life/life.org.gpg")))
(global-set-key (kbd "C-c 4") (lambda() (interactive)(find-file "~/aamystuff/emacs/emacs.org")))
(global-set-key (kbd "C-c 5") (lambda() (interactive)(find-file "~/aamystuff/clojure-examples/clojure-examples.org")))
(global-set-key (kbd "C-c i") (lambda() (interactive)(find-file (expand-file-name "myinit.org" user-emacs-directory))))
(global-set-key (kbd "C-c e") (lambda() (interactive)(find-file (expand-file-name "early-init.el" user-emacs-directory))))
(global-set-key (kbd "C-c r") (lambda() (interactive)(find-file (expand-file-name "init.el" user-emacs-directory))))

(blink-cursor-mode 0)
(global-visual-line-mode)
(setq org-hide-leading-stars t)

;; do not show the startup screen.
(setq inhibit-startup-message t)

;; always select the help window
(setq help-window-select t)

;; disable
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)

;; disable git
(setq vc-handled-backends nil)

;; needed for hotkey startup
(setq frame-title-format "emacs")
