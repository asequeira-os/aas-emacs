
;; sources
;; https://lucidmanager.org/productivity/configure-emacs/

;; no banner
(setq inhibit-startup-message t
      cursor-type 'bar)
;; TODO following are not alwasy available, conditionalize
;; (tool-bar-mode -1)
;; (menu-bar-mode -1)
;; (scroll-bar-mode -1)


;; emacs driven local setting location
;; main file won't be overwritten
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))

;; Define and initialise package repositories
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; use-package to simplify the config file
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)

;; Theme
(use-package dracula-theme
	     :init (load-theme 'dracula t))
