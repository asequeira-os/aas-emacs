
;; sources
;; https://lucidmanager.org/productivity/configure-emacs/

;; no banner
(setq inhibit-startup-message t
      cursor-type 'bar)
;; TODO following are not alwasy available, conditionalize
;; (tool-bar-mode -1)
;; (menu-bar-mode -1)
;; (scroll-bar-mode -1)
(setq visible-bell t)

;; emacs driven local setting location
;; main file won't be overwritten
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))

;; Define and initialise package repositories
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


;; use-package to simplify the config file
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)

;;(use-package command-log-mode)

;; TODO ivy completion setup
;; (use-package ivy
;;   :diminish
;;   :bind (("C-s" . swiper)
;;          :map ivy-minibuffer-map
;;          ("TAB" . ivy-alt-done)	
;;          ("C-l" . ivy-alt-done)
;;          ("C-j" . ivy-next-line)
;;          ("C-k" . ivy-previous-line)
;;          :map ivy-switch-buffer-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-l" . ivy-done)
;;          ("C-d" . ivy-switch-buffer-kill)
;;          :map ivy-reverse-i-search-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-d" . ivy-reverse-i-search-kill))
;;   :config
;;   (ivy-mode 1))


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; column and line numbering
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Theme
(use-package dracula-theme
	     :init (load-theme 'dracula t))

;; native compilation
;; Silence compiler warnings as they can be pretty disruptive
;; (setq native-comp-async-report-warnings-errors nil)
;; ;; Set the right directory to store the native comp cache
;; (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))


;; keep dirs clean
;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(make-directory "~/.cache/emacs/" t)
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))
;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)
;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; safety
(set-default-coding-systems 'utf-8)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))


;; (use-package emojify
;;   :hook (erc-mode . emojify-mode)
;;   :commands emojify-mode)


(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))


;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
;; Revert buffers when the underlying file has changed
;;(global-auto-revert-mode 1)



(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

;; world times
;; (setq display-time-world-list
;;   '(("Etc/UTC" "UTC")
;;     ("America/Los_Angeles" "Seattle")
;;     ("America/New_York" "New York")
;;     ("Europe/Athens" "Athens")
;;     ("Pacific/Auckland" "Auckland")
;;     ("Asia/Shanghai" "Shanghai")
;;     ("Asia/Kolkata" "Hyderabad")))
;; (setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")


;; Set default connection mode to SSH
(setq tramp-default-method "ssh")


;; TAB and space
(setq-default tab-width 2)
;;(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)


;; folding

(use-package origami
  :hook (yaml-mode . origami-mode))


(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))


;; docker
(use-package docker
  :commands docker)

(use-package docker-tramp
  :defer t
  :after docker)




