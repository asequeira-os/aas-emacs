;;-*-Emacs-Lisp-*-
(require 'package)
(add-to-list 'package-archives
       '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defconst aas-emacs-repo-dir
  (file-name-directory (file-chase-links load-file-name))
  "dir of the dot_emacs.el file from https://github.com/asequeira-os/aas-emacs")

(load (concat aas-emacs-repo-dir "aas-util"))

(package-initialize)
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(package-refresh-contents)


(require 'use-package)
(use-package better-defaults :ensure t)
(use-package dracula-theme :ensure t)
(use-package flycheck :ensure t)
(use-package company-terraform :ensure t)
(use-package magit :ensure t)
(use-package powerline :ensure t)

(use-package icomplete-vertical
  :ensure t
  :demand t
  :custom
  (completion-styles '(partial-completion substring))
  (completion-category-overrides '((file (styles basic substring))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :config
  (icomplete-mode)
  (icomplete-vertical-mode)
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
              ("<up>" . icomplete-backward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)))


;; Mac OSX - after brew install aspell
(if (string-equal system-type "darwin")
    ;; OSX aspell etc
    (progn
      (setq ispell-program-name "/usr/local/bin/aspell")
      (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
      (setq exec-path (append exec-path '("/usr/local/bin")))))

(tool-bar-mode -1)


(global-set-key (kbd "<f12>") 'sticky-buffer-mode)
(global-set-key (kbd "<f11>") 'bury-buffer)
(global-set-key (kbd "<f6>") 'save-buffer)
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; window configuration un(re)do and easier moving betwen windows and frames
(winner-mode 1)
(windmove-default-keybindings 'meta)
(setq framemove-hook-into-windmove t)

(global-set-key (kbd "M-C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-C-S-<down>") 'enlarge-window)
(global-set-key (kbd "M-C-S-<up>") 'shrink-window)


(add-hook 'org-mode-hook
          '(lambda ()
            ;; key bindings for org
            (local-set-key (kbd "<f6>") 'aas-save-and-indent)
            ;; disable these so windmove mode can work
            (local-unset-key (kbd "M-<up>"))
            (local-unset-key (kbd "M-<down>"))
            (local-unset-key (kbd "M-<left>"))
            (local-unset-key (kbd "M-<right>"))
            (local-set-key (kbd "C-<up>") 'org-metaup)
            (local-set-key (kbd "C-<down>") 'org-metadown)
            ;; misc
            (flyspell-mode 1)))

(eval-after-load "flyspell"
  '(progn
    (setq ispell-program-name "aspell")
    (setq ispell-list-command "--list")
    (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
    (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(require 'company-terraform)
(company-terraform-init)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;;-------- python --------------------------------------------------------
(use-package elpy :ensure t)
(elpy-enable)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(eval-after-load "elpy"
  '(cl-dolist (key '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
     (define-key elpy-mode-map (kbd key) nil)))

;; (setq elpy-rpc-python-command
;; "/usr/local/bin/docker exec -it containername /container/python/pathn")
;; (elpy-set-project-root "/pathin/container")

;; temp
;; (setq
;;  python-shell-interpreter
;; "/usr/local/bin/docker"
;; python-shell-interpreter-args "exec -it containername /container/python/pathn -i"
;; python-shell-completion-native-enable nil
;; )


;; (use-package company :ensure t)
(autoload 'company-mode "company" nil t)
;(use-package ropemacs :ensure t)
;(use-package pymacs :ensure t)
;(require 'pymacs)
;(pymacs-load "ropemacs" "rope-")
;; (use-package jedi :ensure t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

;;--------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(before-save-hook '(delete-trailing-whitespace))
 '(bmkp-last-as-first-bookmark-file nil)
 '(bookmark-bmenu-file-column 60)
 '(calendar-date-style 'iso)
 '(case-fold-search t)
 '(column-number-mode t)
 '(desktop-save-mode t)
 '(package-selected-packages
   '(icomplete-vertical powerline magit jedi pymacs ropemacs company-mode flycheck dracula-theme better-defaults use-package))
 '(spell-command "aspell"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'dracula t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally

;(setq ido-show-dot-for-dired nil)
(auto-save-visited-mode 1)
(powerline-default-theme)
