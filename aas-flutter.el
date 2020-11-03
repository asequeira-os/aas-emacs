(message "flutter setup")
;; from https://emacs-lsp.github.io/lsp-dart/
(use-package lsp-mode :ensure t)
(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp))

;; (use-package projectile :ensure t) ;; project management
;; (use-package yasnippet
;;   :ensure t
;;   :config (yas-global-mode)) ;; snipets
(use-package lsp-ui :ensure t) ;; UI for LSP
;; (use-package company :ensure t) ;; Auto-complete

;;(use-package hover :ensure t) ;; run app from desktop without emulator
