(message "flutter setup") ;; assumes flutter is installed and is in PATH
;; from https://emacs-lsp.github.io/lsp-dart/
(use-package lsp-mode :ensure t)

(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp))

;; (use-package lsp-dart
;;   :ensure t
;;   :hook (dart-mode . lsp)
;;   :init
;;   (dap-register-debug-template "Flutter :: Custom debug"
;;                                (list :flutterPlatform "x86_64"
;;                                      :program "lib/main.dart"
;;                                      :args '("--flavor" "customer_a"))))



(use-package projectile :ensure t) ;; project management
(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

;; (use-package yasnippet
;;   :ensure t
;;   :config (yas-global-mode)) ;; snipets

(use-package lsp-ui :ensure t) ;; UI for LSP
;; (use-package company :ensure t) ;; Auto-complete

;;(use-package hover :ensure t) ;; run app from desktop without emulator
(dap-dart-setup) ;needed only once per doc
