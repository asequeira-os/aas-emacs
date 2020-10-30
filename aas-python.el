(use-package jedi
  :ensure t)
(use-package pyenv-mode
  :ensure t
  :init (pyenv-mode))
(use-package elpy
  :ensure t
  :config
  :init (elpy-enable)
  :bind (:map elpy-mode-map
              ("M-." . elpy-goto-definition))
  )
(use-package poetry :ensure t)
(add-hook 'elpy-mode-hook 'poetry-tracking-mode)

(defun ssbb-pyenv-hook ()
  "Automatically activates pyenv version if .python-version file exists."
  (f-traverse-upwards
   (lambda (path)
     (let ((pyenv-version-path (f-expand ".python-version" path)))
       (if (f-exists? pyenv-version-path)
	   (pyenv-mode-set (s-trim (f-read-text pyenv-version-path 'utf-8))))))))

(add-hook 'find-file-hook 'ssbb-pyenv-hook)
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; (eval-after-load "elpy"
;;   '(cl-dolist (key '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
;;      (define-key elpy-mode-map (kbd key) nil)))

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


;; (with-elpy-rpc-virtualenv-activated
;;  (message "RPC binaries: '%s'" (executable-find elpy-rpc-python-command)))
;; (message "User binaries: '%s'" (executable-find elpy-rpc-python-command))

(setq elpy-rpc-virtualenv-path 'current)
(setq elpy-rpc-python-command "python3")
