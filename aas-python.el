(use-package jedi
  :ensure t)
(add-hook 'python-mode-hook 'jedi:setup)
;; (use-package pyenv-mode
;;   :ensure t
;;   :init (pyenv-mode))
(use-package elpy
  :ensure t
  :config
  :init (elpy-enable)
  :bind (:map elpy-mode-map
              ("M-." . elpy-goto-definition))
  )

;; from https://stackoverflow.com/questions/25154809/how-can-i-stop-elpy-from-overriding-some-of-my-keybindings/25159354
(eval-after-load "elpy"
  '(cl-dolist (key '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
     (define-key elpy-mode-map (kbd key) nil)))

;; (use-package poetry :ensure t)
;; (add-hook 'elpy-mode-hook 'poetry-tracking-mode)
;; from https://github.com/galaunay/poetry.el/issues/14#issuecomment-589559666
(use-package poetry
  :ensure t
  :config
  (add-hook 'find-file-hook
            (lambda () (poetry-track-virtualenv)))
  (advice-add 'kill-buffer
              :around
              (lambda (func &rest args)
                (let* ((next-buffer-name (buffer-file-name (other-buffer)))
                       (both-file-buffers (and next-buffer-name buffer-file-name)))
                  (apply func args)
                  (if both-file-buffers (poetry-track-virtualenv)))))
  (advice-add 'switch-to-buffer
              :after
              (lambda (&rest args)
                (if buffer-file-name (poetry-track-virtualenv))))
  (advice-add 'windmove-do-window-select
              :after
              (lambda (&rest args)
                (if buffer-file-name (poetry-track-virtualenv))))
  )



;; (defun ssbb-pyenv-hook ()
;;   "Automatically activates pyenv version if .python-version file exists."
;;   (f-traverse-upwards
;;    (lambda (path)
;;      (let ((pyenv-version-path (f-expand ".python-version" path)))
;;        (if (f-exists? pyenv-version-path)
;; 	   (pyenv-mode-set (s-trim (f-read-text pyenv-version-path 'utf-8))))))))

;; (add-hook 'find-file-hook 'ssbb-pyenv-hook)

;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

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

;(setq elpy-rpc-virtualenv-path 'current)
(setq elpy-rpc-python-command "python3")
(jedi:install-server)
