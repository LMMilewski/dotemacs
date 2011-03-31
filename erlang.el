(setq erlang-root-dir (concat my-home-dir "otp/")
      exec-path (cons (concat my-home-dir "otp/bin") exec-path))

(require 'erlang-start)
(require 'erlang-flymake)

(defun erl-init-file ()
  "insert -module and -export directives"
  (interactive)
  (let ((bufname (buffer-name)))
    (string-match "\\(.*\\)\\.erl" bufname)
    (insert "-module(" (match-string 1 bufname) ").\n"))
  (insert "-export([]).\n")
  (insert "\n")
  (end-of-buffer))

(add-hook 'erlang-mode-hook (lambda () (interactive)
                              (when (eobp)
                                (erl-init-file))))

