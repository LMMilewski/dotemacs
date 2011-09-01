;;; erlang-mode usefull shortcuts
;; C-c C-j (generate new clause)
;; C-c C-y (copy arguments from last clause)
;; M-h (mark function)
;; C-c M-h (mark clause)
;; M-a/M-e (goto begining/end of a function)

;;; setup paths to Erlang/OTP
(setq erlang-root-dir (expand-file-name "~/libs/otp/lib/erlang")
      exec-path (cons (expand-file-name "~/libs/otp/bin/") exec-path))

(require 'erlang-start)

;;; Set options for elrang nodes run inside Emacs
(add-hook 'erlang-mode-hook (lambda () (interactive)
                              (setq inferior-erlang-machine-options
                                    '("-sname" "emacs"))))


;;; Add paths to include and ebin dirs for dependencies (in projects using rebar)
;; add deps/ to includes (one usually use include_lib("LIB/include/LIB.hrl")
;; add deps/eqc/ebin, so that you can use flymake with quickcheck
(defun erlang-flymake-get-code-path-dirs ()
  (list (concat (erlang-flymake-get-app-dir) "ebin")
        (concat (erlang-flymake-get-app-dir) "deps/eqc/ebin")))
(defun erlang-flymake-get-include-dirs ()
  (list (concat (erlang-flymake-get-app-dir) "include")
        (concat (erlang-flymake-get-app-dir) "deps")))

;;;;;;;;;;;;;;;;;
;;; setup Distel
;;;;;;;;;;;;;;;;;
;;
;; INSTALL
;; ~~~~~~~
;;   run 'make' in dotemacs/distel directory
;;   add to /etc/hosts entry
;;      127.0.0.1       lmm
;;
;; USAGE
;; ~~~~~
;; Running:
;;   Run erlang node:    C-cC-z
;;   Setup distel node:  C-cC-dn emacs@lmm RET
;;
;; Debugging:
;;   Avoid distel bug:   C-cC-dm k
;;   Compile /w debug:   C-uC-cC-k
;;   Interpret module:   C-cC-di
;;   Toggle breakpoint:  C-xSPC (C-cC-db)
;;
;; Convenient stuff:
;;   Complete module / function name   M-? (M-S-/)
;;   Show arg list                     C-cC-dA
;;   Extract function                  C-cC-df
;;
;; Navigation:
;;   Goto function  M-.
;;   Goto module    C-cC-dF
;;   Go back        M-,
(require 'distel)
(distel-setup)


;;; setup Flymake
;; load flymake
(require 'erlang-flymake)

;; Workaround bug in OTP erlang-mode, so that flymake will work with
;; distel smoothly
(remove-hook 'erlang-mode-hook 'flymake-mode)
(defun run-flymake-in-erlang ()
  (interactive)
  (let ( (extension (file-name-extension (buffer-name))) )
    (if (and extension
             (or (string-equal extension "erl")
                 (string-equal extension "hrl")))
        (flymake-mode t))))
(add-hook 'find-file-hook 'run-flymake-in-erlang)