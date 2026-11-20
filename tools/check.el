;;; check.el --- Check that Emacs Lisp files compile cleanly and are indented  -*- lexical-binding: t; -*-

;; Usage: emacs --batch -Q -l tools/check.el FILE...
;;
;; Compile each FILE with warnings treated as errors, and check that
;; Emacs would not indent any of its lines differently.  Exit with
;; status 1 if a file fails.  No compiled file is left behind.

;;; Code:

(require 'bytecomp)
;; The indentation of the cl- forms is known only once this is loaded.
(require 'cl-lib)

(defun check-compiles-p (file)
  "Return non-nil if FILE compiles without a warning."
  (let* ((compiled (make-temp-file "check" nil ".elc"))
         (byte-compile-dest-file-function (lambda (_source) compiled))
         (byte-compile-error-on-warn t))
    (unwind-protect
        (byte-compile-file file)
      (delete-file compiled))))

(defun check-first-misindented-line (file)
  "Return the number of the first line of FILE that Emacs would move, or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (setq indent-tabs-mode nil)
    (let ((before (split-string (buffer-string) "\n"))
          (inhibit-message t))
      (indent-region (point-min) (point-max))
      (let ((after (split-string (buffer-string) "\n"))
            (number 1))
        (while (and before (equal (car before) (car after)))
          (setq before (cdr before)
                after (cdr after)
                number (1+ number)))
        (and before number)))))

(let ((failed nil))
  (dolist (file command-line-args-left)
    (unless (check-compiles-p file)
      (message "%s: does not compile without warnings" file)
      (setq failed t))
    (when-let* ((line (check-first-misindented-line file)))
      (message "%s:%d: not indented as Emacs indents it" file line)
      (setq failed t)))
  (setq command-line-args-left nil)
  (kill-emacs (if failed 1 0)))

;;; check.el ends here
