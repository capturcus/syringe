;;; syringe.el --- Review code in three windows: diff, live code, conversation  -*- lexical-binding: t; -*-

;;; Commentary:

;; Syringe helps you understand a change by reading it yourself.
;;
;; Commands.  None of them is bound to a key.
;;
;;   syringe-start-last-commit  start a review of the last commit and of
;;                              the work on top of it that is not committed

;;; Code:

(require 'diff-mode)
(require 'subr-x)

(defgroup syringe nil
  "Review code in three windows: diff, live code, conversation."
  :group 'tools)

(defconst syringe--diff-buffer "*syringe-diff*"
  "Name of the buffer that shows the diff under review.")

;;;; The diff

(define-derived-mode syringe-diff-mode diff-mode "Syringe-Diff"
  "Major mode for the diff under review."
  (setq buffer-read-only t
        diff-mode-read-only t))

(defun syringe--git-diff (root args)
  "Return what `git diff' with ARGS prints in the git repository at ROOT."
  (with-temp-buffer
    (setq default-directory root)
    (let ((status (apply #'call-process "git" nil t nil
                         "--no-pager" "diff" "--no-color" "--no-ext-diff"
                         args)))
      (unless (eq status 0)
        (user-error "Git diff failed: %s" (string-trim (buffer-string))))
      (buffer-string))))

(defun syringe--fill-diff-buffer (diff root)
  "Make the diff buffer show DIFF, a diff of the repository at ROOT."
  (with-current-buffer (get-buffer-create syringe--diff-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert diff))
    ;; File names in the diff are relative to the root of the repository.
    (setq default-directory root)
    (syringe-diff-mode)
    (goto-char (point-min))))

(defun syringe--repository (directory)
  "Return the root of the git repository that contains DIRECTORY."
  (expand-file-name
   (or (locate-dominating-file directory ".git")
       (user-error "No git repository at %s" directory))))

(defun syringe--read-directory ()
  "Return the directory to start a review in.
That is the current directory, or one that the user names if the
current directory is in no git repository."
  (if (locate-dominating-file default-directory ".git")
      default-directory
    (read-directory-name "Git repository: ")))

(defun syringe--change-diff (root)
  "Return the diff that a review shows for the repository at ROOT.
It shows the code as it is now: the work that is not committed is
part of it.  The review starts from the commit before the last one."
  (syringe--git-diff root '("HEAD~1")))

(defun syringe--start (directory)
  "Start a review in the repository that contains DIRECTORY.
Show the diff in the selected window."
  (let* ((root (syringe--repository directory))
         (diff (syringe--change-diff root)))
    (when (string-empty-p diff)
      (user-error "Nothing to review: the last commit and the work on top \
of it change nothing"))
    (syringe--fill-diff-buffer diff root)
    (switch-to-buffer syringe--diff-buffer)))

(defun syringe-start-last-commit (directory)
  "Start a review of the last commit in the repository of DIRECTORY.
The diff shows what the commit you have checked out changed, and on
top of that your work that is not committed.  It is what `git diff
HEAD~1' prints.  To review the commits of a branch one by one, check
out each in turn and run this.

See `syringe--start' for what starting a review does."
  (interactive (list (syringe--read-directory)))
  (syringe--start directory))

(provide 'syringe)
;;; syringe.el ends here
