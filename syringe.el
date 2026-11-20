;;; syringe.el --- Review code in three windows: diff, live code, conversation  -*- lexical-binding: t; -*-

;;; Commentary:

;; Syringe helps you understand a change by reading it yourself.
;; A review uses two windows:
;;
;;   diff          what changed, under the message of the commit; RET
;;                 goes to that place in the live code
;;   live code     the real files, to read around in and to edit
;;
;; The windows are recognized by what they show, not by where they are:
;; resize them, move buffers between them, close and reopen them.
;;
;; Commands.  None of them is bound to a key.
;;
;;   syringe-start-last-commit  start a review of the last commit and of
;;                              the work on top of it that is not committed
;;   syringe-start-unstaged     start a review of the unstaged changes and
;;                              the new files

;;; Code:

(require 'diff-mode)
(require 'pulse)
(require 'seq)
(require 'subr-x)

(defgroup syringe nil
  "Review code in three windows: diff, live code, conversation."
  :group 'tools)

(defcustom syringe-diff-visit-selects-code t
  "Non-nil means RET in the diff also moves the cursor to the code window.
When nil, the code window shows the place but the cursor stays in the diff."
  :type 'boolean)

(defconst syringe--diff-buffer "*syringe-diff*"
  "Name of the buffer that shows the diff under review.")

;;;; Windows

;; A window is recognized by what it shows right now, not by where it
;; is.  So you can resize the windows, move buffers between them and
;; close them; a window that is needed and missing is made again.

(defun syringe--window-role (window)
  "Return what WINDOW shows: `diff', `code', or nil.
`code' is any ordinary window that does not show the diff."
  (let ((buffer (window-buffer window)))
    (cond ((equal (buffer-name buffer) syringe--diff-buffer) 'diff)
          ((or (window-dedicated-p window)
               (window-parameter window 'window-side))
           nil)
          (t 'code))))

(defun syringe--window (role)
  "Return the window of the selected frame that shows ROLE, or nil.
Of several such windows, return the most recently used one."
  (car (sort (seq-filter (lambda (window)
                           (eq (syringe--window-role window) role))
                         (window-list nil 'no-minibuffer))
             (lambda (a b) (> (window-use-time a) (window-use-time b))))))

(defun syringe--make-window (role)
  "Create and return a window for ROLE, which is `code'.
The code goes to the right of the diff.  Without a diff window, the
selected window is split."
  (let ((diff (syringe--window 'diff)))
    (pcase role
      ('code (split-window diff nil 'right)))))

(defun syringe--show (buffer role)
  "Show BUFFER in the window for ROLE, creating that window if needed.
Return the window."
  (let ((win (or (syringe--window role) (syringe--make-window role))))
    (set-window-buffer win buffer)
    win))

(defun syringe--show-code (buffer position select)
  "Show POSITION of BUFFER in the code window.
The line is marked until the next key press, to make it easy to find.
With SELECT non-nil, go to that window too."
  (let ((win (syringe--show buffer 'code)))
    (with-selected-window win
      (goto-char position)
      (recenter)
      (pulse-momentary-highlight-one-line (point)))
    (when select
      (select-window win))))

;;;; The diff

(defun syringe-diff-visit ()
  "Show the live code for the diff line at point in the code window.
The line is found by its text, so this still works after the file
has been edited."
  (interactive)
  (pcase-let* ((removed (save-excursion (beginning-of-line) (looking-at "-")))
               (`(,buffer ,_ ,pos ,src . ,_)
                (condition-case err
                    (diff-find-source-location nil (not removed) t)
                  ;; For example a line of a file that was deleted.
                  (error (user-error "No live code for this line: %s"
                                     (error-message-string err))))))
    (syringe--show-code buffer (+ (car pos) (cdr src))
                        syringe-diff-visit-selects-code)))

;; In a read-only diff, RET comes from `diff-mode-shared-map', which wins
;; over the keymap of a major mode.  Hence the remap as well as RET.
(defvar-keymap syringe-diff-mode-map
  "RET" #'syringe-diff-visit
  "<remap> <diff-goto-source>" #'syringe-diff-visit)

(define-derived-mode syringe-diff-mode diff-mode "Syringe-Diff"
  "Major mode for the diff under review.
RET goes to the live code for the line at point."
  (setq buffer-read-only t
        diff-mode-read-only t))

(defun syringe--git (root &rest args)
  "Run git with ARGS in ROOT.
Return its output without the final newline, or nil if git failed."
  (with-temp-buffer
    (setq default-directory root)
    (and (eq 0 (apply #'call-process "git" nil t nil args))
         (string-trim-right (buffer-string)))))

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

(defun syringe--show-diff-buffer ()
  "Show the diff buffer, in the window that shows a diff if there is one."
  (when-let* ((win (syringe--window 'diff)))
    (select-window win))
  (switch-to-buffer syringe--diff-buffer))

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

(defun syringe--untracked-diff (root)
  "Return a diff that adds every file in ROOT that git does not track.
Files that git ignores are left out."
  (mapconcat
   (lambda (file)
     (with-temp-buffer
       (setq default-directory root)
       ;; Against /dev/null, the empty file, a file is all additions.
       ;; Git exits with 1 here, which only says that the two differ.
       (call-process "git" nil t nil
                     "--no-pager" "diff" "--no-color" "--no-ext-diff"
                     "--no-index" "--" "/dev/null" file)
       (buffer-string)))
   (split-string (or (syringe--git root "ls-files" "--others"
                                   "--exclude-standard" "-z")
                     "")
                 "\0" t)
   ""))

;; A review has a kind, which says what it reviews: `last-commit' or
;; `unstaged'.

(defun syringe--change-diff (root kind)
  "Return the diff that a review of KIND shows for the repository at ROOT.
It shows the code as it is now: the work that is not committed and
the files that git does not track are part of it.  A review of the
last commit starts from the commit before it, a review of the
unstaged changes from what is staged.

A review of the last commit has the message of that commit above the
diff, as `git log' prints it."
  (concat (pcase kind
            ('unstaged "")
            (_ (concat (syringe--git root "--no-pager" "log" "-1"
                                     "--no-color" "--format=medium")
                       "\n\n")))
          (syringe--git-diff root (pcase kind
                                    ('unstaged nil)
                                    (_ '("HEAD~1"))))
          (syringe--untracked-diff root)))

(defun syringe--start (directory kind)
  "Start a review of KIND in the repository that contains DIRECTORY.
KIND is `last-commit' or `unstaged'.  Show the diff in the selected
window, or in the window that already shows a diff.  The code window
appears when it is first needed."
  (let* ((root (syringe--repository directory))
         (diff (syringe--change-diff root kind)))
    (when (string-empty-p diff)
      (user-error
       (pcase kind
         ('unstaged
          "Nothing to review: no unstaged changes and no new files")
         (_ "Nothing to review: the last commit and the work on top \
of it change nothing"))))
    (syringe--fill-diff-buffer diff root)
    (syringe--show-diff-buffer)))

(defun syringe-start-last-commit (directory)
  "Start a review of the last commit in the repository of DIRECTORY.
The diff shows what the commit you have checked out changed, and on
top of that your work that is not committed, with the new files that
git does not track.  It is what `git diff HEAD~1' prints.  To review
the commits of a branch one by one, check out each in turn and run
this.

See `syringe--start' for what starting a review does."
  (interactive (list (syringe--read-directory)))
  (syringe--start directory 'last-commit))

(defun syringe-start-unstaged (directory)
  "Start a review of the uncommitted work in the repository of DIRECTORY.
The diff shows the unstaged changes, as `git diff' prints them, and
every new file that git does not track yet, shown as added.  Files
that git ignores are left out, and so is what you have staged.

See `syringe--start' for what starting a review does."
  (interactive (list (syringe--read-directory)))
  (syringe--start directory 'unstaged))

(provide 'syringe)
;;; syringe.el ends here
