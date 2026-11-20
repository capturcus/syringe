;;; syringe.el --- Review code in three windows: diff, live code, conversation  -*- lexical-binding: t; -*-

;;; Commentary:

;; Syringe helps you understand a change by reading it yourself.
;; A review uses two windows:
;;
;;   diff          what changed, under the message of the commit; RET
;;                 goes to that place in the live code
;;   live code     the real files, to read around in and to edit
;;
;; A dot in the left margin marks a line that you want to talk about.
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
;;   syringe-toggle-dot         put a dot on this line, or remove the dot
;;                              that is there
;;   syringe-next-dot           jump to the next dot
;;   syringe-previous-dot       jump to the previous dot

;;; Code:

(require 'cl-lib)
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

(defface syringe-dot '((t :foreground "DodgerBlue" :weight bold))
  "Face of a dot in the left margin.")

;;;; Conversations and their dots

;; A dot is an overlay on one line that draws the dot in the left
;; margin and points to its conversation.

(cl-defstruct (syringe-conversation
               (:constructor syringe--conversation-create)
               (:copier nil))
  "A conversation about one line, which a dot marks."
  diff-dot         ; the dot in the diff, or nil
  code-dot)        ; the dot in the live code, or nil

(defvar syringe--conversations nil
  "Every conversation, newest first.")

(defconst syringe--diff-buffer "*syringe-diff*"
  "Name of the buffer that shows the diff under review.")

(defvar-local syringe--margin-added nil
  "Non-nil when this buffer's left margin has a column added for dots.")

(defun syringe--dots ()
  "Return the dots of the current buffer, from top to bottom."
  (sort (seq-filter (lambda (ov) (overlay-get ov 'syringe-conversation))
                    (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun syringe--dot-at-line ()
  "Return the dot on the current line, or nil."
  (seq-find (lambda (ov) (overlay-get ov 'syringe-conversation))
            (overlays-in (pos-bol) (pos-bol 2))))

(defun syringe--conversation-at-line ()
  "Return the conversation of the dot on the current line, or nil."
  (when-let* ((dot (syringe--dot-at-line)))
    (overlay-get dot 'syringe-conversation)))

(defun syringe--live-dot (dot)
  "Return DOT if it still marks a line in a live buffer, else nil."
  (and dot (overlay-buffer dot) dot))

(defun syringe--sync ()
  "Adapt the current buffer to whether it has dots.
While it has dots, the left margin gets one more column to draw them
in.  Other packages draw in the left margin too, so the column is
added to the width they asked for, and given back when the last dot
goes."
  (let ((want (and (syringe--dots) t)))
    (unless (eq want syringe--margin-added)
      (setq left-margin-width (max 0 (+ left-margin-width (if want 1 -1)))
            syringe--margin-added want)
      (dolist (win (get-buffer-window-list nil nil t))
        (set-window-margins win left-margin-width
                            (cdr (window-margins win)))))))

;; Changing the major mode resets the margin; add the column again.
(add-hook 'after-change-major-mode-hook #'syringe--sync 90)

(defun syringe--make-dot (conversation buffer position)
  "Mark the line at POSITION in BUFFER with a dot for CONVERSATION.
Do nothing if that line already has a dot."
  (with-current-buffer buffer
    (save-excursion
      (goto-char position)
      (unless (syringe--dot-at-line)
        ;; The fourth argument keeps text typed at the start of the line
        ;; out of the overlay, so that the dot follows its line when you
        ;; open a new line above it.
        (let ((dot (make-overlay (pos-bol) (pos-bol 2) nil t nil)))
          (overlay-put dot 'syringe-conversation conversation)
          (if (derived-mode-p 'syringe-diff-mode)
              (setf (syringe-conversation-diff-dot conversation) dot)
            (setf (syringe-conversation-code-dot conversation) dot))
          (syringe--draw-dot dot)
          (syringe--sync))))))

(defun syringe--draw-dot (dot)
  "Draw DOT in the left margin."
  (overlay-put dot 'before-string
               (propertize "x" 'display
                           (list '(margin left-margin)
                                 (propertize "●" 'face 'syringe-dot)))))

(defun syringe--live-dots (conversation)
  "Return the dots of CONVERSATION that still mark a line."
  (delq nil (list (syringe--live-dot
                   (syringe-conversation-diff-dot conversation))
                  (syringe--live-dot
                   (syringe-conversation-code-dot conversation)))))

(defun syringe--drop (conversation)
  "Take CONVERSATION off the screen: remove its dots."
  (dolist (dot (syringe--live-dots conversation))
    (with-current-buffer (overlay-buffer dot)
      (delete-overlay dot)
      (syringe--sync))))

(defun syringe--delete-conversation (conversation)
  "Delete CONVERSATION for good."
  (setq syringe--conversations (delq conversation syringe--conversations))
  (syringe--drop conversation))

(defun syringe--add-dot ()
  "Start a conversation about the current line and return it.
Put a dot on the line."
  (let ((conversation (syringe--conversation-create)))
    (push conversation syringe--conversations)
    (syringe--make-dot conversation (current-buffer) (point))
    conversation))

(defun syringe-toggle-dot ()
  "Put a dot on the current line, or remove the dot that is on it."
  (interactive)
  (let ((conversation (syringe--conversation-at-line)))
    (if conversation
        (syringe--delete-conversation conversation)
      (syringe--add-dot))))

(defun syringe-next-dot ()
  "Jump to the next dot below the current line."
  (interactive)
  (let ((dot (seq-find (lambda (dot) (> (overlay-start dot) (pos-eol)))
                       (syringe--dots))))
    (goto-char (overlay-start (or dot (user-error "No dot below this line"))))))

(defun syringe-previous-dot ()
  "Jump to the previous dot above the current line."
  (interactive)
  (let ((dot (seq-find (lambda (dot) (< (overlay-start dot) (pos-bol)))
                       (reverse (syringe--dots)))))
    (goto-char (overlay-start (or dot (user-error "No dot above this line"))))))

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
appears when it is first needed.  The review that was open is closed."
  (let* ((root (syringe--repository directory))
         (diff (syringe--change-diff root kind)))
    (when (string-empty-p diff)
      (user-error
       (pcase kind
         ('unstaged
          "Nothing to review: no unstaged changes and no new files")
         (_ "Nothing to review: the last commit and the work on top \
of it change nothing"))))
    (syringe--new-review root diff)
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

(defun syringe--close-review ()
  "Take the current review, if there is one, off the screen."
  (let ((conversations syringe--conversations))
    (setq syringe--conversations nil)
    (mapc #'syringe--drop conversations)))

(defun syringe--new-review (root diff)
  "Start a review of DIFF, a diff of the repository at ROOT."
  (syringe--close-review)
  (syringe--fill-diff-buffer diff root))

(provide 'syringe)
;;; syringe.el ends here
