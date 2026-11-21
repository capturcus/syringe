;;; syringe.el --- Review code in three windows: diff, live code, conversation  -*- lexical-binding: t; -*-

;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Syringe helps you understand a change by reading it yourself.
;; A review uses three windows:
;;
;;   diff          what changed, under the messages of its commits; RET
;;                 goes to that place in the live code
;;   live code     the real files, to read around in and to edit
;;   conversation  a chat with the robot about one line
;;
;; A dot in the left margin marks a line that has a conversation.  A
;; line that is both in the diff and in the live code has a dot in both
;; places, and the two dots share one conversation.  When point comes
;; to a line with a dot, the conversation window shows that
;; conversation.  The color of a dot tells the state of its
;; conversation:
;;
;;   blue    nothing new
;;   orange  the robot is thinking
;;   green   the robot has answered and you have not gone there yet
;;   red     the last message failed
;;   gray    the line is lost: it was deleted or rewritten beyond recognition
;;
;; A review is alive.  You edit the code, and so may other programs.
;; The diff is taken again whenever a file of the repository is saved
;; or read again from disk, and the conversations stay.  A dot follows
;; its line: a conversation remembers the text of its line and of the
;; lines around it, and looks for the line again when the code changes.
;;
;; The robot is a coding agent that can read the repository but not
;; change it.  When a review starts, one robot session reads the diff.
;; All the conversations are held in that session, so in each of them
;; the robot knows what was said in the others.  It answers one message
;; at a time; a message that is sent meanwhile waits for its turn.
;; In an answer, a reference such as src/main.c:42 is a link: move the
;; cursor onto it and press RET to go to that line in the live code.
;;
;; The windows are recognized by what they show, not by where they are:
;; resize them, move buffers between them, close and reopen them.
;;
;; Commands.  None of them is bound to a key; bind them in your init file.
;;
;;   syringe-start              start a review of what has changed since a
;;                              revision, such as HEAD~1 for the last commit,
;;                              or with no revision of what is not staged
;;   syringe-toggle-dot         put a dot on this line and open its
;;                              conversation, or remove the dot that is there
;;   syringe-open-conversation  jump to the closest dot, open its conversation
;;   syringe-next-dot           jump to the next dot
;;   syringe-previous-dot       jump to the previous dot
;;   syringe-list-dots          choose a dot from a list and jump to it
;;   syringe-next-link          in a conversation, go to the next link
;;   syringe-previous-link      in a conversation, go to the previous link
;;   syringe-restore            open a saved review
;;
;; A review is saved as you go, in the directory syringe inside the
;; repository's .git, so it is never committed.  A review belongs to
;; the commit that was checked out when it started: starting a review
;; on a commit that already has a saved review opens that one again.

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

(defcustom syringe-brief "\
You are helping me review a code change in this repository.  %s

Help me think, don't think for me.  I want to understand this code
myself, so:
- Answer the question I ask.  Explain what the code does and why, and
  tell me where to read.
- Do not judge the change as a whole, and do not list problems I did
  not ask about.
- Do not write replacement code unless I ask for it.
- When you are not sure, say so, and say what you would check.

Rules:
- Never modify a file.  You may read any file and run read-only
  commands to find things out.
- Refer to code as path:line, for example src/main.c:42, with the path
  relative to the root of the repository and the line numbers of the
  files as they are now.  I can jump to such a reference.
- Be brief.  Write plain text: no headings and no tables.
- I edit the code while we talk.  Before you rely on something you
  read earlier, read the file again.

The review has several conversations, each about one line, and all of
them are held here, one message at a time.  A message of mine says so
when it starts a new conversation, when it goes back to an earlier
one, and when the line of its conversation has moved or changed.
Otherwise it continues the conversation of the message before it.
What was said in one conversation may be used in the others.

Now read the change and the code around it, so that you are ready for
questions.  Read all of the change, in several steps if it is long.
Then reply with the single word: ready"
  "What the robot is told when a review starts.
%s stands for a sentence that says how to see the change under review;
see `syringe--change-description'.
Every conversation is held in the robot session that read this, so
it works as the system prompt.  A change here applies to the next
review, not to the running one."
  :type 'string)

(defface syringe-dot '((t :foreground "DodgerBlue" :weight bold))
  "Face of a dot whose conversation has nothing new.")

(defface syringe-dot-thinking '((t :foreground "orange" :weight bold))
  "Face of a dot whose conversation the robot is thinking about.")

(defface syringe-dot-unread '((t :foreground "green3" :weight bold))
  "Face of a dot whose conversation has an answer you have not gone to.")

(defface syringe-dot-error '((t :foreground "red" :weight bold))
  "Face of a dot whose last message failed.")

(defface syringe-dot-lost '((t :foreground "gray50" :weight bold))
  "Face of a dot whose line is not found in the code any more.")

(defface syringe-user '((t :inherit font-lock-keyword-face))
  "Face of your messages in a conversation.")

(defface syringe-robot '((t :inherit font-lock-string-face))
  "Face of the robot's messages in a conversation.")

(defface syringe-error '((t :inherit error))
  "Face of an error message in a conversation.")

(defface syringe-prompt '((t :inherit minibuffer-prompt))
  "Face of the prompt in a conversation.")

;;;; Conversations and their dots

;; A conversation is the data: a title and the messages.  A dot is an
;; overlay on one line that draws the dot in the left margin and points
;; to its conversation.

(cl-defstruct (syringe-conversation
               (:constructor syringe--conversation-create)
               (:copier nil))
  "A conversation about one line.
It is marked by up to two dots: one on the line in the diff and one
on the same line in the live code."
  title            ; where the line is, a string such as "src/main.rs:42"
  messages         ; (ROLE . TEXT) pairs, oldest first; see `syringe--say'
  buffer           ; the buffer that shows the conversation, or nil
  diff-dot         ; the dot in the diff, or nil
  code-dot         ; the dot in the live code, or nil
  ;; The line as it was last seen in the code, to find it again when
  ;; the code has changed; see `syringe--find-code-line'.
  code-file        ; its file, relative to the root, or nil
  code-line        ; its line number
  code-text        ; its text
  code-above       ; the lines above it, the nearest first
  code-below       ; the lines below it, the nearest first
  ;; The line as it was last seen in the diff, for a line that is not
  ;; in the code, such as a removed line.
  diff-file        ; the file whose part of the diff it is in, or nil
  diff-text        ; the line of the diff, with its first character
  diff-number      ; its line number in that file
  lost             ; non-nil when the line is not found any more
  told             ; (PLACE . TEXT): what the robot last heard about the line
  (state 'read)    ; `read', `thinking', `unread' or `error': the dot's color
  activity)        ; what the robot is doing while it thinks, a string or nil

(defvar syringe--conversations nil
  "Every conversation, newest first.")

(defvar syringe--file nil
  "The file that the current review is saved in, or nil without a review.")

(defvar syringe--about nil
  "Facts about the current review, a property list.
:created is when it started and :kind what it reviews; see
`syringe--change-diff'.  :commit is the commit the review belongs to
and :head its short name and subject.  :changed is non-nil once the
diff has changed since the robot read it.")

(defvar syringe--refresh-timer nil
  "The timer that takes the diff again once Emacs is idle, or nil.")

(defconst syringe--refresh-delay 0.5
  "How long Emacs must be idle before the diff is taken again, in seconds.")

(defvar syringe--root nil
  "Root directory of the repository under review.")

(defvar syringe--revision-history nil
  "The revisions that were typed to start a review, the newest first.")

(defvar syringe--session nil
  "The robot session that read the diff when the review started.
Every conversation is held in it.")

(defvar syringe--session-state nil
  "State of `syringe--session': `preparing', `ready', `failed', or nil.")

(defvar syringe--session-process nil
  "The robot process that is at work in `syringe--session', or nil.
It reads the diff or works on a message; there is never more than one.")

(defvar syringe--session-error nil
  "Why `syringe--session' failed, a string.")

(defvar syringe--asking nil
  "The conversation whose message the robot is working on, or nil.")

(defvar syringe--asked-last nil
  "The conversation that the robot answered in last, or nil.
A message in another conversation says that the subject changes.")

(defvar syringe--waiting nil
  "The messages that wait for the robot to be free, the oldest first.
Each is (CONVERSATION . TEXT).")

(defconst syringe--diff-buffer "*syringe-diff*"
  "Name of the buffer that shows the diff under review.")

(defvar-local syringe--margin-added nil
  "Non-nil when this buffer's left margin has a column added for dots.")

(defvar-local syringe--followed-line nil
  "Start of the line point was on when `syringe--follow' last looked.")

(defvar-local syringe--followed-visit nil
  "The use time of the selected window when `syringe--follow' last looked.
A window gets a new use time whenever it is selected, so this tells
a new visit to the window from the one before.")

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
in, and the conversation window follows point (see `syringe--follow').
Other packages draw in the left margin too, so the column is added to
the width they asked for, and given back when the last dot goes."
  (let ((want (and (syringe--dots) t)))
    (if want
        (add-hook 'post-command-hook #'syringe--follow nil t)
      (remove-hook 'post-command-hook #'syringe--follow t))
    (unless (eq want syringe--margin-added)
      (setq left-margin-width (max 0 (+ left-margin-width (if want 1 -1)))
            syringe--margin-added want)
      (dolist (win (get-buffer-window-list nil nil t))
        (set-window-margins win left-margin-width
                            (cdr (window-margins win)))))))

;; Changing the major mode resets the margin and the hook; set them again.
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

;;;; Finding a line again

;; While you type, a dot moves with its line by itself.  That is not
;; enough: a line can be killed and yanked somewhere else, and a file
;; can be read again from disk after another program changed it, which
;; leaves every dot of its buffer in the wrong place.  So a
;; conversation remembers its line, with a few lines around it, and
;; looks for it again:
;;
;; - when you save the file: a dot that is still on a line is believed,
;;   and what is remembered is brought up to date; a dot whose line was
;;   killed is looked for;
;; - when the file is read again from disk or visited: every line is
;;   looked for.
;;
;; What is remembered changes only when the line is found.  A line that
;; is not found is lost: its dot turns gray and stays near where the
;; line was, and the line is looked for again the next time.

(defconst syringe--context 3
  "How many lines above and below a line are remembered with it.")

(defconst syringe--reach 30
  "How far from its old place a rewritten line is looked for, in lines.")

(defconst syringe--telling-length 4
  "How long a line must be, without the space at its ends, to tell a place.")

(defconst syringe--likeness-needed 0.6
  "How alike a rewritten line must be to the remembered one, from 0 to 1.")

(defconst syringe--likeness-margin 0.15
  "By how much the most alike line must beat every other line near it.")

(defun syringe--lines ()
  "Return the lines of the current buffer as a vector of strings."
  (save-restriction
    (widen)
    (vconcat (split-string (buffer-substring-no-properties
                            (point-min) (point-max))
                           "\n"))))

(defun syringe--line-start (index)
  "Return the position where line INDEX of the current buffer starts.
The first line is line 0."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line index)
      (point))))

(defun syringe--in-review-p ()
  "Return non-nil if the current buffer visits a file of the open review."
  (and syringe--file
       buffer-file-name
       (file-in-directory-p buffer-file-name syringe--root)))

(defun syringe--retitle (conversation title)
  "Give CONVERSATION the TITLE, and its buffer the name that goes with it."
  (unless (equal title (syringe-conversation-title conversation))
    (setf (syringe-conversation-title conversation) title)
    (let ((buffer (syringe-conversation-buffer conversation)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (rename-buffer (format "*syringe: %s*" title) t)
          (force-mode-line-update))))))

(defun syringe--remember-code-line (conversation lines index)
  "Remember line INDEX of LINES as the line of CONVERSATION.
LINES are the lines of the current buffer, which visits a file of
the review.  The conversation is then not lost, and its title says
where the line is."
  (let ((file (file-relative-name buffer-file-name syringe--root))
        (count (length lines)))
    (setf (syringe-conversation-code-file conversation) file
          (syringe-conversation-code-line conversation) (1+ index)
          (syringe-conversation-code-text conversation) (aref lines index)
          (syringe-conversation-code-above conversation)
          (cl-loop for k from 1 to syringe--context
                   when (>= (- index k) 0)
                   collect (aref lines (- index k)))
          (syringe-conversation-code-below conversation)
          (cl-loop for k from 1 to syringe--context
                   when (< (+ index k) count)
                   collect (aref lines (+ index k)))
          (syringe-conversation-lost conversation) nil)
    (syringe--retitle conversation (format "%s:%d" file (1+ index)))))

(defun syringe--telling-p (text)
  "Return non-nil if the line TEXT says enough to know a place by.
A blank line or a closing brace is everywhere."
  (>= (length (string-trim text)) syringe--telling-length))

(defun syringe--likeness (a b)
  "Return how alike the lines A and B are, from 0 to 1.
The space at their ends does not count, so a line that was only
indented differently is 1."
  (let* ((a (string-trim a))
         (b (string-trim b))
         (longest (max (length a) (length b))))
    (if (zerop longest)
        0.0
      (- 1.0 (/ (float (string-distance a b)) longest)))))

(defun syringe--find-code-line (conversation lines)
  "Return the index in LINES of the line of CONVERSATION, or nil.
LINES are the lines of its file as they are now.  The line is looked
for with what the conversation remembers of it, from strict to loose:

1. the line where it was, with the same lines around it;
2. a line that reads the same: the one with the most remembered
   neighbours around it, then the one nearest to where it was;
3. a line that has remembered neighbours on both sides, when the line
   itself was rewritten;
4. the line most like it, near where it was, if no other line there
   is nearly as much like it.

A short line, such as a closing brace, is found only by its
neighbours, and short neighbours do not count."
  (let* ((text (syringe-conversation-code-text conversation))
         (was (1- (syringe-conversation-code-line conversation)))
         (count (length lines))
         ;; How many of NEIGHBOURS are in their places next to line
         ;; INDEX, going in DIRECTION: -1 is up, 1 is down.  With EVERY,
         ;; short neighbours count too.
         (beside
          (lambda (index neighbours direction &optional every)
            (cl-loop for neighbour in neighbours
                     for place = (+ index direction) then (+ place direction)
                     count (and (or every (syringe--telling-p neighbour))
                                (< -1 place count)
                                (equal neighbour (aref lines place))))))
         (above (lambda (index &optional every)
                  (funcall beside index
                           (syringe-conversation-code-above conversation)
                           -1 every)))
         (below (lambda (index &optional every)
                  (funcall beside index
                           (syringe-conversation-code-below conversation)
                           1 every)))
         (around (lambda (index)
                   (+ (funcall above index) (funcall below index))))
         ;; Of the lines CANDIDATES, the one with the most neighbours,
         ;; then the one nearest to where the line was.
         (best
          (lambda (candidates)
            (car (sort candidates
                       (lambda (a b)
                         (let ((around-a (funcall around a))
                               (around-b (funcall around b)))
                           (or (> around-a around-b)
                               (and (= around-a around-b)
                                    (< (abs (- a was)) (abs (- b was)))))))))))
         (same (cl-loop for index below count
                        when (equal text (aref lines index))
                        collect index)))
    (or
     ;; 1. Nothing has changed around the line.
     (and (memq was same)
          (= (funcall above was t)
             (length (syringe-conversation-code-above conversation)))
          (= (funcall below was t)
             (length (syringe-conversation-code-below conversation)))
          was)
     ;; 2. A line that reads the same.
     (funcall best
              (if (syringe--telling-p text)
                  same
                (seq-filter (lambda (index) (> (funcall around index) 0))
                            same)))
     ;; 3. A rewritten line between its old neighbours.  Neighbours on
     ;; one side only are what a killed line leaves behind.
     (funcall best
              (cl-loop for index below count
                       when (and (> (funcall above index) 0)
                                 (> (funcall below index) 0))
                       collect index))
     ;; 4. The line most like it, near where it was.  In code that
     ;; repeats itself many lines are alike, and the line may as well
     ;; have been killed; so the line must stand out from the rest.
     (and (syringe--telling-p text)
          (let ((found nil)
                (most 0.0)
                (next 0.0))
            (cl-loop for index from (max 0 (- was syringe--reach))
                     to (min (1- count) (+ was syringe--reach))
                     for likeness = (syringe--likeness text (aref lines index))
                     do (cond ((> likeness most)
                               (setq next most
                                     most likeness
                                     found index))
                              ((> likeness next)
                               (setq next likeness))))
            (and (> most syringe--likeness-needed)
                 (> (- most next) syringe--likeness-margin)
                 found))))))

(defun syringe--settle-buffer (&optional reloaded)
  "Bring the dots of the current buffer in line with its text.
This concerns every conversation whose line is in this file.  If its
dot is still on a line, that line is its line, however it reads now,
and is remembered afresh.  If its dot has no line left, because the
line was killed, or if it has no dot here, the line is looked for
with `syringe--find-code-line'.  A line that is found gets the dot.
A line that is not found is lost: the dot stays near where the line
was and turns gray, and what is remembered is kept.

RELOADED non-nil means that the buffer has just been read from its
file.  Then no dot is where it belongs, and every line is looked for."
  (when (and buffer-file-name syringe--root)
    (let ((file (file-relative-name buffer-file-name syringe--root))
          (lines nil))
      (dolist (conversation syringe--conversations)
        (when (equal file (syringe-conversation-code-file conversation))
          (unless lines
            (setq lines (syringe--lines)))
          (let* ((dot (syringe--live-dot
                       (syringe-conversation-code-dot conversation)))
                 (dot (and dot (eq (overlay-buffer dot) (current-buffer)) dot))
                 ;; The line under the dot, unless the dot is out of place.
                 (under (and dot (not reloaded)
                             (1- (line-number-at-pos (overlay-start dot) t))))
                 (index (if (and under
                                 (not (syringe-conversation-lost conversation))
                                 ;; An empty dot is all that is left of a
                                 ;; killed line.
                                 (< (overlay-start dot) (overlay-end dot)))
                            under
                          (syringe--find-code-line conversation lines))))
            (when dot
              (delete-overlay dot))
            (if index
                (syringe--remember-code-line conversation lines index)
              (setf (syringe-conversation-lost conversation) t))
            (syringe--make-dot
             conversation (current-buffer)
             (syringe--line-start
              (or index under
                  (min (1- (length lines))
                       (1- (syringe-conversation-code-line conversation)))))))))
      (when lines
        (syringe--sync)
        (syringe--save)))))

(defun syringe--file-saved ()
  "Follow the saving of the current buffer, if its file is in the review.
Its dots are settled and the diff is taken again."
  (when (syringe--in-review-p)
    (syringe--settle-buffer)
    (syringe--refresh-soon)))

(defun syringe--file-reloaded ()
  "Follow the reading of the current buffer from its file again.
That happens when another program has changed the file."
  (when (syringe--in-review-p)
    (syringe--settle-buffer t)
    (syringe--refresh-soon)))

(defun syringe--file-visited ()
  "Give the current buffer, whose file was just visited, its dots."
  (when (syringe--in-review-p)
    (syringe--settle-buffer t)))

(add-hook 'after-save-hook #'syringe--file-saved)
(add-hook 'after-revert-hook #'syringe--file-reloaded)
(add-hook 'find-file-hook #'syringe--file-visited)

(defun syringe--dot-face (conversation)
  "Return the face of the dots of CONVERSATION.
It is gray if the line is lost, and else has the color of the state."
  (if (syringe-conversation-lost conversation)
      'syringe-dot-lost
    (pcase (syringe-conversation-state conversation)
      ('thinking 'syringe-dot-thinking)
      ('unread 'syringe-dot-unread)
      ('error 'syringe-dot-error)
      (_ 'syringe-dot))))

(defun syringe--draw-dot (dot)
  "Draw DOT in the left margin, in the color of its conversation."
  (let ((face (syringe--dot-face (overlay-get dot 'syringe-conversation))))
    (overlay-put dot 'before-string
                 (propertize "x" 'display
                             (list '(margin left-margin)
                                   (propertize "●" 'face face))))))

(defun syringe--live-dots (conversation)
  "Return the dots of CONVERSATION that still mark a line."
  (delq nil (list (syringe--live-dot
                   (syringe-conversation-diff-dot conversation))
                  (syringe--live-dot
                   (syringe-conversation-code-dot conversation)))))

(defun syringe--set-state (conversation state &optional activity)
  "Put CONVERSATION in STATE and show it.
STATE is `read', `thinking', `unread' or `error'; it colors the dots.
ACTIVITY says what the robot is doing; the conversation's header
line shows it."
  (setf (syringe-conversation-state conversation) state
        (syringe-conversation-activity conversation) activity)
  (mapc #'syringe--draw-dot (syringe--live-dots conversation))
  (let ((buffer (syringe-conversation-buffer conversation)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (syringe--show-prompt)
        (force-mode-line-update)))))

(defun syringe--mark-read (conversation)
  "Note that you have gone to CONVERSATION: a green dot turns blue."
  (when (eq (syringe-conversation-state conversation) 'unread)
    (syringe--set-state conversation 'read)
    (syringe--save)))

(defun syringe--drop (conversation)
  "Take CONVERSATION off the screen.
That is its dots and its buffer."
  (dolist (dot (syringe--live-dots conversation))
    (with-current-buffer (overlay-buffer dot)
      (delete-overlay dot)
      (syringe--sync)))
  (when (buffer-live-p (syringe-conversation-buffer conversation))
    (kill-buffer (syringe-conversation-buffer conversation))))

(defun syringe--delete-conversation (conversation)
  "Delete CONVERSATION for good.
A message of it that waits for the robot is not sent, and the robot
is stopped if it is working on one.  What the robot has heard in the
conversation, it still knows."
  ;; First, so that the robot's reply to a killed process is ignored.
  (setq syringe--conversations (delq conversation syringe--conversations)
        syringe--waiting (assq-delete-all conversation syringe--waiting))
  (when (and (eq conversation syringe--asking)
             (process-live-p syringe--session-process))
    (delete-process syringe--session-process))
  (syringe--drop conversation)
  (syringe--ask-next)
  (syringe--save))

(defun syringe--file-title ()
  "Return the current buffer's file relative to its repository, or its name."
  (let ((root (and buffer-file-name
                   (locate-dominating-file buffer-file-name ".git"))))
    (if root
        (file-relative-name buffer-file-name root)
      (buffer-name))))

(defun syringe--line-title ()
  "Return a name for the current line, such as \"src/main.c:42\"."
  (if (derived-mode-p 'diff-mode)
      (pcase (syringe--diff-line-info)
        (`(,file ,line ?-) (format "%s:%d (removed)" file line))
        (`(,file ,line ,_) (format "%s:%d" file line))
        ;; A line of the diff that is not part of a hunk: a line of the
        ;; commit message, which is above the first file, or a header.
        (_ (if (save-excursion (re-search-backward "^diff --git " nil t))
               (format "diff line %d" (line-number-at-pos nil t))
             "the commit message")))
    (format "%s:%d" (syringe--file-title) (line-number-at-pos nil t))))

(defun syringe--closest-dot ()
  "Return the dot the fewest lines away from the current line, or nil."
  (let ((here (pos-bol))
        closest distance)
    (dolist (dot (syringe--dots) closest)
      (let* ((there (overlay-start dot))
             (lines (count-lines (min here there) (max here there))))
        (when (or (null distance) (< lines distance))
          (setq closest dot
                distance lines))))))

(defun syringe--follow ()
  "Make the conversation window show the conversation of point's line.
This runs after every command in a buffer that has dots.  It acts
only when point has come to a line, that line has a dot, and a
conversation window is open.  Point comes to a line by moving there,
or by its window being selected while it is on the line."
  (let ((line (pos-bol))
        (visit (window-use-time)))
    (unless (and (eql line syringe--followed-line)
                 (eql visit syringe--followed-visit))
      (setq syringe--followed-line line
            syringe--followed-visit visit)
      ;; An error here would make Emacs drop this function from the hook.
      (with-demoted-errors "Syringe: %S"
        (when-let* ((conversation (syringe--conversation-at-line))
                    (win (syringe--window 'chat)))
          (let ((buffer (syringe--chat-buffer conversation)))
            (unless (eq buffer (window-buffer win))
              (set-window-buffer win buffer)
              (set-window-point win (with-current-buffer buffer
                                      (point-max))))
            (syringe--mark-read conversation)))))))

(defun syringe--remember-diff-line (conversation)
  "Remember the line of the diff at point as the line of CONVERSATION.
This is for a line that is not in the code, such as a removed line."
  (pcase-let ((`(,file ,number ,_) (syringe--diff-line-info)))
    (setf (syringe-conversation-diff-file conversation) file
          (syringe-conversation-diff-number conversation) number
          (syringe-conversation-diff-text conversation)
          (buffer-substring-no-properties (pos-bol) (pos-eol)))))

(defun syringe--add-code-dot (conversation)
  "Give CONVERSATION, started on the line of the diff at point, a code dot.
Return non-nil if that worked: the line is an added or unchanged
line, and it is found in its file.  This visits the file."
  (pcase (syringe--diff-line-info)
    (`(,file ,number ,(or ?\s ?+))
     (let ((path (expand-file-name file syringe--root))
           (text (buffer-substring-no-properties (1+ (pos-bol)) (pos-eol))))
       (when (file-regular-p path)
         (with-current-buffer (find-file-noselect path)
           (let ((lines (syringe--lines)))
             ;; Look for the line as if the diff had been remembered.
             (setf (syringe-conversation-code-file conversation) file
                   (syringe-conversation-code-line conversation) number
                   (syringe-conversation-code-text conversation) text)
             (let ((index (if (and (<= number (length lines))
                                   (equal text (aref lines (1- number))))
                              ;; The diff is fresh: the line is where it says.
                              (1- number)
                            (syringe--find-code-line conversation lines))))
               (if (not index)
                   (setf (syringe-conversation-code-file conversation) nil)
                 (syringe--remember-code-line conversation lines index)
                 (syringe--make-dot conversation (current-buffer)
                                    (syringe--line-start index))
                 t)))))))))

(defun syringe--add-dot ()
  "Start a conversation about the current line and return it.
Put a dot on the line.  If the line is in the diff and in the live
code, put a dot in both places."
  (let ((conversation (syringe--conversation-create
                       :title (syringe--line-title))))
    (push conversation syringe--conversations)
    (syringe--make-dot conversation (current-buffer) (point))
    (cond ((derived-mode-p 'syringe-diff-mode)
           (unless (syringe--add-code-dot conversation)
             (syringe--remember-diff-line conversation)))
          ((syringe--in-review-p)
           (syringe--remember-code-line conversation (syringe--lines)
                                        (1- (line-number-at-pos nil t)))
           (when-let* ((position (syringe--diff-position conversation)))
             (syringe--make-dot conversation (get-buffer syringe--diff-buffer)
                                position))))
    (syringe--save)
    conversation))

(defun syringe-toggle-dot ()
  "Put a dot on the current line, or remove the dot that is on it.
A new dot starts a conversation and opens it.  If the line is both
in the diff and in the live code, it gets a dot in both places, with
the same conversation.

Removing a dot removes its twin and their conversation; this asks
first when the conversation has messages.  In a conversation window,
remove that conversation: that is how to get rid of one whose line
is lost and has no dot."
  (interactive)
  (let ((conversation (if (derived-mode-p 'syringe-chat-mode)
                          (buffer-local-value 'syringe--conversation
                                              (current-buffer))
                        (syringe--conversation-at-line))))
    (cond ((null conversation)
           (syringe--open-chat (syringe--add-dot)))
          ((or (null (syringe-conversation-messages conversation))
               (y-or-n-p "Delete this dot and its conversation? "))
           (syringe--delete-conversation conversation)))))

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

(defun syringe-open-conversation ()
  "Jump to the dot closest to the current line and open its conversation."
  (interactive)
  (let ((dot (or (syringe--closest-dot) (user-error "No dots in this buffer"))))
    (goto-char (overlay-start dot))
    (syringe--open-chat (overlay-get dot 'syringe-conversation))))

(defun syringe--choose (prompt things label)
  "Let the user choose one of THINGS in the minibuffer; return it.
PROMPT is the prompt.  LABEL is a function that returns the line to
show for a thing.  The lines are shown in the order of THINGS.  With
`helm-mode' the list looks like the other Helm lists."
  (let ((choices nil))
    (dolist (thing things)
      (let ((line (funcall label thing))
            (count 1))
        ;; Two things may read the same; number the later ones.
        (while (assoc line choices)
          (setq line (format "%s (%d)" (funcall label thing) (cl-incf count))))
        (push (cons line thing) choices)))
    (setq choices (nreverse choices))
    (cdr (assoc (completing-read
                 prompt
                 (lambda (string predicate action)
                   (if (eq action 'metadata)
                       ;; Keep the order instead of sorting by name.
                       '(metadata (display-sort-function . identity)
                                  (cycle-sort-function . identity))
                     (complete-with-action action choices string predicate)))
                 nil t)
                choices))))

(defun syringe--dot-label (conversation)
  "Return the line that shows CONVERSATION in the list of dots.
That is its dot, its state unless there is nothing new, its place
and your first message."
  (format "%s %-8s %-28s %s"
          (propertize "●" 'face (syringe--dot-face conversation))
          ;; In words too: the selected line of a list loses its colors.
          (cond ((syringe-conversation-lost conversation) "lost")
                (t (pcase (syringe-conversation-state conversation)
                     ('unread "unread")
                     ('thinking "thinking")
                     ('error "failed")
                     (_ ""))))
          (syringe-conversation-title conversation)
          (truncate-string-to-width
           (string-replace "\n" " "
                           (or (cdr (assq 'user (syringe-conversation-messages
                                                 conversation)))
                               ""))
           60 nil nil t)))

(defun syringe--goto-conversation (conversation)
  "Go to the dot of CONVERSATION and show the conversation next to it.
From the diff, go to its dot in the diff; from anywhere else, to its
dot in the live code.  If it has only one dot, go to that one.  If
it has none, because its line is lost, go to the conversation."
  ;; A dot in a file that is not open comes back when the file is visited.
  (let ((file (syringe-conversation-code-file conversation)))
    (when (and file
               (not (syringe--live-dot
                     (syringe-conversation-code-dot conversation)))
               (file-regular-p (expand-file-name file syringe--root)))
      (find-file-noselect (expand-file-name file syringe--root))))
  (let* ((diff-dot (syringe--live-dot
                    (syringe-conversation-diff-dot conversation)))
         (code-dot (syringe--live-dot
                    (syringe-conversation-code-dot conversation)))
         (dot (if (eq (syringe--window-role (selected-window)) 'diff)
                  (or diff-dot code-dot)
                (or code-dot diff-dot)))
         (buffer (syringe--chat-buffer conversation)))
    (cond ((null dot))
          ((eq dot code-dot)
           (select-window (syringe--show (overlay-buffer dot) 'code)))
          (t
           (when-let* ((win (syringe--window 'diff)))
             (select-window win))
           (switch-to-buffer (overlay-buffer dot))))
    (when dot
      (goto-char (overlay-start dot)))
    (let ((win (syringe--show buffer 'chat)))
      (set-window-point win (with-current-buffer buffer (point-max)))
      (unless dot
        (select-window win)))
    (syringe--mark-read conversation)))

(defun syringe-list-dots ()
  "Choose a dot from a list of all the dots and jump to it.
The dots with an answer you have not gone to come first, then the
failed ones, then the ones the robot is thinking about, then the
rest.  A line shows the dot, its state, its place and your first
message; type to narrow the list."
  (interactive)
  (unless syringe--conversations
    (user-error "There are no dots yet"))
  (let* ((order '(unread error thinking read))
         (rank (lambda (conversation)
                 (seq-position order
                               (syringe-conversation-state conversation))))
         (conversation
          (syringe--choose "Dot: "
                           (sort (copy-sequence syringe--conversations)
                                 (lambda (a b)
                                   (< (funcall rank a) (funcall rank b))))
                           #'syringe--dot-label)))
    (when conversation
      (syringe--goto-conversation conversation))))

;;;; Windows

;; A window is recognized by what it shows right now, not by where it
;; is.  So you can resize the windows, move buffers between them and
;; close them; a window that is needed and missing is made again.

(defun syringe--window-role (window)
  "Return what WINDOW shows: `diff', `chat', `code', or nil.
`code' is any ordinary window that shows neither the diff nor a
conversation."
  (let ((buffer (window-buffer window)))
    (cond ((equal (buffer-name buffer) syringe--diff-buffer) 'diff)
          ((eq (buffer-local-value 'major-mode buffer) 'syringe-chat-mode)
           'chat)
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
  "Create and return a window for ROLE, which is `code' or `chat'.
The code goes to the right of the diff and the conversation below
the code.  Without a diff window, the selected window is split."
  (let ((diff (syringe--window 'diff))
        (code (syringe--window 'code))
        (chat (syringe--window 'chat)))
    (pcase role
      ('code (if chat
                 (split-window chat nil 'above)
               (split-window diff nil 'right)))
      ('chat (if code
                 (split-window code nil 'below)
               (split-window diff nil 'right))))))

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

;;;; The conversation buffer

;; Every conversation has its own buffer.  It holds the old messages,
;; which are read-only, then a prompt, then the text being typed.  The
;; messages are stored in the conversation, so the buffer can be killed
;; and drawn again.

(defconst syringe--prompt "> "
  "The prompt shown before the text being typed.")

(defconst syringe--prompt-thinking "..."
  "What is shown in place of the prompt while the robot thinks.")

(defvar-local syringe--conversation nil
  "The conversation that this buffer shows.")

(defvar-local syringe--input-start nil
  "Marker at the start of the text being typed, just after the prompt.")

(defvar-keymap syringe-chat-mode-map
  "RET" #'syringe-chat-send)

(define-derived-mode syringe-chat-mode nil "Syringe-Chat"
  "Major mode for a syringe conversation.
Type after the prompt and press RET to send the message.  While the
robot thinks, the prompt shows ... and nothing can be typed."
  (setq-local word-wrap t)
  (setq-local truncate-lines nil)
  (add-hook 'kill-buffer-hook #'syringe--leave-windows nil t)
  (add-hook 'window-selection-change-functions #'syringe--chat-selected nil t))

(defun syringe--chat-selected (window)
  "Mark the conversation in WINDOW as read when WINDOW becomes selected."
  (when (eq window (selected-window))
    (let ((conversation (buffer-local-value 'syringe--conversation
                                            (window-buffer window))))
      (when conversation
        (syringe--mark-read conversation)))))

(defun syringe--show-prompt ()
  "Make the prompt and this buffer fit the state of the conversation.
While the robot thinks, the prompt shows ... and the buffer is
read-only, so that nothing can be typed.  The text of the prompt
stays as it is; a `display' property shows the dots in its place."
  (when (markerp syringe--input-start)
    (let ((thinking (eq (syringe-conversation-state syringe--conversation)
                        'thinking))
          (inhibit-read-only t))
      (setq buffer-read-only thinking)
      (put-text-property (- syringe--input-start (length syringe--prompt))
                         syringe--input-start
                         'display
                         (and thinking
                              (propertize syringe--prompt-thinking
                                          'face 'syringe-prompt))))))

(defun syringe--chat-header ()
  "Return the header line of a conversation buffer.
It shows the title and, while the robot thinks, what it is doing."
  (let ((conversation syringe--conversation))
    (when conversation
      ;; A % would be read as a mode line construct.
      (string-replace
       "%" "%%"
       (concat " " (syringe-conversation-title conversation)
               (pcase (syringe-conversation-state conversation)
                 ('thinking
                  (concat "  —  "
                          (or (syringe-conversation-activity conversation)
                              "thinking")))
                 ('error "  —  failed")))))))

(defun syringe--leave-windows ()
  "Take the conversation buffer that is being killed out of its windows.
Emacs would put some other buffer there, and the window would then
count as a code window.  Show another conversation instead, or close
the window if there is none."
  (let ((other (seq-find (lambda (buffer)
                           (and (buffer-live-p buffer)
                                (not (eq buffer (current-buffer)))))
                         (mapcar #'syringe-conversation-buffer
                                 syringe--conversations))))
    (dolist (win (get-buffer-window-list nil nil t))
      (if other
          (set-window-buffer win other)
        ;; The only window of a frame cannot be closed.
        (ignore-errors (delete-window win))))))

(defun syringe--render-message (role text)
  "Insert the message TEXT, written by ROLE, just above the prompt.
The color tells who wrote it.  In a message of the robot, references
to lines of code become links."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (- syringe--input-start (length syringe--prompt)))
      (let ((start (point)))
        (insert (propertize (concat text "\n")
                            'face (pcase role
                                    ('user 'syringe-user)
                                    ('robot 'syringe-robot)
                                    (_ 'syringe-error))
                            'read-only t
                            'front-sticky '(read-only)))
        (when (eq role 'robot)
          (syringe--add-links start (point)))))))

(defun syringe--add-links (start end)
  "Turn references such as src/main.c:42 between START and END into links.
RET on a link goes to that line in the live code."
  (when syringe--root
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\([[:alnum:]_./-]+\\):\\([0-9]+\\)" end t)
        (let ((file (expand-file-name (match-string 1) syringe--root))
              (line (string-to-number (match-string 2))))
          ;; Only what names a real file is a reference.
          (when (file-regular-p file)
            (make-text-button (match-beginning 0) (match-end 0)
                              'face '(button syringe-robot)
                              'help-echo "RET: go to this line"
                              'follow-link t
                              'action (lambda (_button)
                                        (syringe--visit file line)))))))))

(defun syringe--visit (file line)
  "Go to LINE of FILE in the code window."
  (let ((buffer (find-file-noselect file)))
    (syringe--show-code buffer
                        (with-current-buffer buffer
                          (syringe--line-start (1- line)))
                        t)))

(defun syringe-next-link ()
  "Move to the next link in the conversation.
A link is a reference to a line of code in an answer of the robot;
RET on it goes to that line.  After the last link comes the first."
  (interactive)
  (or (forward-button 1 t nil t)
      (user-error "This conversation has no links")))

(defun syringe-previous-link ()
  "Move to the previous link in the conversation.
Before the first link comes the last.  See `syringe-next-link'."
  (interactive)
  (or (forward-button -1 t nil t)
      (user-error "This conversation has no links")))

(defun syringe--say (conversation role text)
  "Add the message TEXT, written by ROLE, to CONVERSATION.
ROLE is `user', `robot' or `error'."
  (setf (syringe-conversation-messages conversation)
        (append (syringe-conversation-messages conversation)
                (list (cons role text))))
  (let ((buffer (syringe-conversation-buffer conversation)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (syringe--render-message role text))))
  (syringe--save))

(defun syringe-chat-send ()
  "Send the text typed after the prompt to the robot.
The answer arrives in the background."
  (interactive)
  (let ((conversation syringe--conversation)
        (text (string-trim (buffer-substring-no-properties
                            syringe--input-start (point-max)))))
    (when (eq (syringe-conversation-state conversation) 'thinking)
      (user-error "The robot is still thinking about your last message"))
    (when (string-empty-p text)
      (user-error "Type a message first"))
    (delete-region syringe--input-start (point-max))
    (syringe--say conversation 'user text)
    (goto-char (point-max))
    (syringe--ask conversation text)))

(defun syringe--chat-buffer (conversation)
  "Return the buffer that shows CONVERSATION, creating it if needed."
  (let ((buffer (syringe-conversation-buffer conversation))
        (title (syringe-conversation-title conversation)))
    (unless (buffer-live-p buffer)
      (setq buffer (generate-new-buffer (format "*syringe: %s*" title)))
      (setf (syringe-conversation-buffer conversation) buffer)
      (with-current-buffer buffer
        (syringe-chat-mode)
        ;; Line numbers make no sense in a chat.  This has to come after
        ;; the mode: `global-display-line-numbers-mode' turns them on then.
        (display-line-numbers-mode -1)
        (setq syringe--conversation conversation
              header-line-format '(:eval (syringe--chat-header)))
        ;; `rear-nonsticky' lets you type after the prompt even though
        ;; the prompt is read-only.  `field' makes C-a stop after it.
        (insert (propertize syringe--prompt
                            'face 'syringe-prompt
                            'field 'prompt
                            'read-only t
                            'front-sticky '(read-only)
                            'rear-nonsticky t))
        (setq syringe--input-start (point-marker))
        (pcase-dolist (`(,role . ,text)
                       (syringe-conversation-messages conversation))
          (syringe--render-message role text))
        (syringe--show-prompt)))
    buffer))

(defun syringe--open-chat (conversation)
  "Show CONVERSATION in the conversation window and go there."
  (select-window (syringe--show (syringe--chat-buffer conversation) 'chat))
  (goto-char (point-max))
  (syringe--mark-read conversation))

;;;; Talking to the robot

;; This part decides what to tell the robot and what to do with its
;; answers.  It reaches the robot only through the two functions of
;; the engine, at the end of this file.

(defun syringe--start-session ()
  "Start the robot session that reads the diff; see `syringe--session'."
  (let ((session (syringe-engine-new-session)))
    (setq syringe--session session
          syringe--session-state 'preparing
          syringe--session-error nil)
    (condition-case err
        (setq syringe--session-process
              (syringe-engine-send
               session
               (format syringe-brief
                       (syringe--change-description
                        (plist-get syringe--about :kind)))
               syringe--root
               #'ignore
               (lambda (ok text)
                 (when (equal session syringe--session)
                   (syringe--session-started ok text)))))
      (error (syringe--session-started nil (error-message-string err))))))

(defun syringe--session-started (ok text)
  "Note that the robot has finished reading the diff.
OK is non-nil if that worked; if not, TEXT says why.  Then the robot
is free for the first message that waited for it."
  (setq syringe--session-state (if ok 'ready 'failed)
        syringe--session-error (unless ok
                                 (format "The robot could not read the diff: %s"
                                         text)))
  (message "Syringe: %s"
           (if ok "the robot has read the diff" syringe--session-error))
  (force-mode-line-update t)
  (syringe--save)
  (syringe--ask-next))

(defun syringe--session-indicator ()
  "Return what the mode line of the diff says about the robot."
  (pcase syringe--session-state
    ('preparing " [robot: reading the diff]")
    ('ready " [robot: ready]")
    ('failed " [robot: failed]")))

(defun syringe--where (conversation)
  "Return where the line of CONVERSATION is now and how it reads.
The value is (PLACE . TEXT), with PLACE a string such as
\"src/main.c:42\", or nil if the line is lost."
  (let ((dot (syringe--live-dot (syringe-conversation-code-dot conversation))))
    (cond ((syringe-conversation-lost conversation) nil)
          ;; The dot knows best: it follows edits that are not saved yet.
          ((and dot (< (overlay-start dot) (overlay-end dot)))
           (with-current-buffer (overlay-buffer dot)
             (save-excursion
               (goto-char (overlay-start dot))
               (cons (if (syringe-conversation-code-file conversation)
                         (format "%s:%d"
                                 (syringe-conversation-code-file conversation)
                                 (line-number-at-pos nil t))
                       (syringe-conversation-title conversation))
                     (buffer-substring-no-properties (pos-bol) (pos-eol))))))
          ((syringe-conversation-code-file conversation)
           (cons (syringe-conversation-title conversation)
                 (syringe-conversation-code-text conversation)))
          (t
           (cons (syringe-conversation-title conversation)
                 (or (syringe-conversation-diff-text conversation) ""))))))

(defun syringe--prompt (conversation text where)
  "Return what to send to the robot for your message TEXT in CONVERSATION.
Before the message comes what the robot needs to place it.  In a
conversation that it has not answered in yet, that is which line the
conversation is about.  Later it is that the conversation is taken
up again, if the robot answered in another one in between, and a
sentence when the line has moved, changed or got lost since the
robot last heard of it.  WHERE is where the line is now; see
`syringe--where'."
  (let* ((told (syringe-conversation-told conversation))
         (title (syringe-conversation-title conversation))
         (about
          (if (not (assq 'robot (syringe-conversation-messages conversation)))
              (concat
               (if where
                   (format "This is a new conversation, about %s.  The line \
is:\n%s"
                           (car where) (cdr where))
                 (format "This is a new conversation, about %s, a line that \
is no longer in the code: it was deleted or rewritten."
                         title))
               (and (plist-get syringe--about :changed)
                    "\nThe code has been edited since you read the change; \
look at the files as they are now."))
            (string-join
             (delq nil
                   (list
                    (unless (eq conversation syringe--asked-last)
                      (format "Back to the conversation about %s."
                              (or (car told) title)))
                    (cond ((equal where told) nil)
                          ((null where)
                           "The line this conversation is about is no \
longer in the code: it was deleted or rewritten.")
                          ((equal (cdr where) (cdr told))
                           (format "The line this conversation is about has \
moved: it is now %s."
                                   (car where)))
                          (t
                           (format "The line this conversation is about has \
changed.  It is now %s and reads:\n%s"
                                   (car where) (cdr where))))))
             "  "))))
    (if (string-empty-p about)
        text
      (concat about "\n\nMy message:\n" text))))

(defun syringe--ask (conversation text)
  "Send your message TEXT in CONVERSATION to the robot.
The answer comes later, through `syringe--answer'.  The robot does
one thing at a time: while it reads the diff or works on another
message, this one waits for its turn."
  (let ((busy (cond ((eq syringe--session-state 'preparing)
                     "waiting for the robot to finish reading the diff")
                    (syringe--asking
                     (format "waiting for the robot to answer in %s"
                             (syringe-conversation-title syringe--asking))))))
    (if (not busy)
        (syringe--ask-now conversation text)
      (syringe--set-state conversation 'thinking busy)
      (setq syringe--waiting
            (append syringe--waiting (list (cons conversation text)))))))

(defun syringe--ask-next ()
  "Send the oldest message that waits for the robot, if the robot is free.
It is free when it is neither reading the diff nor working on the
message of a conversation that still exists."
  (unless (memq syringe--asking syringe--conversations)
    (setq syringe--asking nil))
  (unless (or syringe--asking
              (eq syringe--session-state 'preparing)
              (null syringe--waiting))
    (let ((next (pop syringe--waiting)))
      (syringe--ask-now (car next) (cdr next)))))

(defun syringe--ask-now (conversation text)
  "Run the robot on your message TEXT in CONVERSATION."
  (setq syringe--asking conversation)
  (syringe--set-state conversation 'thinking)
  (condition-case err
      (let ((where (syringe--where conversation)))
        (unless (eq syringe--session-state 'ready)
          (error "%s" (or syringe--session-error
                          "No review is running; start one with \
M-x syringe-start")))
        (setq syringe--session-process
              (syringe-engine-send
               syringe--session
               (syringe--prompt conversation text where)
               syringe--root
               (lambda (activity)
                 (when (eq (syringe-conversation-state conversation)
                           'thinking)
                   (syringe--set-state conversation 'thinking activity)))
               (lambda (ok answer)
                 (when ok
                   ;; The robot now knows where the line is, and this is
                   ;; the conversation it was in last.
                   (setf (syringe-conversation-told conversation) where)
                   (setq syringe--asked-last conversation))
                 (syringe--answer conversation ok answer)))))
    (error (syringe--answer conversation nil (error-message-string err)))))

(defun syringe--answer (conversation ok text)
  "Take the robot's reply to the last message in CONVERSATION.
OK is non-nil if the robot answered; TEXT is then its answer, and
otherwise the reason it failed.  After that the robot is free for
the next message that waits."
  (when (eq conversation syringe--asking)
    (setq syringe--asking nil))
  (when (memq conversation syringe--conversations)
    (syringe--say conversation (if ok 'robot 'error) text)
    (cond ((not ok)
           (syringe--set-state conversation 'error))
          ;; You are in the conversation right now.
          ((eq (window-buffer) (syringe-conversation-buffer conversation))
           (syringe--set-state conversation 'read))
          (t
           (syringe--set-state conversation 'unread)
           (message "Syringe: the robot answered in %s"
                    (syringe-conversation-title conversation))))
    (syringe--save))
  (syringe--ask-next))

;;;; The diff

(defun syringe--each-diff-line (function)
  "Call FUNCTION on the lines of the hunks of the diff, from the top.
The current buffer is the diff.  FUNCTION is called with point at the
start of a line and with three arguments: the file that the hunk is
about, the first character of the line, and the number of the line
in that file.  The character is a space for an unchanged line, `+'
for an added one and `-' for a removed one; a removed line is
numbered as in the old file, the others as in the new file.  Stop
when FUNCTION returns non-nil, and return that value."
  (save-excursion
    (goto-char (point-min))
    (let (file old new result)
      (while (not (or result (eobp)))
        (cond
         ;; The two lines that name the file of the hunks that follow.
         ((looking-at "--- \\(?:a/\\)?\\(.*\\)\n\\+\\+\\+ \\(?:b/\\)?\\(.*\\)$")
          ;; A deleted file has no new name.
          (setq file (if (equal (match-string 2) "/dev/null")
                         (match-string-no-properties 1)
                       (match-string-no-properties 2))
                old nil)
          (forward-line 1))
         ;; The header of a hunk gives the first line number on each side.
         ((looking-at "@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)")
          (setq old (string-to-number (match-string 1))
                new (string-to-number (match-string 2))))
         ;; A line of a hunk.
         ((and old (memq (char-after) '(?\s ?+ ?-)))
          (let ((kind (char-after)))
            (setq result (funcall function file kind
                                  (if (eq kind ?-) old new)))
            (unless (eq kind ?-)
              (setq new (1+ new)))
            (unless (eq kind ?+)
              (setq old (1+ old)))))
         ;; Anything else, but for the note about a missing last
         ;; newline, ends the hunk.
         ((and old (not (eq (char-after) ?\\)))
          (setq old nil)))
        (forward-line 1))
      result)))

(defun syringe--diff-line-info ()
  "Return (FILE LINE KIND) for the diff line at point, or nil outside a hunk.
KIND is the first character of the line: a space for an unchanged
line, `+' for an added one, `-' for a removed one.  LINE counts in
the new file, or in the old file for a removed line."
  (let ((target (pos-bol)))
    (syringe--each-diff-line
     (lambda (file kind number)
       (and (= (point) target)
            (list file number kind))))))

;; The next three functions find the lines of the conversations in the
;; diff and draw their dots there.

(defun syringe--diff-find (file text removed number)
  "Return the start of the line of the diff that shows line TEXT of FILE.
With REMOVED non-nil that is a removed line, and else a line of the
file as it is now.  Of several such lines, return the one whose line
number in the file is nearest to NUMBER.  Return nil if there is
none.  The current buffer is the diff."
  (let (found distance)
    (syringe--each-diff-line
     (lambda (this kind line)
       (when (and (equal this file)
                  (eq (eq kind ?-) (and removed t))
                  (equal text (buffer-substring-no-properties
                               (1+ (point)) (pos-eol))))
         (let ((far (abs (- line (or number line)))))
           (when (or (null distance) (< far distance))
             (setq found (point)
                   distance far))))
       ;; Go on to the end: a later line may be nearer.
       nil))
    found))

(defun syringe--diff-position (conversation)
  "Return the position in the diff of the line of CONVERSATION, or nil.
Return nil if there is no diff, or if the diff does not have the line."
  (let ((buffer (get-buffer syringe--diff-buffer))
        (text (syringe-conversation-diff-text conversation)))
    (when buffer
      (with-current-buffer buffer
        (cond ((syringe-conversation-code-file conversation)
               ;; A line that is lost in the code has no place here either.
               (unless (syringe-conversation-lost conversation)
                 (syringe--diff-find
                  (syringe-conversation-code-file conversation)
                  (syringe-conversation-code-text conversation)
                  nil
                  (syringe-conversation-code-line conversation))))
              ;; A line that is only in the diff.
              ((syringe-conversation-diff-file conversation)
               (syringe--diff-find
                (syringe-conversation-diff-file conversation)
                (substring text 1)
                (eq (aref text 0) ?-)
                (syringe-conversation-diff-number conversation)))
              ;; A line outside the hunks, such as the header of a file.
              (text
               (save-excursion
                 (goto-char (point-min))
                 (and (re-search-forward (concat "^" (regexp-quote text) "$")
                                         nil t)
                      (pos-bol)))))))))

(defun syringe--place-diff-dots ()
  "Draw the dots of the diff afresh.
Every conversation gets a dot on the line of the diff that shows its
line, if the diff has that line.  A conversation about a line that
is only in the diff is lost when the diff does not have the line."
  (when-let* ((buffer (get-buffer syringe--diff-buffer)))
    (with-current-buffer buffer
      (mapc #'delete-overlay (syringe--dots))
      (dolist (conversation syringe--conversations)
        (let ((position (syringe--diff-position conversation)))
          (setf (syringe-conversation-diff-dot conversation) nil)
          (when (and (syringe-conversation-diff-text conversation)
                     (not (syringe-conversation-code-file conversation)))
            (setf (syringe-conversation-lost conversation) (null position)))
          (when position
            (syringe--make-dot conversation buffer position))))
      (syringe--sync))))

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
        diff-mode-read-only t
        mode-line-process '(:eval (syringe--session-indicator)))
  ;; `g' takes the diff of the review again.
  (setq-local revert-buffer-function
              (lambda (&rest _) (syringe--refresh-by-hand))))

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

;; A review has a kind, which says what it reviews.  It is `unstaged'
;; for the changes that are not staged, or a revision, a string as it
;; was typed, for everything that has changed since that revision.  The
;; two functions below are all that depends on it.

(defun syringe--change-diff (root kind)
  "Return the diff that a review of KIND shows for the repository at ROOT.
It shows the code as it is now: the work that is not committed and
the files that git does not track are part of it.  A review of the
unstaged changes starts from what is staged, a review since a
revision from that revision.

A review since a revision has the messages of the commits that came
after it above the diff, as `git log' prints them."
  (concat (pcase kind
            ('unstaged "")
            (_ (let ((log (syringe--git root "--no-pager" "log" "--no-color"
                                        "--format=medium"
                                        (concat kind "..HEAD"))))
                 ;; No commit came after the one that is checked out.
                 (if (member log '(nil ""))
                     ""
                   (concat log "\n\n")))))
          (syringe--git-diff root (pcase kind
                                    ('unstaged nil)
                                    (_ (list kind))))
          (syringe--untracked-diff root)))

(defun syringe--change-description (kind)
  "Return a sentence that tells the robot how to see the change of KIND."
  (pcase kind
    ('unstaged "The change is my work that is not committed: what the \
command \"git diff\" prints, run from the root of the repository, together \
with the new files that git does not track yet; the command \"git ls-files \
--others --exclude-standard\" lists those.")
    (_ (format "The change is everything between the revision %s and the \
files as they are now, my work that is not committed included: what the \
command \"git diff %s\" prints, run from the root of the repository, \
together with the new files that git does not track yet; the command \"git \
ls-files --others --exclude-standard\" lists those.  The command \"git log \
%s..HEAD\" prints the messages of the commits that came after the revision; \
read them too."
               kind kind kind))))

(defun syringe--current-p ()
  "Return non-nil if the open review is of the commit that is checked out."
  (and syringe--file
       (equal (plist-get syringe--about :commit)
              (syringe--git syringe--root "rev-parse" "HEAD"))))

(defun syringe--refresh-diff ()
  "Take the diff of the open review again and show it.
The conversations stay: their dots are drawn again in the new diff.
Do nothing if the diff has not changed."
  (when (and syringe--file (get-buffer syringe--diff-buffer))
    (let ((diff (syringe--change-diff syringe--root
                                      (plist-get syringe--about :kind))))
      (with-current-buffer syringe--diff-buffer
        (unless (equal diff (save-restriction
                              (widen)
                              (buffer-substring-no-properties
                               (point-min) (point-max))))
          ;; Keep the buffer and every window that shows it on the line
          ;; they were on.
          (let ((line (line-number-at-pos nil t))
                (places
                 (mapcar (lambda (win)
                           (list win
                                 (line-number-at-pos (window-point win) t)
                                 (line-number-at-pos (window-start win) t)))
                         (get-buffer-window-list nil nil t))))
            (syringe--fill-diff-buffer diff syringe--root)
            (goto-char (syringe--line-start (1- line)))
            (pcase-dolist (`(,win ,point ,start) places)
              (set-window-point win (syringe--line-start (1- point)))
              (set-window-start win (syringe--line-start (1- start)))))
          (syringe--place-diff-dots)
          ;; From now on a new conversation tells the robot that the
          ;; code is not what it read.
          (setq syringe--about (plist-put syringe--about :changed t))
          (syringe--write-text (syringe--diff-file syringe--file) diff)
          (syringe--save))))))

(defun syringe--refresh-if-current ()
  "Take the diff again if the review is of the commit that is checked out.
After a `git checkout' of another commit the review keeps its diff:
that commit has a review of its own."
  (with-demoted-errors "Syringe: %S"
    (when (syringe--current-p)
      (syringe--refresh-diff))))

(defun syringe--refresh-soon ()
  "Take the diff again once Emacs is idle.
Several files that change at once then cost one diff."
  (when (timerp syringe--refresh-timer)
    (cancel-timer syringe--refresh-timer))
  (setq syringe--refresh-timer
        (run-with-idle-timer syringe--refresh-delay nil
                             #'syringe--refresh-if-current)))

(defun syringe--refresh-by-hand ()
  "Take the diff of the open review again, because you asked for it.
If another commit is checked out than the one the review belongs to,
offer to carry the review over to that commit."
  (unless syringe--file
    (user-error "No review is open"))
  (if (syringe--current-p)
      (syringe--refresh-diff)
    (unless (y-or-n-p "This review is of another commit.  Carry it over to \
the commit that is checked out? ")
      (user-error "The review stays with its commit"))
    (let ((before syringe--about))
      (setq syringe--about
            (plist-put (plist-put (copy-sequence before)
                                  :commit (syringe--git syringe--root
                                                        "rev-parse" "HEAD"))
                       :head (syringe--git syringe--root
                                           "log" "-1" "--format=%h %s")))
      ;; If the diff cannot be taken, the review stays where it was.
      (condition-case err
          (syringe--refresh-diff)
        (error (setq syringe--about before)
               (signal (car err) (cdr err))))))
  (syringe--save))

(defun syringe--start (directory kind)
  "Start a review of KIND in the repository that contains DIRECTORY.
KIND is `unstaged' or a revision; see `syringe--change-diff'.  Show
the diff in the selected window, or in the window that already shows
a diff.  The code and conversation windows appear when they are first
needed.

A review belongs to the commit that is checked out.  If the open
review is this one already, only take its diff again.  If a review
of this kind and commit is saved, open it, with its conversations.
Else start a new one and have the robot read the diff.  The review
that was open is saved and closed.

The review is saved as you go, and its diff follows the code: see
`syringe--refresh-diff'."
  (let* ((root (syringe--repository directory))
         (commit (syringe--git root "rev-parse" "HEAD"))
         (this (lambda (about)
                 (and (equal kind (plist-get about :kind))
                      (equal commit (plist-get about :commit)))))
         (saved nil))
    (cond ((and syringe--file
                (equal root syringe--root)
                (funcall this syringe--about)
                (get-buffer syringe--diff-buffer))
           (syringe--refresh-diff))
          ((setq saved
                 (seq-find (lambda (file)
                             (funcall this (plist-get
                                            (ignore-errors
                                              (read (syringe--read-text file)))
                                            :about)))
                           (syringe--review-files root)))
           (syringe--open-review saved root)
           (syringe--refresh-diff)
           (message "Syringe: opened the saved review of this commit"))
          (t
           (let ((diff (syringe--change-diff root kind)))
             (when (string-empty-p diff)
               (user-error
                "Nothing to review: %s"
                (pcase kind
                  ('unstaged "no unstaged changes and no new files")
                  (_ (format "nothing has changed since %s" kind)))))
             (syringe--new-review root diff kind commit))))
    (syringe--show-diff-buffer)))

;;;###autoload
(defun syringe-start (directory &optional revision)
  "Start a review of the changes since REVISION in the repository of DIRECTORY.
The diff shows everything between REVISION and the files as they are
now: the commits that came after it, and on top of them your work
that is not committed, with the new files that git does not track.
It is what `git diff REVISION' prints, and it is taken again whenever
a file changes.  REVISION can be anything that names a commit, and
the commit need not be an ancestor of the one that is checked out.
HEAD~1 reviews the last commit; to review the commits of a branch
one by one, check out each in turn and run this.

With REVISION nil or empty, review the unstaged changes: what `git
diff' prints without a revision, and the new files.  What you have
staged is then left out.

Interactively, ask for the revision.  RET alone takes none, and M-n
brings up HEAD~1.

See `syringe--start' for what starting a review does."
  (interactive
   (list (syringe--read-directory)
         (read-from-minibuffer
          "Review the changes since revision (none: the unstaged changes): "
          nil nil nil 'syringe--revision-history "HEAD~1")))
  (let ((revision (string-trim (or revision ""))))
    (cond ((string-empty-p revision)
           (syringe--start directory 'unstaged))
          ((syringe--git (syringe--repository directory)
                         "rev-parse" "--verify" "--quiet"
                         (concat revision "^{commit}"))
           (syringe--start directory revision))
          (t
           (user-error "No commit is called %s" revision)))))

;;;; Saved reviews

;; A review is two files in the directory syringe inside the
;; repository's .git: NAME.eld holds the conversations as Lisp data,
;; and NAME.diff the diff as it was last shown.  NAME is the time the
;; review started.  The files are written after every change.

(defun syringe--directory (root)
  "Return the directory with the saved reviews of the repository at ROOT."
  (expand-file-name "syringe"
                    (or (syringe--git root "rev-parse" "--absolute-git-dir")
                        (user-error "No git repository at %s" root))))

(defun syringe--review-files (root)
  "Return the files of the saved reviews of the repository at ROOT.
The newest comes first."
  (let ((directory (syringe--directory root)))
    (and (file-directory-p directory)
         ;; The name of a file is the time its review started.
         (sort (directory-files directory t "\\.eld\\'")
               (lambda (a b)
                 (string> (file-name-sans-extension a)
                          (file-name-sans-extension b)))))))

(defun syringe--diff-file (file)
  "Return the file with the diff of the review saved in FILE."
  (concat (file-name-sans-extension file) ".diff"))

;; `utf-8-emacs' writes any text that a buffer can hold, also the bytes
;; of a file in the diff that is not valid UTF-8, and reads it back the same.

(defun syringe--read-text (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-emacs))
      (insert-file-contents file))
    (buffer-string)))

(defun syringe--write-text (file text)
  "Write the string TEXT to FILE."
  (let ((coding-system-for-write 'utf-8-emacs))
    (with-temp-file file
      (insert text))))

(defconst syringe--saved-slots
  '(title
    state
    told
    lost
    code-file
    code-line
    code-text
    code-above
    code-below
    diff-file
    diff-text
    diff-number
    messages)
  "The slots of a conversation that are saved, in the order of the file.")

(defun syringe--conversation-data (conversation)
  "Return what is saved of CONVERSATION, as a property list."
  (cl-loop for slot in syringe--saved-slots
           collect (intern (format ":%s" slot))
           collect (cl-struct-slot-value 'syringe-conversation slot
                                         conversation)))

(defun syringe--conversation-from-data (data)
  "Return the conversation that the property list DATA describes."
  (let ((conversation (syringe--conversation-create)))
    (dolist (slot syringe--saved-slots)
      (setf (cl-struct-slot-value 'syringe-conversation slot conversation)
            (plist-get data (intern (format ":%s" slot)))))
    ;; The review was closed while the robot was thinking.
    (when (eq (syringe-conversation-state conversation) 'thinking)
      (setf (syringe-conversation-state conversation) 'error
            (syringe-conversation-messages conversation)
            (append (syringe-conversation-messages conversation)
                    (list (cons 'error "The robot was stopped before it \
answered.  Send your message again.")))))
    conversation))

(defun syringe--save ()
  "Save the current review, if there is one, to its file."
  (when syringe--file
    (let ((data (list :about syringe--about
                      :session syringe--session
                      :session-ready (eq syringe--session-state 'ready)
                      :conversations (mapcar #'syringe--conversation-data
                                             syringe--conversations)))
          (new (concat syringe--file ".new"))
          (print-length nil)
          (print-level nil)
          (indent-tabs-mode nil))
      ;; Write a new file and put it in place of the old one, so that a
      ;; crash in the middle leaves the old one whole.
      (syringe--write-text new (pp-to-string data))
      (rename-file new syringe--file t))))

(add-hook 'kill-emacs-hook #'syringe--save)

(defun syringe--close-review ()
  "Save the current review, if there is one, and take it off the screen."
  (syringe--save)
  (let ((conversations syringe--conversations)
        (process syringe--session-process))
    ;; Forget the review first, so that what its robot still reports
    ;; when it is stopped is ignored.
    (setq syringe--conversations nil
          syringe--file nil
          syringe--about nil
          syringe--session nil
          syringe--session-state nil
          syringe--session-process nil
          syringe--session-error nil
          syringe--asking nil
          syringe--asked-last nil
          syringe--waiting nil)
    (when (process-live-p process)
      (delete-process process))
    (mapc #'syringe--drop conversations)))

(defun syringe--new-review (root diff kind commit)
  "Start a review of DIFF, a diff of the repository at ROOT.
KIND is the kind of the review; see `syringe--change-diff'.  COMMIT is
the commit that is checked out, which the review belongs to."
  (syringe--close-review)
  (let* ((directory (syringe--directory root))
         (name (format-time-string "%Y%m%d-%H%M%S"))
         (file (expand-file-name (concat name ".eld") directory))
         (count 1))
    ;; Two reviews can start in the same second; do not write over one.
    (while (file-exists-p file)
      (setq file (expand-file-name (format "%s-%d.eld" name (cl-incf count))
                                   directory)))
    (make-directory directory t)
    (syringe--write-text (syringe--diff-file file) diff)
    (setq syringe--file file
          syringe--root root
          syringe--about
          (list :created (format-time-string "%F %R")
                :kind kind
                :commit commit
                :head (syringe--git root "log" "-1" "--format=%h %s")))
    (syringe--fill-diff-buffer diff root)
    (syringe--start-session)
    (syringe--save)))

(defun syringe--open-review (file root)
  "Make the review saved in FILE the current one.
ROOT is the root of its repository.  The diff is shown as it was
saved; the dots are put where the lines of the conversations are now."
  ;; Read first: if that fails, the current review stays open.
  (let ((data (read (syringe--read-text file)))
        (diff (syringe--read-text (syringe--diff-file file))))
    (syringe--close-review)
    (setq syringe--file file
          syringe--root root
          syringe--about (plist-get data :about)
          syringe--session (plist-get data :session)
          syringe--session-state (and (plist-get data :session-ready) 'ready)
          syringe--conversations (mapcar #'syringe--conversation-from-data
                                         (plist-get data :conversations)))
    (syringe--fill-diff-buffer diff root)
    (syringe--place-diff-dots)
    ;; The dots in the code come back now in the files that are open,
    ;; and in the others when they are visited.
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (syringe--in-review-p)
          (syringe--settle-buffer t))))
    ;; The robot had not finished reading the diff when the review closed.
    (unless (eq syringe--session-state 'ready)
      (syringe--start-session))))

(defun syringe--review-label (file)
  "Return the line that shows the review saved in FILE in a list.
That is when it started, what it reviews, the commit that was
checked out and the number of conversations."
  (let* ((data (read (syringe--read-text file)))
         (about (plist-get data :about)))
    (format "%s  %-18s  %s  %d conversations"
            (plist-get about :created)
            (pcase (plist-get about :kind)
              ('unstaged "unstaged")
              (revision (truncate-string-to-width
                         (format "since %s" revision) 18 nil nil t)))
            (truncate-string-to-width (or (plist-get about :head) "")
                                      50 nil nil t)
            (length (plist-get data :conversations)))))

;;;###autoload
(defun syringe-restore (directory)
  "Open a saved review of the git repository that contains DIRECTORY.
Choose it from a list, the newest first.  A line shows when the
review started, what it reviews, the commit that was checked out and
the number of conversations.  The review that was open is saved and
closed.

The review shows the diff as it was saved.  If the commit it belongs
to is checked out, the diff is then taken again; if not, it stays,
and `g' in the diff offers to carry the review over to the commit
that is checked out.  The dots go to where their lines are now."
  (interactive (list default-directory))
  (let* ((root (syringe--repository directory))
         (file (syringe--choose
                "Review: "
                (or (syringe--review-files root)
                    (user-error "This repository has no saved reviews"))
                #'syringe--review-label)))
    (when file
      (syringe--open-review file root)
      (syringe--show-diff-buffer)
      (syringe--refresh-soon))))

;;;; The engine: GitHub Copilot CLI

;; Everything above reaches the robot through two functions only:
;;
;;   (syringe-engine-new-session)
;;       Return a new session.  A session is a string that names one
;;       conversation with the robot.
;;   (syringe-engine-send SESSION PROMPT DIRECTORY ON-ACTIVITY ON-DONE)
;;       Send PROMPT in SESSION, with the robot working in DIRECTORY.
;;       Return at once with the process.  While the robot works, call
;;       ON-ACTIVITY with a few words about what it is doing.  At the
;;       end call ON-DONE with t and the answer, or with nil and the
;;       reason for the failure.
;;
;; To use another coding agent, rewrite these two functions.

(defcustom syringe-copilot-program "copilot"
  "The GitHub Copilot CLI program."
  :type 'string)

(defcustom syringe-copilot-args
  '("--no-ask-user"
    "--deny-tool=write"
    "--allow-tool=shell(git diff)" "--allow-tool=shell(git log)"
    "--allow-tool=shell(git show)" "--allow-tool=shell(git blame)"
    "--allow-tool=shell(git status)" "--allow-tool=shell(git grep)"
    "--allow-tool=shell(git ls-files)" "--allow-tool=shell(rg)"
    "--allow-tool=shell(grep)" "--allow-tool=shell(ls)"
    "--allow-tool=shell(cat)" "--allow-tool=shell(head)"
    "--allow-tool=shell(tail)" "--allow-tool=shell(wc)"
    "--model" "gpt-6.2-astra")
  "Arguments for every run of Copilot.
The default keeps the robot read-only: it may not write files, and
the only commands it may run without asking are the ones listed.
Copilot cannot ask for permission here, so it refuses the rest.
To choose the model, add \"--model\" and its name."
  :type '(repeat string))

(defun syringe-engine-new-session ()
  "Return a new session: a random UUID, which Copilot wants as the name."
  (format "%08x-%04x-4%03x-%04x-%06x%06x"
          (random #x100000000) (random #x10000) (random #x1000)
          (+ #x8000 (random #x4000))
          (random #x1000000) (random #x1000000)))

(defun syringe-copilot--activity (data directory)
  "Describe the tool call DATA of a Copilot event in a few words.
File names are shown relative to DIRECTORY."
  (or (ignore-errors
        (let-alist data
          (let ((what (or .arguments.path .arguments.command
                          .arguments.pattern .arguments.query "")))
            (when (file-name-absolute-p what)
              (setq what (file-relative-name what directory)))
            (truncate-string-to-width
             (string-replace "\n" " "
                             (string-trim (format "%s %s"
                                                  (or .toolTitle .toolName)
                                                  what)))
             70 nil nil t))))
      "working"))

(defun syringe-engine-send (session prompt directory on-activity on-done)
  "Send PROMPT to Copilot in SESSION, working in DIRECTORY.
Return the process.  Call ON-ACTIVITY with a string whenever Copilot
starts to use a tool.  When it ends, call ON-DONE with t and its
answer, or with nil and the reason it failed."
  (unless (executable-find syringe-copilot-program)
    (error "Cannot find the program `%s'" syringe-copilot-program))
  (let* ((default-directory (file-name-as-directory directory))
         (answer nil)                   ; the last text Copilot wrote
         (errors "")                    ; output that is not JSON
         (rest "")                      ; an incomplete last line
         (process
          (make-process
           :name "syringe-copilot"
           :command (append (list syringe-copilot-program
                                  "--session-id" session
                                  "--output-format" "json")
                            syringe-copilot-args
                            (list "-p" prompt))
           :connection-type 'pipe
           :coding 'utf-8
           :noquery t
           :filter
           (lambda (_process chunk)
             ;; Copilot prints one JSON object per line, and a chunk
             ;; can end in the middle of a line.
             (let ((lines (split-string (concat rest chunk) "\n")))
               (setq rest (car (last lines)))
               (dolist (line (butlast lines))
                 (let ((event (ignore-errors
                                (json-parse-string line :object-type 'alist
                                                   :null-object nil
                                                   :false-object nil))))
                   (if (not (consp event))
                       ;; Copilot prints its error messages as plain text.
                       (setq errors (concat errors line "\n"))
                     (let-alist event
                       (pcase .type
                         ("tool.execution_start"
                          (funcall on-activity
                                   (syringe-copilot--activity .data directory)))
                         ;; Copilot writes several messages while it
                         ;; works; the last one with text is the answer.
                         ("assistant.message"
                          (when (and (stringp .data.content)
                                     (not (string-blank-p .data.content)))
                            (setq answer .data.content))))))))))
           :sentinel
           (lambda (process _event)
             (unless (process-live-p process)
               (if (and answer (eq (process-exit-status process) 0))
                   (funcall on-done t (string-trim answer))
                 (let ((why (string-trim (concat errors rest))))
                   (funcall on-done nil (if (string-empty-p why)
                                            "Copilot gave no answer"
                                          why)))))))))
    ;; Copilot gets the prompt as an argument; do not let it wait for input.
    (process-send-eof process)
    process))

(provide 'syringe)
;;; syringe.el ends here
