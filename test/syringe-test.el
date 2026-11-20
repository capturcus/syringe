;;; syringe-test.el --- Tests for syringe  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run the tests with `make check'.
;;
;; A test that needs a repository makes a scratch one and removes it
;; again.  There the robot is a fake that records what it is sent and
;; answers when the test tells it to: no test runs Copilot.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ert-x)
(require 'syringe)

;;;; Finding a line again

(defconst syringe-test-file "\
use std::fmt;

fn alpha(x: i32) -> i32 {
    let doubled = x * 2;
    doubled + 1
}

fn beta(name: &str) -> String {
    let greeting = format!(\"hello {}\", name);
    greeting
}

fn gamma() {
    println!(\"gamma\");
}
"
  "The file whose lines the tests of `syringe--find-code-line' remember.")

(defun syringe-test-edit (from to)
  "Return `syringe-test-file' with the text FROM replaced by TO."
  (string-replace from to syringe-test-file))

(defun syringe-test-find (line new-text &optional old-text)
  "Remember LINE of `syringe-test-file' and look for it in NEW-TEXT.
Return the number of the line it is found on, or nil if it is lost.
Lines are counted from 1.  With OLD-TEXT, remember the line of that
text instead."
  (let ((syringe--root "/repository/")
        (buffer-file-name "/repository/file.rs")
        (conversation (syringe--conversation-create))
        (lines (lambda (text) (vconcat (split-string text "\n")))))
    (syringe--remember-code-line conversation
                                 (funcall lines
                                          (or old-text syringe-test-file))
                                 (1- line))
    (when-let* ((index (syringe--find-code-line conversation
                                                (funcall lines new-text))))
      (1+ index))))

(ert-deftest syringe-test-find-edited-line ()
  "A line is found again after an edit, or is lost when it is gone."
  (pcase-dolist
      (`(,what ,line ,new-text ,expected)
       `(("nothing changed" 4 ,syringe-test-file 4)
         ("the line reworded between its neighbours"
          4
          ,(syringe-test-edit "let doubled = x * 2;"
                              "let doubled = x.checked_mul(2).unwrap();")
          4)
         ("the line reworded and a line added above"
          9
          ,(concat "// new\n"
                   (syringe-test-edit "format!(\"hello {}\", name)"
                                      "format!(\"hi {name}\")"))
          10)
         ("the line killed"
          4
          ,(syringe-test-edit "    let doubled = x * 2;\n" "")
          nil)
         ("the line moved into another function"
          14
          ,(string-replace
            "fn gamma() {\n    println!(\"gamma\");\n"
            "fn gamma() {\n"
            (syringe-test-edit "    greeting\n}"
                               "    println!(\"gamma\");\n    greeting\n}"))
          10)
         ("a closing brace whose function was rewritten"
          11
          ,(syringe-test-edit
            "    let greeting = format!(\"hello {}\", name);\n    greeting\n"
            "    name.to_uppercase()\n")
          10)
         ("the line and its neighbours indented, a neighbour reworded"
          9
          ,(syringe-test-edit
            "    let greeting = format!(\"hello {}\", name);\n    greeting"
            "        let greeting = format!(\"hello {}\", name);\n\
        greeting.clone()")
          9)
         ("the whole file replaced"
          9
          "struct Other;\n\nimpl Other {\n    fn new() -> Self { Other }\n}\n"
          nil)
         ("the same line twice: the one with its neighbours"
          14
          ,(concat syringe-test-file
                   "\nfn delta() {\n    println!(\"gamma\");\n}\n")
          14)
         ("a variable renamed in the line and in a neighbour"
          4
          ,(syringe-test-edit "    let doubled = x * 2;\n    doubled + 1"
                              "    let twice = x * 2;\n    twice + 1")
          4)))
    (ert-info (what)
      (should (equal (syringe-test-find line new-text) expected)))))

(ert-deftest syringe-test-find-shifted-line ()
  "Every line is found again when lines are added above it.
That includes the blank lines and the closing braces, which only
their neighbours tell apart."
  (let ((count (1- (length (split-string syringe-test-file "\n")))))
    (dotimes (added 5)
      (let ((new-text (concat (apply #'concat (make-list added "// new\n"))
                              syringe-test-file)))
        (dotimes (index count)
          (ert-info ((format "line %d, %d added above" (1+ index) added))
            (should (equal (syringe-test-find (1+ index) new-text)
                           (+ 1 index added)))))))))

(ert-deftest syringe-test-find-look-alike ()
  "A line that only looks like the remembered one must stand out to be it."
  (let ((old-text "\
fn sum() -> i32 {
    let a = load(1);
    let b = load(2);
    let c2 = load(33);
    a + b + c2
}
"))
    ;; The line was killed.  Its neighbours look like it, one more than
    ;; the other, but not by much.
    (should-not (syringe-test-find
                 3
                 (string-replace "    let b = load(2);\n" "" old-text)
                 old-text))
    ;; The line and all around it was reworded.  One line still looks
    ;; like it, and no other comes near.
    (should (equal (syringe-test-find 3 "\
fn total() -> i64 {
    let x = fetch();
    let b = load(2)?;
    let y = fetch();
    x + b + y
}
"
                                      old-text)
                   3))))

(ert-deftest syringe-test-likeness ()
  "The likeness of two lines is 1 for the same text and less otherwise."
  (should (= (syringe--likeness "let x = 1;" "let x = 1;") 1.0))
  ;; The space at the ends does not count.
  (should (= (syringe--likeness "    let x = 1;" "let x = 1;  ") 1.0))
  (should (= (syringe--likeness "" "   ") 0.0))
  (dolist (pair '(("let x = 1;" . "let x = 2;")
                  ("let x = 1;" . "fn main() {")
                  ("a" . "")))
    (let ((likeness (syringe--likeness (car pair) (cdr pair))))
      (should (<= 0.0 likeness))
      (should (< likeness 1.0))
      ;; It does not matter which line is the old one.
      (should (= likeness (syringe--likeness (cdr pair) (car pair))))))
  (should (> (syringe--likeness "let x = 1;" "let x = 2;")
             (syringe--likeness "let x = 1;" "fn main() {"))))

;;;; A scratch repository and a fake robot

(defvar syringe-test-root nil
  "The root of the scratch repository of the running test.")

(defvar syringe-test-sent nil
  "What the fake robot was sent and has not answered yet, oldest first.
Each element is (SESSION PROMPT ON-ACTIVITY ON-DONE), the arguments
of `syringe-engine-send' without the directory.")

(defvar syringe-test-yes t
  "What the fake `y-or-n-p' answers.")

(defvar syringe-test-asked nil
  "The questions that the fake `y-or-n-p' was asked, newest first.")

(defun syringe-test-git (&rest args)
  "Run git with ARGS in the scratch repository and return its output."
  (let ((default-directory syringe-test-root))
    (with-temp-buffer
      (unless (eq 0 (apply #'call-process "git" nil t nil args))
        (error "Git %s failed: %s" (string-join args " ") (buffer-string)))
      (buffer-string))))

(defun syringe-test-path (file)
  "Return the full name of FILE of the scratch repository."
  (expand-file-name file syringe-test-root))

(defun syringe-test-write (file text &optional append)
  "Write TEXT to FILE of the scratch repository, behind the back of Emacs.
With APPEND non-nil, add TEXT to the end of the file."
  (make-directory (file-name-directory (syringe-test-path file)) t)
  (write-region text nil (syringe-test-path file) append 'silent))

(defun syringe-test-line (number &optional says)
  "Return line NUMBER of src/main.rs as it is in the first commit.
With SAYS, return the line with that text in place of its own."
  (format "fn function_%d() { println!(\"%s\"); }"
          number (or says (format "line %d" number))))

(defun syringe-test-make-repository ()
  "Fill the scratch repository with two commits.
The first has src/main.rs with 60 lines, see `syringe-test-line'.
The second, which the reviews are of, rewords line 10 and removes
line 40 of it, adds a line to notes.txt and deletes src/gone.rs."
  (let ((lines (mapcar #'syringe-test-line (number-sequence 1 60))))
    (syringe-test-git "init" "-q")
    (syringe-test-write ".gitignore" "*.log\n")
    (syringe-test-write "notes.txt" "first note\nsecond note\n")
    (syringe-test-write "src/gone.rs" "// not needed any more\n")
    (syringe-test-write "src/main.rs" (concat (string-join lines "\n") "\n"))
    (syringe-test-git "add" ".")
    (syringe-test-git "commit" "-q" "-m" "one")
    (setf (nth 9 lines) (syringe-test-line 10 "CHANGED ten"))
    (syringe-test-write "src/main.rs"
                        (concat (string-join (append (seq-take lines 39)
                                                     (nthcdr 40 lines))
                                             "\n")
                                "\n"))
    (syringe-test-write "notes.txt" "third note\n" t)
    (delete-file (syringe-test-path "src/gone.rs"))
    (syringe-test-git "add" "--all")
    (syringe-test-git "commit" "-q" "-m" "two\n\nWhy the second commit.")))

(defun syringe-test-clean-up ()
  "Close the review of the running test and remove its repository."
  (syringe--close-review)
  (when (timerp syringe--refresh-timer)
    (cancel-timer syringe--refresh-timer))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (or (equal (buffer-name) syringe--diff-buffer)
                (and buffer-file-name
                     (file-in-directory-p buffer-file-name
                                          syringe-test-root)))
        (set-buffer-modified-p nil)
        (kill-buffer))))
  (delete-directory syringe-test-root t))

(defun syringe-test-call-with-repository (function)
  "Call FUNCTION in a scratch repository, with a fake robot.
Git sees no configuration but that of the repository, and every
question of `y-or-n-p' is answered with `syringe-test-yes'."
  (let* ((syringe-test-root
          (file-name-as-directory
           (file-truename (make-temp-file "syringe-test" t))))
         (syringe-test-sent nil)
         (syringe-test-yes t)
         (syringe-test-asked nil)
         (sessions 0)
         (process-environment
          (append '("GIT_CONFIG_GLOBAL=/dev/null"
                    "GIT_CONFIG_NOSYSTEM=1"
                    "GIT_AUTHOR_NAME=Tester"
                    "GIT_AUTHOR_EMAIL=tester@example.com"
                    "GIT_COMMITTER_NAME=Tester"
                    "GIT_COMMITTER_EMAIL=tester@example.com")
                  process-environment))
         ;; Should a test get past the fake robot, it must not find Copilot.
         (syringe-copilot-program "syringe-test-no-such-program")
         (inhibit-message t))
    (cl-letf (((symbol-function 'syringe-engine-new-session)
               (lambda () (format "session-%d" (cl-incf sessions))))
              ((symbol-function 'syringe-engine-fork)
               (lambda (session)
                 (format "%s/fork-%d" session (cl-incf sessions))))
              ((symbol-function 'syringe-engine-send)
               (lambda (session prompt _directory on-activity on-done)
                 (setq syringe-test-sent
                       (append syringe-test-sent
                               (list (list session prompt
                                           on-activity on-done))))
                 nil))
              ((symbol-function 'y-or-n-p)
               (lambda (question)
                 (push question syringe-test-asked)
                 syringe-test-yes)))
      (unwind-protect
          (save-window-excursion
            (syringe-test-make-repository)
            (funcall function))
        (syringe-test-clean-up)))))

(defmacro syringe-test-with-repository (&rest body)
  "Run BODY in a scratch repository, with a fake robot.
See `syringe-test-call-with-repository'."
  (declare (indent 0) (debug t))
  `(syringe-test-call-with-repository (lambda () ,@body)))

(defun syringe-test-answer (text &optional failed)
  "Have the fake robot answer the oldest message it was sent with TEXT.
With FAILED non-nil the robot fails instead, and TEXT says why."
  (let ((message (pop syringe-test-sent)))
    (funcall (nth 3 message) (not failed) text)))

(defun syringe-test-start ()
  "Start a review of the last commit and let the robot read the change."
  (syringe-start-last-commit syringe-test-root)
  (syringe-test-answer "ready"))

(defun syringe-test-diff ()
  "Return the text of the diff buffer."
  (with-current-buffer syringe--diff-buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun syringe-test-in-diff (text)
  "Go to the diff window and there to the first line that has TEXT."
  (select-window (syringe--window 'diff))
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line))

;;;; Starting a review

(ert-deftest syringe-test-last-commit ()
  "A review of the last commit shows its message, its changes and new work."
  (syringe-test-with-repository
    (syringe-test-write "notes.txt" "not committed\n" t)
    (syringe-test-write "fresh.txt" "a new file\n")
    (syringe-test-write "build.log" "a file that git ignores\n")
    (syringe-start-last-commit syringe-test-root)
    (should (equal (buffer-name) syringe--diff-buffer))
    (should (derived-mode-p 'syringe-diff-mode))
    (should buffer-read-only)
    (let ((diff (syringe-test-diff)))
      ;; The message of the commit comes before the first file.
      (should (< (string-search "Why the second commit." diff)
                 (string-search "diff --git" diff)))
      (dolist (line (list (concat "+" (syringe-test-line 10 "CHANGED ten"))
                          (concat "-" (syringe-test-line 10))
                          (concat "-" (syringe-test-line 40))
                          "+third note"
                          "+not committed"
                          "+a new file"
                          "--- a/src/gone.rs"))
        (ert-info (line)
          (should (string-search (concat "\n" line "\n") diff))))
      (should-not (string-search "build.log" diff)))))

(ert-deftest syringe-test-unstaged ()
  "A review of the unstaged changes shows them and the new files only."
  (syringe-test-with-repository
    ;; A clean tree has nothing to review.
    (should-error (syringe-start-unstaged syringe-test-root)
                  :type 'user-error)
    (syringe-test-write "notes.txt" "staged\n" t)
    (syringe-test-git "add" "notes.txt")
    (syringe-test-write "src/main.rs" "// not staged\n" t)
    (syringe-test-write "fresh.txt" "a new file\n")
    (syringe-start-unstaged syringe-test-root)
    (let ((diff (syringe-test-diff)))
      (should (string-prefix-p "diff --git" diff))
      (should (string-search "\n+// not staged\n" diff))
      (should (string-search "\n+a new file\n" diff))
      (should-not (string-search "+staged" diff))
      (should-not (string-search "CHANGED ten" diff)))))

(ert-deftest syringe-test-visit-code ()
  "RET in the diff shows the line in its file and goes there."
  (syringe-test-with-repository
    (syringe-test-start)
    (syringe-test-in-diff "CHANGED ten")
    (syringe-diff-visit)
    (should (file-equal-p buffer-file-name (syringe-test-path "src/main.rs")))
    (should (= (line-number-at-pos) 10))
    (should (eq (syringe--window-role (selected-window)) 'code))
    ;; With the option off the cursor stays in the diff.
    (let ((syringe-diff-visit-selects-code nil))
      (syringe-test-in-diff "+third note")
      (syringe-diff-visit)
      (should (equal (buffer-name) syringe--diff-buffer))
      (with-selected-window (syringe--window 'code)
        (should (file-equal-p buffer-file-name
                              (syringe-test-path "notes.txt")))
        (should (= (line-number-at-pos) 3))))))

;;;; Dots

(defun syringe-test-in-code (file line)
  "Show FILE of the scratch repository in the code window; go to its LINE."
  (select-window (syringe--show (find-file-noselect (syringe-test-path file))
                                'code))
  (goto-char (syringe--line-start (1- line))))

(defun syringe-test-main ()
  "Return the buffer that visits src/main.rs of the scratch repository."
  (find-buffer-visiting (syringe-test-path "src/main.rs")))

(defun syringe-test-dot (line)
  "Put a dot on LINE of src/main.rs and return its conversation.
The conversation is open afterwards, as the command leaves it."
  (syringe-test-in-code "src/main.rs" line)
  (syringe-toggle-dot)
  (car syringe--conversations))

(defun syringe-test-dots (buffer)
  "Return the numbers of the lines of BUFFER that have a dot, from the top."
  (with-current-buffer buffer
    (mapcar (lambda (dot) (line-number-at-pos (overlay-start dot) t))
            (syringe--dots))))

(defun syringe-test-say (text)
  "Type TEXT in the conversation that the current buffer shows and send it."
  (goto-char (point-max))
  (insert text)
  (syringe-chat-send))

(ert-deftest syringe-test-diff-lines ()
  "Every line of a hunk knows its file and its line number there."
  (syringe-test-with-repository
    (syringe-test-write "src/main.rs" "// not committed\n" t)
    (syringe-test-write "fresh.txt" "a new file\nof two lines\n")
    (syringe-test-start)
    (let* ((now (lambda (file)
                  (with-temp-buffer
                    (insert-file-contents (syringe-test-path file))
                    (split-string (buffer-string) "\n"))))
           (before (lambda (file)
                     (split-string
                      (syringe-test-git "show" (concat "HEAD~1:" file))
                      "\n")))
           (diff (syringe-test-diff))
           (in-hunks (seq-count
                      (lambda (line)
                        (and (string-match-p "\\`[-+ ]" line)
                             (not (string-match-p "\\`\\(?:---\\|\\+\\+\\+\\) "
                                                  line))))
                      (split-string (substring diff (string-search "\n@@" diff))
                                    "\n")))
           (checked 0))
      (goto-char (point-min))
      (while (not (eobp))
        (pcase (syringe--diff-line-info)
          (`(,file ,number ,kind)
           (cl-incf checked)
           (should (equal (buffer-substring-no-properties (1+ (point))
                                                          (pos-eol))
                          (nth (1- number)
                               (funcall (if (eq kind ?-) before now)
                                        file))))))
        (forward-line 1))
      ;; All the lines of the hunks were looked at, and no other line.
      (should (= checked in-hunks)))))

(ert-deftest syringe-test-diff-find ()
  "A line is found in the diff by its file, its text and whether it is removed."
  (with-temp-buffer
    (insert "\
diff --git a/one.txt b/one.txt
--- a/one.txt
+++ b/one.txt
@@ -1,3 +1,3 @@
-same
 keep
+same
 last
@@ -10,2 +10,3 @@
 ten
+same
 eleven
diff --git a/two.txt b/two.txt
--- a/two.txt
+++ b/two.txt
@@ -1 +1,2 @@
 first
+same
")
    ;; Each case is the file, the text, whether the line is removed, a
    ;; line number in the file to be near to, and the line of the diff
    ;; that is expected.
    (pcase-dolist (`(,file ,text ,removed ,number ,expected)
                   '(("one.txt" "same" t 2 5)
                     ("one.txt" "same" nil 1 7)
                     ("one.txt" "same" nil 6 7)
                     ("one.txt" "same" nil 7 11)
                     ("one.txt" "same" nil nil 7)
                     ("one.txt" "keep" nil 1 6)
                     ("one.txt" "keep" t 2 nil)
                     ("two.txt" "same" nil 2 18)
                     ("two.txt" "same" t 2 nil)
                     ("three.txt" "same" nil 2 nil)))
      (ert-info ((format "%S" (list file text removed number)))
        (let ((found (syringe--diff-find file text removed number)))
          (should (equal (and found (line-number-at-pos found))
                         expected)))))))

(ert-deftest syringe-test-dot-in-diff ()
  "A dot on a line of the diff has a twin on that line of its file."
  (syringe-test-with-repository
    (syringe-test-start)
    (syringe-test-in-diff "CHANGED ten")
    (let ((line (line-number-at-pos)))
      (syringe-toggle-dot)
      (let ((conversation (car syringe--conversations)))
        ;; The new conversation is open and named after its line.
        (should (eq syringe--conversation conversation))
        (should (equal (syringe-conversation-title conversation)
                       "src/main.rs:10"))
        (should (equal (syringe-test-dots syringe--diff-buffer) (list line)))
        (should (equal (syringe-test-dots (syringe-test-main)) '(10)))
        (with-current-buffer (syringe-test-main)
          (goto-char (syringe--line-start 9))
          (should (eq (syringe--conversation-at-line) conversation)))
        ;; Removing the dot in the diff removes its twin and the
        ;; conversation.
        (syringe-test-in-diff "CHANGED ten")
        (syringe-toggle-dot)
        (should-not syringe--conversations)
        (should-not (syringe-test-dots syringe--diff-buffer))
        (should-not (syringe-test-dots (syringe-test-main)))
        (should-not (buffer-live-p
                     (syringe-conversation-buffer conversation)))))))

(ert-deftest syringe-test-dot-in-code ()
  "A dot on a line of a file has a twin in the diff if the diff has the line."
  (syringe-test-with-repository
    (syringe-test-start)
    (let ((changed (syringe-test-dot 10))
          (unchanged (syringe-test-dot 9))
          (elsewhere (syringe-test-dot 30)))
      (should (equal (syringe-test-dots (syringe-test-main)) '(9 10 30)))
      (should (equal (syringe-conversation-title elsewhere) "src/main.rs:30"))
      (should-not (syringe-conversation-diff-dot elsewhere))
      (with-current-buffer syringe--diff-buffer
        (pcase-dolist (`(,conversation . ,text)
                       `((,changed
                          . ,(concat "+" (syringe-test-line 10 "CHANGED ten")))
                         (,unchanged . ,(concat " " (syringe-test-line 9)))))
          (goto-char (overlay-start
                      (syringe-conversation-diff-dot conversation)))
          (should (equal (buffer-substring-no-properties (pos-bol) (pos-eol))
                         text)))))))

(ert-deftest syringe-test-dot-outside-code ()
  "A line that is only in the diff gets a dot there and a name that says so."
  (syringe-test-with-repository
    (syringe-test-start)
    (pcase-dolist (`(,text . ,title)
                   '(("-fn function_40" . "src/main.rs:40 (removed)")
                     ("Why the second commit." . "the commit message")
                     ("+++ b/notes.txt" . "diff line %d")))
      (syringe-test-in-diff text)
      (let ((line (line-number-at-pos)))
        (syringe-toggle-dot)
        (let ((conversation (car syringe--conversations)))
          (should (equal (syringe-conversation-title conversation)
                         (format title line)))
          (should (syringe-conversation-diff-dot conversation))
          (should-not (syringe-conversation-code-dot conversation)))))
    (should (= (length (syringe-test-dots syringe--diff-buffer)) 3))))

(ert-deftest syringe-test-remove-dot ()
  "Removing a dot asks first if its conversation has messages."
  (syringe-test-with-repository
    (syringe-test-start)
    (let ((conversation (syringe-test-dot 30)))
      (syringe-test-say "why?")
      (syringe-test-answer "because")
      (setq syringe-test-yes nil)
      (syringe-test-in-code "src/main.rs" 30)
      (syringe-toggle-dot)
      (should (= (length syringe-test-asked) 1))
      (should (equal syringe--conversations (list conversation)))
      (setq syringe-test-yes t)
      (syringe-toggle-dot)
      (should (= (length syringe-test-asked) 2))
      (should-not syringe--conversations))
    ;; A conversation without messages goes without a question.
    (syringe-test-dot 30)
    (syringe-test-in-code "src/main.rs" 30)
    (syringe-toggle-dot)
    (should (= (length syringe-test-asked) 2))
    (should-not syringe--conversations)))

(ert-deftest syringe-test-margin ()
  "A buffer with dots has one more column in its left margin, and no more."
  (syringe-test-with-repository
    (syringe-test-start)
    (syringe-test-in-code "src/main.rs" 1)
    ;; As if another package had asked for two columns.
    (setq left-margin-width 2)
    (syringe-test-dot 5)
    (syringe-test-dot 6)
    (with-current-buffer (syringe-test-main)
      (should (= left-margin-width 3)))
    (syringe-test-in-code "src/main.rs" 5)
    (syringe-toggle-dot)
    (should (= left-margin-width 3))
    (forward-line 1)
    (syringe-toggle-dot)
    (should (= left-margin-width 2))))

(ert-deftest syringe-test-next-and-previous ()
  "The next and previous commands walk the dots and stop at the ends."
  (syringe-test-with-repository
    (syringe-test-start)
    (mapc #'syringe-test-dot '(20 5 50))
    (syringe-test-in-code "src/main.rs" 1)
    (dolist (line '(5 20 50))
      (syringe-next-dot)
      (should (= (line-number-at-pos) line)))
    (should-error (syringe-next-dot) :type 'user-error)
    (dolist (line '(20 5))
      (syringe-previous-dot)
      (should (= (line-number-at-pos) line)))
    (should-error (syringe-previous-dot) :type 'user-error)))

(ert-deftest syringe-test-open-closest ()
  "The command that opens a conversation takes the dot closest to point."
  (syringe-test-with-repository
    (syringe-test-start)
    (let ((first (syringe-test-dot 5))
          (second (syringe-test-dot 50)))
      (pcase-dolist (`(,from ,to ,conversation)
                     `((12 5 ,first) (40 50 ,second) (50 50 ,second)))
        (syringe-test-in-code "src/main.rs" from)
        (syringe-open-conversation)
        (should (eq syringe--conversation conversation))
        (with-selected-window (syringe--window 'code)
          (should (= (line-number-at-pos) to)))))
    (syringe-test-in-code "notes.txt" 1)
    (should-error (syringe-open-conversation) :type 'user-error)))

(ert-deftest syringe-test-follow ()
  "The conversation window shows the conversation of the line point is on."
  (syringe-test-with-repository
    (syringe-test-start)
    (let ((first (syringe-test-dot 5))
          (second (syringe-test-dot 50)))
      ;; The line between the two has no dot and changes nothing.
      (pcase-dolist (`(,line . ,conversation)
                     `((5 . ,first) (50 . ,second) (20 . ,second)))
        (syringe-test-in-code "src/main.rs" line)
        (run-hooks 'post-command-hook)
        (should (eq (window-buffer (syringe--window 'chat))
                    (syringe-conversation-buffer conversation)))
        ;; The cursor stays in the code.
        (should (eq (syringe--window-role (selected-window)) 'code))))))

;;;; Dots that follow their lines

(defun syringe-test-edit-main (function)
  "Call FUNCTION at the start of the buffer of src/main.rs, then save it."
  (with-current-buffer (syringe-test-main)
    (goto-char (point-min))
    (funcall function)
    (save-buffer)))

(ert-deftest syringe-test-line-edited ()
  "A dot stays with its line through edits, and so does the name."
  (syringe-test-with-repository
    (syringe-test-start)
    (let ((conversation (syringe-test-dot 30)))
      (syringe-test-edit-main (lambda () (insert "// one\n// two\n")))
      (should (equal (syringe-test-dots (syringe-test-main)) '(32)))
      (should (= (syringe-conversation-code-line conversation) 32))
      (should (equal (syringe-conversation-title conversation)
                     "src/main.rs:32"))
      (should (equal (buffer-name (syringe-conversation-buffer conversation))
                     "*syringe: src/main.rs:32*"))
      ;; A line that is reworded under its dot is still the line.
      (syringe-test-edit-main (lambda ()
                                (search-forward "line 30")
                                (replace-match "another text")))
      (should (equal (syringe-test-dots (syringe-test-main)) '(32)))
      (should (equal (syringe-conversation-code-text conversation)
                     (syringe-test-line 30 "another text")))
      (should-not (syringe-conversation-lost conversation)))))

(ert-deftest syringe-test-line-moved ()
  "A line that is cut and pasted elsewhere takes its dot along."
  (syringe-test-with-repository
    (syringe-test-start)
    (let ((conversation (syringe-test-dot 30)))
      (syringe-test-edit-main
       (lambda ()
         (forward-line 29)
         (let ((line (delete-and-extract-region (pos-bol) (pos-bol 2))))
           (goto-char (point-min))
           (forward-line 4)
           (insert line))))
      (should (equal (syringe-test-dots (syringe-test-main)) '(5)))
      (should (= (syringe-conversation-code-line conversation) 5))
      (should-not (syringe-conversation-lost conversation)))))

(ert-deftest syringe-test-line-lost ()
  "A dot whose line is deleted turns gray, until the line is back."
  (syringe-test-with-repository
    (syringe-test-start)
    (let ((conversation (syringe-test-dot 30)))
      (syringe-test-edit-main (lambda ()
                                (forward-line 29)
                                (delete-region (pos-bol) (pos-bol 2))))
      (should (syringe-conversation-lost conversation))
      (should (eq (syringe--dot-face conversation) 'syringe-dot-lost))
      ;; The dot stays where the line was, and the line is not forgotten.
      (should (equal (syringe-test-dots (syringe-test-main)) '(30)))
      (should (equal (syringe-conversation-code-text conversation)
                     (syringe-test-line 30)))
      (syringe-test-edit-main (lambda ()
                                (forward-line 29)
                                (insert (syringe-test-line 30) "\n")))
      (should-not (syringe-conversation-lost conversation))
      (should (eq (syringe--dot-face conversation) 'syringe-dot))
      (should (equal (syringe-test-dots (syringe-test-main)) '(30))))))

(ert-deftest syringe-test-file-changed-on-disk ()
  "After a file is read again from disk, its dots are on their lines."
  (syringe-test-with-repository
    (syringe-test-start)
    (let ((kept (syringe-test-dot 10))
          (deleted (syringe-test-dot 30))
          (text (with-current-buffer (syringe-test-main) (buffer-string))))
      (syringe-test-write "src/main.rs"
                          (concat "// one\n// two\n// three\n"
                                  (string-replace
                                   (concat (syringe-test-line 30) "\n") ""
                                   text)))
      (with-current-buffer (syringe-test-main)
        (revert-buffer t t))
      (should (= (syringe-conversation-code-line kept) 13))
      (should-not (syringe-conversation-lost kept))
      (should (syringe-conversation-lost deleted))
      (should (equal (syringe-test-dots (syringe-test-main)) '(13 30))))))

(ert-deftest syringe-test-file-visited-again ()
  "The dots of a file are back when the file is visited again."
  (syringe-test-with-repository
    (syringe-test-start)
    (let ((conversations (mapcar #'syringe-test-dot '(10 30))))
      (kill-buffer (syringe-test-main))
      (dolist (conversation conversations)
        (should-not (syringe--live-dot
                     (syringe-conversation-code-dot conversation))))
      (find-file-noselect (syringe-test-path "src/main.rs"))
      (should (equal (syringe-test-dots (syringe-test-main)) '(10 30))))))

;;;; Saved reviews

(defun syringe-test-saved ()
  "Return what the file of the open review holds."
  (read (syringe--read-text syringe--file)))

(defun syringe-test-choose (choice function)
  "Call FUNCTION and answer the completion that it asks for with CHOICE.
CHOICE is the number of a choice, counted from 0, or a text that
the choice has.  Return the choices that were offered, in order."
  (let ((offered nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (setq offered (all-completions "" table))
                 (if (numberp choice)
                     (nth choice offered)
                   (seq-find (lambda (line) (string-search choice line))
                             offered)))))
      (funcall function))
    offered))

(ert-deftest syringe-test-saved-as-you-go ()
  "A review is on disk, in the git directory, after every change to it."
  (syringe-test-with-repository
    (syringe-test-start)
    (should (file-in-directory-p syringe--file
                                 (syringe-test-path ".git/syringe")))
    (should (equal (syringe--read-text (syringe--diff-file syringe--file))
                   (syringe-test-diff)))
    (should-not (plist-get (syringe-test-saved) :conversations))
    (syringe-test-dot 30)
    (syringe-test-say "why?")
    (let ((saved (car (plist-get (syringe-test-saved) :conversations))))
      (should (equal (plist-get saved :title) "src/main.rs:30"))
      (should (equal (plist-get saved :code-text) (syringe-test-line 30)))
      (should (equal (plist-get saved :messages) '((user . "why?")))))
    ;; Git does not see the saved review.
    (should (equal (syringe-test-git "status" "--porcelain") ""))))

(ert-deftest syringe-test-restore ()
  "A saved review is opened from the list, with its conversations and dots."
  (syringe-test-with-repository
    (syringe-test-start)
    (syringe-test-in-diff "CHANGED ten")
    (syringe-toggle-dot)
    (syringe-test-say "why ten?")
    (syringe-test-answer "because")
    (syringe-test-in-diff "-fn function_40")
    (syringe-toggle-dot)
    (let ((in-diff (syringe-test-dots syringe--diff-buffer)))
      (should (= (length in-diff) 2))
      (syringe--close-review)
      (should-not syringe--conversations)
      (should-not (syringe-test-dots syringe--diff-buffer))
      (should-not (syringe-test-dots (syringe-test-main)))
      (let ((offered (syringe-test-choose
                      0 (lambda () (syringe-restore syringe-test-root)))))
        (should (= (length offered) 1))
        (should (string-search "last commit" (car offered)))
        (should (string-search "2 conversations" (car offered))))
      (should (equal (mapcar #'syringe-conversation-title
                             syringe--conversations)
                     '("src/main.rs:40 (removed)" "src/main.rs:10")))
      (should (equal (syringe-conversation-messages
                      (cadr syringe--conversations))
                     '((user . "why ten?") (robot . "because"))))
      (should (equal (syringe-test-dots syringe--diff-buffer) in-diff))
      (should (equal (syringe-test-dots (syringe-test-main)) '(10)))
      ;; The robot has read the change and is not asked to read it again.
      (should-not syringe-test-sent))))

(ert-deftest syringe-test-start-again ()
  "Starting a review of a commit that has a review opens that review."
  (syringe-test-with-repository
    (syringe-test-start)
    (syringe-test-dot 30)
    (let ((file syringe--file))
      ;; While the review is open:
      (syringe-start-last-commit syringe-test-root)
      (should (equal syringe--file file))
      (should (= (length syringe--conversations) 1))
      ;; And after it was closed:
      (syringe--close-review)
      (syringe-start-last-commit syringe-test-root)
      (should (equal syringe--file file))
      (should (= (length syringe--conversations) 1))
      (should (equal (syringe-test-dots (syringe-test-main)) '(30)))
      (should-not syringe-test-sent)
      ;; A review of the unstaged changes is another review.
      (syringe-test-write "notes.txt" "not committed\n" t)
      (syringe-start-unstaged syringe-test-root)
      (syringe-test-answer "ready")
      (should-not (equal syringe--file file))
      (should (eq (plist-get syringe--about :kind) 'unstaged))
      (should-not syringe--conversations)
      (syringe-start-last-commit syringe-test-root)
      (should (equal syringe--file file))
      (should (= (length syringe--conversations) 1)))))

(ert-deftest syringe-test-diff-follows-code ()
  "The diff is taken again after an edit, and the conversations stay."
  (syringe-test-with-repository
    (syringe-test-start)
    (syringe-test-in-diff "CHANGED ten")
    (syringe-toggle-dot)
    (let ((conversation (car syringe--conversations)))
      (syringe-test-edit-main (lambda () (insert "// new\n")))
      ;; Saving asks for the diff once Emacs is idle; do not wait for that.
      (should (timerp syringe--refresh-timer))
      (syringe--refresh-if-current)
      (should (string-search "\n+// new\n" (syringe-test-diff)))
      (should (equal (syringe--read-text (syringe--diff-file syringe--file))
                     (syringe-test-diff)))
      (should (equal (syringe-conversation-title conversation)
                     "src/main.rs:11"))
      (with-current-buffer syringe--diff-buffer
        (goto-char (overlay-start
                    (syringe-conversation-diff-dot conversation)))
        (should (equal (buffer-substring-no-properties (pos-bol) (pos-eol))
                       (concat "+" (syringe-test-line 10 "CHANGED ten"))))))))

(ert-deftest syringe-test-other-commit ()
  "A review keeps its diff when its commit is left, until it is carried over."
  (syringe-test-with-repository
    (syringe-test-start)
    (syringe-test-dot 30)
    (let ((commit (plist-get syringe--about :commit)))
      (syringe-test-write "notes.txt" "fourth note\n" t)
      (syringe-test-git "commit" "-q" "--all" "-m" "three")
      (syringe--refresh-if-current)
      (should (string-search "CHANGED ten" (syringe-test-diff)))
      ;; `g' in the diff offers to carry the review over.
      (setq syringe-test-yes nil)
      (with-current-buffer syringe--diff-buffer
        (should-error (revert-buffer) :type 'user-error))
      (should (equal (plist-get syringe--about :commit) commit))
      (should (string-search "CHANGED ten" (syringe-test-diff)))
      (setq syringe-test-yes t)
      (with-current-buffer syringe--diff-buffer
        (revert-buffer))
      (should (equal (plist-get syringe--about :commit)
                     (string-trim (syringe-test-git "rev-parse" "HEAD"))))
      (should-not (string-search "CHANGED ten" (syringe-test-diff)))
      (should (string-search "\n+fourth note\n" (syringe-test-diff)))
      (should (= (length syringe--conversations) 1))
      (should (equal (plist-get (syringe-test-saved) :about)
                     syringe--about)))))

;;;; Conversations with the robot

(defun syringe-test-prompt ()
  "Return the oldest prompt that the fake robot has not answered yet."
  (nth 1 (car syringe-test-sent)))

(defun syringe-test-header (conversation)
  "Return the header line of the buffer of CONVERSATION."
  (with-current-buffer (syringe-conversation-buffer conversation)
    (syringe--chat-header)))

(defun syringe-test-dot-color (dot)
  "Return the face that DOT is drawn in."
  (get-text-property
   0 'face (cadr (get-text-property 0 'display
                                    (overlay-get dot 'before-string)))))

(ert-deftest syringe-test-robot-reads-change ()
  "When a review starts, one robot session is told to read the change."
  (syringe-test-with-repository
    (syringe-start-last-commit syringe-test-root)
    (should (= (length syringe-test-sent) 1))
    (should (equal (caar syringe-test-sent) syringe--base))
    (should (string-search "\"git diff HEAD~1\"" (syringe-test-prompt)))
    (should (string-search "Help me think, don't think for me."
                           (syringe-test-prompt)))
    (should (equal (syringe--base-indicator) " [robot: reading the diff]"))
    (syringe-test-answer "ready")
    (should (equal (syringe--base-indicator) " [robot: ready]"))
    ;; A review of the unstaged changes names another command.
    (syringe-test-write "notes.txt" "not committed\n" t)
    (syringe-start-unstaged syringe-test-root)
    (should (string-search "\"git diff\"" (syringe-test-prompt)))
    (should-not (string-search "HEAD~1" (syringe-test-prompt)))))

(ert-deftest syringe-test-first-message ()
  "The first message of a conversation says which line it is about."
  (syringe-test-with-repository
    (syringe-test-start)
    (let ((conversation (syringe-test-dot 30)))
      (syringe-test-say "why?")
      ;; The conversation has a session of its own, a copy of the one
      ;; that read the change.
      (should (equal (caar syringe-test-sent)
                     (syringe-conversation-session conversation)))
      (should (string-prefix-p (concat syringe--base "/")
                               (syringe-conversation-session conversation)))
      (should (equal (syringe-test-prompt)
                     (concat "This conversation is about src/main.rs:30.  "
                             "The line is:\n"
                             (syringe-test-line 30)
                             "\n\nMy message:\nwhy?")))
      (syringe-test-answer "because")
      (should (equal (syringe-conversation-messages conversation)
                     '((user . "why?") (robot . "because"))))
      ;; The next message goes to the same session, as it is.
      (syringe-test-say "and then?")
      (should (equal (seq-take (car syringe-test-sent) 2)
                     (list (syringe-conversation-session conversation)
                           "and then?"))))))

(ert-deftest syringe-test-states ()
  "A conversation is thinking, read, unread or failed, and its dot shows it."
  (syringe-test-with-repository
    (syringe-test-start)
    (let* ((conversation (syringe-test-dot 30))
           (state (lambda () (syringe-conversation-state conversation)))
           (color (lambda ()
                    (syringe-test-dot-color
                     (syringe-conversation-code-dot conversation)))))
      (should (eq (funcall color) 'syringe-dot))
      (syringe-test-say "why?")
      (should (eq (funcall state) 'thinking))
      (should (eq (funcall color) 'syringe-dot-thinking))
      (should (string-search "thinking"
                             (syringe-test-header conversation)))
      ;; While the robot thinks, nothing can be typed or sent.
      (should buffer-read-only)
      (should-error (syringe-chat-send) :type 'user-error)
      ;; What the robot is doing shows in the header line.
      (funcall (nth 2 (car syringe-test-sent)) "reading src/main.rs")
      (should (string-search "reading src/main.rs"
                             (syringe-test-header conversation)))
      ;; An answer that arrives while you are in the conversation is read.
      (syringe-test-answer "because")
      (should (eq (funcall state) 'read))
      (should (eq (funcall color) 'syringe-dot))
      (should-not buffer-read-only)
      ;; One that arrives while you are elsewhere is unread until you go
      ;; to the conversation.
      (syringe-test-say "and then?")
      (syringe-test-in-code "src/main.rs" 1)
      (syringe-test-answer "nothing")
      (should (eq (funcall state) 'unread))
      (should (eq (funcall color) 'syringe-dot-unread))
      (syringe-open-conversation)
      (should (eq (funcall state) 'read))
      ;; A failure is shown in the conversation.
      (syringe-test-say "once more?")
      (syringe-test-answer "no network" t)
      (should (eq (funcall state) 'error))
      (should (eq (funcall color) 'syringe-dot-error))
      (should (string-search "failed" (syringe-test-header conversation)))
      (should (equal (car (last (syringe-conversation-messages conversation)))
                     '(error . "no network"))))))

(ert-deftest syringe-test-first-message-fails ()
  "After a first message that failed, the conversation starts over."
  (syringe-test-with-repository
    (syringe-test-start)
    (let ((conversation (syringe-test-dot 30)))
      (syringe-test-say "why?")
      (let ((session (syringe-conversation-session conversation)))
        (syringe-test-answer "no network" t)
        (should-not (syringe-conversation-session conversation))
        (syringe-test-say "why, again?")
        (should-not (equal (syringe-conversation-session conversation)
                           session))
        (should (string-prefix-p "This conversation is about src/main.rs:30."
                                 (syringe-test-prompt)))))))

(ert-deftest syringe-test-message-waits ()
  "A message that is sent while the robot reads the change waits for that."
  (syringe-test-with-repository
    (syringe-start-last-commit syringe-test-root)
    (let ((conversation (syringe-test-dot 30)))
      (syringe-test-say "why?")
      ;; Only the reading of the change is on its way.
      (should (= (length syringe-test-sent) 1))
      (should (eq (syringe-conversation-state conversation) 'thinking))
      (should (string-search "waiting" (syringe-test-header conversation)))
      (syringe-test-answer "ready")
      (should (= (length syringe-test-sent) 1))
      (should (string-suffix-p "My message:\nwhy?" (syringe-test-prompt))))))

(ert-deftest syringe-test-robot-cannot-read-change ()
  "If the robot could not read the change, a message fails and says why."
  (syringe-test-with-repository
    (syringe-start-last-commit syringe-test-root)
    (syringe-test-answer "no network" t)
    (should (equal (syringe--base-indicator) " [robot: failed]"))
    (let ((conversation (syringe-test-dot 30)))
      (syringe-test-say "why?")
      (should-not syringe-test-sent)
      (should (eq (syringe-conversation-state conversation) 'error))
      (pcase-let ((`(,role . ,text)
                   (car (last (syringe-conversation-messages conversation)))))
        (should (eq role 'error))
        (should (string-search "no network" text))))))

(ert-deftest syringe-test-news ()
  "A message tells the robot what was said in the other conversations."
  (syringe-test-with-repository
    (syringe-test-start)
    (let ((first (syringe-test-dot 5))
          (second (syringe-test-dot 30)))
      (syringe--open-chat first)
      (syringe-test-say "what is five?")
      (syringe-test-answer "a function")
      (syringe--open-chat second)
      (syringe-test-say "and thirty?")
      (should (equal (syringe-test-prompt)
                     (concat "Said in the other conversations of this review "
                             "since your last answer here:\n\n"
                             "[src/main.rs:5] Me: what is five?\n\n"
                             "[src/main.rs:5] You: a function\n\n"
                             "This conversation is about src/main.rs:30.  "
                             "The line is:\n"
                             (syringe-test-line 30)
                             "\n\nMy message:\nand thirty?")))
      (syringe-test-answer "another function")
      ;; The news is told once.
      (syringe-test-say "sure?")
      (should (equal (syringe-test-prompt) "sure?"))
      (syringe-test-answer "yes")
      ;; The other conversation hears what was said here, not itself.
      (syringe--open-chat first)
      (syringe-test-say "more?")
      (should (equal (syringe-test-prompt)
                     (concat "Said in the other conversations of this review "
                             "since your last answer here:\n\n"
                             "[src/main.rs:30] Me: and thirty?\n\n"
                             "[src/main.rs:30] You: another function\n\n"
                             "[src/main.rs:30] Me: sure?\n\n"
                             "[src/main.rs:30] You: yes\n\n"
                             "My message:\nmore?")))
      ;; A message that failed did not deliver the news; it is told again.
      (syringe-test-answer "no network" t)
      (syringe-test-say "more, please?")
      (should (string-search "[src/main.rs:30] You: yes"
                             (syringe-test-prompt))))))

(ert-deftest syringe-test-line-told ()
  "A message says so when its line has moved, changed or got lost."
  (syringe-test-with-repository
    (syringe-test-start)
    (syringe-test-dot 30)
    (syringe-test-say "why?")
    (syringe-test-answer "because")
    (pcase-dolist
        (`(,edit ,told)
         `((,(lambda () (insert "// one\n// two\n"))
            "The line this conversation is about has moved: it is now \
src/main.rs:32.")
           ;; Nothing has happened to the line since the last message.
           (ignore nil)
           (,(lambda ()
               (search-forward "line 30")
               (replace-match "another text"))
            ,(concat "The line this conversation is about has changed.  \
It is now src/main.rs:32 and reads:\n"
                     (syringe-test-line 30 "another text")))
           (,(lambda ()
               (search-forward "another text")
               (delete-region (pos-bol) (pos-bol 2)))
            "The line this conversation is about is no longer in the \
code: it was deleted or rewritten.")))
      (syringe-test-edit-main edit)
      (syringe-test-say "and now?")
      (should (equal (syringe-test-prompt)
                     (if told
                         (concat told "\n\nMy message:\nand now?")
                       "and now?")))
      (syringe-test-answer "I see"))))

(ert-deftest syringe-test-links ()
  "A reference to a line of a file in an answer is a link to that line."
  (syringe-test-with-repository
    (syringe-test-start)
    (syringe-test-dot 30)
    ;; A conversation without an answer has no links.
    (should-error (syringe-next-link) :type 'user-error)
    (syringe-test-say "where? src/main.rs:20 maybe")
    (syringe-test-answer
     "Look at src/main.rs:10 and notes.txt:2, not at nowhere.rs:3 or 12:30.")
    ;; Only what names a file of the repository is a link, and only in
    ;; what the robot wrote.
    (goto-char (point-min))
    (let ((links nil))
      (while (forward-button 1 nil nil t)
        (push (button-label (button-at (point))) links))
      (should (equal (nreverse links) '("src/main.rs:10" "notes.txt:2"))))
    ;; After the last link comes the first.
    (goto-char (point-max))
    (syringe-next-link)
    (should (equal (button-label (button-at (point))) "src/main.rs:10"))
    (syringe-previous-link)
    (should (equal (button-label (button-at (point))) "notes.txt:2"))
    (push-button)
    (should (file-equal-p buffer-file-name (syringe-test-path "notes.txt")))
    (should (= (line-number-at-pos) 2))
    (should (eq (syringe--window-role (selected-window)) 'code))))

(ert-deftest syringe-test-closed-while-thinking ()
  "A review that is closed while the robot thinks opens with a failure."
  (syringe-test-with-repository
    (syringe-test-start)
    (syringe-test-dot 30)
    (syringe-test-say "why?")
    (let ((answer-late (nth 3 (car syringe-test-sent))))
      (syringe--close-review)
      ;; What the stopped robot still says is ignored.
      (funcall answer-late t "too late")
      (should-not syringe--conversations)
      (syringe-start-last-commit syringe-test-root)
      (let ((conversation (car syringe--conversations)))
        (should (eq (syringe-conversation-state conversation) 'error))
        (should (equal (mapcar #'car
                               (syringe-conversation-messages conversation))
                       '(user error)))
        ;; The robot may never have heard of the conversation.
        (should-not (syringe-conversation-session conversation))))))

;;;; The engine

;; These tests run the real functions of the engine, with a shell
;; script in the place of Copilot and a scratch directory in the place
;; of the one where Copilot keeps its sessions.

(defun syringe-test-run-engine (script)
  "Send a prompt with `syringe-engine-send' to SCRIPT in place of Copilot.
SCRIPT is the text of a shell script.  Wait until it has ended and
return (ARGUMENTS ACTIVITIES OK TEXT): the arguments it was run
with, what `syringe-engine-send' reported as activity, and what it
reported at the end.  The script runs in a scratch directory, which
it can name as $PWD."
  (ert-with-temp-directory directory
    (let* ((syringe-copilot-program (expand-file-name "copilot" directory))
           (arguments (expand-file-name "arguments" directory))
           (activities nil)
           (result nil))
      (write-region (concat "#!/bin/sh\n"
                            "printf '%s\\n' \"$@\" > arguments\n"
                            script)
                    nil syringe-copilot-program nil 'silent)
      (set-file-modes syringe-copilot-program #o700)
      (let ((process (syringe-engine-send
                      "the-session" "the prompt" directory
                      (lambda (activity) (push activity activities))
                      (lambda (ok text) (setq result (list ok text))))))
        (with-timeout (30 (error "The script has not ended"))
          (while (not result)
            (accept-process-output process 0.1))))
      (cons (split-string (with-temp-buffer
                            (insert-file-contents arguments)
                            (buffer-string))
                          "\n" t)
            (cons (nreverse activities) result)))))

(ert-deftest syringe-test-engine-send ()
  "The engine runs Copilot read-only and takes its last message as the answer."
  (skip-unless (executable-find "sh"))
  (pcase-let ((`(,arguments ,activities ,ok ,text)
               (syringe-test-run-engine "cat <<END
{\"type\":\"tool.execution_start\",\"data\":{\"toolName\":\"view\",\
\"arguments\":{\"path\":\"$PWD/src/main.rs\"}}}
{\"type\":\"assistant.message\",\"data\":{\"content\":\"Let me look.\"}}
{\"type\":\"tool.execution_start\",\"data\":{\"toolName\":\"shell\",\
\"arguments\":{\"command\":\"git log -1\"}}}
{\"type\":\"assistant.message\",\"data\":{\"content\":\"\"}}
{\"type\":\"assistant.message\",\"data\":{\"content\":\"The answer.\\\\n\"}}
END
")))
    (should ok)
    (should (equal text "The answer."))
    (should (equal activities '("view src/main.rs" "shell git log -1")))
    ;; The session and the prompt are passed on, and the robot may not
    ;; write.
    (should (equal (cadr (member "--session-id" arguments)) "the-session"))
    (should (equal (cadr (member "-p" arguments)) "the prompt"))
    (should (member "--deny-tool=write" arguments))))

(ert-deftest syringe-test-engine-fails ()
  "The engine reports why Copilot failed, or that it gave no answer."
  (skip-unless (executable-find "sh"))
  (pcase-dolist (`(,script . ,why)
                 '(("echo 'Error: not logged in'\nexit 1\n"
                    . "Error: not logged in")
                   ;; It worked, by its exit status, but said nothing.
                   ("exit 0\n" . "Copilot gave no answer")))
    (pcase-let ((`(,_arguments ,_activities ,ok ,text)
                 (syringe-test-run-engine script)))
      (should-not ok)
      (should (equal text why))))
  ;; A program that is not there is an error at once.
  (let ((syringe-copilot-program "syringe-test-no-such-program"))
    (should-error (syringe-engine-send "the-session" "the prompt"
                                       temporary-file-directory
                                       #'ignore #'ignore))))

(ert-deftest syringe-test-engine-sessions ()
  "A new session has a name of its own, in the form of a UUID."
  (let ((sessions (cl-loop repeat 200 collect (syringe-engine-new-session))))
    (dolist (session sessions)
      (should (string-match-p
               (rx bos (= 8 hex) "-" (= 4 hex) "-4" (= 3 hex) "-"
                   (any "89ab") (= 3 hex) "-" (= 12 hex) eos)
               session)))
    (should (= (length (delete-dups sessions)) 200))))

(ert-deftest syringe-test-engine-fork ()
  "A fork is a copy of the files of a session under a new name."
  (ert-with-temp-directory home
    (let* ((process-environment
            (cons (concat "COPILOT_HOME=" home) process-environment))
           (session "11111111-2222-4333-8444-555555555555")
           (read (lambda (session file)
                   (with-temp-buffer
                     (insert-file-contents
                      (expand-file-name
                       (file-name-concat "session-state" session file)
                       home))
                     (buffer-string))))
           (files `(("workspace.yaml" . ,(format "id: %s\ncwd: /repo\n"
                                                 session))
                    ("events.jsonl" . ,(format "{\"sessionId\":\"%s\"}\n\
{\"parent\":\"%s\"}\n"
                                               session session))
                    ("other.txt" . "kept as it is\n"))))
      (make-directory (file-name-concat home "session-state" session) t)
      (pcase-dolist (`(,file . ,text) files)
        (write-region text nil
                      (file-name-concat home "session-state" session file)
                      nil 'silent))
      (let ((fork (syringe-engine-fork session)))
        (should-not (equal fork session))
        (pcase-dolist (`(,file . ,text) files)
          ;; The fork goes by its own name wherever the session is named.
          (should (equal (funcall read fork file)
                         (string-replace session fork text)))
          ;; The session it was copied from is as it was.
          (should (equal (funcall read session file) text)))))))

(ert-deftest syringe-test-engine-activity ()
  "What the robot is doing is told in a few words, on one line."
  (pcase-dolist
      (`(,data . ,activity)
       '((((toolName . "view") (arguments (path . "/repo/src/main.rs")))
          . "view src/main.rs")
         ;; A title is better than a name.
         (((toolTitle . "Read") (toolName . "view")
           (arguments (path . "/repo/notes.txt")))
          . "Read notes.txt")
         (((toolName . "shell") (arguments (command . "git log\n  -1")))
          . "shell git log   -1")
         (((toolName . "grep") (arguments (pattern . "fn main")))
          . "grep fn main")
         (((toolName . "think")) . "think")))
    (should (equal (syringe-copilot--activity data "/repo/") activity)))
  ;; A long command is cut short.
  (should (<= (length (syringe-copilot--activity
                       `((toolName . "shell")
                         (arguments (command . ,(make-string 200 ?x))))
                       "/repo/"))
              70)))

;;;; The list of dots

(ert-deftest syringe-test-list ()
  "The list of dots has what needs attention first and jumps to the choice."
  (syringe-test-with-repository
    (syringe-test-start)
    (syringe-test-dot 5)
    (syringe-test-say "what is five?")
    (syringe-test-answer "a function")
    (syringe-test-dot 20)
    (syringe-test-say "what is twenty?")
    (syringe-test-answer "no network" t)
    (let ((unread (syringe-test-dot 35)))
      (syringe-test-say "what is thirty-five?")
      (syringe-test-in-code "src/main.rs" 1)
      (syringe-test-answer "a function too")
      (syringe-test-dot 50)
      (syringe-test-say "what is fifty?")
      (syringe-test-in-code "src/main.rs" 1)
      (let ((offered (syringe-test-choose 0 #'syringe-list-dots)))
        (should (= (length offered) 4))
        (pcase-dolist (`(,line . ,parts)
                       (cl-mapcar
                        #'cons offered
                        '(("unread" "src/main.rs:35" "what is thirty-five?")
                          ("failed" "src/main.rs:20" "what is twenty?")
                          ("thinking" "src/main.rs:50" "what is fifty?")
                          ("src/main.rs:5" "what is five?"))))
          (dolist (part parts)
            (should (string-search part line)))))
      ;; The choice was the unread one: point is on its dot, its
      ;; conversation is shown, and it is read now.
      (should (eq (current-buffer) (syringe-test-main)))
      (should (= (line-number-at-pos) 35))
      (should (eq (window-buffer (syringe--window 'chat))
                  (syringe-conversation-buffer unread)))
      (should (eq (syringe-conversation-state unread) 'read)))))

(ert-deftest syringe-test-list-from-diff ()
  "From the diff the list goes to the dot in the diff, else to the code."
  (syringe-test-with-repository
    (syringe-test-start)
    (syringe-test-dot 10)
    (syringe-test-dot 30)
    (syringe-test-in-diff "+third note")
    (syringe-test-choose "src/main.rs:10" #'syringe-list-dots)
    (should (equal (buffer-name) syringe--diff-buffer))
    (should (equal (buffer-substring-no-properties (pos-bol) (pos-eol))
                   (concat "+" (syringe-test-line 10 "CHANGED ten"))))
    ;; A line that the diff does not have is gone to in its file.
    (syringe-test-choose "src/main.rs:30" #'syringe-list-dots)
    (should (eq (current-buffer) (syringe-test-main)))
    (should (= (line-number-at-pos) 30))
    ;; From the code, the line that is in both is gone to in the code.
    (syringe-test-choose "src/main.rs:10" #'syringe-list-dots)
    (should (eq (current-buffer) (syringe-test-main)))
    (should (= (line-number-at-pos) 10))))

(ert-deftest syringe-test-conversation-without-dot ()
  "A conversation whose line left the diff is reached and removed all the same."
  (syringe-test-with-repository
    (syringe-test-start)
    (syringe-test-in-diff "-fn function_40")
    (syringe-toggle-dot)
    (syringe-test-say "why removed?")
    (syringe-test-answer "nothing uses it")
    (let ((conversation (car syringe--conversations)))
      ;; Put the removed line back: the diff does not have it any more.
      (syringe-test-in-code "src/main.rs" 1)
      (syringe-test-edit-main (lambda ()
                                (forward-line 39)
                                (insert (syringe-test-line 40) "\n")))
      (syringe--refresh-if-current)
      (should (syringe-conversation-lost conversation))
      (should-not (syringe--live-dots conversation))
      (let ((offered (syringe-test-choose 0 #'syringe-list-dots)))
        (should (string-search "lost" (car offered))))
      ;; Without a dot to go to, the list goes to the conversation.
      (should (eq syringe--conversation conversation))
      ;; There the command that removes a dot removes the conversation.
      (syringe-toggle-dot)
      (should (= (length syringe-test-asked) 1))
      (should-not syringe--conversations))))

(provide 'syringe-test)
;;; syringe-test.el ends here
