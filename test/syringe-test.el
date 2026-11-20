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

(provide 'syringe-test)
;;; syringe-test.el ends here
