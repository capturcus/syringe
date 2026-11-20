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

(provide 'syringe-test)
;;; syringe-test.el ends here
