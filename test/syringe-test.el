;;; syringe-test.el --- Tests for syringe  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run the tests with `make check'.

;;; Code:

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

(provide 'syringe-test)
;;; syringe-test.el ends here
