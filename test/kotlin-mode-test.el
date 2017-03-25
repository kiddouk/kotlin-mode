;;; kotlin-mode-test.el --- ERT Tests for kotlin-mode.el

(load-file "kotlin-mode.el")
(setq-default indent-tabs-mode nil)
(setq kotlin-tab-width 4)
              
(require 'ert)

(defun kotlin-compare-code-after-manip (original point-pos manip-func expected got)
  (equal expected got))

(defun kotlin-test-manip-code (original point-pos manip-func expected)
  (with-temp-buffer
    (kotlin-mode)
    (insert original)
    (goto-char point-pos)
    (funcall manip-func)
    (should (kotlin-compare-code-after-manip
             original point-pos manip-func expected (buffer-string)))))

(defun test-indent (indented &optional deindented)
  (let ((deindented (or deindented (replace-regexp-in-string "^[[:blank:]]*" "    " indented))))
    (kotlin-test-manip-code
     deindented
     1
     (lambda ()
       (indent-region 1 (+ 1 (buffer-size))))
     indented)))

(ert-deftest kotlin-mode--top-level-indent-test ()
  (test-indent
    "package com.gregghz.emacs

import java.util.*
import foo.Bar
import bar.Bar as bBar
"))

(ert-deftest kotlin-mode--single-level-indent-test ()
  (test-indent
    "
fun sum(a: Int, b: Int): Int {
    return a + b
}"))

(ert-deftest kotlin-mode--chained-methods ()
  (test-indent
   "
fun sum(a: Int, b: Int): Int {
    var me = this.is()
                 .a()
                 .method()
                 .chaining();
}"))

(ert-deftest kotlin-mode--chained-methods-with-inner-chain ()
  (test-indent
   "
fun sum(a: Int, b: Int): Int {
    var me = this.is(a.method()
                      .chaining())
                 .with()
                 .innerchaining();
}"))

(ert-deftest kotlin-mode--chained-methods-with-a-flat-inner-chain ()
  (test-indent
   "
fun sum(a: Int, b: Int): Int {
    var me = this.is(a.method())
                 .chaining()
                 .with()
                 .a_flat()
                 .innerchaining();
}"))

(ert-deftest kotlin-mode--chained-methods-without-parens ()
  (test-indent
   "
fun sum(a: Int, b: Int): Int {
    var me = this.is { method }
                 .chaining { it };
    
}"))


(ert-deftest kotlin-mode--chained-methods-without-parens ()
  (test-indent
   "
fun sum(a: Int, b: Int): Int {
    var me = this.is { method.test { it }
                             .indent() }
                 .chaining { it };
    
}"))


(ert-deftest kotlin-mode--chained-methods-with-parens-and-curly ()
  (test-indent
   "
fun sum(a: Int, b: Int): Int {
    var me = this.is({ method.test { it }
                             .indent() })
                 .chaining({ it });
}"))
