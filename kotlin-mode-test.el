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

(ert-deftest kotlin-mode--two-levels-indent-test ()
  (test-indent
    "
fun sum(a: Int, b: Int): Int {
    object {
        sum(a, b)
    }
}"))

(ert-deftest kotlin-mode--two-levels-with-paren-indent-test ()
  (test-indent
    "
fun sum(a: Int, b: Int): Int {
    object {
        funcall(
            foo()
        )
    }
}"))


(ert-deftest kotlin-mode--three-levels-and-close-two-on-the-same-line-indent-test ()
  (test-indent
    "
class T {
    fun sum(a: Int, b: Int): Int {
        object {
            test(a, b)
        }}
}"))

(ert-deftest kotlin-mode--arguments-indent-test ()
  (test-indent
    "
class T {
    fun sum(a: Int, b: Int): Int {
        object {
            test(thisIsALongArgument,
                    thisIsAnotherOne)
        }}
}"))

(ert-deftest kotlin-mode--nested-arguments-indent-test ()
  (test-indent
    "
class T {
    fun sum(a: Int, b: Int): Int {
        object {
            test(thisIsALongArgument,
                    function(anArgumentPassed,
                            andASecondOne))
        }}
}"))


(ert-deftest kotlin-mode--chained-methods-with-assignment ()
  (test-indent
   "
fun sum(a: Int, b: Int): Int {
    var me = myobj.is()
                  .a()
                  .method()
                  .chaining();
}"))

(ert-deftest kotlin-mode--simple-chained-methods ()
  (test-indent
   "
fun sum(a: Int, b: Int): Int {
    myobj.is()
         .a()
         .method()
         .chaining();
}"))

(ert-deftest kotlin-mode--simple-chained-methods-on-new-line ()
  (test-indent
   "
fun sum(a: Int, b: Int): Int {
    myobject
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

(ert-deftest kotlin-mode--chained-methods-with-parens-and-curly ()
  (test-indent
   "
fun sum(a: Int, b: Int): Int {
    var me = this.is({ method.test { it }
                             .indent() })
                 .chaining({ it });
}"))

(ert-deftest kotlin-mode--object-and-chaining ()
  (test-indent
   "
class T {
    var obj = object : R {
        fun sum(a: Int, b: Int): Int {
            var me = this.is({ method.test { it }
                                     .indent() })
                         .chaining({ it });
        }
    }
}"))

(ert-deftest kotlin-mode--object-and-chaining-in-lambda ()
  (test-indent
   "
class T {
    var obj = object : R {
        fun sum(a: Int, b: Int): Int {
            var me = obj.apply { x ->
                method.test { it }
                      .indent()
            }
                        .chaining({ it })
        }
    }
}"))

(ert-deftest kotlin-mode--straight-lambda ()
  (test-indent
   "
fun sum(a: Int, b: Int): Int {
    var me = obj.apply { x ->
        method.test { it }
    }
}"))


(ert-deftest kotlin-mode--lambda-from-new-line ()
  (test-indent
   "
fun sum(a: Int, b: Int): Int {
    var me = obj.apply { 
        x ->
            method.test { x }
    }
}"))
