;;; kotlin-mode.el --- Major mode for kotlin -*- lexical-binding: t; -*-

;; Copyright © 2015  Shodai Yokoyama

;; Author: Shodai Yokoyama (quantumcars@gmail.com)
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'rx)

(defgroup kotlin nil
  "A Kotlin major mode."
  :group 'languages)

(defcustom kotlin-tab-width default-tab-width
  "The tab width to use for indentation."
  :type 'integer
  :group 'kotlin-mode
  :safe 'integerp)

(defvar kotlin-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<tab>") 'c-indent-line-or-region)
    map)
  "Keymap for kotlin-mode")

(defvar kotlin-mode-syntax-table
  (let ((st (make-syntax-table)))

    ;; Strings
    (modify-syntax-entry ?\" "\"" st)

    ;; `_' as being a valid part of a word
    (modify-syntax-entry ?_ "w" st)

    ;; b-style comment
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st))


;;; Font Lock

(defconst kotlin-mode--misc-keywords
  '("package" "import"))

(defconst kotlin-mode--type-decl-keywords
  '("nested" "inner" "data" "class" "interface" "trait" "typealias" "enum" "object"))

(defconst kotlin-mode--fun-decl-keywords
  '("fun"))

(defconst kotlin-mode--val-decl-keywords
  '("val" "var"))

(defconst kotlin-mode--statement-keywords
  '(;; Branching
    "if" "else"
    ;; Exceptions
    "try" "catch" "finally" "throw"
    ;; Loops
    "while" "for" "do" "continue" "break"
    ;; Miscellaneous
    "when" "is" "in" "as" "return"))

(defconst kotlin-mode--context-variables-keywords
  '("this" "super"))

(defvar kotlin-mode--keywords
  (append kotlin-mode--misc-keywords
          kotlin-mode--type-decl-keywords
          kotlin-mode--fun-decl-keywords
          kotlin-mode--val-decl-keywords
          kotlin-mode--statement-keywords
          kotlin-mode--context-variables-keywords)
  "Keywords used in Kotlin language.")

(defconst kotlin-mode--constants-keywords
  '("null" "true" "false"))

(defconst kotlin-mode--modifier-keywords
  '("open" "private" "protected" "public" "lateinit"
    "override" "abstract" "final" "companion"
    "annotation" "internal" "const" "in" "out")) ;; "in" "out"

(defconst kotlin-mode--property-keywords
  '("by")) ;; "by" "get" "set"

(defconst kotlin-mode--initializer-keywords
  '("init" "constructor"))

(defvar kotlin-mode--font-lock-keywords
  `(;; Keywords
    (,(rx-to-string
       `(and bow (group (or ,@kotlin-mode--keywords)) eow)
       t)
     1 font-lock-keyword-face)

    ;; Package names
    (,(rx-to-string
       `(and (or ,@kotlin-mode--misc-keywords) (+ space)
             (group (+ (any word ?.))))
       t)
     1 font-lock-string-face)

    ;; Types
    (,(rx-to-string
       `(and bow upper (group (* (or word "<" ">" "." "?" "!"))))
       t)
     0 font-lock-type-face)

    ;; Classes/Enums
    (,(rx-to-string
       `(and bow (or ,@kotlin-mode--type-decl-keywords) eow (+ space)
             (group (+ word)) eow)
       t)
     1 font-lock-type-face)

    ;; Constants
    (,(rx-to-string
       `(and bow (group (or ,@kotlin-mode--constants-keywords)) eow)
       t)
     0 font-lock-constant-face)

    ;; Value bindings
    (,(rx-to-string
       `(and bow (or ,@kotlin-mode--val-decl-keywords) eow
             (+ space)
             (group (+ word)) (* space)  (\? ":"))
       t)
     1 font-lock-variable-name-face t)

    ;; Function names
    (,(rx-to-string
       `(and (or ,@kotlin-mode--fun-decl-keywords)
             (+ space) bow (group (+ (any alnum word))) eow)
       t)
     1 font-lock-function-name-face)

    ;; Access modifier
    ;; Access modifier is valid identifier being used as variable
    ;; TODO: Highlight only modifiers in the front of class/fun
    (,(rx-to-string
       `(and bow (group (or ,@kotlin-mode--modifier-keywords))
             eow)
       t)
     1 font-lock-keyword-face)

    ;; Properties
    ;; by/get/set are valid identifiers being used as variable
    ;; TODO: Highlight keywords in the property declaration statement
    ;; (,(rx-to-string
    ;;    `(and bow (group (or ,@kotlin-mode--property-keywords)) eow)
    ;;    t)
    ;;  1 font-lock-keyword-face)

    ;; Constructor/Initializer blocks
    (,(rx-to-string
       `(and bow (group (or ,@kotlin-mode--initializer-keywords)) eow)
       t)
     1 font-lock-keyword-face)

    ;; String interpolation
    (kotlin-mode--match-interpolation 0 font-lock-variable-name-face t))
  "Default highlighting expression for `kotlin-mode'")

(defun kotlin-mode--new-font-lock-keywords ()
  '(
    ("package\\|import" . font-lock-keyword-face)
    ))

(defun kotlin-mode--syntax-propertize-interpolation ()
  (let* ((pos (match-beginning 0))
         (context (save-excursion
                    (save-match-data (syntax-ppss pos)))))
    (when (nth 3 context)
      (put-text-property pos
                         (1+ pos)
                         'kotlin-property--interpolation
                         (match-data)))))

(defun kotlin-mode--syntax-propertize-function (start end)
  (let ((case-fold-search))
    (goto-char start)
    (remove-text-properties start end '(kotlin-property--interpolation))
    (funcall
     (syntax-propertize-rules
      ((let ((identifier '(any alnum " !%&()*+-./:<>?[]^_|~")))
         (rx-to-string
          `(or (group "${" (* ,identifier) "}")
               (group "$" (+ ,identifier)))))
       (0 (ignore (kotlin-mode--syntax-propertize-interpolation)))))
     start end)))

(defun kotlin-mode--match-interpolation (limit)
  (let ((pos (next-single-char-property-change (point)
                                               'kotlin-property--interpolation
                                               nil
                                               limit)))
    (when (and pos (> pos (point)))
      (goto-char pos)
      (let ((value (get-text-property pos 'kotlin-property--interpolation)))
        (if value
            (progn (set-match-data value)
                   t)
          (kotlin-mode--match-interpolation limit))))))

(defun kotlin-paren-level () (nth 0 (syntax-ppss)))
(defun kotlin-rewind-to-start-of-inner-expr () (goto-char (nth 1 (syntax-ppss))))
(defun kotlin-in-str-or-cmnt () (nth 8 (syntax-ppss)))
(defun kotlin-rewind-past-str-cmnt () (goto-char (nth 8 (syntax-ppss))))
(defun kotlin-rewind-to-previous-paren () (while (not (looking-at "(")) (backward-char 1)))
(defun kotlin-prev-line ()
  "Moves up to the nearest non-empty line. This doesn't support comments between
between function chaining or in function calls or declarations."
  (if (not (bobp))
      (progn
        (forward-line -1)
        (while (and (looking-at "^[ \t]*$") (not (bobp)))
          (forward-line -1)))))

(defun rewind-to-beginning-of-expr ()
  "this defun is trying to find the start of the expression backward. At the
  end, the point should be on the first character of the expression. 

  This function is mainly used in function chaining to point at the right level
  of indentation."
  (let ((current-level (kotlin-paren-level))
        (expected-indentation (* kotlin-tab-width (kotlin-paren-level))))
    (while (or (and (>= (kotlin-paren-level) current-level)
                    (not (kotlin-is-decl))
                    (not (= expected-indentation (current-indentation))))
               (kotlin-is-method-chaining))
      
      (progn
        (kotlin-prev-line)
        (back-to-indentation))
      )

    ;; the previous while loop may put us one line above the desired start of
    ;; expression if we didn't match a declaration. We fix that here
    ;; FIXME: kotlin-prev-line can jump empty lines and we dont take that into
    ;; account. It is not that bad since we will end up in the right inner level
    ;; anyway but we should investigate it.
    (if (> current-level (kotlin-paren-level))
        (if (kotlin-is-fun-or-class)
            (kotlin-get-down-in-fun-or-class)
          (while (not (= current-level (kotlin-paren-level)))
            (down-list)))))
  )

(defvar kotlin-decl-re (regexp-opt '("var" "val" "companion" "class" "fun"
                                     "data")))

(defvar kotlin-fun-class-decl-re (regexp-opt '("fun" "class")))

(defun kotlin-get-down-in-fun-or-class ()
  (end-of-line nil)
  (search-backward "{" nil 1 1)
  (message (string (following-char)))
  (down-list)
  (forward-line)
  (message (string (following-char))))

(defun kotlin-is-decl ()
  (save-excursion
    (back-to-indentation)
    (looking-at kotlin-decl-re)
    ))

(defun kotlin-is-fun-or-class ()
  (save-excursion
    (back-to-indentation)
    (looking-at kotlin-fun-class-decl-re)
    ))
        
(defun kotlin-is-method-chaining ()
  "Return true if the current line is a method chaining"
  (save-excursion
    (back-to-indentation)
    (looking-at "\\."))
  )

(defun kotlin-find-last-chain-member-on-line ()
  "looks for the last call on '.' for this line. The lookup is done
  backward in order to prevent the return of a false positive. 
  ex: test.foo(bar.prop)
          ^       ^
          |       not this
          + this

  This is mostly called for method chaining alignement in order to
point at the last method chainer. 
"
  (save-excursion
    (let ((start-position (point))
          (current-level (kotlin-paren-level)))
      (message (string (following-char)))
      (move-end-of-line nil)

      ;; iterate until we find a . that is not in an inner expression
      (while (and (> (point) start-position)
                  (or (not (search-backward "." start-position 1 1))
                      (> (kotlin-paren-level) current-level)))
        t)

      ;; Make sure that we found that . otherwise inform the caller
      (if (not (looking-at "\\."))
          nil
        (point)))
      ))

(defun kotlin-mode--indent-line ()
  "Indent current line as kotlin code"
  (interactive)
  (save-excursion
      (back-to-indentation)
      (cond ((looking-at "}\\|)") ; line starts with .
             (setq cur-indent (* kotlin-tab-width (- (kotlin-paren-level) 1))))

            ((looking-at "\\.") ;; We are in a function chaining case so we are
                                ;; trying to go to the beginning of the
             ;; statement
             (let ((line-to-indent-level (kotlin-paren-level)))
             (save-excursion
               (rewind-to-beginning-of-expr)
               (let ((chainer-position (kotlin-find-last-chain-member-on-line)))
                 (if chainer-position
                     (progn (back-to-indentation)
                            (setq cur-indent (- chainer-position (line-beginning-position)))
                            )
                   (setq cur-indent (* kotlin-tab-width (+ 1 line-to-indent-level))))))
             ))

            ;; for all the cases that requires that we analyse the previous line
            ;; instead of the current line. This should help us to get some more
            ;; context and help to make a decision.
            (t
             (let ((line-to-indent-level (kotlin-paren-level)))
               (save-excursion
                 (kotlin-prev-line)
                 (back-to-indentation)
                 (cond (;; Ending with a coma, this must be line warp for a defun of
                        ;; or a funcall so we apply twice the indent as stated in
                        ;; java coding style from Oracle
                        (looking-at ".*,[ \t]*$")
                        (setq cur-indent (+ (current-indentation)
                                             (* 2 kotlin-tab-width))))
                       


                       ;; Any other case is getting a normal indentation
                       ;; according to how deep we are nested
                       (t
                        (setq cur-indent (* kotlin-tab-width
                                            line-to-indent-level)))
                       )))))
      
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0))))

;;;###autoload
(define-derived-mode kotlin-mode prog-mode "Kotlin"
  "Major mode for editing Kotlin."

  (setq font-lock-defaults '((kotlin-mode--font-lock-keywords) nil nil))
  (setq-local syntax-propertize-function #'kotlin-mode--syntax-propertize-function)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'indent-line-function) 'kotlin-mode--indent-line)

  :group 'kotlin
  :syntax-table kotlin-mode-syntax-table)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode) t)

(provide 'kotlin-mode)
;;; kotlin-mode.el ends here
