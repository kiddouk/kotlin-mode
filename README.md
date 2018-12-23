# Emacs Kotlin Mode

[![Build Status](https://secure.travis-ci.org/kiddouk/kotlin-mode.png?branch=master,staging,production)](http://travis-ci.org/kiddouk/kotlin-mode)

This mode is a fork of the original Kotlin-Mode that is published in [in this repo](https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode).

As the time of writing, the official repo has stalled for the development and I had an urge for trying my developper skills at Lisp. This is work in progress.

Here are a few things that this version of kotlin-mode is trying to address compared to the original mode:
* correct method chaining that aligns dots correctly
* long function calls that has parameter names on a new line
* getter and setter indentation
* closing parenthesis back indentation
* more comprehensive integration tests

All this comes with one restriction though:

This major mode is more strict has it always try to assign an indentation according to what has been parsed. This means that, for now, you cannot change the previous line's indentation and hope to get the rest of the file align on that.

Credits:
* The original developpers of kotlin-mode
* Sebastien Requiem
* You maybe? Send your patches so we can enjoy kotlin on emacs.

