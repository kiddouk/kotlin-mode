#!/bin/bash

cd ../
emacs -batch -l test/kotlin-mode-test.el -f ert-run-tests-batch-and-exit
cd test
