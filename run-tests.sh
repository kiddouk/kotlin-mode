#!/bin/bash

emacs -batch -l kotlin-mode-test.el -f ert-run-tests-batch-and-exit
