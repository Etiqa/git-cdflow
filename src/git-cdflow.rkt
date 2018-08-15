#! /usr/bin/env racket

#lang racket/base

(require racket/cmdline
         racket/system
         racket/string
         racket/path)

(define version "git-cdflow version 0.2.0.SNAPSHOT")

(define help #<<MESSAGE
usage: git cdflow <command>

Available command are:
   feature   Manage your feature branches.
   release   Manage your release branches.
   parent    Manage updates from parent branch.
   tree      Show information about the whole releases tree.
   issue     Manage issues on tracker system.
   version   Shows version information.

Try 'git cdflow <command> help' for details.

MESSAGE
)

(define subcommands '("feature" "release" "parent" "tree" "issue"))

(define (subcommand command params)
  (void (system (string-join (cons (string-append "git-cdflow-" command) params)))))

; Main Body
(define args (vector->list (current-command-line-arguments)))

(cond
  [(= (length args) 0) (display help)]
  [(equal? (car args) "version") (displayln version)]
  [(member (car args) subcommands) (subcommand (car args) (cdr args))]
  [else (display help)])
;(command-line #:args (filename) filename)
