#! /usr/bin/env racket

#lang racket/base

(require racket/cmdline
         racket/system
         racket/string
         racket/list
         racket/file
         "lib/utils.rkt"
         "lib/feature.rkt"
         "lib/parent.rkt")

(define help #<<MESSAGE

usage: git cdflow feature start <feature-name>
       git cdflow feature [-d, --delete-local] finish
       git cdflow feature checkout <feature-name>
       git cdflow feature public
       git cdflow feature private

       start    Create a new branch and switch the working tree
                to feature/<feature-name>.

                Example usage:
                git cdflow feature start myfeature

       finish   Switch to parent branch and merge there the current branch.
                The parent branch is the branch that generated the current one
                or the one that has been set with `git cdflow parent set`
                command.

                Example usage:
                git cdflow feature finish

       checkout Checking out a branch updates the files in the working
                directory.
                In case on origin there are both a private feature branch and a
                public one a menu will ask to the user to select the right one.

       public   Push the current branch on origin:
                origin/feature/<feature-name>

       private  Push the current branch on origin but on private path:
                origin/private/feature/<feature-name>
OPTIONS
        -d, --delete-local
            After merging the feature with its parent, delete the local feature branch

MESSAGE
)

(define delete-local? (make-parameter #f))

(define (start feature-name)
  (cond
    [feature-name (create-feature-branch (get-feature-branch feature-name))]
    [else (display-err "Please specify the name of the feature branch\n")]))

(define (public)
  (cond
    [(regexp-match #px"^feature\\/" (git-current-branch)) (git-push-origin (git-current-branch))]
    [else (display-err "Please move in a feature branch\n")]))

(define (private)
  (cond
    [(regexp-match #px"^feature\\/" (git-current-branch)) (git-push-origin (format "private/~a" (git-current-branch)))]
    [else (display-err "Please move in a feature branch\n")]))

(define (checkout branch-name)
  (cond
    [(not branch-name) (display-err "No branch selected.\nUsage: git cdflow feature checkout <branch-name>\n")]
    [(git-local-branch-exists (get-feature-branch branch-name)) (git-checkout-branch (get-feature-branch branch-name))]
    [else (let ([remote-branch (git-select-remote-feature-branch branch-name)])
            (cond [remote-branch
                   (git-checkout-remote-branch-to remote-branch (get-feature-branch branch-name))]))]))

(define (finish)
  (let ([current-branch (git-current-branch)]
        [parent (get-parent)]
        [files-to-commit (git-files-to-commit)])
    (cond
      [(not (equal? files-to-commit '())) (display-err "There are files to commit. Aborted!\n") ]
      [(not parent) (display-err "No parent has been set, please see `git cdflow parent help`\n") ]
      [(not (regexp-match #px"^feature\\/" current-branch)) (display-err "Please move in a feature branch\n")]
      [else (close-feature-branch)
            (when (delete-local?) (git-delete-branch (get-feature-branch current-branch)))])))

(define (main)
  (let-values (
    [(action feature-name)
      (command-line
       #:once-each
       [("-d" "--delete-local") "Delete the local feature branch" (delete-local? #t)]
        #:args (action [feature-name #f])
        (values action feature-name))])

    (cond
      [(equal? action "help") (display help)]
      [(equal? action "start") (start feature-name)]
      [(equal? action "public") (public)]
      [(equal? action "private") (private)]
      [(equal? action "checkout") (checkout feature-name)]
      [(equal? action "finish") (finish)]
      [else (display help)])))

;; (current-directory "/Users/rserra/dev/etiqa-website")

(void (main))
