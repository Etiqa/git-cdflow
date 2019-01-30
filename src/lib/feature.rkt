#! /usr/bin/env racket

#lang racket/base

(require "../lib/utils.rkt")
(require "../lib/parent.rkt")
(require "../lib/release.rkt")

(provide (all-defined-out))

(define (create-feature-branch feature-branch)
  (let ([current-branch (git-current-branch)])
    (git-branch feature-branch)
    (git-notes-add-parent current-branch feature-branch)))

(define (feature-branch? branch)
  (regexp-match #px"^feature\\/" branch))

(define (try-close-in-remote-branch parent-branch)
  (let ([remote-branch (git-select-remote-feature-branch parent-branch)]
        [current-branch (git-current-branch)])
    (cond
      [remote-branch (git-checkout-remote-branch-to remote-branch parent-branch)
                     (git-merge current-branch)]
      [else (display-err "Remote branch not available or selected. Aborted!")])))

(define (close-feature-branch)
  (let ([current-branch  (git-current-branch)]
        [parent-branch  (get-parent)])
    (cond
      [(git-local-branch-exists parent-branch) (git-merge-from-to current-branch parent-branch)]
      [(release-branch? parent-branch) (git-checkout-remote-branch-to parent-branch parent-branch)
                                       (git-merge current-branch)]
      [else (try-close-in-remote-branch parent-branch)])))

(define (get-feature-branch feature-name)
  (cond
    [(not feature-name) #f]
    [(regexp-match #px"^feature\\/" feature-name) feature-name]
    [else (format "feature/~a" feature-name)]))

(define (git-select-remote-feature-branch branch-name)
  (git-fetch)
  (let* ([feature-branch (get-feature-branch branch-name)]
         [possible-branches (list feature-branch (format "private/~a" feature-branch))]
         [branches  (filter git-remote-branch-exists possible-branches)])
    (cond
      [(not (equal? branches '())) (show-menu "Select remote branch" (cons "NONE" branches) 1)]
      [else #f])))
