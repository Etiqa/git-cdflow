#! /usr/bin/env racket

#lang racket/base

(require racket/cmdline)
(require racket/system)
(require racket/string)
(require "lib/utils.rkt"
         "lib/issue.rkt"
         "lib/parent.rkt"
         "lib/feature.rkt")

(define-values (in out) (make-pipe))

(define help #<<MESSAGE
usage: git cdflow issue start
       git cdflow issue finish
       git cdflow issue config
       git cdflow issue status

       start    List of all issues in the 'opened' status.
                Select an issue from the list and create a new branch and
                switch the working tree to feature/<issue-id>.

                Put the issue in the 'in-progress' status.

       finish   Switch to parent branch and merge there the current branch.
                The parent branch is the branch that generated the current one
                or the one that has been set with `git cdflow parent set`
                command.

                Put the issue in the 'resolved' status.

       config   Start the configuration process in order to setup a Redmine
                Issue Tracker linked with the repository.

       status   Show the current configuration.

MESSAGE
  )

(define (display-insert-setting-label label)
  (display (string-append label " (enter to confirm, empty string will be ignored) \n")))

(define (configure-issue-url)
  (let []
    (display-insert-setting-label "Insert Issue Tracker URL:")
    (define url (read-line))
    (if (non-empty-string? url)
      (let []
        (git-fetch)
        (git-notes-remove-issue "url")
        (git-notes-add-issue-note "url" url)
        (git-notes-push))
        #f)))

(define (configure-issue-project)
  (let []
    (display-insert-setting-label "Insert Issue Tracker Project:")
    (define project (read-line))
    (if (non-empty-string? project)
      (let []
        (git-fetch)
        (git-notes-remove-issue "project")
        (git-notes-add-issue-note "project" project)
        (git-notes-push))
        #f)))

(define (configure-api-key)
  (display-insert-setting-label "Insert Issue Tracker Api Key:")
  (define key (read-line))
  (if (non-empty-string? key)
    (let []
      (create-settings-folder-if-not-exists)
      (save-setting "apikey" key))
      #f))

(define (select-issue-status statuses type)
  (let* ([item (show-menu
                 (format "Select the \"~a\" status from the list:\n" type)
                 (map (lambda (s) (string-append (format "~a : ~a" (hash-ref s 'id) (hash-ref s 'name)))) statuses)
                 0)])
    (string-trim (car (string-split item ":")))
    ))

(define (configure-issue-statuses)
  (let* ([resp (call-tracker-api "GET" "issue_statuses.json")]
         [statuses (hash-ref resp 'issue_statuses)]
         [open (select-issue-status statuses "OPEN")]
         [in-progress (select-issue-status statuses "IN PROGRESS")]
         [resolved (select-issue-status statuses "RESOLVED")])

    (clear-terminal-screen)

    (display "Pushing settings...\n")

    (git-fetch)

    (git-notes-remove-issue "status-open")
    (git-notes-remove-issue "status-in-progress")
    (git-notes-remove-issue "status-resolved")

    (git-notes-add-issue-note "status-open" open)
    (git-notes-add-issue-note "status-in-progress" in-progress)
    (git-notes-add-issue-note "status-resolved" resolved)

    (git-notes-push)))

(define (issue-configuration)
  (configure-issue-url)
  (configure-issue-project)
  (configure-api-key)

  (if (and (is-set-tracker-note? "url") (is-set-tracker-note? "project") (is-set-apikey?))
    (configure-issue-statuses)
    #f
  )
  (issue-status))

(define (is-configuration-ok?)
  (if (and
        (is-set-tracker-note? "url")
        (is-set-tracker-note? "project")
        (is-set-apikey?)
        (is-set-tracker-note? "status-open")
        (is-set-tracker-note? "status-in-progress")
        (is-set-tracker-note? "status-resolved")
        )
    #t
    #f))

(define (issue-status)
  (display "ISSUE TRACKER SETUP\n\n")
  (display "[Redmine URL]: \t")
  (if (is-set-tracker-note? "url")
    (display (get-issue-note "url"))
    (display "-"))

  (display "\n[Project]: \t")
  (if (is-set-tracker-note? "project")
    (display (get-issue-note "project"))
    (display "-"))

  (display "\n[API Key]: \t")
  (if (is-set-apikey?)
    (display (get-setting "apikey"))
    (display "-"))

  (display "\n[open]: \t")
  (if (is-set-tracker-note? "status-open")
    (display (get-issue-note "status-open"))
    (display "-"))

  (display "\n[in-progress]: \t")
  (if (is-set-tracker-note? "status-in-progress")
    (display (get-issue-note "status-in-progress"))
    (display "-"))

  (display "\n[resolved]: \t")
  (if (is-set-tracker-note? "status-resolved")
    (display (get-issue-note "status-resolved"))
    (display "-"))

  (display "\n\nCONFIGURATION: ")
  (if (is-configuration-ok?)
    (display "OK\n\n")
    (display "Not Ready! Run: git cdflow issue config\n\n")
  ))

(define (put-issue-inprogress id)
  (let* ([data (format "{\"issue\":{\"status_id\":\"~a\"}}" (get-issue-note "status-in-progress"))])
    (call-tracker-api "PUT" (format "issues/~a.json" id) "" data)))

(define (resolve-issue id)
  (let* ([data (format "{\"issue\":{\"status_id\":\"~a\"}}" (get-issue-note "status-resolved"))])
    (call-tracker-api "PUT" (format "issues/~a.json" id) "" data)))

(define (get-my-issues-list)
  (if (is-configuration-ok?)
    (let* ([query (string-append "assigned_to_id=me&sort=fixed_version:desc,priority:desc&status_id=" (get-issue-note "status-open"))]
           [resp (call-tracker-api "GET" "issues.json" query)]
           [issues (hash-ref resp 'issues)]
           [item (show-menu "Select the issue to close" (map (lambda (s) (build-issue-row s)) issues) 0)]
           [issue-id (string-replace (car (regexp-match #px"#\\d+" item)) "#" "")]
           [new-feature-name (string-append "feature/issue-" issue-id)])

           (clear-terminal-screen)

           (if (feature-branch? (git-current-branch))
             (let []
                  (git-checkout-branch (get-parent))
                  (git-pull))
             #f)

           (display (string-append "Starting " new-feature-name "\n"))

           (put-issue-inprogress issue-id)

           (if (git-local-branch-exists new-feature-name)
              (let []
                (git-checkout-branch new-feature-name)
                (git-pull))

              (create-feature-branch new-feature-name))

           (open-browser-page (string-append (get-issue-note "url") "/issues/" issue-id))
      )
      (display "Project Issue Tracker not configured!\nRun: git cdflow issue config\n")))

(define (close-issue-branch)
  (let* ([feature (git-current-branch)]
         [issue-id (string-replace feature "feature/issue-" "")])
    (display (string-append "Finishing Issue: " issue-id "\n"))
    (resolve-issue issue-id)
    (close-feature-branch)
    (git-delete-branch feature)))

(define (issue-finish)
  (let ([current-branch (git-current-branch)]
        [parent (get-parent)]
        [files-to-commit (git-files-to-commit)])
    (cond
      [(not (equal? files-to-commit '())) (display-err "There are files to commit. Aborted!\n") ]
      [(not parent) (display-err "No parent has been set, please see `git cdflow parent help`\n") ]
      [(not (regexp-match #px"^feature\\/issue-" current-branch)) (display-err "Please move in a feature branch created from issue\n")]
      [else (close-issue-branch)])))

(define (main)
  (let-values (
    [(action)
      (command-line
        #:args ([action #f])
        (values action))])

    (cond
      [(equal? action "help") (display help)]
      [(equal? action "start") (get-my-issues-list)]
      [(equal? action "finish") (issue-finish)]
      [(equal? action "config") (issue-configuration)]
      [(equal? action "status") (issue-status)]
      )))

(void (main))
