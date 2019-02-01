#! /usr/bin/env racket

#lang racket/base

(require racket/cmdline
         racket/system
         racket/string
         racket/list
         racket/file
         "lib/utils.rkt"
         "lib/release.rkt")

(define help #<<MESSAGE

usage: git cdflow release list
       git cdflow release start [--no-push] <version> [<base>]

       list       Show the list of release branches available on origin.
                  Local branches are ignored and branches with wrong syntax are
                  ignored. In order to be considered a valid release branch,
                  the branch has to match the following name convention:
                  release/v[major].[minor].[maintenance], eg. release/v9.1.0

        start     Start a new release.
                  The <version> parameter is mandatory but it's possible
                  to specify partial version name.
                  All the following version names are valid: v8.0.0, 8, 8.0,
                  8.0.0.
                  In all the above scenarios the branch release/v8.0.0 will be
                  created.

                  The <base> parameter is optional and it's intended for
                  automation.

                  In case the base parameter is missing, a menu will be
                  displayed to user in order to choose the release to branch
                  from. The release to branch from must be present on origin
                  and it has to follow the right name convention
                  (see "git cdflow release list")

                  Example usage:
                  git cdflow release start 10

         checkout Checkout a release branch.

MESSAGE
)

(define push? (make-parameter #t))

(define sh-release-list
  "git branch -r | sed 's/origin\\/HEAD.*//' | egrep -i \"release/v[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}$\" | cut -d/ -f3")

(define (release-branches)
  (sh->list "git branch -r | sed 's/origin\\/HEAD.*//' | egrep -i \"release/v[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}$\" | cut -d/ -f3"))

;mobe to lib
(define (release-exists? version)
  (member (release-name version) (release-branches)))

(define (show-release-from-menu branches)
  (let ([menu-items (take-upto (sort-releases branches #t) 10)])
    (show-menu "Select the release branch you want to branch from" menu-items 0)))

(define (branch-release-from [base #f])
  (cond
    [base base]
    [(equal? (release-branches) '()) "master"]
    [else (string-append
            "release/"
            (show-release-from-menu (release-branches)))]))

(define (show-releases)
  (git-fetch)
  (display (sh->string sh-release-list)))

(define (sort-by-depth file-list)
  (map cdr (sort (map (lambda (e) (cons (length (regexp-split #rx"/" e)) e)) file-list) #:key car <)))

(define (select-file filename title [exclude #f])
  (let ([files (sort-by-depth (sh->list (format "find . ~a -name '~a'" (if exclude (string-append "-not -path */" exclude "/*") "") filename)))])
    (cond
      [(equal? files '()) #f]
      ;[(= (length files) 1) (car files)]
      [(> (length files) 0) (show-menu title (cons "NONE" files) 1)])))

(define (project-file filename [exclude #f])
  (select-file filename "Select the project file to update" exclude))

(define (project.clj)
  (project-file "project.clj"))

(define (project-clj-set-version file version)
  (display-to-file
    (replace-clj-project-version (file->string file) version)
    file
    #:exists 'replace))

(define (handle-clojure-project version)
  (let ([file (project.clj)])
    (cond [file (project-clj-set-version file version)
                (git-commit file "Version number updated")]
          [else #f])))

;;; Handling node projects

(define (project-node-set-version file version)
  (display-to-file
    (replace-node-project-version (file->string file) version)
    file
    #:exists 'replace))

(define (package.json)
  (project-file "package.json" "node_modules"))

(define (handle-node-project version)
  (let ([file (package.json)])
    (cond [file (project-node-set-version file version)
                (git-commit file "Version number updated")]
          [else #f])))

(define (pom.xml)
  (project-file "pom.xml"))

(define (settings.xml)
  (select-file "settings.xml" "Do you want to use a custom settings.xml?"))

(define (pom-set-version file version [settings #f])
  (let ([new-version (version-snapshot version)])
    (sh (cond
       [settings (format "mvn -f ~a versions:set -DnewVersion='~a' --settings ~a" file new-version settings)]
       [else (format "mvn -f ~a versions:set -DnewVersion='~a'" file new-version)]))))

(define (versionset-modified)
  (map (lambda (item)
    (substring item 0 (- (string-length item) 15)))
    (sh->list "find . -iname 'pom.xml.versionsBackup'")))

(define (git-commit-versionset-modified)
  (git-commit (versionset-modified) "Version number updated"))

(define (remove-versionset-backup-files)
  (sh "find . -name \"pom.xml.versionsBackup\" -delete"))

(define (handle-maven-project version)
  (let ([file (pom.xml)]
        [settings (settings.xml)])
    (cond [file (pom-set-version file version settings)
                (git-commit-versionset-modified)
                (remove-versionset-backup-files)]
          [else #f])))

(define (git-create-release start version)
  (let ([new-branch (release-branch version)])
    (git-branch-from start new-branch)
    ;Set project version
    (or (handle-clojure-project version)
        (handle-maven-project version)
        (handle-node-project version))
    (cond
      [(push?) (git-push-origin new-branch)
               (git-notes-push)])))

(define (display-help)
  (display help))

(define (create-release version base)
  (cond
    [(not version) (err "Missing release version" display-help)]
    [(release-exists? version) (err "Release already exists")]
    [(not (release-name version)) (err "Invalid release name")]
    [else
      (git-fetch)
      (git-create-release (branch-release-from base) version)]))

(define (checkout version)
  (cond
    [(not version) (display-err "Missing version.\nUsage: git cdflow checkout <version-number>\n")]
    [else  (void (git-fetch))
           (git-checkout-branch (release-branch version))]))

(define (push)
  (cond
    [(release-branch? (git-current-branch))
       (git-push-origin (git-current-branch))
       (git-notes-push)]
    [else (display-err "You are not in a release branch.\n")]))


(define (main)
  ;; (handle-node-project "3")
  (let-values (
    [(action version base)
      (command-line
        #:once-each
        [("--no-push") "The release will not be pushed on origin." (push? #f)]
        #:args (action [version #f] [base #f])
        (values action version base))])

    (cond
      [(equal? action "help") (display help)]
      [(equal? action "list") (show-releases)]
      [(equal? action "checkout") (checkout version)]
      [(equal? action "push") (push)]
      [(equal? action "start") (create-release version base)])))

(void (main))
