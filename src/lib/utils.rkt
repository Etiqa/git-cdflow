#lang racket/base

(require charterm
         racket/system
         racket/string
         racket/list
         racket/format
         racket/match
         (only-in racket/port with-output-to-string))

(provide (all-defined-out))

(define (get-settings-folder) (expand-user-path "~/.cdflow"))

(define (err m [callback #f])
  (printf "\033[0;31m~a\033[0m\n" m)
  (cond [callback (callback)]))

(define (display-err msg)
  (display msg (current-error-port)))

(define (sh command)
  (cond [(not (system command)) (exit)]))

(define (list<? l1 l2)
  (cond
    [(equal? l1 '()) #t]
    [(< (car l1) (car l2)) #t]
    [(> (car l1) (car l2)) #f]
    [else (list<? (cdr l1) (cdr l2))]))

(define (take-upto l n)
  (take l (min n (length l))))

(define (string->list str)
  (string-split str "\n"))

(define (list->string lst)
  (string-join lst "\n"))

(define (sh->string command)
  (with-output-to-string (lambda () (sh command))))

(define (sh->list command)
  (string-split (sh->string command) "\n"))

(define (sh->bool command)
  (system command))

(define (menu items n-active)
  (let ([active (list-ref items n-active)])
    (map
      (lambda (item)
        (if (eqv? item active)
          (printf "  \033[36mο\033[0m ~a\033[0m\n" item)
          (printf "    \033[90m~a\033[0m\n" item)))
        items)))

(define (clear-terminal-screen)
  (with-charterm
    (unless (equal? (charterm-protocol (current-charterm)) 'ascii)
      (charterm-clear-screen))))

(define (show-menu title items [n-active 0])
  (clear-terminal-screen)

  (printf "~a\n\n" title)
  (menu items n-active)

  (let ([char (with-charterm (charterm-read-key))]
        [prev (if (> n-active 0) (- n-active 1) 0)]
        [next (if (= n-active (- (length items) 1))
                  (- (length items) 1)
                  (+ n-active 1))])

    (cond
      [(eqv? char 'down) (show-menu title items next)]
      [(eqv? char 'up) (show-menu title items prev)]
      [(eqv? char 'return) (let ([val (list-ref items n-active)])
                             (if (string=? val "NONE") #f val))]
      [else (exit)])))


(define (multichoice-menu items active selected)
  (let ([checked "\033[36m[\u2713]"]
        [unchecked "\033[90m[ ]"]
        [text "\033[90m"]
        [text-active "\033[0m"])

  (for ([i (in-naturals)]
        [item items])
        (if (member i selected)
          (printf "  ~a" checked)
          (printf "  ~a" unchecked))

        (if (eqv? i active)
          (printf " ~a~a \033[0m\n" text-active item)
          (printf " ~a~a \033[0m\n" text item)))))

(define (toggle el li)
  (if (member el li) (remv el li) (cons el li)))

(define (select idx l)
  (foldr (lambda (id ret) (cons (list-ref l id) ret)) '() idx))

(define (show-multichoice-menu title items active selected)
  (clear-terminal-screen)

  (printf "~a\n\n" title)
  (multichoice-menu items active selected)

  (let ([char (with-charterm (charterm-read-key))]
        [prev (modulo (- active 1) (length items))]
        [next (modulo (+ active 1) (length items))])

    (cond
      [(eqv? char 'down) (show-multichoice-menu title items next selected)]
      [(eqv? char 'up) (show-multichoice-menu title items prev selected)]
      [(eqv? char #\space) (show-multichoice-menu title items active (toggle active selected))]
      [(eqv? char 'return) (select (sort selected <) items)]
      [else (exit)])))

(define (git-checkout-branch branch-name)
  (sh (format "git checkout ~a" branch-name)))

(define (git-current-branch)
  (car (sh->list "git rev-parse --abbrev-ref HEAD")))

(define (git-checkout-remote-branch-to remote-branch-name local-branch-name)
  (sh (format "git checkout -b ~a origin/~a" local-branch-name remote-branch-name)))

(define (git-branch branch-name)
  (sh (format "git checkout -b ~a" branch-name)))

(define (git-add files)
  (for-each (lambda (file)
    (sh (format "git add \"~a\"" file)))
    (if (string? files) `(,files) files)))

(define (git-commit files message)
  (git-add files)
  (sh (format "git commit -m \"~a\"" message)))

(define (git-pull)
  (sh "git pull"))

(define (git-notes path)
  (sh->list "git log --show-notes=cdflow"))

(define (git-objects-with-notes path)
    (map (lambda (row)
      (cadr (string-split row " ")))
      (sh->list (format "cd ~a; git notes --ref=cdflow" path))))

(define (git-object-show-notes path obj)
  (sh->string (format "cd ~a; git notes --ref=cdflow show ~a" path obj)))

(define (git-objects-notes path)
  (map (lambda (obj)
    (list obj (git-object-show-notes path obj)))
    (git-objects-with-notes path)))

(define (git-notes-add-parent from to)
  (sh
    (format "git notes --ref cdflow append -m \"[~a -> ~a]\"" from to)))

(define (git-notes-add-issue-note type url)
  (let ([note-key (string-append "issue-tracker-" type)])
    (sh
      (format "git notes --ref cdflow append -m \"[~a :: ~a]\"" note-key url))))

(define (git-notes-replace notes object)
  (sh (format "git notes --ref cdflow add -f -m \"~a\" ~a" notes object)))

(define (git-notes-push)
  (sh "git push origin refs/notes/cdflow"))

(define (git-remote-branch-exists branch-name)
  (non-empty-string? (sh->string (format "git branch -r --list \"origin/~a\"" branch-name))))

(define (git-remote-branch-exists? repo-path branch-name)
  (non-empty-string? (sh->string (format "cd ~a; git branch -r --list \"origin/~a\"" repo-path branch-name))))

(define (git-remote-branches-list repo-path)
  (map (lambda (row) (string-replace row #px"\\s*origin/" "")) (sh->list (format "cd ~a; git branch -r --list" repo-path))))

(define (git-local-branch-exists branch-name)
  (non-empty-string? (sh->string (format "git branch -l --list \"~a\"" branch-name))))

(define (git-remote-commits repo-path)
  (map (lambda (lst) (list (string-replace (caddr lst) "refs/remotes/origin/" "") (car lst)))
    (map (lambda (line) (string-split line #px"[\\s]+"))
      (sh->list (format "git --git-dir=~a/.git for-each-ref refs/remotes/origin/" repo-path)))))

(define (git-remote-branch-contains-commit? repo-path branch commit)
  (match (sh->string (format "git --git-dir=~a/.git for-each-ref refs/remotes/origin/~a --contains=~a" repo-path branch commit))
    [(? non-empty-string?) #t]
    [_ #f]))

(define (git-branch-from from to)
  (git-checkout-branch from)
  (git-pull)
  (git-branch to)
  (git-checkout-branch to)
  (git-notes-add-parent from to))

(define (git-files-to-commit)
  (sh->list "git status --untracked-files=no --porcelain"))

(define (git-fetch)
  (sh "git fetch origin")
  (system "git fetch origin \"refs/notes/cdflow:refs/notes/origin/cdflow\" 2>/dev/null")
  (system "git notes --ref cdflow merge -s theirs origin/cdflow 2>/dev/null"))

;Push the current branch to origin with the branch name passed
(define (git-push-origin branch)
  (git-notes-push)
  (sh (format "git push -u origin HEAD:~a" branch)))

(define (git-merge from)
  (sh (format "git merge ~a" from)))

(define (git-merge-from-to from to)
  (git-checkout-branch to)
  (git-merge from))

(define (git-delete-branch branch)
  (sh (format "git branch -d ~a" branch)))

(define (git-delete-remote-branch branch)
  (sh (format "git push :~a" branch)))

(define (open-browser-page url)
  (sh (string-append "open " url)))

(define (extract-version-number str)
  (let ([v-num (regexp-match #px"^v[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}" str)])
    (if (and v-num (= (length v-num) 1))
      (car v-num)
      str)))
