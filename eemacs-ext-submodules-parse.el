;;; eemacs-ext-submodules-parse.el --- gitmodules parse library for eemacs-ext
;;
;; * Copyright (C) 2019  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; Package-Version: 0.1.0
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; #+END_EXAMPLE
;;
;; * Commentary:

;; Parse gitmodules and generate bash script file for batch operation.

;; This library get git modules into =MATCHED-LIST=, a alist to represent
;; one git submodule with its 'name' 'branch' 'url' 'path'.

;; Bash script can be generated for below aims:

;; - toggle-branch to tempo one
;; - toggle-branch to head's commit hash
;; - create one git submodules 'add' batch bash script

;; * Code:
;; ** require
(require 'files)
(require 'cl-lib)

;; ** variables pre
(defvar eemacs-ext/ggsh--root-dir (expand-file-name (file-name-directory load-file-name)))

(defvar eemacs-ext/ggsh--gitmodules-file
  (expand-file-name ".gitmodules" eemacs-ext/ggsh--root-dir))

(defvar eemacs-ext/ggsh--submodules-common-branch-toggle-batch-file
  (expand-file-name "annex/bin/submodules-common-toggle-branch.sh" eemacs-ext/ggsh--root-dir))

(defvar eemacs-ext/ggsh--submodules-adding-batch-file
  (expand-file-name "annex/bin/submodules-get.sh" eemacs-ext/ggsh--root-dir))

(defvar eemacs-ext/ggsh--submodules-toggle-final-release-batch-file
  (expand-file-name "annex/bin/submodules-toggle-final-release.sh" eemacs-ext/ggsh--root-dir))

(defvar eemacs-ext/ggsh--submodules-stick-upstream-batch-file
  (expand-file-name "annex/bin/submodules-stick-upstream.sh" eemacs-ext/ggsh--root-dir))

(defvar eemacs-ext/ggsh--entry-head-regexp
  "^\\[submodule \"\\([^ ]+\\)\"\\]$")


;; ** library
;; *** basic macro
(defmacro eemacs-ext/ggsh--append-submodule-object ($submodule-object submodule-prop)
  (declare (indent defun))
  `(let* ((module ,$submodule-object)
          (prop ,submodule-prop)
          (prop-key (car prop)))
     (while (assoc prop-key module)
       (setq module (remove (assoc prop-key module) module)))
     (append module (list prop))))

(defmacro eemacs-ext/ggsh--with-gitmodule-file-buffer (&rest body)
  (let ()
    `(with-current-buffer
         (find-file-noselect eemacs-ext/ggsh--gitmodules-file nil t)
       ,@body)))

(defmacro eemacs-ext/ggsh--with-submodule-status-check ($submodule-object &rest body)
  (declare (indent defun))
  `(let ((submodule-branch (alist-get 'branch ,$submodule-object))
         (submodule-dir (expand-file-name (alist-get 'path ,$submodule-object)
                                          eemacs-ext/ggsh--root-dir)))
     (if (and submodule-branch
              (file-exists-p
               (expand-file-name
                ".git"
                submodule-dir)))
         (progn ,@body)
       (cond
        ((null submodule-branch)
         (error "submodule '%s' doesn't follow any upstream branch!" submodule-dir))
        ((null (file-exists-p
                (expand-file-name
                 ".git"
                 submodule-dir)))
         (error "submodule '%s' doesn't initialized." submodule-dir))))))

;; *** basic functions
(defun eemacs-ext/ggsh--unquote-callback (str)
  (dolist (regexp '("^\"" "\"$"))
    (setq str (replace-regexp-in-string regexp "" str)))
  str)

;; *** check unregular submodules
(defun eemacs-ext/ggsh--check-unregular-submodule-path-name ($submodule-object &optional fbk)
  "Check whether submodule base name are different from the url
suffix name."
  (let ((url (cdr (assoc 'url $submodule-object)))
        (path (cdr (assoc 'path $submodule-object)))
        path-trail url-trail rtn)
    (setq path-trail
          (replace-regexp-in-string
           "^.*/\\([^ /]+\\)$" "\\1" path)
          url-trail
          (replace-regexp-in-string
           "^.*/\\([^ /]+?\\)\\(\\.git\\)?$" "\\1" url))
    (unless (equal url-trail path-trail)
      (when fbk
        (push $submodule-object (symbol-value fbk)))
      (setq rtn t))
    rtn))

;; *** gitmodule file parse
;; **** subroutine for parse gitmoudle file base
(defun eemacs-ext/ggsh--goto-entry-head ()
  (re-search-forward eemacs-ext/ggsh--entry-head-regexp nil t))

(defun eemacs-ext/ggsh--get-entry-region ()
  (let ((pcur (point))
        pend)
    (save-excursion
      (if (re-search-forward eemacs-ext/ggsh--entry-head-regexp
                             nil t)
          (setq pend (progn (forward-line -1)
                            (line-end-position)))
        (setq pend (point-max))))
    (cons pcur pend)))

;; **** subroutine for parse gitmoudle file core
;; ***** options
(defun eemacs-ext/ggsh--get-submodule-registered-commit ($submodule-object)
  (when (alist-get 'path $submodule-object)
    (let* ((path (alist-get 'path $submodule-object))
           (default-directory (expand-file-name
                               eemacs-ext/ggsh--root-dir))
           (commit (car
                    (process-lines
                     "git"
                     "submodule" "status" "--cached" path)))
           (commit-tail-regexp "[[:blank:]](.*)$")
           (commit-regexp-1 "^\\([[:blank:]-+]\\)\\([a-z0-9]+\\)[[:blank:]]\\(.+\\)$")
           (commit-regexp-2 "^\\([[:blank:]-+]\\)\\([a-z0-9]+\\)[[:blank:]]\\(.+\\)[[:blank:]]\\((.+)\\)$")
           commit-regexp)
      (setq commit-regexp
            (if (string-match-p commit-tail-regexp commit)
                (progn
                  commit-regexp-2)
              commit-regexp-1))
      (when (and (stringp commit)
                 (string-match commit-regexp commit))
        (let ((hash (match-string 2 commit))
              (path-indicate (match-string 3 commit))
              (status-char (match-string 1 commit)))
          (when (and (not (eq 0 (length hash)))
                     (file-equal-p (expand-file-name path eemacs-ext/ggsh--root-dir)
                                   (expand-file-name path-indicate eemacs-ext/ggsh--root-dir)))
            (setq $submodule-object
                  (eemacs-ext/ggsh--append-submodule-object
                    $submodule-object `(registered-head . ,hash)))
            (setq $submodule-object
                  (eemacs-ext/ggsh--append-submodule-object
                    $submodule-object `(status . ,status-char))))))))
  $submodule-object)

(defun eemacs-ext/ggsh--get-submodule-current-commit ($submodule-object)
  (eemacs-ext/ggsh--with-submodule-status-check $submodule-object
    (let* ((path (alist-get 'path $submodule-object))
           (default-directory (expand-file-name path eemacs-ext/ggsh--root-dir))
           current-commit-hash current-commit-subject)
      (setq current-commit-hash
            (car (process-lines
                  "git"
                  "show-ref"
                  "--head"
                  "-s" "1"
                  "--abbrev=8")))
      (setq current-commit-subject
            (append
             `(:title
               ,(eemacs-ext/ggsh--unquote-callback
                 (car (process-lines "git" "log" "--pretty=format:\"%s\"" "-1"))))
             `(:author
               ,(eemacs-ext/ggsh--unquote-callback
                 (car (process-lines "git" "log" "--pretty=format:\"%an\"" "-1"))))
             `(:email
               ,(eemacs-ext/ggsh--unquote-callback
                 (car (process-lines "git" "log" "--pretty=format:\"%ae\"" "-1"))))
             `(:date
               ,(eemacs-ext/ggsh--unquote-callback
                 (car (process-lines "git" "log" "--pretty=format:\"%cd\"" "-1"))))))
      (when current-commit-hash
        (setq $submodule-object
              (eemacs-ext/ggsh--append-submodule-object
                $submodule-object
                `(current-head . ,current-commit-hash)))
        (setq $submodule-object
              (eemacs-ext/ggsh--append-submodule-object
                $submodule-object
                `(current-subject . ,current-commit-subject))))
      $submodule-object)))

(defun eemacs-ext/ggsh--get-submodule-final-release-tag ($submodule-object)
  (let* ((submodule-dir (alist-get 'path $submodule-object))
         (default-directory (expand-file-name submodule-dir eemacs-ext/ggsh--root-dir))
         (tag-regexp "^[vV]?\\(\\([0-9]+\\.\\)+\\([0-9]+\\)\\).*$")
         (tags (process-lines "git" "tag"))
         vtags-pre release)
    (dolist (tag tags)
      (when (string-match tag-regexp tag)
        (let (version)
          (setq version (match-string-no-properties 1 tag))
          (push (cons version tag) vtags-pre))))

    (when vtags-pre
      (setq release
            (sort
             (copy-tree vtags-pre)
             (lambda (x y)
               (if (version< (car y) (car x))
                   t
                 nil))))
      (setq release (cdar release))
      (setq $submodule-object
            (eemacs-ext/ggsh--append-submodule-object
              $submodule-object
              `(final-release . ,release))))
    $submodule-object))


;;; TODO
;; (defun eemacs-ext/ggsh--get-submodule-branches ())

;; ***** main
(defun eemacs-ext/ggsh--search-pair (region &optional use-plugin)
  (let ((keys '((path . "path = \\([^ ]+\\)$")
                (url . "url = \\([^ ]+\\)$")
                (branch . "branch = \\([^ ]+\\)$")))
        $submodule-object
        matched-values
        format-str)
    (dolist (key keys)
      (save-excursion
        (goto-char (car region))
        (catch :matched
          (while (not (eq (point) (cdr region)))
            (when (re-search-forward (cdr key) (line-end-position) t)
              (setq matched-value (match-string-no-properties 1))
              (push (cons (car key) matched-value) $submodule-object)
              (throw :matched nil))
            (next-line 1)
            (forward-line 0)))))
    (when use-plugin
      (dolist (plugin use-plugin)
        (cond
         ((eq plugin 'registered-commit)
          (setq $submodule-object
                (eemacs-ext/ggsh--get-submodule-registered-commit
                 $submodule-object)))
         ((eq plugin 'current-commit)
          (setq $submodule-object
                (eemacs-ext/ggsh--get-submodule-current-commit
                 $submodule-object)))
         ((eq plugin 'final-release)
          (setq $submodule-object
                (eemacs-ext/ggsh--get-submodule-final-release-tag
                 $submodule-object)))
         (t (error "unsupported plugin used '%s'" (symbol-name plugin))))))
    $submodule-object))

;; **** main
(defun eemacs-ext/ggsh--get-submodules-list (plugin &optional check-unregular just-check-unregular)
  (message "Get submodules infos, please waiting ...")
  (let (submodule-module-list bottom temp_match unregular)
    (eemacs-ext/ggsh--with-gitmodule-file-buffer
     (goto-char (point-min))
     (while (not (eobp))
       (setq bottom (eemacs-ext/ggsh--goto-entry-head))
       (when bottom
         (setq temp_match
               (eemacs-ext/ggsh--search-pair
                (eemacs-ext/ggsh--get-entry-region) plugin))
         (when check-unregular
           (eemacs-ext/ggsh--check-unregular-submodule-path-name temp_match 'unregular))
         (unless just-check-unregular
           (push temp_match submodule-module-list))
         (end-of-line))
       (unless bottom
         (goto-char (point-max)))))
    (if just-check-unregular unregular
      (if check-unregular (cons submodule-module-list unregular) submodule-module-list))))

;; ** usage
;; *** gen submodule add batch
(defun eemacs-ext/ggsh--format-submodule-add-bash ($submodule-object)
  (let ((format-str (if (assoc 'branch $submodule-object)
                        (cons 'branch "git submodule add -b %s %s %s")
                      (cons 'non-branch "git submodule add %s %s"))))
    (cond
     ((eq (car format-str) 'branch)
      (format (cdr format-str)
              (cdr (assoc 'branch $submodule-object))
              (cdr (assoc 'url $submodule-object))
              (cdr (assoc 'path $submodule-object))))
     (t
      (format (cdr format-str)
              (cdr (assoc 'url $submodule-object))
              (cdr (assoc 'path $submodule-object)))))))

(defun eemacs-ext/ggsh--get-batch-submodules-add-bash-script-list ()
  (let ((module-list (eemacs-ext/ggsh--get-submodules-list nil))
        rtn)
    (dolist (el module-list)
      (push
       (eemacs-ext/ggsh--format-submodule-add-bash el)
       rtn))
    rtn))

(defun eemacs-ext/ggsh-gen-submodules-add-bash-script ()
  (interactive)
  (let ((fmtstr-list (eemacs-ext/ggsh--get-batch-submodules-add-bash-script-list))
        (inhibit-read-only t))
    (with-current-buffer (find-file-noselect eemacs-ext/ggsh--submodules-adding-batch-file nil t)
      (goto-char (point-min))
      (dolist (cmd fmtstr-list)
        (insert (concat "cd " eemacs-ext/ggsh--root-dir " && "
                        cmd "\n")))
      (save-buffer)
      (kill-buffer))
    (message "Submodules getting commands generated done!")))

;; *** gen common branch toggle batch

(defun eemacs-ext/ggsh-gen-submodules-common-branch-toggle-bash-script (&optional recovery)
  (interactive "P")
  (let ((module-list (eemacs-ext/ggsh--get-submodules-list nil))
        cache
        (inhibit-read-only t)
        (flag (format-time-string "%Y%m%d%H%M%S"))
        (count 1))
    (dolist (el module-list)
      (let* ((path (expand-file-name (cdr (assoc 'path el)) eemacs-ext/ggsh--root-dir))
             (branch (cdr (assoc 'branch el)))
             (submodule-dir (expand-file-name path eemacs-ext/ggsh--root-dir)))
        (eemacs-ext/ggsh--with-submodule-status-check el
          (if recovery
              (progn
                (push "echo -e \"\\n==================================================\""
                      cache)
                (push (format "echo \"%s: for path '%s' toggle branch to '%s'\""
                              count path branch)
                      cache)
                (push "echo -e \"==================================================\\n\""
                      cache)
                (push
                 (format "cd %s && git checkout %s; cd %s"
                         path
                         (let ((head (alist-get 'current-head
                                                (eemacs-ext/ggsh--get-submodule-current-commit
                                                 el))))
                           (if head head (error "can not get head for '$s'" (file-name-base path))))
                         eemacs-ext/ggsh--root-dir)
                 cache)
                (push "" cache))
            (progn
                (push "echo -e \"\\n==================================================\""
                      cache)
                (push (format "echo \"%s: for path '%s' toggle branch to 'EemacsExtTempo-%s-%s'\""
                              count path branch flag)
                      cache)
                (push "echo -e \"==================================================\\n\""
                      cache)
                (push
                 (format "cd %s && git checkout -b EemacsExtTempo-%s-%s && git branch -u origin/%s; cd %s"
                         path branch flag branch eemacs-ext/ggsh--root-dir)
                 cache)
                (push "" cache)))
          (setq count (1+ count)))))
    (when cache
      (setq cache (reverse cache))
      (with-current-buffer (find-file-noselect eemacs-ext/ggsh--submodules-common-branch-toggle-batch-file nil t)
        (erase-buffer)
        (goto-char (point-min))
        (dolist (el cache)
          (insert (concat el "\n")))
        (save-buffer)
        (kill-buffer)))
    (if cache
        (message "Toggle-branch batch file generated done!")
      (message "Submodules not initialized!"))))

;; *** gen stick upstream batch

(defun eemacs-ext/ggsh-gen-submodules-stick-upstream-script ()
  (interactive)
  (let ((submodules (eemacs-ext/ggsh--get-submodules-list nil))
        cmds)
    (dolist (module submodules)
      (let ((path (expand-file-name (alist-get 'path module) eemacs-ext/ggsh--root-dir))
            (url (alist-get 'url module))
            (branch (alist-get 'branch module)))
        (eemacs-ext/ggsh--with-submodule-status-check module
          (push
           (format "cd %s && git checkout %s && git merge origin/%s && cd %s"
                   path
                   branch
                   branch
                   eemacs-ext/ggsh--root-dir)
           cmds))))
    (with-current-buffer
        (find-file-noselect
         eemacs-ext/ggsh--submodules-stick-upstream-batch-file)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (goto-char (point-min))
        (dolist (cmd cmds)
          (insert (concat cmd "\n")))
        (save-buffer)
        (message "Stick upstream bash script generated!")))))


;; *** gen final release batch
(defun eemacs-ext/ggsh-gen-submodules-toggle-stable-release-bash-script ()
  (interactive)
  (let ((file eemacs-ext/ggsh--submodules-toggle-final-release-batch-file)
        (modules (eemacs-ext/ggsh--get-submodules-list nil))
        cmds)
    (dolist (module modules)
      (let* ((path (expand-file-name
                    (alist-get 'path module)
                    eemacs-ext/ggsh--root-dir))
             (url (alist-get 'url module))
             (branch (alist-get 'branch module))
             release)
        (eemacs-ext/ggsh--with-submodule-status-check module
          (setq release
                (alist-get 'final-release (eemacs-ext/ggsh--get-submodule-final-release-tag module)))
          (when release
            (push (format "cd %s && git checkout %s && cd %s"
                          path release eemacs-ext/ggsh--root-dir
                          )
                  cmds)))))
    (with-current-buffer (find-file-noselect file)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (goto-char (point-min))
        (dolist (cmd cmds)
          (insert (concat cmd "\n")))
        (save-buffer)
        (message "Toggle final release bash script generated!")))))

(provide 'eemacs-ext-submodules-parse)
