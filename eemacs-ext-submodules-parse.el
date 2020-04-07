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

;; This library get git modules into =$submodule-object=, a alist to
;; represent one git submodule with its 'name' 'branch' 'url' 'path'
;; and so more such as its tag, commit etc ...

;; Bash script can be generated for below aims:

;; - toggle-branch to tempo one
;; - toggle-branch to head's commit hash
;; - toggle head to final release tag
;; - create one git submodules 'add' batch bash script


;; *Plugin system*

;; Variable type =$submodule-object= was a flexible data to represent
;; one gitmodule, basically, there are three core keys commonly used
;; for be a expression of a git module, they are necessary for being
;; as:
;; - 'name'   :: the registerred gitmodule name
;; - 'path'   :: the registerred gitmodule local hosted place
;; - 'branch' :: the remote repo branch which this git module followed with
;; - 'url'    :: the remote repo url

;; Further more, we always can get other attribtes of one
;; =$submodule-object= follow above keys slot value. So as that we
;; could build some functions to append new key-pair into it, we
;; called these type functions =$submodule-parse-plugin=.

;; * Code:
;; ** require
(require 'files)
(require 'cl-lib)

;; ** variables pre
(defvar eemacs-ext/ggsh--root-dir (expand-file-name (file-name-directory load-file-name)))

(defvar eemacs-ext/ggsh--gitmodules-file
  (expand-file-name ".gitmodules" eemacs-ext/ggsh--root-dir))

(defun eemacs-ext/expand-bin-path (fbase-name)
  (expand-file-name
   (format "annex/bin/%s" fbase-name)
   eemacs-ext/ggsh--root-dir))

(defvar eemacs-ext/ggsh--submodules-common-branch-toggle-batch-file
  (eemacs-ext/expand-bin-path "submodules-common-toggle-branch.sh"))

(defvar eemacs-ext/ggsh--submodules-adding-batch-file
  (eemacs-ext/expand-bin-path "submodules-get.sh"))

(defvar eemacs-ext/ggsh--submodules-toggle-final-release-batch-file
  (eemacs-ext/expand-bin-path "submodules-toggle-final-release.sh"))

(defvar eemacs-ext/ggsh--submodules-stick-upstream-batch-file
  (eemacs-ext/expand-bin-path "submodules-stick-upstream.sh"))

(defvar eemacs-ext/ggsh--entry-head-regexp
  "^\\[submodule \"\\([^ ]+\\)\"\\]$")

(defvar eemacs-ext/ggsh-gitmodule-parse-plugin-register nil
  "Alist of the gitmodule parse plugins which the car of each
element was a symbol for indicating the plugin name and the cdr
was the manipulation function.

Each plugin has just one required argument the $submodule-object,
and returning the appended new one.
 ")


;; ** library
;; *** basic macro
(defmacro eemacs-ext/ggsh--append-submodule-object ($submodule-object submodule-prop)
  "Add SUBMODULE-PROP to $SUBMODULE-OBJECT with replacing the
origin one if it exists."
  (declare (indent defun))
  `(let* ((module ,$submodule-object)
          (prop ,submodule-prop)
          (prop-key (car prop)))
     (while (assoc prop-key module)
       (setq module (remove (assoc prop-key module) module)))
     (append module (list prop))))

(defmacro eemacs-ext/ggsh--with-gitmodule-file-buffer (&rest body)
  "Do sth with the current .gitmodule file buffer."
  (let ()
    `(with-current-buffer
         (find-file-noselect eemacs-ext/ggsh--gitmodules-file nil t)
       ,@body)))

(defmacro eemacs-ext/ggsh--with-submodule-status-check ($submodule-object &rest body)
  "Do sth with a healthy gitmodule.

 A healthy gitmodule has two feature:
 - has been inited
 - has persistent upstream branch given"
  (declare (indent defun))
  `(let* ((submodule-branch (alist-get 'branch ,$submodule-object))
          (submodule-dir (expand-file-name (alist-get 'path ,$submodule-object)
                                           eemacs-ext/ggsh--root-dir))
          (initialized-p
           (file-exists-p
            (expand-file-name
             ".git"
             submodule-dir))))

     (if (and submodule-branch
              initialized-p)
         (progn ,@body)
       (cond
        ((null submodule-branch)
         (error "submodule '%s' doesn't follow any upstream branch!" submodule-dir))
        ((null initialized-p)
         (error "submodule '%s' doesn't initialized." submodule-dir))))))

;; *** basic functions
(defun eemacs-ext/ggsh--unquote-callback (str)
  (dolist (regexp '("^\"" "\"$"))
    (setq str (replace-regexp-in-string regexp "" str)))
  str)

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
;; ***** plugins
(defun eemacs-ext/ggsh--get-submodule-registered-commit ($submodule-object)
  "Append gitmodule's 'registered-head' and 'status' key pairs

*Key slot description:*

- 'registered-head' :: a hash string indicated the submodule
  cached commit's hash code

- 'status' :: string of \"[+-U ]\", \"-\" if the submodule is not
  initialized, \"+\" if the currently checked out submodule commit
  does not match the SHA-1 found in the index of the containing
  repository and \"U\" if the submodule has merge
  conflicts. Otherwise the SPC char indicates that its fine as
  what it should be.
"
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
  "Add 'current-head' and 'current-subject' key-pairs

 *key slot description:*

 - 'current-head' :: a string of what git head hash code for
   gitmodule current state
 - 'current-subject' :: a plist represent the current general
   information of the gitmodule that for thus:
   1) key =:title=: current commit's commentary for the head of the gitmodule
   2) key =:author=: current commit's author for the head of the
      gitmodule.
   3) key =:email=: current commit's author's email for the head of
      the gitmodule
   4) key =:date=: current commit's date for the head of the gitmodule"
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
  "Add 'final-release' key-pairs for $SUBMODULE-OBJECT.

The value of the key 'final-release' was a string which indicate
the last tag release for the gitmodule relying on the fetched
log."
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

;; registering plugins
(setq eemacs-ext/ggsh-gitmodule-parse-plugin-register
      (append eemacs-ext/ggsh-gitmodule-parse-plugin-register
              '((registered-commit . eemacs-ext/ggsh--get-submodule-registered-commit)
                (current-commit . eemacs-ext/ggsh--get-submodule-current-commit)
                (final-release . eemacs-ext/ggsh--get-submodule-final-release-tag))))

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
          (while (< (point) (cdr region))
            (when (re-search-forward (cdr key) (line-end-position) t)
              (setq matched-value (match-string-no-properties 1))
              (push (cons (car key) matched-value) $submodule-object)
              (throw :matched nil))
            (next-line 1)
            (forward-line 0)))))
    (when use-plugin
      (dolist (plugin use-plugin)
        (let ((plugin-func (alist-get plugin eemacs-ext/ggsh-gitmodule-parse-plugin-register)))
          (if (not (functionp plugin-func))
              (error "The plugin '%s' can not be found!" plugin)
            (setq $submodule-object
                  (funcall plugin-func $submodule-object))))))
    $submodule-object))

;; **** get all submodule object from the .gitmodule file
(defun eemacs-ext/ggsh--get-submodules-list (plugin &optional check-unregular)
  "Get all $submodule-object through PLUGIN via the .gitmodule file and return the list of it.

Optional argument CHECK-UNREGULAR when non-nil just return the
unregular $submodule-object list via the judger
`eemacs-ext/ggsh--check-unregular-submodule-path-name'.
 "
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
         (push temp_match submodule-module-list)
         (end-of-line))
       (unless bottom
         (goto-char (point-max)))))
    (if check-unregular
        unregular
      submodule-module-list)))

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
  "Generate a bash script to checkout out for a temporary branch
named as 'EemacsExtTempo-BRANCH-DATE' and following with the
original upstream branch for each gitmodule under current git
repo.

If optional argument RECOVERY was non-nil, generating the bash
script for checking out for the original branch, its used for
reverting the above manipulation."
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
          ;; insert the title
          (push "echo -e \"\\n==================================================\""
                cache)
          (if recovery
              (push (format "echo \"%s: for path '%s' toggle branch to '%s'\""
                            count path branch)
                    cache)
            (push (format "echo \"%s: for path '%s' toggle branch to 'EemacsExtTempo-%s-%s'\""
                          count path branch flag)
                  cache))
          (push "echo -e \"==================================================\\n\""
                cache)

          ;; insert the commands
          (if recovery
              (push
               (format "cd %s && git checkout %s; cd %s"
                       path
                       (let ((head (alist-get 'current-head
                                              (eemacs-ext/ggsh--get-submodule-current-commit
                                               el))))
                         (if head head (error "can not get head for '$s'" (file-name-base path))))
                       eemacs-ext/ggsh--root-dir)
               cache)
            (push
             (format "cd %s && git checkout -b EemacsExtTempo-%s-%s && git branch -u origin/%s; cd %s"
                     path branch flag branch eemacs-ext/ggsh--root-dir)
             cache))

          ;; insert tail
          (push "" cache)

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
  "Generate bash script to let all submodule up-to-date with
upstream without fetchting."
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
 "Generate bash script for toggle gitmodule branch to the
final-release tag when possible."
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

;; * provide
(provide 'eemacs-ext-submodules-parse)
