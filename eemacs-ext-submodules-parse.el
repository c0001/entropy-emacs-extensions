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
;; - 'submodule-name'   :: the registerred gitmodule name
;; - 'submodule-local-path'   :: the registerred gitmodule local hosted place
;; - 'submodule-follow-branch' :: the remote repo branch which this git module followed with
;; - 'submodule-remote-url'    :: the remote repo url

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
(defmacro eemacs-ext/ggsh--with-gitmodule-file-buffer (&rest body)
  "Do sth with the current .gitmodule file buffer."
  (let ()
    `(with-current-buffer
         (find-file-noselect eemacs-ext/ggsh--gitmodules-file nil t)
       ,@body)))

(defmacro eemacs-ext/ggsh--common-with-submodule ($submodule-object &rest body)
  "Do sth with a healthy gitmodule.

 A healthy gitmodule has two feature:
 - has been inited
 - has persistent upstream branch given"
  (declare (indent defun))
  `(let* ((--submodule-branch-- (alist-get 'submodule-follow-branch ,$submodule-object))
          (--submodule-dir-- (expand-file-name (alist-get 'submodule-local-path ,$submodule-object)
                                               eemacs-ext/ggsh--root-dir))
          (--initialized-p--
           (file-exists-p
            (expand-file-name
             ".git"
             --submodule-dir--))))

     (if (and --submodule-branch--
              --initialized-p--)
         (let ((default-directory --submodule-dir--))
           ,@body)
       (cond
        ((null --submodule-branch--)
         (error "submodule '%s' doesn't follow any upstream branch!"
                --submodule-dir--))
        ((null --initialized-p--)
         (error "submodule '%s' doesn't initialized."
                --submodule-dir--))))))

;; *** basic functions

(defun eemacs-ext/ggsh--append-submodule-object ($submodule-object submodule-prop)
  "Add SUBMODULE-PROP to $SUBMODULE-OBJECT with replacing the
origin one if it exists.

SUBMODULE-PROP is a cons of which car is the prop symbol and cdr
was the value, it also can be a self-list of thus (i.e. car of
:self-lis and cdr of the list of thus)."
  (let* ((module $submodule-object)
         (prop submodule-prop)
         (prop-key (car prop)))
    (if (and (listp submodule-prop)
             (eq (car submodule-prop) :self-list))
        (progn
          (dolist (el (cdr submodule-prop))
            (setq module
                  (eemacs-ext/ggsh--append-submodule-object
                    module el))))
      (while (assoc prop-key module)
        (setq module (remove (assoc prop-key module) module)))
      (setq module (append module (list prop))))
    module))

(defun eemacs-ext/ggsh--unquote-callback (str)
  (dolist (regexp '("^\"" "\"$"))
    (setq str (replace-regexp-in-string regexp "" str)))
  str)

(defun eemacs-ext/ggsh--remove-str-messy-common (str)
  (let ((regexp-str (regexp-quote "-----END PGP SIGNATURE-----"))
        (inhibit-read-only t)
        pend)
    (when (string-match-p regexp-str str)
      (setq str
            (with-temp-buffer
              (insert str)
              (goto-char (point-min))
              (re-search-forward regexp-str nil)
              (ignore-errors (progn (next-line 2)
                                    (forward-line 0)))
              (buffer-substring-no-properties (point) (point-max)))))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (goto-char (point-max))
      (forward-line 0)
      (while (and (not (bobp))
                  (looking-at "^$"))
        (forward-line -1))
      (end-of-line)
      (setq str
            (buffer-substring-no-properties (point-min) (point))))
    str))

(defun eemacs-ext/ggsh--check-unregular-submodule-path-name ($submodule-object &optional fbk)
  "Check whether submodule base name are different from the
=submodule-remote-url= suffix name."
  (let ((submodule-remote-url (cdr (assoc 'submodule-remote-url $submodule-object)))
        (submodule-local-path (cdr (assoc 'submodule-local-path $submodule-object)))
        submodule-local-path-trail submodule-remote-url-trail rtn)
    (setq submodule-local-path-trail
          (replace-regexp-in-string
           "^.*/\\([^ /]+\\)$" "\\1" submodule-local-path)
          submodule-remote-url-trail
          (replace-regexp-in-string
           "^.*/\\([^ /]+?\\)\\(\\.git\\)?$" "\\1" submodule-remote-url))
    (unless (equal submodule-remote-url-trail submodule-local-path-trail)
      (when fbk
        (push $submodule-object (symbol-value fbk)))
      (setq rtn t))
    rtn))

(defun eemacs-ext/ggsh--get-remote-head-hash
    ($submodule-object &optional remote-name)
  (eemacs-ext/ggsh--common-with-submodule $submodule-object
    (let ((submodule-follow-branch (alist-get 'submodule-follow-branch $submodule-object)))
      (car (ignore-errors
             (process-lines "git" "show-ref"
                            (format "%s/%s" (or remote-name "origin") submodule-follow-branch)
                            "-s" "1" "--abbrev=8"))))))

(defun eemacs-ext/ggsh--get-commit-date ($submodule-object commit)
  "Get submodule $SUBMODULE-OBJECT commit date information via
COMMIT specifiied.

Return a plist has two slot:

- ':list': return a list time indicator integer via form of
  YEAR,MONTH,DATE,HOUR,MIN,SECS.

- ':raw': return a raw time spec string (i.e. the secs integer
  count from 1970.01.01) used to investigate by
  `format-time-string' or other elisp time function."
  (eemacs-ext/ggsh--common-with-submodule $submodule-object
    (let* ((cbk
            (split-string
             (shell-command-to-string
              (format "git show %s --pretty=format:%%cd -q --date=raw"
                      commit))
             " " t))
           (time-str (car cbk))
           (zone-str (cadr cbk)))
      (list :list
            (mapcar (lambda (x)
                      (string-to-number (replace-regexp-in-string "^0+" "" x)))
                    (split-string (format-time-string "%Y-%m-%d-%H-%M-%S"
                                                      (string-to-number time-str)
                                                      zone-str)
                                  "-" t))
            :raw
            (string-to-number time-str)))))

(defun eemacs-ext/ggsh--calc-commits-time-relative
    ($submodule-object commit-base commit-compare &optional rel-spec)
  "Compare two commits time relative based on COMMIT-BASE to
COMMIT-COMPARE via the spec submodule $SUBMODULE-OBJECT. Return a
date relative number specified by REL-SPEC .

When REL-SPEC in:

- 'year': the return number was based on the unit of a year which
  averaged by 365 days.
- 'month': the return number was based on the unit of a mounth
  which averaged by the 12 mouths a year
- 'day': the return number was based on the unit of a day i.e. 24h
- 'hour': the return number was based on the unit of an hour
- 'minute': etc ...
- 'sec': etc ...

The return number is positive when COMMIT-BASE is ahead of
COMMIT-COMPARE, other wise is negative or `eq' 0 when they are
equal."
  (eemacs-ext/ggsh--common-with-submodule $submodule-object
    (let* ((t-base-obj
            (eemacs-ext/ggsh--get-commit-date $submodule-object commit-base))
           (t-compare-obj
            (eemacs-ext/ggsh--get-commit-date $submodule-object commit-compare))
           (t-base (plist-get t-base-obj :raw))
           (t-compare (plist-get t-compare-obj :raw)))
      (apply '/ (float (- t-base t-compare))
             (delete
              nil
              (list
               (when t
                 1)
               (when (member rel-spec '(year month day hour minute))
                 60)
               (when (member rel-spec '(year month day hour))
                 60)
               (when (member rel-spec '(year month day))
                 24)
               (when (member rel-spec '(year month))
                 30)
               (when (eq rel-spec 'year)
                 12)))))))

(defun eemacs-ext/ggsh--calc-commits-ahead-compare
    ($submodule-object commit-base commit-compare)
  "Compare commit base cont by <git rev-list> function from
COMMIT-BASE to COMMIT-COMPARE.

The return number is positive when COMMIT-BASE is ahead of
COMMIT-COMPARE, other wise is negative or `eq' 0 when they are
equal."
  (eemacs-ext/ggsh--common-with-submodule $submodule-object
    (let (ahead-list)
      (setq ahead-list
            (ignore-errors
              (split-string
               (car
                (ignore-errors
                  (process-lines "git" "rev-list" "--left-right"
                                 (format "%s...%s" commit-base commit-compare)
                                 "--count")))
               "	" t)))
      (when (and ahead-list
                 (listp ahead-list))
        (ignore-errors
          (let ((base-up (string-to-number (car ahead-list)))
                (compare-up (string-to-number (cadr ahead-list))))
            (- base-up compare-up)))))))

(defun eemacs-ext/ggsh--get-commit-subject ($submodule-object commit &optional type)
  (eemacs-ext/ggsh--common-with-submodule $submodule-object
    (let ()
      (eemacs-ext/ggsh--remove-str-messy-common
       (cl-case type
         (subject
          (shell-command-to-string
           (format "git log %s --pretty=format:%%s -1"
                   commit)))
         (body
          (shell-command-to-string
           (format "git log %s --pretty=format:%%b -1"
                   commit)))
         (commentary
          (shell-command-to-string
           (format "git log %s --pretty=format:%%B -1"
                   commit)))
         ((t nil)
          (shell-command-to-string
           (format "git log %s --pretty=format:%%s -1"
                   commit))))))))

(defun eemacs-ext/ggsh--get-commit-author-name ($submodule-object commit)
  (eemacs-ext/ggsh--common-with-submodule $submodule-object
    (let ((cbk
           (shell-command-to-string
            (format "git log %s --pretty=format:%%an -1"
                    commit))))
      (eemacs-ext/ggsh--remove-str-messy-common cbk))))

(defun eemacs-ext/ggsh--get-commit-author-email ($submodule-object commit)
  (eemacs-ext/ggsh--common-with-submodule $submodule-object
    (let ((cbk
           (shell-command-to-string
            (format "git log %s --pretty=format:%%ae -1"
                    commit))))
      (eemacs-ext/ggsh--remove-str-messy-common cbk))))

(defun eemacs-ext/ggsh--get-commit-object ($submodule-object commit)
  (eemacs-ext/ggsh--common-with-submodule $submodule-object
    (let* ((remote-head-hash (alist-get 'submodule-remote-head $submodule-object))
           (commit-author-name (eemacs-ext/ggsh--get-commit-author-name $submodule-object commit))
           (commit-author-email (eemacs-ext/ggsh--get-commit-author-email $submodule-object commit))
           (commit-subject (eemacs-ext/ggsh--get-commit-subject $submodule-object commit 'subject))
           (commit-body (eemacs-ext/ggsh--get-commit-subject $submodule-object commit 'body))
           (commit-commentary (eemacs-ext/ggsh--get-commit-subject $submodule-object commit 'commentary))
           (commit-date (eemacs-ext/ggsh--get-commit-date $submodule-object commit))
           (commit-ahead-day (eemacs-ext/ggsh--calc-commits-time-relative
                              $submodule-object commit remote-head-hash))
           (commit-ahead-count (eemacs-ext/ggsh--calc-commits-ahead-compare
                                $submodule-object commit remote-head-hash)))
      (list
       :commit commit
       :commit-author-name commit-author-name
       :commit-author-email commit-author-email
       :commit-subject commit-subject
       :commit-body commit-body
       :commit-commentary commit-commentary
       :commit-date commit-date
       :commit-ahead-day commit-ahead-day
       :commit-ahead-count commit-ahead-count))))

;; *** gitmodule file parse
;; **** subroutine for parse gitmoudle file base
(defun eemacs-ext/ggsh--goto-entry-head ()
  (re-search-forward eemacs-ext/ggsh--entry-head-regexp nil t))

(defun eemacs-ext/ggsh--get-entry-region ()
  (let ((pcur (line-beginning-position))
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
(defun eemacs-ext/ggsh--patch-submodule-obj-with-registerred-info ($submodule-object)
  "Patch $SUBMODULE-OBJECT with registerred head info.

*Key slot description:*

- 'submodule-current-status' :: string of \"[+-U ]\", \"-\" if the
  submodule is not initialized, \"+\" if the currently checked out
  submodule commit does not match the SHA-1 found in the index of
  the containing repository and \"U\" if the submodule has merge
  conflicts. Otherwise the SPC char indicates that its fine as
  what it should be.

- 'submodule-registerred-head-obj' :: the registerred head
  =commit-object= generated by
  `eemacs-ext/ggsh--get-commit-object'.
"
  (when (alist-get 'submodule-local-path $submodule-object)
    (let* ((submodule-local-path (alist-get 'submodule-local-path $submodule-object))
           (default-directory (expand-file-name
                               eemacs-ext/ggsh--root-dir))
           (commit (car
                    (process-lines
                     "git"
                     "submodule" "status" "--cached" submodule-local-path)))
           (commit-tail-regexp "[[:blank:]](.*)$")
           (commit-regexp-1
            "^\\([[:blank:]-+]\\)\\([a-z0-9]+\\)[[:blank:]]\\(.+\\)$")
           (commit-regexp-2
            "^\\([[:blank:]-+]\\)\\([a-z0-9]+\\)[[:blank:]]\\(.+\\)[[:blank:]]\\((.+)\\)$")
           commit-regexp)
      (setq commit-regexp
            (if (string-match-p commit-tail-regexp commit)
                (progn
                  commit-regexp-2)
              commit-regexp-1))
      (when (and (stringp commit)
                 (string-match commit-regexp commit))
        (let ((hash (match-string 2 commit))
              (submodule-local-path-indicate (match-string 3 commit))
              (status-char (match-string 1 commit)))
          (when (and (not (eq 0 (length hash)))
                     (file-equal-p
                      (expand-file-name submodule-local-path eemacs-ext/ggsh--root-dir)
                      (expand-file-name submodule-local-path-indicate eemacs-ext/ggsh--root-dir)))
            (setq $submodule-object
                  (eemacs-ext/ggsh--append-submodule-object
                    $submodule-object
                    `(:self-list
                      (submodule-current-status . ,status-char)
                      (submodule-registerred-head-obj
                       .
                       ,(eemacs-ext/ggsh--get-commit-object
                         $submodule-object hash))))))))))
  $submodule-object)

(defun eemacs-ext/ggsh--patch-submodule-obj-with-current-commit-info
    ($submodule-object)
  "Patch $SUBMODULE-OBJECT with its current head info.

 *Added key slots description:*

 + 'submodule-current-head-obj' :: a =commit-object= of current
   head of this submodule generated by
   `eemacs-ext/ggsh--get-commit-object'.
 "
  (eemacs-ext/ggsh--common-with-submodule $submodule-object
    (let* ((submodule-local-path (alist-get 'submodule-local-path $submodule-object))
           (submodule-follow-branch (alist-get 'submodule-follow-branch $submodule-object))
           (default-directory (expand-file-name submodule-local-path eemacs-ext/ggsh--root-dir))
           submodule-current-commit-hash)
      (setq submodule-current-commit-hash
            (car (process-lines
                  "git"
                  "show-ref"
                  "--head"
                  "-s" "1"
                  "--abbrev=8")))
      (when submodule-current-commit-hash
        (setq $submodule-object
              (eemacs-ext/ggsh--append-submodule-object
                $submodule-object
                `(submodule-current-head-obj
                  .
                  ,(eemacs-ext/ggsh--get-commit-object
                    $submodule-object
                    submodule-current-commit-hash)))))
      $submodule-object)))

(defun eemacs-ext/ggsh--patch-submodule-obj-with-release-info
    ($submodule-object)
  "Patch $SUBMODULE-OBJECT with release info.

The value of the key 'submodule-final-release-tag' was a string
which indicate the last tag release for the gitmodule relying on
the fetched log.

Further more, this function also add
'submodule-final-release-tag-commit-obj' key-pairs (value of that
tag =commit-object= generated by
`eemacs-ext/ggsh--get-commit-object') and all of listed tags from
current submodule repo with 'submodule-tags' key-pair."
  (let* ((submodule-dir (alist-get 'submodule-local-path $submodule-object))
         (default-directory (expand-file-name submodule-dir eemacs-ext/ggsh--root-dir))
         (tag-regexp "^[vV]?\\(\\([0-9]+\\.\\)+\\([0-9]+\\)\\).*$")
         (tags (process-lines "git" "tag"))
         vtags-pre release release-hash)
    (dolist (tag tags)
      (when (string-match tag-regexp tag)
        (let (version)
          (setq version (match-string-no-properties 1 tag))
          (push (cons version tag) vtags-pre))))

    (when vtags-pre
      (setq vtags-pre
            (sort
             (copy-tree vtags-pre)
             (lambda (x y)
               (if (version< (car y) (car x))
                   t
                 nil))))
      (setq release (cdar vtags-pre)
            release-hash
            (let ((hash
                   (shell-command-to-string
                    (format "git show-ref %s -s 1 --abbrev=8" release))))
              (when (and (not (string-empty-p hash))
                         (string-match-p "^[0-9a-z]+" hash))
                (replace-regexp-in-string "\n" "" hash))))
      (setq $submodule-object
            (eemacs-ext/ggsh--append-submodule-object
              $submodule-object
              `(:self-list
                (submodule-final-release-tag . ,release)
                (submodule-final-release-tag-commit-obj
                 .
                 ,(eemacs-ext/ggsh--get-commit-object $submodule-object release-hash))
                (submodule-tags . ,(mapcar (lambda (tag-pair) (cdr tag-pair)) vtags-pre))))))
    $submodule-object))


;;; TODO
;; (defun eemacs-ext/ggsh--get-submodule-branches ())

;; registering plugins
(setq eemacs-ext/ggsh-gitmodule-parse-plugin-register
      (append eemacs-ext/ggsh-gitmodule-parse-plugin-register
              '((registered . eemacs-ext/ggsh--patch-submodule-obj-with-registerred-info)
                (current . eemacs-ext/ggsh--patch-submodule-obj-with-current-commit-info)
                (release . eemacs-ext/ggsh--patch-submodule-obj-with-release-info))))

;; ***** main
(defun eemacs-ext/ggsh--search-pair (region &optional use-plugin)
  (let ((keys '((submodule-name . "\\[submodule \"\\(.+\\)\"]")
                (submodule-local-path . "path = \\([^ ]+\\)$")
                (submodule-remote-url . "url = \\([^ ]+\\)$")
                (submodule-follow-branch . "branch = \\([^ ]+\\)$")))
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
    (setq $submodule-object (reverse $submodule-object)
          $submodule-object
          (eemacs-ext/ggsh--append-submodule-object
            $submodule-object
            `(submodule-remote-head
              .
              ,(eemacs-ext/ggsh--get-remote-head-hash $submodule-object))))
    (when use-plugin
      (dolist (plugin use-plugin)
        (let ((plugin-func (alist-get plugin eemacs-ext/ggsh-gitmodule-parse-plugin-register)))
          (if (not (functionp plugin-func))
              (error "The plugin '%s' can not be found!" plugin)
            (setq $submodule-object
                  (funcall plugin-func $submodule-object))))))
    $submodule-object))

;; **** get all submodule object from the .gitmodule file
(defun eemacs-ext/ggsh--get-submodules-list (&optional plugin check-unregular)
  "Get all $submodule-object through PLUGIN via the .gitmodule file
and return the list of it.

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
  (let ((format-str (if (assoc 'submodule-follow-branch $submodule-object)
                        (cons 'submodule-follow-branch "git submodule add -b %s %s %s")
                      (cons 'non-branch "git submodule add %s %s"))))
    (cond
     ((eq (car format-str) 'submodule-follow-branch)
      (format (cdr format-str)
              (cdr (assoc 'submodule-follow-branch $submodule-object))
              (cdr (assoc 'submodule-remote-url $submodule-object))
              (cdr (assoc 'submodule-local-path $submodule-object))))
     (t
      (format (cdr format-str)
              (cdr (assoc 'submodule-remote-url $submodule-object))
              (cdr (assoc 'submodule-local-path $submodule-object)))))))

(defun eemacs-ext/ggsh--get-batch-submodules-add-bash-script-list ()
  (let ((module-list (eemacs-ext/ggsh--get-submodules-list))
        rtn)
    (dolist (el module-list)
      (push
       (eemacs-ext/ggsh--format-submodule-add-bash el)
       rtn))
    rtn))

;;;###autoload
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

;;;###autoload
(defun eemacs-ext/ggsh-gen-submodules-common-branch-toggle-bash-script (&optional recovery)
  "Generate a bash script to checkout out for a temporary branch
named as 'EemacsExtTempo-BRANCH-DATE' and following with the
original upstream branch for each gitmodule under current git
repo.

If optional argument RECOVERY was non-nil, generating the bash
script for checking out for the original
=submodule-follow-branch=, its used for reverting the above
manipulation."
  (interactive "P")
  (let ((module-list (eemacs-ext/ggsh--get-submodules-list))
        cache
        (inhibit-read-only t)
        (flag (format-time-string "%Y%m%d%H%M%S"))
        (count 1))
    (dolist (el module-list)
      (let* ((submodule-local-path (expand-file-name
                                    (alist-get 'submodule-local-path el)
                                    eemacs-ext/ggsh--root-dir))
             (submodule-follow-branch (alist-get 'submodule-follow-branch el))
             (submodule-dir (expand-file-name submodule-local-path eemacs-ext/ggsh--root-dir)))
        (eemacs-ext/ggsh--common-with-submodule el
          ;; insert the title
          (push "echo -e \"\\n==================================================\""
                cache)
          (if recovery
              (push (format "echo \"%s: for path '%s' toggle branch to '%s'\""
                            count submodule-local-path submodule-follow-branch)
                    cache)
            (push (format "echo \"%s: for path '%s' toggle branch to 'EemacsExtTempo-%s-%s'\""
                          count submodule-local-path submodule-follow-branch flag)
                  cache))
          (push "echo -e \"==================================================\\n\""
                cache)

          ;; insert the commands
          (if recovery
              (push
               (format "cd %s && git checkout %s; cd %s"
                       submodule-local-path
                       (let ((head (plist-get
                                    (alist-get
                                     'submodule-current-head-obj
                                     (eemacs-ext/ggsh--patch-submodule-obj-with-current-commit-info
                                      el))
                                    :commit)))
                         (if head head
                           (error "can not get head for '$s'" (file-name-base submodule-local-path))))
                       eemacs-ext/ggsh--root-dir)
               cache)
            (push
             (format "cd %s && git checkout -b EemacsExtTempo-%s-%s && git branch -u origin/%s; cd %s"
                     submodule-local-path submodule-follow-branch flag
                     submodule-follow-branch eemacs-ext/ggsh--root-dir)
             cache))

          ;; insert tail
          (push "" cache)

          (setq count (1+ count)))))
    (when cache
      (setq cache (reverse cache))
      (with-current-buffer
          (find-file-noselect
           eemacs-ext/ggsh--submodules-common-branch-toggle-batch-file nil t)
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

;;;###autoload
(defun eemacs-ext/ggsh-gen-submodules-stick-upstream-script ()
  "Generate bash script to let all submodule up-to-date with
upstream without fetchting."
  (interactive)
  (let ((submodules (eemacs-ext/ggsh--get-submodules-list))
        cmds)
    (dolist (module submodules)
      (let ((submodule-local-path
             (expand-file-name (alist-get 'submodule-local-path module) eemacs-ext/ggsh--root-dir))
            (submodule-remote-url (alist-get 'submodule-remote-url module))
            (submodule-follow-branch (alist-get 'submodule-follow-branch module)))
        (eemacs-ext/ggsh--common-with-submodule module
          (push
           (format "cd %s && git checkout %s && git merge origin/%s && cd %s"
                   submodule-local-path
                   submodule-follow-branch
                   submodule-follow-branch
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

;;;###autoload
(defun eemacs-ext/ggsh-gen-submodules-toggle-stable-release-bash-script ()
  "Generate bash script for toggle gitmodule branch to the
submodule-final-release-tag when possible."
  (interactive)
  (let ((file eemacs-ext/ggsh--submodules-toggle-final-release-batch-file)
        (modules (eemacs-ext/ggsh--get-submodules-list '(release)))
        cmds)
    (dolist (module modules)
      (let* ((submodule-local-path (expand-file-name
                                    (alist-get 'submodule-local-path module)
                                    eemacs-ext/ggsh--root-dir))
             (submodule-remote-url (alist-get 'submodule-remote-url module))
             (submodule-follow-branch (alist-get 'submodule-follow-branch module))
             (release
              (alist-get 'submodule-final-release-tag module)))
        (when release
          (push (format "cd %s && git checkout %s && cd %s"
                        submodule-local-path release eemacs-ext/ggsh--root-dir)
                cmds))))
    (if cmds
        (with-current-buffer (find-file-noselect file)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (goto-char (point-min))
            (dolist (cmd cmds)
              (unless (string-match-p       ;exclude some extensions
                                            ;which stable version are
                                            ;not properly to
                                            ;entropy-emacs
                       (rx (seq (or "/submodules/elpa"
                                    "/submodules/melpa"
                                    "/upstream/maigt-.*" "/transient"
                                    "/upstream/outshine"
                                    "/upstream/outorg"
                                    "/upstream/emacs-doom-themes"
                                    "/upstream/eterm-256color"
                                    )))
                       cmd)
                (insert (concat cmd "\n"))))
            (save-buffer)
            (message "Toggle final release bash script generated!")))
      (message "No submodule has release tag annotaion!"))))

;; * provide
(provide 'eemacs-ext-submodules-parse)
