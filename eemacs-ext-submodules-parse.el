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


(defvar eemacs-ext/ggsh--root-dir (expand-file-name (file-name-directory load-file-name)))

(defvar eemacs-ext/ggsh--submodule-file
  (expand-file-name ".gitmodules" eemacs-ext/ggsh--root-dir))

(defvar eemacs-ext/ggsh--branch-toggle-file
  (expand-file-name "toggle-branch.sh" eemacs-ext/ggsh--root-dir))

(defvar eemacs-ext/ggsh--batch-file
  (expand-file-name "get-modules.sh" eemacs-ext/ggsh--root-dir))

(defvar eemacs-ext/ggsh--entry-head-regexp
  "^\\[submodule \"\\([^ ]+\\)\"\\]$")

(defmacro eemacs-ext/ggsh--with-submodule-buffer (&rest body)
  (let ((buffer (find-file-noselect eemacs-ext/ggsh--submodule-file nil t)))
    `(with-current-buffer ,buffer
       ,@body)))

(defun eemacs-ext/ggsh--get-submodule-head (submodule-dir)
  (let ((default-directory (expand-file-name submodule-dir)))
    (car (process-lines
          "git"
          "show-ref"
          "--head"
          "-s" "1"
          "--abbrev=8"))))

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

(defun eemacs-ext/ggsh--search-pair (region)
  (let ((keys '((path . "path = \\([^ ]+\\)$")
                (url . "url = \\([^ ]+\\)$")
                (branch . "branch = \\([^ ]+\\)$")))
        matched-list
        matched-values
        format-str)
    (dolist (key keys)
      (save-excursion
        (goto-char (car region))
        (catch :matched
          (while (not (eq (point) (cdr region)))
            (when (re-search-forward (cdr key) (line-end-position) t)
              (setq matched-value (match-string-no-properties 1))
              (push (cons (car key) matched-value) matched-list)
              (throw :matched nil))
            (next-line 1)
            (forward-line 0)))))
    matched-list))

(defun eemacs-ext/ggsh--format-submodule-add-bash (matched-list)
  (let ((format-str (if (assoc 'branch matched-list)
                        (cons 'branch "git submodule add -b %s %s %s")
                      (cons 'non-branch "git submodule add %s %s"))))
    (cond
     ((eq (car format-str) 'branch)
      (format (cdr format-str)
              (cdr (assoc 'branch matched-list))
              (cdr (assoc 'url matched-list))
              (cdr (assoc 'path matched-list))))
     (t
      (format (cdr format-str)
              (cdr (assoc 'url matched-list))
              (cdr (assoc 'path matched-list)))))))

(defun eemacs-ext/ggsh--check-unregular-submodule-path-name (matched-list &optional fbk)
  "Check whether submodule base name are different from the url
suffix name."
  (let ((url (cdr (assoc 'url matched-list)))
        (path (cdr (assoc 'path matched-list)))
        path-trail url-trail rtn)
    (setq path-trail
          (replace-regexp-in-string
           "^.*/\\([^ /]+\\)$" "\\1" path)
          url-trail
          (replace-regexp-in-string
           "^.*/\\([^ /]+?\\)\\(\\.git\\)?$" "\\1" url))
    (unless (equal url-trail path-trail)
      (when fbk
        (push matched-list (symbol-value fbk)))
      (setq rtn t))
    rtn))

(defun eemacs-ext/ggsh--get-submodules-list (&optional check-unregular just-check-unregular)
  (let (submodule-module-list bottom temp_match unregular)
    (eemacs-ext/ggsh--with-submodule-buffer
     (goto-char (point-min))
     (while (not (eobp))
       (setq bottom (eemacs-ext/ggsh--goto-entry-head))
       (when bottom
         (setq temp_match
               (eemacs-ext/ggsh--search-pair
                (eemacs-ext/ggsh--get-entry-region)))
         (when check-unregular
           (eemacs-ext/ggsh--check-unregular-submodule-path-name temp_match 'unregular))
         (unless just-check-unregular
           (push temp_match submodule-module-list))
         (end-of-line))
       (unless bottom
         (goto-char (point-max)))))
    (if just-check-unregular unregular
      (if check-unregular (cons submodule-module-list unregular) submodule-module-list))))

(defun eemacs-ext/ggsh--get-batch-submodules-add-bash-script-list ()
  (let ((module-list (eemacs-ext/ggsh--get-submodules-list))
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
    (with-current-buffer (find-file-noselect eemacs-ext/ggsh--batch-file nil t)
      (goto-char (point-min))
      (dolist (cmd fmtstr-list)
        (insert (concat cmd "\n")))
      (save-buffer)
      (kill-buffer))
    (message "Submodules getting commands generated done!")))

(defun eemacs-ext/ggsh-gen-submodules-branch-toggle-bash-script (&optional recovery)
  (interactive "P")
  (let ((module-list (eemacs-ext/ggsh--get-submodules-list))
        cache
        (inhibit-read-only t)
        (flag (format-time-string "%Y%m%d%H%M%S"))
        (count 1))
    (dolist (el module-list)
      (let* ((path (cdr (assoc 'path el)))
             (branch (cdr (assoc 'branch el)))
             (submodule-dir (expand-file-name path eemacs-ext/ggsh--root-dir)))
        (when (and branch
                   (file-exists-p
                    (expand-file-name
                     ".git"
                     submodule-dir)))
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
                         (eemacs-ext/ggsh--get-submodule-head submodule-dir)
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
      (with-current-buffer (find-file-noselect eemacs-ext/ggsh--branch-toggle-file nil t)
        (erase-buffer)
        (goto-char (point-min))
        (dolist (el cache)
          (insert (concat el "\n")))
        (save-buffer)
        (kill-buffer)))
    (if cache
        (message "Toggle-branch batch file generated done!")
      (message "Submodules not initialized!"))))


(provide 'eemacs-ext-submodules-parse)
