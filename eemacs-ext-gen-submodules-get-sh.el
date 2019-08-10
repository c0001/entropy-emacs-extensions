(defvar eemacs-ext/ggsh--root-dir (expand-file-name (file-name-directory load-file-name)))

(defvar eemacs-ext/ggsh--submodule-file
  (expand-file-name ".gitmodules" eemacs-ext/ggsh--root-dir))

(defvar eemacs-ext/ggsh--entry-head-regexp
  "^\\[submodule \"\\([^ ]+\\)\"\\]$")

(defmacro eemacs-ext/ggsh--with-submodule-buffer (&rest body)
  (let ((buffer (find-file-noselect eemacs-ext/ggsh--submodule-file)))
    `(with-current-buffer ,buffer
       ,@body)))

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

(defun eemacs-ext/ggsh--format-sh (matched-list)
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

(defun eemacs-ext/ggsh--check-unregular (matched-list fbk)
  (let ((url (cdr (assoc 'url matched-list)))
        (path (cdr (assoc 'path matched-list)))
        path-trail url-trail)
    (setq path-trail
          (replace-regexp-in-string
           "^.*/\\([^ /]+\\)$" "\\1" path)
          url-trail
          (replace-regexp-in-string
           "^.*/\\([^ /]+?\\)\\(\\.git\\)?$" "\\1" url))
    (unless (equal url-trail path-trail)
      (push url (symbol-value fbk)))))

(defun eemacs-ext/ggsh--get-sh-list (&optional check-unregular just-check-unregular)
  (let (sh-list bottom temp_match unregular)
    (eemacs-ext/ggsh--with-submodule-buffer
     (goto-char (point-min))
     (while (not (eobp))
       (setq bottom (eemacs-ext/ggsh--goto-entry-head))
       (when bottom
         (setq temp_match
               (eemacs-ext/ggsh--search-pair
                (eemacs-ext/ggsh--get-entry-region)))
         (when check-unregular
           (eemacs-ext/ggsh--check-unregular temp_match 'unregular))
         (unless just-check-unregular
           (push (eemacs-ext/ggsh--format-sh
                  temp_match)
                 sh-list))
         (end-of-line))
       (unless bottom
         (goto-char (point-max)))))
    (if just-check-unregular unregular
      (if check-unregular (cons sh-list unregular) sh-list))))

(provide 'eemacs-ext-gen-submodules-get-sh)
