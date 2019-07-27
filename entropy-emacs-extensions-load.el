(defvar eemacs-ext-root (file-name-directory load-file-name))
(defvar eemacs-ext-submodules-root (expand-file-name "elements/submodules" eemacs-ext-root))
(defvar eemacs-ext-info-root (expand-file-name "elements/info-files" eemacs-ext-root))

(defun eemacs-ext-list-subdir (dir-root)
  "List subdir of root dir DIR-ROOT"
  (let ((dirlist (entropy/emacs-list-dir-lite dir-root))
        (rtn nil))
    (if dirlist
        (progn
          (dolist (el dirlist)
            (if (equal "D" (car el))
                (push (cdr el) rtn)))
          (if rtn
              rtn
            nil))
      nil)))

(defun eemacs-ext--add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (when (not (string-match-p "yasnippet-snippets" dir))
      (normal-top-level-add-subdirs-to-load-path))))

(defun eemacs-ext--load-path (top-dir)
  (let ((subdirs (eemacs-ext-list-subdir top-dir)))
    (dolist (el subdirs)
      (add-to-list 'load-path el)
      (eemacs-ext--add-subdirs-to-load-path el))))

;; info path adding
(setq Info-default-directory-list
      (append (list eemacs-ext-info-root) Info-default-directory-list))

;; library load-path adding
(eemacs-ext--load-path eemacs-ext-submodules-root)

;; theme path loading
(let* ((base-dir (expand-file-name (expand-file-name "elements/submodules" eemacs-ext-root)))
       (theme-list '("color-theme-sanityinc-tomorrow"
                     "birds-of-paradise-plus-theme.el"
                     "gotham-theme"
                     "atom-dark-theme-emacs"
                     "atom-one-dark-theme"
                     "GitHub-Theme-for-Emacs"
                     "doneburn-theme"
                     "emacs-klere-theme"
                     "emacs-material-theme"
                     "spacemacs-theme"
                     "emacs-color-themes"
                     "darkokai"
                     "color-theme-ujelly"
                     "srcery-emacs"
                     "emacs-chocolate-theme")))
  (setq theme-list (mapcar #'(lambda (x)
                               (expand-file-name x base-dir))
                           theme-list))
  (mapc #'(lambda (x)
            (add-to-list 'custom-theme-load-path x))
        theme-list))

(provide 'entropy-emacs-extensions-load.el)
