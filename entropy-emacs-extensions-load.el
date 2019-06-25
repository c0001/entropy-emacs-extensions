(defvar eemacs-ext-root (file-name-directory load-file-name))
(defvar eemacs-ext-submodules-root (expand-file-name "elements/submodules" eemacs-ext-root))
(defvar eemacs-ext-info-root (expand-file-name "elements/info-files" eemacs-ext-root))

(defun eemacs-ext--add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (when (not (string-match-p "yasnippet-snippets" dir))
      (normal-top-level-add-subdirs-to-load-path))))


(defun eemacs-ext--load-path (top-dir)
  (let ((subdirs (entropy/emacs-list-subdir top-dir)))
    (dolist (el subdirs)
      (add-to-list 'load-path el)
      (eemacs-ext--add-subdirs-to-load-path el))))


(setq Info-default-directory-list
      (append (list eemacs-ext-info-root) Info-default-directory-list))


(eemacs-ext--load-path eemacs-ext-submodules-root)

(provide 'entropy-emacs-extensions-load.el)
