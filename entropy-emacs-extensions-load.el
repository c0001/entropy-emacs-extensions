;;; entropy-emacs-extensions-load.el --- enntropy emacs git charged extensions management
;;
;; * Copyright (C) date  author
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs-extensions/blob/master/entropy-emacs-extensions-load.el
;; Package-Version: 0.1.6
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "25.3") (cl-lib "0.5"))
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
;; This package was the management for elisp loading part for
;; [[https://github.com/c0001/entropy-emacs][entropy-emacs]].
;;
;; The mainly functional aim of it was to add all submodules into
;; emacs's load-path and theme-load-path.
;;
;; * Configuration:
;;
;; Although this package are originally and designed for
;; entropy-emacs, but the functional parts are independently beside
;; it, thus the common usage method are proper for this package too,
;; this means that you can use this project for your own emacs
;; configuration too.
;;
;; #+BEGIN_SRC elisp
;;   (add-to-list 'load-path "path-of-this")
;;   (require 'entropy-emacs-extensions-load)
;; #+END_SRC
;; 
;; * Code:

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

;; Info path adding
(setq Info-default-directory-list
      (append (list eemacs-ext-info-root) Info-default-directory-list))

;; Library load-path adding
(eemacs-ext--load-path eemacs-ext-submodules-root)

;; Theme path loading
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

;; Profide
(provide 'entropy-emacs-extensions-load.el)
