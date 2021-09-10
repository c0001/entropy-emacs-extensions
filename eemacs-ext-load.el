;;; entropy-emacs-extensions-load.el --- enntropy emacs git charged extensions management
;;
;; * Copyright (C) 2018 Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs-extensions/blob/master/entropy-emacs-extensions-load.el
;; Package-Version: 1.0.2
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
;; *This project was collection of emacs extensions used for
;; [[https://github.com/c0001/entropy-emacs][entropy-emacs]].*

;; Submodules under this project are collected from the corresonding
;; package repo of melpa or elpa extensions, version respected by
;; [[https://github.com/c0001/entropy-emacs][entropy-emacs]] for the config tieing, not up-to-date with upstream, but
;; will update in term of the updates of =entropy-emacs=.

;; Project can be used individually in your own wish but without any
;; warranty for the reason mentioned above. Load the loader
;; =entropy-emacs-extensions-load.el= in case for that.

;; For each =entropy-emacs= user, before using this repo whenever be
;; after each updating or at the initialization, run the =make all=
;; for finishing both of the updating or initialization, this was
;; required at top level.

;; This is the way for =entropy-emacs= ecosystem complements, the
;; local-melpa `package-archives' post way. The main purpose for
;; maintain all =entropy-emacs= extensions as the commit remained
;; submodules is to guarantee the extensions compatibility with
;; =entropy-emacs=. i.e. The designation for
;; =entropy-emacs-extensions= is to make it as the `package-archives`
;; as what did as [[https://melpa.org][melpa]] do to install all =entropy-emacs= depended
;; commit specific extensions froms this project, as that for what,
;; =entropy-emacs= forked =melpa= to built the =entropy-emacs-melpa=
;; using for [[https://melpa.org/#/getting-started][package.el]], on this way, this package will set the
;; ~package-archives~ to as form as ~("entropy-emacs"
;; . "path-to-local-malpa")~.

;; Rely on this usage you selected, the customized variable
;; =entropy/emacs-ext-elpkg-get-type= of =entropy-emacs= was what you
;; needed to set to enable this project efficiently, the valid value
;; of thus is a symbol: =entropy-emacs-extenisons-project=

;; This variable is pre-defined as 'origin' in =entropy-emacs= so
;; that you should specified it in your =custom.el= before start
;; =entropy-emacs=.

;; The last needed notice is to set =entropy-emacs= customized
;; variable =entropy/emacs-ext-eemacs-elpkg-archive-project-dir=
;; correctly which is the path of this project let =entropy-emacs=
;; locates this project correctly, of cource write it in your
;; =custom.el= also.

;; * Configuration:

;; Although this package are originally and designed for
;; entropy-emacs, but the functional parts are independently beside
;; it, thus the common usage method are proper for this package too,
;; this means that you can use this project for your own emacs
;; configuration too.

;; #+BEGIN_SRC elisp
;;   (add-to-list 'load-path "path-of-this")
;;   (require 'entropy-emacs-extensions-load)
;; #+END_SRC

;; * Code:
(require 'rx)
(require 'cl-lib)

;; ** variables
;; *** customized variable

;; *** const variables
(defconst eemacs-ext-root (file-name-directory load-file-name))
(defconst eemacs-ext-melpa-root (expand-file-name "elements/submodules/melpa" eemacs-ext-root))
(defconst eemacs-ext-melpa-packages (expand-file-name "packages" eemacs-ext-melpa-root))
(defconst eemacs-ext-elpa-root (expand-file-name "elements/submodules/elpa" eemacs-ext-root))
(defconst eemacs-ext-elpa-packages (expand-file-name "archive" eemacs-ext-elpa-root))
(defconst eemacs-ext-elpa-packages-devel (expand-file-name "archive-devel" eemacs-ext-elpa-root))

;; ** libraries
;; *** common library
(defun eemacs-ext-list-dir-lite (dir-root)
  "Return directory list with type of whichever file or
directory."
  (let (rtn-full rtn-lite rtn-attr)
    (when (and (file-exists-p dir-root)
               (file-directory-p dir-root))
      (setq rtn-full (directory-files dir-root t))
      (dolist (el rtn-full)
        (if (not (string-match-p "\\(\\\\\\|/\\)\\(\\.\\|\\.\\.\\)$" el))
            (push el rtn-lite)))
      (if rtn-lite
          (progn
            (dolist (el rtn-lite)
              (if (file-directory-p el)
                  (push `("D" . ,el) rtn-attr)
                (push `("F" . ,el) rtn-attr)))
            rtn-attr)
        nil))))

(defun eemacs-ext-list-subdir (dir-root)
  "List subdir of root dir DIR-ROOT"
  (let ((dirlist (eemacs-ext-list-dir-lite dir-root))
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

;; ** Intialize procedure
(setq package-archives
      `(("entropy-melpa"      . ,eemacs-ext-melpa-packages)
        ("entropy-elpa"       . ,eemacs-ext-elpa-packages)
        ("entropy-elpa-devel" . ,eemacs-ext-elpa-packages-devel)))

;; * Provide
(provide 'eemacs-ext-load)
