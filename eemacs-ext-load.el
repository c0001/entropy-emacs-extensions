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
;; after each updating or at the initialization, run the =make= for
;; finishing both of the updating or initialization, this was
;; required at top level.

;; There's two loading way for =entropy-emacs=, the submodule loading
;; directl road and the local-melpa `package-archives' post way. By
;; defautly, this package will scanning all the submodules of which
;; was one emacs-extensions package cloned from the popular
;; repository hoster and adding them into the `load-path' or
;; `custom-theme-load-path' so that =entropy-emacs= can use load them
;; directly instead of retrieving them from the upstream when some
;; features are missing. The main purpose for maintain all
;; =entropy-emacs= extensions as the commit remained submodules to
;; guarantee the extensions compatibility with =entropy-emacs=.

;; So as what metioned in above paragraph, the extensions
;; compatibility tracking is through the package repo commit specific
;; way, but loading directly from the extension version charged repo
;; will using too much ~loading-time~ against what the traditionally
;; emacs package loading mechanism by loading the bite-compiled file
;; instead of using the raw =elisp= file to reducing ~loading-time~,
;; thus the second using way for =entropy-emacs-extensions= was to
;; make it as the `package-archives` as what did as [[https://melpa.org][melpa]] do to
;; install all =entropy-emacs= depended commit specific extensions
;; froms this project, as that for what, =entropy-emacs= forked
;; =melpa= to built the =entropy-emacs-melpa= using for [[https://melpa.org/#/getting-started][package.el]],
;; on this way, this package will set the ~package-archives~ to
;; ~("entropy-emacs" . "path-to-local-malpa")~.

;; Rely on which usage you selected, the customized variable
;; =entropy/emacs-ext-elpkg-get-type= was what you needed to set
;; according to your wish, there's two valid options in this project
;; for assigning into it (defaultly was ~'submodules~):
;; 1) 'submodules: loading directly from the submodules of
;;    =entrop-emacs-extensions='s submodules.

;; 2) 'submodules-melpa-local: using =entropy-emacs-extensions= as the
;;    local melpa which hosted all =entropy-emacs= specified
;;    extensions.

;; This variable is pre-defined in =entrop-emacs= (as what mentioned,
;; this project was designed for thus) so that you can specified it
;; in your =custom.el= directly.

;; * Configuration:
;;
;; Although this package are originally and designed for
;; entropy-emacs, but the functional parts are independently beside
;; it, thus the common usage method are proper for this package too,
;; this means that you can use this project for your own emacs
;; configuration too.
;;
;; #+BEGIN_SRC elisp
;;   (setq entropy/emacs-use-extensions-type 'submodules-melpa-local)
;;   (add-to-list 'load-path "path-of-this")
;;   (require 'entropy-emacs-extensions-load)
;; #+END_SRC
;;
;; * Code:
;; ** variables
;; *** customized variable
(defvar entropy/emacs-ext-elpkg-get-type 'submodules)

;; *** const variables
(defconst eemacs-ext-root (file-name-directory load-file-name))
(defconst eemacs-ext-submodules-upstream-root (expand-file-name "elements/submodules/upstream" eemacs-ext-root))
(defconst eemacs-ext-info-root (expand-file-name "elements/info-files" eemacs-ext-root))
(defconst eemacs-ext-melpa-root (expand-file-name "elements/submodules/melpa" eemacs-ext-root))
(defconst eemacs-ext-melpa-packages (expand-file-name "packages" eemacs-ext-melpa-root))
(defconst eemacs-ext-elpa-root (expand-file-name "elements/submodules/elpa" eemacs-ext-root))
(defconst eemacs-ext-elpa-packages (expand-file-name "archive/packages" eemacs-ext-elpa-root))


;; ** libraries
;; *** common library
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

(defun eemacs-ext--load-theme-path (top-dir theme-subdir)
  (let (theme-list)
    (setq theme-list (mapcar #'(lambda (x)
                                 (expand-file-name x top-dir))
                             theme-subdir))
    (mapc #'(lambda (x)
              (add-to-list 'custom-theme-load-path x))
          theme-list)))

;; ** Intialize procedure
;; *** For common usage
(when (eq entropy/emacs-ext-elpkg-get-type 'submodules)
  ;; Info path adding
  (setq Info-default-directory-list
        (append (list eemacs-ext-info-root) Info-default-directory-list))

  ;; Library load-path adding
  (eemacs-ext--load-path eemacs-ext-submodules-upstream-root)
  ;; reverse load path so that all the native libraries are ahead of
  ;; all rest to prevent locating thus same named library for other
  ;; repo test stuffs from messing up the emacs refs invocation.
  (setq load-path (reverse load-path))

  ;; Theme path loading
  (eemacs-ext--load-theme-path (expand-file-name eemacs-ext-submodules-upstream-root)
                               '("color-theme-sanityinc-tomorrow"
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
                                 "emacs-chocolate-theme"
                                 "emacs-doom-themes/themes")))

;; *** For melpa usage
(when (eq entropy/emacs-ext-elpkg-get-type 'submodules-melpa-local)
  (setq package-archives
        `(("entropy-melpa" . ,eemacs-ext-melpa-packages)
          ("entropy-elpa"  . ,eemacs-ext-elpa-packages))))


;; * Provide
(provide 'eemacs-ext-load)
