((nil . ((eval . (when (and 
                        (buffer-file-name)
                        (not (file-directory-p (buffer-file-name)))
                        (string-match-p "^[^.]" (buffer-file-name)))
                   (unless (derived-mode-p 'emacs-lisp-mode)
                     (emacs-lisp-mode)))))))
