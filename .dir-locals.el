;;; .dir-locals.el -- Per-directory local variables for GNU Emacs 23 and later.

((nil         . ((fill-column . 80)
                 (tab-width . 4)))
 (c-mode      . ((c-file-style . "gnu")))
 (scheme-mode . ((indent-tabs-mode . nil)
                 (eval . (put 'sxml-match-let 'scheme-indent-function 2))
                 (eval . (put 'sxml-match 'scheme-indent-function 1)))))

;;; .dir-locals.el ends here
