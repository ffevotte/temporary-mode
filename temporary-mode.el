;;; temporary-mode.el --- Define temporary key bindings

;;; Commentary:
;;

;;; Code:

(defun temporary-mode/symbol (symbol format-string)
  "Generate a symbol named after SYMBOL.

FORMAT-STRING is a control string containing '%s', which will be
replaced by the name of SYMBOL to form the name of the newly
created symbol."
  (intern (format format-string (symbol-name symbol))))

(defmacro define-temporary-mode (name keys doc &rest args)
  "Define a new temporary mode.

NAME is the name of the newly created mode.

KEYS is a list of key bindings in the form (key . command),
where KEY is in a suitable format for `kbd' and COMMAND.

DOC is the documentation string of the new mode.

If present, ARGS is given as extra arguments to `define-derived-mode'."
  (let ((commands (temporary-mode/symbol name "%s/commands"))
        (update   (temporary-mode/symbol name "%s/update"))
        (advise   (temporary-mode/symbol name "%s/advise"))
        (setup    (temporary-mode/symbol name "%s/setup"))
        (precmd   (temporary-mode/symbol name "%s/precmd-hook"))
        (keymap   (temporary-mode/symbol name "%s-map")))
    `(progn
       (defvar ,keymap
         (let ((map (make-sparse-keymap)))
           ,@(mapcar (lambda (binding)
                       `(define-key map (kbd ,(car binding)) ,(cdr binding)))
                     keys)
           map)
         ,(format "Key map for `%s'" (symbol-name name)))

       (defvar ,commands nil
         ,(concat
           (format "List of commands handled by `%s'.\n" (symbol-name name))
           (format "It should be updated using `%s' after\n" (symbol-name update))
           (format "`%s' is modified." (symbol-name keymap))))

       (defun ,advise (command)
         ,(format "Advise COMMAND so that it activates `%s'." (symbol-name name))
         (eval
          (list 'defadvice command '(before ,name activate)
                '(unless ,name
                   (,name 1)))))

       (defun ,update ()
         ,(concat
           (format "Update the list of commands handled by `%s'.\n" (symbol-name name))
           (format "This command should be run each time `%s' is modified." (symbol-name keymap)))
         (map-keymap (lambda (key command)
                       (add-to-list (quote ,commands) command))
                     ,keymap)
         (mapc (quote ,advise) ,commands))

       (defun ,setup (prefix &optional keymap)
         ,(concat
           (format "Install global bindings for commands handled by `%s'.\n" (symbol-name name))
           (format "If PREFIX is given, all commands appearing in `%s'\n" (symbol-name keymap))
           "are installed under PREFIX in KEYMAP (which defaults to `global-map').\n"
           "\n"
           (format "Commands are also advised to activate `%s'."  (symbol-name name)))
         (let ((keymap (or keymap
                           global-map))
               (prefix (string-to-vector (kbd prefix))))
           (map-keymap (lambda (key command)
                         (define-key keymap (vconcat prefix (vector key)) command))
                       ,keymap)))

       (defun ,precmd ()
         ,(format "Exit `%s' when executing another command." (symbol-name name))
         (unless (memq this-command ,commands)
           (,name -1)))

       (define-minor-mode ,name
         ,(concat doc "\n\n"
                  (format "\\{%s}" (symbol-name keymap)))
         ,@args

         (if ,name
             (add-hook 'pre-command-hook (quote ,precmd))
           (remove-hook 'pre-command-hook (quote ,precmd))))

       (,update))))

(provide 'temporary-mode)

;;; temporary-mode.el ends here
