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

(define-minor-mode temporary-mode
  "Global minor mode which is mostly used to install the keymap
in which all keys will be (re)bound by `define-temporary-mode' by
default."
  :global     t
  :init-value t
  :keymap     (make-sparse-keymap))

(defmacro define-temporary-mode (mode doc &rest args)
  "Define a new temporary mode.

MODE is the name of the newly created mode.

DOC is the documentation string of the new mode.

If present, ARGS is given as extra arguments to `define-derived-mode'."
  (let* ((precmd   (temporary-mode/symbol mode "%s/precmd-hook"))
         (keymap   (temporary-mode/symbol mode "%s-map"))

         (bindings     (plist-get args :bindings))
         (install      (plist-get args :install))
         (rebind       (plist-get args :rebind))
         (lighter      (plist-get args :lighter))
         (entry-points (plist-get args :entry-points))
         (body         (plist-get args :body))

         local-keymap
         install-forms)

    ;; :lighter LIGHTER
    ;; ================
    (when lighter
      (setq lighter (list :lighter lighter)))

    ;; :rebind X
    ;; =========
    ;;
    ;; is an alias for:
    ;;   :install  X
    ;;   :bindings X
    (when rebind
      (setq install  rebind
            bindings rebind))

    ;; :bindings
    ;; =========
    ;;
    ;; set the local mode map in `local-keymap'
    (let ((bindings-prefix nil)
          (bindings-keymap 'global-map))
      (cond

       ;; :bindings PREFIX
       ;; ----------------
       ((stringp bindings)
        (setq bindings-prefix bindings))

       ;; :bindings (PREFIX . KEYMAP)
       ;; ---------------------------
       ((and (consp bindings)
             (stringp (car bindings))
             (keymapp (symbol-value (cdr bindings))))
        (setq bindings-prefix (car bindings)
              bindings-keymap (cdr bindings))))

      ;; Read an existing keymap
      (if bindings-prefix
          (setq local-keymap
                (lookup-key (symbol-value bindings-keymap) (kbd bindings-prefix)))

        ;; :bindings (list (KEY . COMMAND) ...)
        ;; ------------------------------------
        ;;
        ;; Define a new keymap
        (setq local-keymap
              (let ((map (make-sparse-keymap)))
                (mapc (lambda (binding)
                        (define-key map (kbd (car binding)) (cdr binding)))
                      bindings)
                map))))

    ;; :entry-points
    ;; =============
    (setq entry-points (or entry-points 'all))
    (unless (eq entry-points 'all)
      ;; All strings (keys) are replaced by their first letter
      ;; (we only support simple chords, not sequences)
      (setq entry-points (mapcar (lambda (entry)
                                   (if (stringp entry)
                                       (aref entry 0)
                                     entry))
                                 entry-points)))

    ;; :install
    ;; ========
    ;;
    ;; Put all forms needed to install global bindings in `install-forms'
    (when install
      (let ((install-prefix nil)
            (install-keymap 'temporary-mode-map))
        (cond

         ;; :install PREFIX
         ;; ---------------
         ((stringp install)
          (setq install-prefix install))

         ;; :install (PREFIX GLOBAL-KEYMAP)
         ;; -------------------------------
         ((consp install)
               (setq install-prefix (car install)
                     install-keymap (cdr install))))

        (setq install-prefix (string-to-vector (kbd install-prefix)))
        (setq install-forms
              (let ((forms nil))
                (map-keymap (lambda (key command)
                              (when (or (eq entry-points 'all)
                                        (memq command entry-points)
                                        (memq key     entry-points))
                                (push `(define-key
                                         ,install-keymap
                                         ,(vconcat install-prefix
                                                   (vector key))
                                         (defun ,(intern (format "%s/%s"
                                                                 (symbol-name mode)
                                                                 (symbol-name command)))
                                             ()
                                           ,(format "Activate `%s' and execute `%s'."
                                                    (symbol-name mode)
                                                    (symbol-name command))
                                           (interactive)
                                           (unless ,mode
                                             (,mode 1))
                                           (call-interactively ',command)))
                                      forms)))
                            local-keymap)
                forms))))

    `(progn

       ;; pre-command-hook used to exit the minor mode when pressing a
       ;; locally-unhandled key sequence.
       (defun ,precmd ()
         ,(format "Exit `%s' when executing another command." (symbol-name mode))
         (unless (eq this-command
                     (lookup-key ,keymap (this-command-keys-vector)))
           (,mode -1)))

       ;; Actually define the temporary minor mode
       (define-minor-mode ,mode
         ,(concat doc "\n\n"
                  (format "\\{%s}" (symbol-name keymap)))
         ,@lighter
         :init-value nil
         :keymap ',local-keymap

         (if ,mode
             (add-hook 'pre-command-hook ',precmd)
           (remove-hook 'pre-command-hook ',precmd))

         ,@(when body
             (list body)))

       ;; Install global key bindings
       ,@install-forms)))

(provide 'temporary-mode)

;;; temporary-mode.el ends here
