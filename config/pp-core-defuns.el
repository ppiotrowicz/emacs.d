(require 'dash)

(defmacro add-hook! (&rest args)
  "A convenience macro for `add-hook'. Takes, in order:

  1. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  2. The hooks: either an unquoted major mode, an unquoted list of major-modes,
     a quoted hook variable or a quoted list of hook variables. If unquoted, the
     hooks will be resolved by appending -hook to each symbol.
  3. A function, list of functions, or body forms to be wrapped in a lambda.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! :append (one-mode second-mode) 'enable-something)
    (add-hook! :local (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))
    (add-hook! :append :local (one-mode second-mode) (setq v 5) (setq a 2))

Body forms can access the hook's arguments through the let-bound variable
`args'."
  (declare (indent defun) (debug t))
  (let ((hook-fn 'add-hook)
        append-p local-p)
    (while (keywordp (car args))
      (pcase (pop args)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq hook-fn 'remove-hook))))
    (let ((hooks (doom--resolve-hooks (pop args)))
          (funcs
           (let ((val (car args)))
             (if (memq (car-safe val) '(quote function))
                 (if (cdr-safe (cadr val))
                     (cadr val)
                   (list (cadr val)))
               (list args))))
          forms)
      (dolist (fn funcs)
        (setq fn (if (symbolp fn)
                     `(function ,fn)
                   `(lambda (&rest _) ,@args)))
        (dolist (hook hooks)
          (push (cond ((eq hook-fn 'remove-hook)
                       `(remove-hook ',hook ,fn ,local-p))
                      (t
                       `(add-hook ',hook ,fn ,append-p ,local-p)))
                forms)))
      `(progn ,@(nreverse forms)))))

;;;###autoload
(defmacro def-popup! (&rest params)
  `(push ',params shackle-rules))
(provide 'pp-core-defuns)

;;;###autoload
(defvar doom-memoized-table (make-hash-table :test 'equal :size 10)
  "A lookup table containing memoized functions. The keys are argument lists,
and the value is the function's return value.")

;;;###autoload
(defun doom-memoize (name)
  "Memoizes an existing function. NAME is a symbol."
  (let ((func (symbol-function name)))
    (put name 'function-documentation
         (concat (documentation func) " (memoized)"))
    (fset name
          `(lambda (&rest args)
             (let ((key (cons ',name args)))
               (or (gethash key doom-memoized-table)
                   (puthash key (apply ',func args)
                            doom-memoized-table)))))))

;;;###autoload
(defmacro def-memoized! (name arglist &rest body)
  "Create a memoize'd function. NAME, ARGLIST, DOCSTRING and BODY
have the same meaning as in `defun'."
  (declare (indent defun) (doc-string 3))
  `(,(if (bound-and-true-p byte-compile-current-file)
         'with-no-warnings
       'progn)
     (defun ,name ,arglist ,@body)
     (doom-memoize ',name)))

(defvar doom--transient-counter 0)
(defmacro add-transient-hook! (hook &rest forms)
  "Attaches transient forms to a HOOK.

HOOK can be a quoted hook or a sharp-quoted function (which will be advised).

These forms will be evaluated once when that function/hook is first invoked,
then it detaches itself."
  (declare (indent 1))
  (let ((append (eq (car forms) :after))
        (fn (intern (format "doom-transient-hook-%s" (cl-incf doom--transient-counter)))))
    `(when ,hook
       (fset ',fn
             (lambda (&rest _)
               ,@forms
               (cond ((functionp ,hook) (advice-remove ,hook #',fn))
                     ((symbolp ,hook)   (remove-hook ,hook #',fn)))
               (unintern ',fn nil)))
       (cond ((functionp ,hook)
              (advice-add ,hook ,(if append :after :before) #',fn))
             ((symbolp ,hook)
              (add-hook ,hook #',fn ,append))))))

(defun doom--resolve-hooks (hooks)
  (let ((quoted-p (eq (car-safe hooks) 'quote)))
    (when quoted-p
      (setq hooks (cadr hooks)))
    (cl-loop with hooks = (if (listp hooks) hooks (list hooks))
             for hook in hooks
             if (eq (car-safe hook) 'quote)
               collect (cadr hook)
             else if quoted-p
               collect hook
             else collect (intern (format "%s-hook" (symbol-name hook))))))

(defmacro after! (feature &rest forms)
  "A smart wrapper around `with-eval-after-load'. Supresses warnings during
compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (bound-and-true-p byte-compile-current-file))
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         #'progn
       #'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))
