(load "patmatch.lisp")

(defparameter *occurs-check*
  t
  "Should we do the occurs check?")


(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings"
  (cond ((eq bindings fail) fail)
        ((eq x y) bindings)
        ((variable-p x) (unify-variable x y bindings))
        ((variable-p y) (unify-variable y x bindings))
        ((and (consp x) (consp y))
         (unify (rest x) (rest y)
                (unify (first x) (first y) bindings)))
        (t fail)))


(defun lookup (var bindings)
  "Get the value part (for var) from a binding list"
  (binding-val (get-binding var bindings)))

(defun occurs-check (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (variable-p x) (get-binding x bindings))
         (occurs-check var (lookup x bindings) bindings))
        ((consp x) (or (occurs-check var (first x) bindings)
                       (occurs-check var (rest x) bindings)))
        (t nil)))

;; still not grokking the difference
;; between (lookup x bindings)
;; and (get-binding x bindings)

(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
taking recursively bound variables into account"
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((and (variable-p x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))

(defun unifier (x y)
  "Return something that unifies both x and y (or fail)"
  (subst-bindings (unify x y) x))
