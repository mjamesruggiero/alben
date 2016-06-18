(defconstant fail nil)

(defconstant no-bindings '((t . t)))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun make-binding (var val)
  (cons var val))

(defun binding-var (binding)
  "Get the variable part of a single binding"
  (car binding))

(defun binding-val (binding)
  "Get the value part of a binding"
  (cdr binding))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list"
  (cons (cons var val)
        ;; when you add a real binding
        ;; you can get rid of dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with ?)"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun match-variable (var input bindings)
  "Does var match input? Uses or updates and returns bindings"
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun segment-match-fn (x)
  "Get the segment-match function for x,
  if it is a symbol that has one"
  (when (symbolp x) (get x 'segment-match)))

(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  (and (consp pattern) (consp (first pattern))
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun single-match-fn (x)
  "Get the single-match function for x,
  if x is a symbol that actually has one"
  (when (symbolp x) (get x 'single-match)))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern
  e.g. (?is x predicate) (?and . patterns) (?or . patterns)"
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern"
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern"
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings."
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-matcher pattern input bindings))
        ((single-pattern-p pattern)
         (single-matcher pattern input bindings))
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input)
                               bindings)))
        (t fail)))
