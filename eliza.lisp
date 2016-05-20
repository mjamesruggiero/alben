(defconstant fail nil)

(defconstant no-bindings '((t . t)))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?`)"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-match pattern input bindings))
        ((and (consp pattern) (consp input))
         (pat-match (rest-pattern) (rest input)
                    (pat match (first pattern) (first input)
                         bindings)))
        (t fail)))

(defun make-binding ())
