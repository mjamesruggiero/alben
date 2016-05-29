(defconstant fail nil)

(defconstant no-bindings '((t . t)))

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon but uses append instead of nconc"
  (apply #'append (mapcar fn list)))

(defun mklist (x)
  "Return x if it is a list, otherwise (x)"
  (if (listp x)
      x
      (list x)))

(defun flatten (the-list)
  "Append together elements (or lists) in the list"
  (mappend #'mklist the-list))

(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern ((?* var) . pat)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input"
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        ;; We assume that pat starts with a constant
        ;; In other words, a pattern can't have 2 consecutive vars
        (let ((pos (position (first pat) input
                             :start start :test #'equal)))
          (if (null pos)
              fail
              (let ((b2 (pat-match
                         pat (subseq input pos)
                         (match-variable var (subseq input 0 pos)
                                         bindings))))
                ;; If this match failed, try another longer one
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-match pattern input bindings))
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input)
                         bindings)))
        (t fail)))

(defun match-variable (var input bindings)
  "Does var match input? Uses (or updates) bindings and returns them"
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun make-binding (var val)
  (cons var val))

(defun binding-var (binding)
  "Get the variable part of a single binding"
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding"
  (cdr binding))

(defun get-binding (var bindings)
  "Find a (variable . val) pair in a binding list"
  (assoc var bindings))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list"
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Ad a (var . value) pair to a binding list"
  (cons (cons var val)
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun read-line-no-punct ()
  "Read an input line, ignoring punctuation"
  (read-from-string
   (concatenate 'string "(" (substitute-if #\space #'punctuation-p
                                           (read-line))
                ")")))

(defun punctuation-p (char)
  (find char ",,;:`!?#-()\\\""))

(defun eliza ()
  "Respond to user input using pattern matching rules"
  (loop
    (print 'eliza>)
    (let* ((input (read-line-no-punct))
           (response (flatten (use-eliza-rules input))))
      (print-with-spaces response)
      (if (equal response )))))

(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y))
     (How do you do. Please state your problem.))
    (((?* ?x) computer (?* ?y))
     (Do computers worry about you?)
     (What do you think about computers?)
     (What do you think machines have to do with your problem?))
    (((?* ?x) name (?* ?y))
     (I am not interested in names))
    (((?* ?x) sorry (?* ?y))
     (Please don't apologize.)
     (Apologies are not necessary.)
     (What feelings do you have when you apologize?))
    (((?* ?x) I remember (?* ?y))
     (Do you often think of ?y)
     (Does thinking of ?y bring anything else to mind?)
     (What else do you remember?)
     (Why do you recall ?y right now?)
     (What in the present situation reminds you of ?y ?)
     (What is the connection between me and ?y ?))
    (((?* ?x) do you remember (?* ?y))
     (Did you think I would forget ?y ?)
     (Why do you think I should recall ?y now?)
     (What about ?y)
     (You mentioned ?y .))
    (((?* ?x) if (?* ?y))
     (Do you really think it is likely that ?y ?)
     (Do you wish that ?y ?)
     (What do you think about ?y ?)
     (Really -- if ?y ?))
    (((?*  ?x) if (?* ?y))
     (Do you really think it's likely that ?y)
     (Do you wish that ?y)
     (What do you think about ?y)
     (Really -- if ?y))
    (((?* ?x) I dreamt (?* ?y))
     (Really -- ?y)
     (Have you ever fantasized ?y while you were awake?)
     (Have you dreamt about ?y before?))
    (((?* ?x) dream about (?* ?y))
     (How do you feel about ?y in reality?))
    (((?* ?x) dream (?* ?y))
     (What does this dream suggest to you?)
     (Do you dream often?)
     (What persons appear in your dreams?)
     (Don't you believe that dream has something to do with your problem?))
    (((?* ?x) my mother (?* ?y))
     (Who else in your family ?y ?)
     (Tell me more about your family.))
    (((?* ?x) my father (?* ?y))
     (Your father.)
     (Does he influence you strongly?)
     (What else comes to mind when you think about your father?))
    (((?* ?y) I want (?* ?y))
     (What would it mean if you got ?y ?)
     (Why do you want ?y ?)
     (Suppose you got y? soon?))
    (((?* ?y) I am glad (?* ?y)
      (How have I helped you to be ?y)
      (What makes you happy just now?)
      (Can you explain why you are suddenly ?y ?)))
    (((?* ?x) I am sad (?* ?y))
     (I'm sorry to hear you are depressed.)
     (I'm sure it's not pleasant to be sad.))))
