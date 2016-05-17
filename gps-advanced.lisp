(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been identified"
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun debug (&rest ids)
  "Start dbg output on the given ids"
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids"
  (setf *dbg-ids* (if (null ids) nil
             (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified"
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ " " *debug-io*))
    (apply #'format *debug-io* format-string args)))

(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun executing-p (x)
  "Is this list in the form of (executing ...)"
  (starts-with x 'executing))

(defun convert-op (op)
  "Make op conform to the (excuting op) convention"
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (executing op) convention"
  (convert-op
   (make-op :action action :preconds preconds
            :add-list add-list :del-list del-list)))

(defvar *ops* nil "A list of available operators.")

(defstruct op
  "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable"
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state2)
      ;;return an updated state
      (dbg-indent :gps (length goal-stack) "Action: ~a" (ops-action op))
      (append (remove-if #'(lambda (x)
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is its add list"
  (member-equal goal (op-add-list op)))

(defun use (oplist)
  "Use oplist as the default list of operators"
  ;; return something useful, but not too verbose
  ;; the number of operators
  (length (setf *ops* oplist)))

;; -------------------------------------------------------------
;; the earlier #achieve and #achieve-all methods Norvig defines
;; -------------------------------------------------------------

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
or if there is an appropriate op fo it that is applicable"
  (dbg-indent :gps (length goal-stack) "Goal ~a" goal)
  (cond ((member-equal goal-state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriate-p)))))

(defun achieve-all (state goals goal-stack)
  "Achieve each goal and make sure they still hold at the end"
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defparameter *banana-ops*
  (list
   (op 'climb-on-chair
       :preconds '(chair-at-middle-room at-middle-room on-floor)
       :add-list '(at-bananas on-chair)
       :del-list '(at-middle-room on-floor))
   (op 'push-chair-from-door-to-middle-room
       :preconds '(chair-at-door at-door)
       :add-list '(chair-at-middle-room at-middle-room)
       :del-list '(chair-at-door at-door))
   (op 'walk-from-door-to-middle-room
       :preconds '(at-door on-floor)
       :add-list '(at-middle-room)
       :del-list '(at-door))
   (op 'grasp-bananas
       :preconds '(at-bananas empty-handed)
       :add-list '(has-bananas)
       :del-list '(empty-handed))
   (op 'drop-ball
       :preconds '(has-ball)
       :add-list '(empty-handed)
       :del-list '(has-ball))
   (op 'eat-bananas
       :preconds '(has-bananas)
       :add-list '(empty-handed not-hungry)
       :del-list '(has-bananas hungry))))

(defun GPS (state goals &optional (*ops* *ops*))
  "General problem solver: from state, achieve gols using ops"
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil)))
