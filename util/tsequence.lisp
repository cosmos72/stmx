;;;; sequence functions


(defgeneric treverse (x)
  (:documentation
   "Return a new transactional sequence containing the same elements but in reverse order."))

(defmethod treverse ((x (eql nil)))
  nil)

(defmethod treverse ((x tcons))
  (declare (type tcons x))
  (let ((copy))
    (do-tlist (obj x)
      (tpush obj copy))
    copy))



(defgeneric tnreverse (x)
  (:documentation
   "Return a transactional sequence of the same elements in reverse order;
  the argument is destroyed."))

(defmethod tnreverse ((x (eql nil)))
  nil)

(defmethod tnreverse ((x tcons))
  #-(and)
  (loop
     for top = x then curr
     for curr = (prog1 (tcons-rest x)
                  (setf (tcons-rest x) nil)) then next
     with next
     until (tendp curr)
     do
       (setf next (tcons-rest curr)
             (tcons-rest curr) top)
     finally (return top))

  ;; equivalent to loop above, shorter compiled code on SBCL
  #+(and)
  (do ((top x curr)
       (curr (prog1 (tcons-rest x) (setf (tcons-rest x) nil))
             next)
       (next))
      ((tendp curr) top)

    (setf next (tcons-rest curr)
          (tcons-rest curr) top)))


