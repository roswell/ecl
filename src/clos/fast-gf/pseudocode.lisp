;;; what about change class for eql-specializers?
;;; -> former class invalidate
;;; -> later  class invalidate

(tagbody
   (if (< stamp 7)
       (if (< stamp 4)
           (if (= stamp 1)
               (go m1)
               (go miss))
           (go m2))
       (if (= stamp 8)
           (go m3)
           (if (= stamp 10)
               (go m4)
               (go miss))))
 m1*
   ;; ...
   (go out)
 m2*
   ;; ...
   (go out)
 m1
   ;; the function has eql specializers of the class numbered 1
   (case arg
     (eql-value-1 (go m1*))
     (eql-value-2 (go m2*)))

   ;; invoke method for class numbered 1
   ;; ...
   (go out)
 m2
   ;; invoke method for classes numbered 4, 5, 6
   ;; ...
   (go out)
 m3
   ;; invoke method for class numbered 8
   ;; ...
   (go out)
 m4
   ;; invoke method for class numbered 10
   ;; ...
   (go out)
 miss
   ;; handle miss
   ;; ...
 out)
