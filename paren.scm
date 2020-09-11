;; This extracted scheme code relies on some additional macros
;; available at http://www.pps.univ-paris-diderot.fr/~letouzey/scheme
(load "macros_extr.scm")


(define add (lambdas (n m) (match n
                              ((O) m)
                              ((S p) `(S ,(@ add p m))))))
  
(define sub (lambdas (n m)
  (match n
     ((O) n)
     ((S k) (match m
               ((O) n)
               ((S l) (@ sub k l)))))))
  
(define length (lambda (s)
  (match s
     ((EmptyString) `(O))
     ((String _ s~) `(S ,(length s~))))))
  
(define substring (lambdas (n m s)
  (match n
     ((O)
       (match m
          ((O) `(EmptyString))
          ((S m~)
            (match s
               ((EmptyString) s)
               ((String c s~) `(String ,c ,(@ substring `(O) m~ s~)))))))
     ((S n~) (match s
                ((EmptyString) s)
                ((String _ s~) (@ substring n~ m s~)))))))
  
(define _position_of_matching_parenthesis (lambdas (str position count)
  (match count
     ((O) `(Some ,position))
     ((S _)
       (match str
          ((EmptyString) `(None))
          ((String c str~)
            (match c
               ((Ascii b b0 b1 b2 b3 b4 b5 b6)
                 (match b
                    ((True)
                      (match b0
                         ((True)
                           (@ _position_of_matching_parenthesis str~
                             (@ add position `(S ,`(O))) count))
                         ((False)
                           (match b1
                              ((True)
                                (@ _position_of_matching_parenthesis str~
                                  (@ add position `(S ,`(O))) count))
                              ((False)
                                (match b2
                                   ((True)
                                     (match b3
                                        ((True)
                                          (@ _position_of_matching_parenthesis
                                            str~ (@ add position `(S ,`(O)))
                                            count))
                                        ((False)
                                          (match b4
                                             ((True)
                                               (match b5
                                                  ((True)
                                                    (@ _position_of_matching_parenthesis
                                                      str~
                                                      (@ add position `(S
                                                        ,`(O))) count))
                                                  ((False)
                                                    (match b6
                                                       ((True)
                                                         (@ _position_of_matching_parenthesis
                                                           str~
                                                           (@ add position
                                                             `(S ,`(O)))
                                                           count))
                                                       ((False)
                                                         (@ _position_of_matching_parenthesis
                                                           str~
                                                           (@ add position
                                                             `(S ,`(O)))
                                                           (@ sub count `(S
                                                             ,`(O)))))))))
                                             ((False)
                                               (@ _position_of_matching_parenthesis
                                                 str~
                                                 (@ add position `(S ,`(O)))
                                                 count))))))
                                   ((False)
                                     (@ _position_of_matching_parenthesis
                                       str~ (@ add position `(S ,`(O)))
                                       count))))))))
                    ((False)
                      (match b0
                         ((True)
                           (@ _position_of_matching_parenthesis str~
                             (@ add position `(S ,`(O))) count))
                         ((False)
                           (match b1
                              ((True)
                                (@ _position_of_matching_parenthesis str~
                                  (@ add position `(S ,`(O))) count))
                              ((False)
                                (match b2
                                   ((True)
                                     (match b3
                                        ((True)
                                          (@ _position_of_matching_parenthesis
                                            str~ (@ add position `(S ,`(O)))
                                            count))
                                        ((False)
                                          (match b4
                                             ((True)
                                               (match b5
                                                  ((True)
                                                    (@ _position_of_matching_parenthesis
                                                      str~
                                                      (@ add position `(S
                                                        ,`(O))) count))
                                                  ((False)
                                                    (match b6
                                                       ((True)
                                                         (@ _position_of_matching_parenthesis
                                                           str~
                                                           (@ add position
                                                             `(S ,`(O)))
                                                           count))
                                                       ((False)
                                                         (@ _position_of_matching_parenthesis
                                                           str~
                                                           (@ add position
                                                             `(S ,`(O)))
                                                           (@ add count `(S
                                                             ,`(O)))))))))
                                             ((False)
                                               (@ _position_of_matching_parenthesis
                                                 str~
                                                 (@ add position `(S ,`(O)))
                                                 count))))))
                                   ((False)
                                     (@ _position_of_matching_parenthesis
                                       str~ (@ add position `(S ,`(O)))
                                       count)))))))))))))))))
  
(define position_of_matching_parenthesis (lambdas (str position)
  (@ _position_of_matching_parenthesis
    (@ substring (@ add position `(S ,`(O))) (length str) str) position `(S
    ,`(O)))))

