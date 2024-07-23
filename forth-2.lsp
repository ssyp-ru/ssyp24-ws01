;;
;; ЛШЮП-2024
;;  Простой Форт
;; 
;; авт:  Личман В.И.
;;

(defun car_stk (wrd_ stk_ if_list_ else_list_ loop_list_)
    (let ((wrd wrd_)
          (stk stk_)
          (if_list if_list_)
          (else_list else_list_)
          (loop_list loop_list_))
        (cond
            ((eq (car stk) "if")
                 (pop stk)
                 (setq if_list(append if_list (list wrd)))
                 (push "if" stk))

            ((eq (car stk) "else")
                 (pop stk)
                 (setq else_list(append else_list (list wrd)))
                 (push "else" stk))

            ((eq (car stk) "loop")
                 (pop stk)
                 (setq loop_list (append loop_list (list wrd)))
                 (push "loop" stk))

            (t (return (list wrd_ stk_ if_list_ else_list_ loop_list_))))
        (list wrd stk if_list else_list loop_list)))

(defun exec (lst_ stk_ dict_ v_stk_ q_ r_)
    (let ((lst lst_)
          (stk stk_)
          (q q_)
          (list_dict nil)
          (dict dict_)
          (v_stk v_stk_)
          (if_list nil)
          (else_list nil)
          (loop_list nil)
          (lst_all nil)
          (r r_)
          (index nil)
          (limit nil)
          (flag 0))

        (iter (for word! in lst)

            (iter (for wrd in dict)
                (when (and(not(eq (car stk)"forget_word"))(eq word! (car wrd))(= r 0))
                   (progn
                          (setq lst_all (exec (cdr wrd) stk dict v_stk q r))
                          (setq lst (car lst_all))
                          (setq stk (cadr lst_all))
                          (setq dict (caddr lst_all))
                          (setq v_stk (nth 5 lst_all))
                          (setq q (nth 6 lst_all))
                          (setq r (nth 7 lst_all))
                          (setq flag 1)
                          (return t))))

            (if (= flag 0)
              (cond ((eq word! "quit")
                       (printline "Bye!") (setq q t))

                    ((eq word! "then")
                       (if (= r 1)
                          (progn (pop stk)
                             (setq r 0)
                             (if (= v_stk 0)
                                (progn
                                    (setq lst_all (exec else_list stk dict v_stk q 0))
                                    ;;(setq lst (car lst_all))
                                    (setq stk (cadr lst_all))
                                    (setq dict (caddr lst_all))
                                    (setq q (nth 4 lst_all)))
                                (progn 
                                    (setq lst_all (exec if_list stk dict v_stk q 0))
                                    ;;(setq lst (car lst_all))
                                    (setq stk (cadr lst_all))
                                    (setq dict (caddr lst_all))
                                    (setq q (nth 4 lst_all)))))
                          (progn(setq r (- r 1))
                             (setq lst_all (car_stk word! stk if_list else_list loop_list))
                             (setq if_list (caddr lst_all))
                             (setq else_list (cadddr lst_all))
                             (setq loop_list (nth 5 lst_all)))))

                 ((eq word! "if")
                      (if (= r 0)
                         (progn 
                              (setq v_stk (car stk))
                              (push "if" stk)
                              (setq r 1)
                              (say v_stk))
                              
                         (progn(setq r (+ r 1))
                              (setq lst_all (car_stk word! stk if_list else_list loop_list))
                              (setq if_list (caddr lst_all))
                              (setq else_list (cadddr lst_all))
                              (setq loop_list (nth 5 lst_all)))))
                              
                 ((and (eq word! "else")(= r 1))
                     (pop stk)
                     (push "else" stk))

                 ((eq word! "loop")
                       (if (= r 1)
                          (progn 
                             (pop stk)
                             (setq r (- r 1))
                             (iter (for i from index to limit)
                                (setq lst_all (exec loop_list stk dict v_stk q 0))
                                (setq lst (car lst_all))
                                (setq stk (cadr lst_all))
                                (setq dict (caddr lst_all))
                                (setq v_stk (nth 3 lst_all))
                                (setq q (nth 4 lst_all))))
                                (progn
                                   (setq r (- r 1))
                                   (setq lst_all (car_stk word! stk if_list else_list loop_list))
                                   (setq if_list (caddr lst_all))
                                   (setq else_list (cadddr lst_all))
                                   (setq loop_list (nth 5 lst_all)))))

                 ((eq word! "do")
                      (if (= r 0)
                        (progn 
                             (setq r (+ r 1))
                             (setq index (pop stk))
                             (setq limit (pop stk))
                             (push "loop" stk))
                        (progn(setq r (+ r 1))
                             (setq lst_all (car_stk word! stk if_list else_list loop_list))
                             (setq if_list (caddr lst_all))
                             (setq else_list (cadddr lst_all))
                             (setq loop_list (nth 5 lst_all)))))

                   ((eq (car stk) "if")
                        (progn
                             (pop stk)
                             (if (not(= v_stk 0))
                             (setq if_list(append if_list (list word!))))
                             (push "if" stk)))

                    ((eq (car stk) "else")
                         (progn
                             (pop stk)
                             (when (= v_stk 0) ;; !!! if
                                 (setq else_list (append else_list (list word!))))
                             (push "else" stk)))

                    ((eq word! "do")
                          (setq index (pop stk))
                          (setq limit (pop stk))
                          (push "loop" stk))

                    ((eq (car stk) "loop")
                          (pop stk)
                          (setq loop_list (append loop_list (list word!)))
                          (push "loop" stk))

                    ((eq (car stk) "forget_word")
                          (pop stk)
                          (setq dict (remove-if (lambda (p) (eq (car p) word!)) dict)))

                    ((eq word! ":")
                          (push "new_name" stk))

                    ((eq word! ";")
                          (pop stk)
                          (iter (for i in dict)
                              (if (eq (car i) (car list_dict))(remove i dict)))
                          (push list_dict dict)
                          (setq list_dict nil))

                    ((eq (car stk) "new_name")
                          (pop stk)
                          (setq list_dict (append list_dict (list word!)))
                          (push "new_name" stk))

                    ((eq word! "forget")
                          (push "forget_word" stk))

                    ((eq word! "saveDict")
                          (push "save" stk))

                    ((eq (car stk) "save")
                          (pop stk)
                          (filOpen 'save_dict word! _OUTPUT)
                          (filPutLine 'save_dict (output dict))
                          (filClose 'save_dict))

                    ((eq word! "loadDict")
                          (push "load" stk))

                    ((eq (car stk) "load")
                          (pop stk)
                          (filOpen 'load_dict word! _INPUT)
                          (setq dict (input (filGetLine 'load_dict)))
                          (filClose 'load_dict))

                    ((eq word! "!out") 
                          (printline stk))

                    ((eq word! "!dict")
                          (printline dict))

                    ((eq word! "=")
                        (if (= (pop stk)(pop stk))
                            (push -1 stk)
                            (push 0 stk)))

                    ((eq word! ">")
                        (if (<= (pop stk)(pop stk))
                            (push -1 stk)
                            (push 0 stk)))
                                         
                    ((eq word! "<")
                         (if (>= (pop stk)(pop stk))
                             (push -1 stk)
                             (push 0 stk)))

                    ((eq word! "+")
                         (push (+ (pop stk)(pop stk)) stk))

                    ((eq word! "-")
                       (let ((a2 (pop stk))
                             (a1 (pop stk)))
                          (push (- a1 a2) stk)))

                    ((eq word! "*")
                       (push ( * (pop stk) (pop stk)) stk))

                    ((eq word! "/")
                       (let ((a2 (pop stk))
                             (a1 (pop stk)))
                       (push (/ a1 a2) stk)))
 
                 ((eq word! "or")
                      (push (bit2fix (logOr (fix2bit(pop stk)) (fix2bit(pop stk)))) stk))  

                 ((eq word! "and")
                      (push (bit2fix (logAnd (fix2bit(pop stk)) (fix2bit(pop stk)))) stk))      
 
                 ((eq word! "xor")
                      (push (bit2fix (logXor (fix2bit(pop stk)) (fix2bit(pop stk)))) stk))

                 ((eq word! "mod")
                      (let ((a2 (pop stk))
                            (a1 (pop stk)))
                           (push(% a1 a2)stk))) 

                 ((eq word! ".")
                     (prints (pop stk)))

                 ((eq word! "!.")
                     (printline (car stk)))

                 ((eq word! "clear_stack")
                     (setq stk nil))

                 ((eq word! "clear_dict")
                     (setq dict nil))

                 ((eq word! "drop")
                     (pop stk))

                 ((eq word! "over")
                     (push (cadr stk)stk))

                 ((eq word! "dup")
                     (push(car stk)stk))

                 ((eq word! "rot")
                     (let ((a1 (pop stk))
                           (a2 (pop stk))
                           (a3 (pop stk)))
                        (push a2 stk)
                        (push a1 stk)
                        (push a3 stk)))

                 ((eq word! "pick")
                        (push (nth (car stk) stk) stk))

                 ((eq word! "swap")
                     (let((a1 (pop stk))
                          (a2 (pop stk)))
                       (push a1 stk)
                       (push a2 stk)))

                 ((isNumber word!)
                       (push (input word!) stk))

                 (t (printline "Неизвестная команда!")))

         (setq flag 0))
         (list lst stk dict v_stk q r))))

(defun forth ()
    (let ((stk ())
          (q nil)
          (list_dict ())
          (dict ())
          (v_stk nil)
          (lst_all ()))
      (loop
        (try
            (let ((lst (strWords (read t))))
               (setq lst_all (exec lst stk dict v_stk q 0))
               (setq lst (car lst_all))
               (setq stk (cadr lst_all))
               (setq dict (caddr lst_all))
               (setq v_stk (nth 3 lst_all))
               (setq q (nth 4 lst_all))
               (when (eq q t) (return t)))
        except 
               (printline (errormessage)))
        (when (eq q t) (return t)))))
