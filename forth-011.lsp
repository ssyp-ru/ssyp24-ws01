
;; Простой ФОРТ Версия 11 от 30.08.2024
;; Запускать в версии HomeLisp 1.13.69
;; Файфель Б.Л. catstail@ya.ru

;; Сохранить стек и словарь во внешнем файле

(defun save-in-file (fname dict stack)
   (let ((fo (gensym 'fo)))
        (filOpen fo fname _OUTPUT)
        (filPutLine fo (output stack))
        (iter (for tmp in dict)
           (filPutLine fo (strCat "(" (strChr 34) (output (car tmp)) (strChr 34)))
           (filPutLine fo (strCat "      " (output (cadr tmp)) ")"))
           (filPutLine fo ""))
        (filClose fo)))

;; Вывести состояние словаря в консоль ФОРТа

(defun show-dict (dict)
        (iter (for tmp in dict)
           (printsline (strCat (output (car tmp))))
           (printsline (strCat "      " (output (cadr tmp))))
           (printsline "")))

;; Восстановить стек и словарь из внешнего файла

(defun restore-from-file (fname)
   (let ((fi (gensym 'fi))
         (new-stack nil)
         (new-dict  nil)
         (tmp "("))
       (filOpen fi fname _INPUT)
       (setq new-stack (input (filGetLine fi)))
       (loop
            (when (filEOF fi) (return nil))
            (setq tmp (strCat tmp (filGetLine fi))))
       (setq tmp (strCat tmp ")")) 
       (setq new-dict (input tmp))
       (list new-stack new-dict)))     

;;(defun logNot (n)
;;  (cond ((= n -1) 0)
;;        ((>= n 0) (- (+ 1 n)))
;;        (t (+ 1 (abs n)))))

;; Увеличить служебную переменную $do$ 

(defun LvDoIncr (dict incr)
   (mapcar (lambda (p) (if (eq (car p) "$do$") (list "$do$" (+ incr (cadr p))) p)) dict))

;; Получить значение служебной переменной $do$

(defun LvDo (dict)
   (cadr (assoc "$do$" dict)))

;; Разбиение на слова

(defun strWords (stri)
  (let ((wd "")
        (res nil)
        (fldq nil))
    (iter (for a in-string stri)
          (cond ((eq a " ")
                 (if (not fldq)
                     (when (> (strLen wd) 0) (collecting wd into res) (setq wd ""))
                     (setq wd (strCat wd a))))
                ((eq a (strChr 34))
                     (setq wd (strCat wd a))
                     (setq fldq (not fldq)))
                (t (setq wd (strCat wd a)))))
    (when (> (strLen wd) 0) (setq res (append res (list wd)))) res))

;; Создать переменую <name> со значением <value>

(defun setvar (dict name value)
    (cond ((null dict) nil)
          ((eq (caar dict) name)
               (cons (cons (caar dict) (list value)) (cdr dict)))
          (t (cons (car dict) (setvar (cdr dict) name value)))))

;; Парсер "do" - выделение тела текущего "do" и остатка

(defun parse-do (cmd-list)
   (let ((loop-count 0)
           (if-count 0)
         (body nil)
             (len-cmd-list (length cmd-list))
             (term 0))
     (iter (for cmd in cmd-list) (for p upfrom 0)
         (cond ((eq cmd "do")
                  (setq loop-count (+ 1 loop-count))
                          (when (> loop-count 1) (collecting cmd into body)))
               ((eq cmd "loop")
                  (setq loop-count (- loop-count 1))
                  (if (= 0 loop-count)
                          (return (setq term p))
                            (collecting cmd into body)))
               ((eq cmd "if") (setq if-count (+ if-count 1)) (collecting cmd into body))
               ((or (eq cmd "endif") (eq cmd "then")) (setq if-count (- if-count 1)) (collecting cmd into body))
               (t (collecting cmd into body))))
        (when (/= 0 if-count) (raiseerror "Error in nesting if and do!"))
      (if (< term len-cmd-list)
          (list body (subseq cmd-list (+ 1 term)))
          (list body nil))))

;; Парсер "if" - выделение тела текущего "if" и остатка

(defun parse-if (cmd-list)
  (let ((if-count 0)
        (loop-count 0)
        (if-true nil)
        (if-false nil)
        (flg-else nil)
        (len-cmd-list (length cmd-list))
        (term 0))

     (iter (for cmd in cmd-list) (for p upfrom 0)
         (cond ((eq cmd "do")   (setq loop-count (+ 1 loop-count)) (if flg-else (collecting cmd into if-false) (collecting cmd into if-true)))
               ((eq cmd "loop") (setq loop-count (- loop-count 1)) (if flg-else (collecting cmd into if-false) (collecting cmd into if-true)))
               ((eq cmd "if")
                    (setq if-count (+ 1 if-count))
                    (when (> if-count 1)
                       (if flg-else (collecting cmd into if-false) (collecting cmd into if-true))))

               ((eq cmd "else")
                        (if (= 1 if-count)
                                 (setq flg-else t)
                                   (if flg-else (collecting cmd into if-false) (collecting cmd into if-true))))

               ((or (eq cmd "endif") (eq cmd "then")) (setq if-count (- if-count 1))
                    (if (= 0 if-count) (return (setq term p))
                        (if flg-else (collecting cmd into if-false) (collecting cmd into if-true))))
               (flg-else (collecting cmd into if-false))
               (t (collecting cmd into if-true))))
     (when (/= loop-count 0) (raiseerror "Error in nesting do and if"))
     (if (< term len-cmd-list)
         (list if-true if-false (subseq cmd-list (+ 1 term)))
         (list if-true if-false nil))))

;; Извлечь слово из словаря

(defun get-word (w dict)
  (iter (for pair in dict)
     (when (eq (car pair) w) (return (cadr pair)))))

;; Выполнить список соманд cmd-list

(defun exec (cmd-list stack dict)

 (if (null cmd-list) (list stack dict nil)

  (let* ((word (car cmd-list))        ;; Очередное слово
         (rest (cdr cmd-list))        ;; Остаток команды
         (fd   (get-word word dict))) ;; Если есть в словаре

  ;;(when *trace-stack* (printsline (strCat "Stack: " (output stack)))
  ;;                (terpri))
  ;;(when *trace-dict*  (printsline (strCat "Dict:  " (output dict)))
  ;;                (terpri))
  ;;(printline word)

  (try

   (cond ;; Центральный переключатель

     ((eq word "quit")
          (list t t t))

     ((eq word ":")
          (let ((newword (car rest))
                (psemi   (position ";" rest)))
               (if (null psemi) (list '! "Semicolon not found")
                   (let ((ndict (remove-if (lambda (p) (eq (car p) newword)) dict)))
                        (exec (subseq rest (+ 1 psemi)) stack (cons (cons newword (list (subseq rest 1 psemi))) ndict))))))

     ((eq word "forget")
         (let ((newdict (remove-if (lambda (p) (eq (car p) (cadr cmd-list))) dict)))
              (exec (cddr cmd-list) stack newdict)))

     ((eq word "$do$") (raiseerror "Illegal word ($do$)"))

     (fd (let ((tmp (exec fd stack dict))) ;; !!! Если слово есть в словаре !!!
              (exec rest (car tmp) (cadr tmp))))

     ((eq word "if")
              (let* ((tmp (parse-if cmd-list))
                     (if-true  (car tmp))
                     (if-false (cadr tmp))
                     (reside   (caddr tmp))
                     (tmp-res   nil))

                (if (= 0 (car stack))
                     (setq tmp-res (exec if-false stack dict))
                     (setq tmp-res (exec if-true stack dict)))

                (if (caddr tmp-res) (append tmp-res (list t))
                                    (progn
                                        (exec reside (car tmp-res) (cadr tmp-res))))))

     ((eq word "do")
               (let* ((tmp (parse-do cmd-list))
                      (body (car tmp))
                      (reside (cadr tmp))
                      (start (pop stack))
                      (fin   (pop stack))
                      (tmp-res nil)
                      (flg-err nil)
                      (flg-leave nil))

                 (setq dict (LvDoIncr dict 1))

                 (iter (for $1 from start to fin)
                      (setq tmp-res (exec body stack dict))

                      (setq stack (car tmp-res))
                      (setq dict  (cadr tmp-res))
                      (when (caddr tmp-res) (return (setq flg-leave t)))
                      (when (eq (car tmp-res) '!) (return (setq flg-err t))))
                 (if flg-err tmp-res
                      (if flg-leave
                          (exec reside stack dict)
                          (exec reside stack (LvDoIncr dict -1))))))

     ((eq word "leave")
         (if (zerop (LvDo dict))
             (raiseerror "Leave outside do-loop")
             (let ((new-dict (LvDoIncr dict -1)))
                   (list stack new-dict t))))

     ((eq word ".")
              (printsline (car stack))
              (exec rest (cdr stack) dict))

     ((eq word ".s")
              (printsline (car stack))
              (exec rest stack dict))

     ((isNumber word)
              (exec rest (cons (input word) stack) dict))

     ((eq word "!stack")
              (printline stack)
              (exec rest stack dict))

     ((eq word "!clear")
              (exec rest nil dict))

     ((eq word "!dict")
              ;;(printline dict)
              (show-dict dict)
              (exec rest stack dict))

     ((eq word "mkvar")
              (let* ((var (car rest))
                     (val (car stack))
                     (svar (output var)))
              (when (eq svar "$do$") (raiseerror "Mkvar: illegal name"))
              (when (null val)       (raiseerror "Mkvar: stack is empty"))
              (exec (cdr rest) stack (cons (list svar (list (output val))) dict))))

     ((eq word "setvar")
              (let* ((var (car rest))
                     (val (car stack))
                     (svar (output var)))
              (when (eq svar "$do$") (raiseerror "Setvar: illegal name"))
              (when (null val)       (raiseerror "Setvar: stack is empty"))
              (exec (cdr rest) stack (setvar dict svar (list (output val))))))

     ((eq word "erase")
              (let* ((svar (output (car rest)))
                     (ndict (if (eq svar "$do$") (raiseerror "Erase: illegal name")(remove-if (lambda (p) (eq (car p) svar)) dict :count 1))))
                     (exec (cdr rest) stack ndict)))

     ((eq word "erase-all")
              (let* ((svar (output (car rest)))
                     (ndict (if (eq svar "$do$") (raiseerror "Erase: illegal name")(remove-if (lambda (p) (eq (car p) svar)) dict))))
                     (exec (cdr rest) stack ndict)))

     ((eq word "cat")
          (exec rest (cons (strCat (car stack) (cadr stack)) (cddr stack)) dict))

     ((eq word "dup")
              (exec rest (cons (car stack) stack) dict))

     ((eq word "drop")
              (exec rest (cdr stack) dict))

     ((eq word "over")
              (exec rest (cons (cadr stack) stack) dict))

     ((eq word "swap")
              (let ((a2 (car stack))
                    (a1 (cadr stack)))
              (exec rest (cons a1 (cons a2 (cddr stack))) dict)))

     ((eq word "rot")
              (let ((a3 (car stack))
                    (a2 (cadr stack))
                    (a1 (caddr stack)))
              (exec rest (cons a1 (cons a3 (cons a2 (cdddr stack)))) dict)))

     ((eq word "pick")
              (let ((n (car stack))
                    (rs (cdr stack)))
                 (exec rest (cons (nth n rs) rs) dict)))

     ((eq word "+")
              (let ((a2 (car stack))
                    (a1 (cadr stack)))
              (exec rest (cons (+ a1 a2) (cddr stack)) dict)))

     ((eq word "-")
              (let ((a2 (car stack))
                    (a1 (cadr stack)))
              (exec rest (cons (- a1 a2) (cddr stack)) dict)))

     ((eq word "*")
              (let ((a2 (car stack))
                    (a1 (cadr stack)))
              (exec rest (cons (* a1 a2) (cddr stack)) dict)))

     ((eq word "/")
              (let ((a2 (car stack))
                    (a1 (cadr stack)))
              (exec rest (cons (/ a1 a2) (cddr stack)) dict)))

     ((or (eq word "\") (eq word "div"))
              (let ((a2 (car stack))
                    (a1 (cadr stack)))
              (exec rest (cons (\ a1 a2) (cddr stack)) dict)))

     ((eq word "mod")
              (let ((a2 (car stack))
                    (a1 (cadr stack)))
              (exec rest (cons (% a1 a2) (cddr stack)) dict)))

     ((eq word "/mod")
              (let ((a2 (car stack))
                    (a1 (cadr stack)))
              (exec rest (cons (% a1 a2) (cons (\ a1 a2) (cddr stack))) dict)))

     ((eq word "neg")
              (exec rest (cons (- (car stack)) stack) dict))

     ((eq word "1+")
              (exec rest (cons (+ 1 (car stack)) (cdr stack)) dict))

     ((eq word "1-")
              (exec rest (cons (- (car stack) 1) (cdr stack)) dict))

     ((eq word "2+")
              (exec rest (cons (+ 2 (car stack)) (cdr stack)) dict))

     ((eq word "2-")
              (exec rest (cons (- (car stack) 2) (cdr stack)) dict))

     ((eq word "2*")
              (exec rest (cons (* (car stack) 2) (cdr stack)) dict))

     ((eq word "2div")
              (exec rest (cons (/ (car stack) 2) (cdr stack)) dict))

     ((eq word "abs")
              (exec rest (cons (abs (car stack)) (cdr stack)) dict))

     ((eq word "or")
              (let* ((a1 (fix2bit (car stack)))
                     (a2 (fix2bit (cadr stack))))
                  (exec rest (cons (bit2fix (logOr a1 a2)) (cddr stack)) dict)))

     ((eq word "and")
              (let* ((a1 (fix2bit (car stack)))
                     (a2 (fix2bit (cadr stack))))
                   (exec rest (cons (bit2fix (logAnd a1 a2)) (cddr stack)) dict)))

     ((eq word "xor")
              (let* ((a1 (fix2bit (car stack)))
                     (a2 (fix2bit (cadr stack))))
                   (exec rest (cons (bit2fix (logXor a1 a2)) (cddr stack)) dict)))

     ((eq word "not")
                  (if (= 0 (car stack))
                  (exec rest (cons 1 (cdr stack)) dict)
                  (exec rest (cons 0 (cdr stack)) dict)))

     ((eq word "0>")
                  (exec rest (cons (if (> 0 (car stack)) 1 0) stack) dict))

     ((eq word "=")
              (let ((a1 (car stack))
                    (a2 (cadr stack)))
              (exec rest (cons (if (= a1 a2) -1 0) (cddr stack)) dict)))

     ((eq word "/=")
              (let ((a1 (car stack))
                    (a2 (cadr stack)))
              (exec rest (cons (if (/= a1 a2) -1 0) (cddr stack)) dict)))

     ((eq word ">")
              (let ((a1 (car stack))
                    (a2 (cadr stack)))
              (exec rest (cons (if (> a1 a2) -1 0) (cddr stack)) dict)))

     ((eq word ">=")
              (let ((a1 (car stack))
                    (a2 (cadr stack)))
              (exec rest (cons (if (>= a1 a2) -1 0) (cddr stack)) dict)))

     ((eq word "<")
              (let ((a1 (car stack))
                    (a2 (cadr stack)))
              (exec rest (cons (if (< a1 a2) -1 0) (cddr stack)) dict)))

     ((eq word "<=")
              (let ((a1 (car stack))
                    (a2 (cadr stack)))
              (exec rest (cons (if (<= a1 a2) -1 0) (cddr stack)) dict)))

     ((eq word "save")
              ;;(let ((fo (gensym 'fo))
              ;;      (fname (pop stack)))
              ;;  (filOpen fo fname _OUTPUT)
              ;;  (filPutLine fo (output stack))
              ;;  (filPutLine fo (output dict))
              ;;  (filClose fo)
              ;;  (printsline (strCat "Saved in " fname))
            (let ((fname (pop stack)))
              (save-in-file fname dict stack)
              (printsline (strCat "Saved in " fname))
              (exec rest (cdr stack) dict)))

     ((eq word "restore")
              ;;(let ((fi (gensym 'fi))
              ;;      (fname (pop stack))
              ;;      (new-stack nil)
              ;;      (new-dict nil))
              ;;  (filOpen fi fname _INPUT)
              ;;  (setq new-stack (input (filGetLine fi)))
              ;;  (setq new-dict  (input (filGetLine fi)))
              ;;  (filClose fi)
                (let* ((fname (pop stack))
                       (a (restore-from-file fname)))
                (printsline (strCat "Restored from " fname))
                (exec nil (car a) (cadr a))))

     ((eq (strChr 34) (strMid word 1 1))  ;; Строка в доп кавычках для занесения в стек строковых данных
           (let ((lw (strLen word)))
                (exec rest (cons (strMid word 2 (- lw 2)) stack) dict)))

     (t (list '! (strCat word "?"))))

    except

     (list '! (errormessage))))))

;; Конец exec 

;; Головная функция 

(defun forth (&optional trace-stack trace-dict)

  ;;(setq *trace-stack* trace-stack)
  ;;(setq *trace-dict* trace-dict)

  (let ((stk        nil)                     ;; рабочий стек
        (dict       (list (list "$do$" 0)))  ;; словарь
        (cmd_stack  nil)                     ;; стек команд
        (cmd_pos    0))

     (loop

          (let* ((txt        (strRep (strRep (read t) (strChr 10) " ") (strChr 13) " "))
                 (lst-cmd    (mapcar 'strLCase (strWords txt)))
                 (tmp        nil))

                (setq tmp (exec lst-cmd stk dict))

                (cond ((eq (car tmp) t)  (printline 'Bye!!!) (return t))
                      ((eq (car tmp) '!) (printsline (cadr tmp)))
                      (t (printline 'OK)
                      (setq stk  (car tmp))
                      (setq dict (cadr tmp))))))))

;; Конец FORTH
