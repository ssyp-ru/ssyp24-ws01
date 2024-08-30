
Для запуска проектов необходимо выполнить следующее:

Скачайте с сайта http://homelisp.ru портабельную версию homelisp и установите
в какую-либо директорию (просто распакуйте).

В эту же директорию поместите файлы forth011.lsp и lib-06.txt. Последний содержит
 несколько слов (сумма и произведение списков, факториал, проверка числа на простоту
 и т.п.)

Запустите homelispide.exe и выполните команду:

(rds "forth011.lsp")

Если загрузка пройдет нормально, запускайте ФОРТ и проверяйте работоспособность:

> (forth)                    ;; Запускаем ФОРТ
> "lib-06.txt" restore       ;; Грузим библиотеку
Restored from lib-06.txt
OK
> !stack                     ;; Выводим стек
NIL
OK
> 2 17 range !stack          ;; формируем в стеке диапазон целых
(17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2)
OK
> 16 sum-list .              ;; суммируем 16 элементов стека
152
OK
> 10 fact .                  ;; считаем 10!
3628800
OK
> 123 is-prime .             ;; 123 - простое?
0                            ;; нет
OK
> 129 is-prime .             ;; 129 - простое 
0                            ;; нет
OK
> 67 is-prime .              ;; 67 - простое
-1                           ;; да
OK
> 1 100 range 100 sum-list . ;; считаем сумму чисел от 1 до 100
5050
OK

