exe/1: one.c
	gcc one.c -o exe/1

exe/8: eight.sml
	mosmlc eight.sml -o exe/8

1: exe/1
	./exe/1

2:
	swipl -q -l two.pl

3:
	./three.bash

4:
	./four.rb

5:
	guile -l five.scm -c '(main)'

6:
	./six.perl

7:
	scala seven.scala

8: exe/8
	./exe/8

9:
	python3 nine.py

10:
	runhaskell ten.hs
