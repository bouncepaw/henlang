reflex main.l && c++ -Ireflex/include -o main lex.yy.cpp lib/libreflex.a && cat example.hen | ./main
