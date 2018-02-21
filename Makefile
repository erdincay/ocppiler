all: cppiler

parser.tab.c parser.tab.h:	parser.y
	bison -d parser.y -v

lex.yy.c: lexer.l parser.tab.h
	flex lexer.l

cppiler: lex.yy.c parser.tab.c parser.tab.h
	g++ -o cppiler parser.tab.c lex.yy.c

clean:
	rm cppiler parser.tab.c lex.yy.c parser.tab.h parser.output

test:
	bash tests/test.sh
