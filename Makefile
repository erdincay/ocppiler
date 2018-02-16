parser.tab.c parser.tab.h: parser.y
	bison -d parser.y

lex.yy.c: lexer.l parser.tab.h
	flex lexer.l

cppiler: lex.yy.c parser.tab.c parser.tab.h
	g++ parser.tab.c lex.yy.c -lfl -o cppiler

clean:
	$(RM) cppiler
	$(RM) tmp.txt
	$(RM) lex.yy.c
	$(RM) parser
	$(RM) parser.tab.c
	$(RM) parser.tab.h

test:
	bash tests/test.sh
