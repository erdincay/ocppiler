all: cppiler.cpp
	g++ -g -Wall -std=c++11 lexer.cpp parser.cpp cppiler.cpp -o cppiler

clean:
	$(RM) cppiler
	$(RM) tmp.txt

test:
	bash tests/test.sh
