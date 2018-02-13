all: cppiler.cpp
	g++ -g -Wall -std=c++11 lexer.cpp parser.cpp cppiler.cpp -o cppiler

clean:
	$(RM) cppiler
	$(RM) tmp.txt

test:
	g++ -g -Wall -std=c++0x cppiler.cpp -o cppiler
	bash tests/test.sh
