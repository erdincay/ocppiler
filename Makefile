all: cppiler.cpp
	g++ -g -Wall -std=c++11 cppiler.cpp lexer.cpp parser.cpp -o cppiler

clean:
	$(RM) cppiler
	$(RM) tmp.txt

test:
	g++ -g -Wall -std=c++0x cppiler.cpp -o cppiler
	bash tests/test.sh
