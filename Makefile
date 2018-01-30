all: cppiler.cpp
	g++ -g -Wall -std=c++0x cppiler.cpp -o cppiler

clean:
	$(RM) cppiler

test:
	g++ -g -Wall -std=c++0x cppiler.cpp -o cppiler
	bash tests/test.sh
