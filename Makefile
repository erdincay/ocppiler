all: cppiler.cpp
	g++ -g -Wall -std=c++0x cppiler.cpp -o cppiler

clean:
	$(RM) cppiler
