CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++17 -O2
LDFLAGS =

.PHONY: all clean

all: serwer

serwer: main.o req_parse.o server_util.o server_response.o err.o
	$(CXX) $(LDFLAGS) -o $@ $^ -lstdc++fs

err.o: err.c err.h
	$(CXX) $(CXXFLAGS) -c $<

server_response.o: server_response.cpp server_response.h
	$(CXX) $(CXXFLAGS) -c $<

server_util.o: server_util.cpp server_util.h err.h
	$(CXX) $(CXXFLAGS) -c $<

req_parse.o: req_parse.cpp req_parse.h server_response.h err.h
	$(CXX) $(CXXFLAGS) -c $<

main.o: main.cpp req_parse.h server_response.h server_util.h err.h
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -f *.o serwer
