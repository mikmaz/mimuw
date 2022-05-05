CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++17 -O2
LDFLAGS =

.PHONY: all clean

all: screen-worms-server

screen-worms-server: server_main.o WormsServer.o WormsEngine.o Player.o Event.o Socket.o ServerArgs.o server_utils.o crc32.o
	$(CXX) $(LDFLAGS) -o $@ $^

crc32.o: crc32.cpp crc32.h
	$(CXX) $(CXXFLAGS) -c $<

server_utils.o: server_utils.cpp server_utils.h
	$(CXX) $(CXXFLAGS) -c $<

ServerArgs.o: ServerArgs.cpp ServerArgs.h
	$(CXX) $(CXXFLAGS) -c $<

Socket.o: Socket.cpp Socket.h
	$(CXX) $(CXXFLAGS) -c $<

Event.o: Event.cpp Event.h server_utils.h
	$(CXX) $(CXXFLAGS) -c $<

Player.o: Player.cpp Player.h server_utils.h
	$(CXX) $(CXXFLAGS) -c $<

WormsEngine.o: WormsEngine.cpp WormsEngine.h Player.h Event.h ServerArgs.h
	$(CXX) $(CXXFLAGS) -c $<

WormsServer.o: WormsServer.cpp WormsServer.h WormsEngine.h Socket.h crc32.h
	$(CXX) $(CXXFLAGS) -c $<

server_main.o: server_main.cpp WormsServer.h
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -f *.o screen-worms-server