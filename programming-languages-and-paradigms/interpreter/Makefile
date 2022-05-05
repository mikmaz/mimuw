.PHONY : all grammar build clean distclean 

all : grammar build

BNFC := $(shell which bnfc)
ifeq (, $(BNFC))
	BNFC := /home/students/inf/PUBLIC/MRJP/bin/bnfc
endif

grammar:
	cd src && \
	$(BNFC) -m --haskell --functor -p Grammar ../CPC.cf

build:
	cd src && make
	ghc-8.4.4 -isrc src/Main.hs -o interpreter

clean:
	rm -f \
	interpreter \
	src/Interpreter/*.o src/Interpreter/*.hi \
	src/TypeChecker/*.o src/TypeChecker/*.hi \
	src/*.o src/*.hi
	cd src && \
	make clean

distclean:
	make clean
	cd src && \
	make distclean
