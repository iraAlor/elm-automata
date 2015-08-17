.PHONY:build clean
build:
	ghc Main.hs Re.hs -o backasourusRex
	mv  backasourusRex ../../build/bend
	$(MAKE) clean
clean:
	rm -f *.dyn_*
	rm -f *.o
	rm -f *.hi
