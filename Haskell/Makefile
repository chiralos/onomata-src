# Makefile Onomaata Haskell host

PACKAGES=\
-package memory\
-package haskeline\

ono :
	ghc -O2 -I. -main-is Onomata.Main --make Onomata.Main ${PACKAGES}
	cp Onomata/Main ./ono

clean :
	rm -f Onomata/Main
	rm -f ono
	find -E . -regex ".*(\.o|\.hi|\.hc)" -delete
# find . -regextype posix-extended -regex ".*(\.o|\.hi|\.hc)" -delete
