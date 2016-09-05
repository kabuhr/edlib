all: Main Edlib.o libedlib_shim.so

CFLAGS = -fpic $(INCLUDES)
INCLUDES = -I../edlib/include
#LDFLAGS = -L../build/lib -l edlib

Main: Main.hs Edlib.o libedlib_shim.so
	stack ghc -- -o $@ $< -L../build/lib -ledlib -L. -ledlib_shim

libedlib_shim.so: edlib_shim.o
	$(LD) -shared -o $@ $^

edlib_shim.o: edlib_shim.c edlib_shim.h

Edlib.hs: Edlib.hsc edlib_shim.h
	hsc2hs $(INCLUDES) $<

Edlib.o: Edlib.hs
	stack ghc -- $<

clean:
	$(RM) libedlib_shim.so edlib_shim.o Edlib.hs Edlib.o \
	      Main.o *_make.c *_make.hs Main *.hi
