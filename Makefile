# Makefile for goforth
# The original intent was to build a Forth that did not require the C standard library,
#   but this intent ran afould of the need to get a keystroke "immediately".  To accommodate
#   this requirement, the C library is included so tcgetattr() and tcsetattr() can be called.

all: goforth

goforth: goforth.o
	#ld -o $@ $+
	gcc -o $@ $+

goforth.o: goforth.s
	as -a -g -o $@ $< > goforth.lst

clean:
	rm -f goforth *.o *.lst

dist-clean: clean
	rm -f *~

test: goforth
	./goforth


