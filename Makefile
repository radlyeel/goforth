# Makefile for goforth

all: goforth

goforth: goforth.o
	ld -o $@ $+

goforth.o: goforth.s
	as -a -g -o $@ $< > goforth.lst

clean:
	rm -f goforth *.o *.lst

test: goforth
	./goforth


