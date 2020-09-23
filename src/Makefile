CC = gcc
CFLAGS = -Wall -std=c11 -g -ggdb

all: sim

../src.zip: *.h *c
	cd .. && zip src.zip src/*.c src/*.h src/Makefile


sim: *.c *.h
	$(CC) $(CFLAGS) *.c -o sim

clean:
	rm -rf *.o sim test_runs
