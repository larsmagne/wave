DLIBS =

CC = gcc

SWITCHES = -Wall -g -funroll-loops\
	-fstrength-reduce -ffast-math -falign-functions=4 \
	-falign-jumps=4 -falign-loops=4
INSTALLATION_DIRECTORY=../bin

all: summarize

.c.o: 
	$(CC) $(SWITCHES) -c $*.c

clean:
	rm -f *.o summarize

include .depend

.depend:
	-gcc -MM -MG *.c >> .depend

depend:
	rm -f .depend
	make .depend
