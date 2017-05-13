# Eric Laursen, 31 May 2017, CS 442P-003 Term Project
# Makefile for Minichess - A minichess player

OBJS = mc.o board.o

.adb.o:
	gcc -c -gnat2012 -O3 $<

.SUFFIXES: .adb .o

mc:	$(OBJS)
	gnatbind mc.ali
	gnatlink mc.ali

clean:
	rm -f *~ *.o *.ali mc

tidy:
	rm -f *~
