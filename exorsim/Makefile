DATADIR ?= /usr/local/share/exorsim/
BINDIR ?= /usr/local/bin/
MANDIR ?= /usr/local/man/man1/

CFLAGS = -g -Wall

CC = gcc

all : exor unasm6800  lpf

unasm6800 : obj/unasm.o obj/utils.o obj/unasm6800.o
	$(CC) -o unasm6800 obj/unasm.o obj/utils.o obj/unasm6800.o

.PHONY: exor
exor :
	make -f Makefile.00 DATADIR=$(DATADIR)


lpf : obj/lpf.o
	$(CC) -o lpf obj/lpf.o

.PHONY: clean
clean :
	rm -rf obj
	make -f Makefile.00 clean
	rm -f exor
	rm -f unasm6800
	rm -f lpf

.PHONY: install
install : mdos exor unasm6800 lpf
	install -D unasm6800 $(BINDIR)unasm6800
	install -D lpf $(BINDIR)lpf
	install -D exor $(BINDIR)exor
	install -D -m 644 facts $(DATADIR)facts
	install -D -m 644 exbug.bin $(DATADIR)exbug.bin
	install -D -m 644 swtbug.bin $(DATADIR)swtbug.bin

# include dependancy files if they exist
-include obj/unasm.d obj/utils.d obj/unasm6800.d obj/lpf.d

# compile and generate dependency info
obj/%.o: %.c
	@echo
	@mkdir -p obj/$(shell dirname $*)
	$(CC) -c $(CFLAGS) -MT $@ -MMD -MP -MF obj/$*.d $*.c -o obj/$*.o
