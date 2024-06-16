CC = gcc
CFLAGS = -Wall
LDFLAGS = -ll
EXECUTABLE = fec

all: $(EXECUTABLE)

run: clean $(EXECUTABLE) clean2 exe

$(EXECUTABLE): lex.yy.o rustgram.tab.o main.o token.o tree.o symtab.o
	$(CC) $^ -o $@ $(LDFLAGS)

tac.o: tac.c
	$(CC) $(CFLAGS) -c $< -o $@

symtab.o: symtab.c
	$(CC) $(CFLAGS) -c $< -o $@

tree.o: tree.c
	$(CC) $(CFLAGS) -c $< -o $@

token.o: token.c
	$(CC) $(CFLAGS) -c $< -o $@

main.o: main.c
	$(CC) $(CFLAGS) -c $< -o $@

lex.yy.o: lex.yy.c rustgram.tab.h token.h
	$(CC) $(CFLAGS) -c $< -o $@

rustgram.tab.o: rustgram.tab.c rustgram.tab.h
	$(CC) $(CFLAGS) -c $< -o $@

lex.yy.c: rustlex.l
	flex $<

rustgram.tab.c rustgram.tab.h: rustgram.y token.h
	bison -d $<

clean:
	rm -f $(EXECUTABLE) main.o lex.yy.* rustgram.tab.* token.o tree.o symtab.o tac.o

clean2:
	rm -f main.o lex.yy.* rustgram.tab.c rustgram.tab.o token.o tree.o symtab.o tac.o

exe:
	./fec -tree -symtab ../test.rs

.PHONY: all clean run