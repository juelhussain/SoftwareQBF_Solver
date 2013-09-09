#CFLAGS=-Wextra -Wall -Wno-unused -pedantic -std=c99 -DNDEBUG -O3 
CFLAGS=-Wextra -Wall -Wno-unused -pedantic -std=c99 -DNDEBUG -O3 -static
#CFLAGS=-Wextra -Wall -Wno-unused -pedantic -std=c99 -g3 -static
#CFLAGS=-Wextra -Wall -Wno-unused -pedantic -std=c99 -DNDEBUG -g3 -pg -fprofile-arcs -ftest-coverage -static
LFLAGS= -L../../cdepqbf/depqbf -lqdpll -I../../cdepqbf/depqbf 

depService: service.o 
	$(CC) $(CFLAGS) -o $@ service.o $(LFLAGS)
service.o: service.c
	$(CC) $(CFLAGS) -c service.c $(LFLAGS)

clean:
	rm -f *.a *.o *.gcno *.gcda *.gcov *~ gmon.out depqbf
