build:
	gnatmake tetris.adb
run:
	./tetris
clean:
	rm -f *.ali
	rm -f *.o
	rm -f ./tetris
