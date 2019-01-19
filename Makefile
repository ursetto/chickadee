build:
	chicken-install -n
clean:
	rm -f *.so *.o *.c *.import.scm *.link *.install.sh *.build.sh chickadee-cmd
install:
	chicken-install
