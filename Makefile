FFLAGS=-Ofast -march=native

fortnet-mnist: fortnet.a main.o
	gfortran main.o fortnet.a -O3 -flto -o net.exe -lopenblas


main.o: main.f90 mod_random.f90 nanograd.f90 functions.f90
	gfortran -c main.f90 -Ofast


fortnet.a: mod_random.f90 nanograd.f90 functions.f90 network.f90
	gfortran -c mod_random.f90  ${FFLAGS}
	gfortran -c nanograd.f90    ${FFLAGS}
	gfortran -c functions.f90   ${FFLAGS}
	gfortran -c network.f90   ${FFLAGS}
	ar rcs fortnet.a mod_random.o nanograd.o functions.o network.o
clean:
	rm *.o *.a *.mod
