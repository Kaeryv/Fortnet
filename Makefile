fortnet-mnist: fortnet.a main.o
	gfortran main.o fortnet.a -O3 -flto -o net.exe


main.o: main.f90 mod_random.f90 nanograd.f90 functions.f90
	gfortran -c main.f90 -O3

fortnet.a: mod_random.f90 nanograd.f90 functions.f90 network.f90
	gfortran -c mod_random.f90  -O3
	gfortran -c nanograd.f90    -O3
	gfortran -c functions.f90   -O3
	gfortran -c network.f90   -O3
	ar rcs fortnet.a mod_random.o nanograd.o functions.o network.o