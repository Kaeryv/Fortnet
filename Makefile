FFLAGS=-O2 -march=native -flto 

fortnet-mnist: fortnet.a main.o
	gfortran *.o ${FFLAGS} -o net.exe 


main.o: main.f90 mod_random.f90 nanograd.f90 functions.f90
	gfortran -c main.f90 ${FFLAGS}


fortnet.a: mod_random.f90 nanograd.f90 functions.f90 network.f90
	gfortran -c mod_random.f90  ${FFLAGS}
	gfortran -c nanograd.f90    ${FFLAGS}
	gfortran -c functions.f90   ${FFLAGS}
	gfortran -c network.f90   ${FFLAGS}
clean:
	rm *.o *.a *.mod
