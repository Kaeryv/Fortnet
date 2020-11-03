module mod_random

    ! Provides a random number generator with
    ! normal distribution, centered on zero.
  
  
    implicit none
  
    private
    public :: randn,init_random_seed
  
    real, parameter :: pi = 4 * atan(1.)
  
    interface randn
      module procedure :: randn1d, randn2d
    end interface randn
    interface init_random_seed
      module procedure :: init_random_seed
    end interface init_random_seed
  
  contains
  
    function randn1d(n) result(r)
      ! Generates n random numbers with a normal distribution.
      integer, intent(in) :: n
      real :: r(n), r2(n)
      call random_number(r)
      call random_number(r2)
      r = sqrt(-2 * log(r)) * cos(2 * pi * r2)
    end function randn1d
  
    function randn2d(m, n) result(r)
      ! Generates m x n random numbers with a normal distribution.
      integer, intent(in) :: m, n
      real :: r(m, n), r2(m, n)
      call random_number(r)
      call random_number(r2)
      r = sqrt(-2 * log(r)) * cos(2 * pi * r2)
    end function randn2d
    SUBROUTINE init_random_seed()
        INTEGER :: i, n, clock
        INTEGER, DIMENSION(:), ALLOCATABLE :: seed
      
        CALL RANDOM_SEED(size = n)
        ALLOCATE(seed(n))
      
        CALL SYSTEM_CLOCK(COUNT=clock)
        clock = 42
        seed = clock + 37 * (/ (i - 1, i = 1, n) /)
        CALL RANDOM_SEED(PUT = seed)
      
        DEALLOCATE(seed)
      END SUBROUTINE
  end module mod_random