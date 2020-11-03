module nanograd

    interface load_idx
      module procedure :: load_idx1, load_idx3
    end interface

    abstract interface
    pure function activation_fn(x) result(res)
      ! First derivative of the sigmoid activation function.
      real, intent(in) :: x(:,:)
      real :: res(size(x,1),size(x,2))
    end function activation_fn
    end interface

   

    contains

 

    subroutine init_layer(w)
        real, dimension(:,:), intent(inout) :: w
        call random_number(w)
        w = -1.0 + (w * 2.0)
        w =  w * sqrt(6.0 / (real(size(w, dim=1)+size(w, dim=2))))
    end subroutine
    

  subroutine load_idx1(file, output)
    implicit none
    character(*), intent(in)                             :: file
    integer(1),   intent(out), dimension(:), allocatable :: output

    integer :: magic, size
    open(unit=11, file=file,access="stream", form="unformatted", status="old", convert="big_endian")
    read(11) magic, size
    allocate(output(size))
    read(11) output
    close(unit=11)
  end subroutine

  subroutine load_idx3(file, output)
    implicit none
    character(*), intent(in)                                 :: file
    integer(1),   intent(out), dimension(:,:,:), allocatable :: output

    integer :: magic, m, l, n
    open(unit=11, file=file,access="stream", form="unformatted", status="old", convert="big_endian")
    read(11) magic, m, l, n
    allocate(output(n, l, m))
    read(11) output
    close(unit=11)
  end subroutine

  subroutine load_mnist(x_train, y_train, x_test, y_test)
    implicit none
    real, intent(out), dimension(:,:), allocatable :: x_train, x_test
    real, intent(out), dimension(:, :), allocatable :: y_train, y_test

    integer(1), dimension(:), allocatable :: buffer
    integer(1), dimension(:, :, :), allocatable :: buffer3
    integer :: i

    character(*), parameter :: x_train_file = "dataset/train-images.idx3-ubyte"
    character(*), parameter :: x_test_file  = "dataset/t10k-images.idx3-ubyte"
    character(*), parameter :: y_train_file = "dataset/train-labels.idx1-ubyte"
    character(*), parameter :: y_test_file  = "dataset/t10k-labels.idx1-ubyte"


    ! Training set labels
    call load_idx1(y_train_file, buffer)
    allocate(y_train(10,size(buffer,1)))
    y_train = 0.0
    do i = 1, size(buffer, 1)
      y_train(buffer(i), i) = 1.0
    end do
    deallocate(buffer)

    ! Testing set labels
    call load_idx1(y_test_file, buffer)
    allocate(y_test(10,size(buffer,1)))
    y_test = 0.0
    do i = 1, size(buffer, 1)
      y_test(buffer(i), i) = 1.0
    end do
    deallocate(buffer)


    call load_idx(x_train_file, buffer3)
    x_train = reshape(buffer3, (/28*28,size(buffer3,3)/))
    deallocate(buffer3)
    call load_idx(x_test_file, buffer3)
    x_test = reshape(buffer3, (/28*28,size(buffer3,3)/))
    deallocate(buffer3)

    where (x_test  < 0.0)
      x_test = 128.0 - x_test
    end where

    where (x_train  < 0.0)
      x_train = 128.0 - x_train
    end where

    x_train = x_train / 255.0
    x_test = x_test / 255.0

  end subroutine
end module nanograd