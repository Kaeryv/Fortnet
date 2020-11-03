program micrograd
use nanograd
use mod_random
use functions
use class_network

implicit none

real, parameter :: lr = 0.01
integer, parameter :: BS=64
integer, parameter :: netsize = 128

real, dimension(:, :), allocatable :: x_train, x_test
real, dimension(:, :), allocatable :: y_train, y_test

real, dimension(:, :), allocatable :: x
real, dimension(:,:), allocatable :: hl1, al1, dhl1, dal1 
real, dimension(:,:), allocatable :: hl2, al2, dal2, dhl2
real, dimension(:, :), allocatable :: y, y_pred

integer, dimension(10000) :: test_predictions
real :: accuracy

type (Network) :: net

call init_random_seed()

call net % add_layer("relu", netsize, input_size=28*28)
call net % add_layer("sigmoid", 10)
call net % print


call load_mnist(x_train, y_train, x_test, y_test)

call net % train(x_train, y_train, BS=128, LR=0.01, epoch=10)
call net % validate(x_test, y_test, accuracy)
! allocate(hl1(netsize,10000), al1(netsize,10000),dhl1(netsize,10000), dal1(netsize,10000))
! allocate(hl2(10,10000), al2(10,10000), dal2(10,10000), dhl2(10,10000), y(10,10000), y_pred(10,10000))


! hl1 = matmul(transpose(net % begin % w), x_test)
! al1 = relu(hl1)
! hl2 = matmul(transpose(net % end % w), al1)
! al2 = 1 / (1 + exp(-hl2))
! test_predictions = maxloc(al2, dim=1)-1
print "('Validation accuracy:',1x,F6.2,1x,'percents.')", accuracy
!     100.0*sum(merge(1.0, 0.0, test_predictions .eq. (maxloc(y_test, dim=1)-1))) / real(size(y_test,2))

end program micrograd
