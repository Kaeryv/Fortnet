program micrograd
use nanograd
use mod_random
use functions
use class_network

implicit none

real, dimension(:, :), allocatable :: x_train, x_test
real, dimension(:, :), allocatable :: y_train, y_test

real :: accuracy

type (Network) :: net

call init_random_seed()

call net % add_layer("relu", 128, input_size=28*28)
call net % add_layer("tanh", 128)
call net % add_layer("sigmoid", 10)
call net % print
call load_mnist(x_train, y_train, x_test, y_test)
call net % train(x_train, y_train, BS=32, LR=0.01, epoch=5)
call net % validate(x_test, y_test, accuracy=accuracy)

print "('Validation accuracy:',1x,F6.2,1x,'percents.')", accuracy

end program micrograd
