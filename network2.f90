module class_network
    use nanograd
    use functions
    implicit none
    private
    type :: Layer
        procedure (activation_fn), pointer, nopass :: activation => null ()
        procedure (activation_fn), pointer, nopass :: activation_prime => null ()
        real, dimension(:, :), allocatable :: b, w, h, a, dh, da
        type(Layer), pointer :: next => null(), previous => null()
        integer :: size
        character(:), allocatable :: activation_str
    end type

    type, public :: Network
        type(Layer), pointer :: begin, end
    contains
        procedure :: add_layer => network_add_layer
        procedure :: print => network_print
        procedure :: train => network_train
        procedure :: validate => network_validate
    end type Network

    contains

    subroutine network_add_layer(self, activation, size, input_size)
        class(Network), intent(inout) :: self
        character(*), intent(in) :: activation
        integer, intent(in) :: size
        integer, intent(in), optional :: input_size
        type(Layer), pointer :: l

        integer :: ifan, ofan


        allocate(l)

        if (present(input_size)) then
            ifan = input_size
            self % begin => l
        elseif(associated(self % end)) then
            ifan = self % end % size
            l % previous => self % end
            self % end % next => l
        else
            stop "First layer must have explicit input shape"
        end if
        ofan = size

        print *, "layer", ifan, ofan

        select case(trim(activation))
        case('relu')
            l % activation => relu
            l % activation_prime => relu_prime
            l % activation_str = activation
        case('sigmoid')
            l % activation => sigmoid
            l % activation_prime => sigmoid_prime
            l % activation_str = activation
        end select

        l % size = size

        allocate(l % w(ifan, ofan))
        allocate(l % b(1, ofan))
        call init_layer(l % w)

        self % end => l

    end subroutine

    subroutine network_print(self)
        class(Network), intent(inout) :: self
        print *, "Hello network"
    end subroutine

    subroutine forward(begin, x)
        type(Layer), intent(in), pointer :: begin
        type(Layer), pointer :: current
        real, dimension(:, :), intent(in) :: x

        current => begin
        forward_pass: do
            if(associated(current % previous)) then
                current % h = matmul(transpose(current % w), current % previous % a)
            else
                current % h = matmul(transpose(current % w), x)
            end if
            current % a = current % activation(current % h)
            if(associated(current % next)) then
                current => current % next
            else
                exit forward_pass
            end if
        end do forward_pass
    end subroutine

    subroutine backward(end, dloss)
        type(Layer), intent(in), pointer :: end
        type(Layer), pointer :: current
        real, dimension(:, :), intent(in) :: dloss

        current => end
        current % da = dloss
        backward_pass: do
            current % dh = current % da * current % activation_prime(current % h)
            if(associated(current % previous)) then
                current % previous % da = matmul(current % w, current % dh)
                current => current % previous
            else
                exit backward_pass
            end if
        end do backward_pass
    end subroutine

    subroutine network_validate(self, x, y, accuracy)
        implicit none
        real, dimension(:, :) :: x, y
        class(Network), intent(inout) :: self
        type(Layer), pointer :: current
        integer, dimension(size(y,2)) :: predictions

        real, intent (out) :: accuracy
        integer :: num

        num = size(y, 2)
        current => self % begin

        allocation: do
            allocate(current % h (current % size, num))
            allocate(current % a (current % size, num))
            allocate(current % dh(current % size, num))
            allocate(current % da(current % size, num))
            if(associated(current % next)) then
                current => current % next
            else
                exit allocation
            end if
        end do allocation
        
        call forward(self % begin, x)
        predictions = maxloc(self % end % a, dim=1)-1
        accuracy = 100.0*sum(merge(1.0, 0.0, predictions .eq. (maxloc(y, dim=1)-1))) / real(num)
    end subroutine

    subroutine network_train(self, x, y, BS,LR,epoch)
        implicit none
        real, dimension(:, :) :: x, y
        real, dimension(:, :), allocatable :: x_batch, y_batch
        class(Network), intent(inout) :: self
        integer, intent(in), optional :: BS,epoch
        real, intent(in), optional :: LR

        type(Layer), pointer :: current
        integer :: batch, batch_start, i
        real :: loss

        ! Allocate buffers
        current => self % begin
        allocation: do
            print *, "alloc ",  current % activation_str, current % size,BS
            allocate(current % h (current % size,BS))
            allocate(current % a (current % size,BS))
            allocate(current % dh(current % size,BS))
            allocate(current % da(current % size,BS))
            if(associated(current % next)) then
                current => current % next
            else
                exit allocation
            end if
        end do allocation

        allocate(x_batch(size(x,1), BS))
        allocate(y_batch(size(y,1), BS))
        print *, size(x,1), BS
        do i = 1, epoch
            do batch = 1, 500
                ! Prepare for the forward pass
                batch_start = MODULO(irand(), 60000-BS+1)
                x_batch = x(:, batch_start:batch_start+BS-1)
                y_batch = y(:, batch_start:batch_start+BS-1)

                current => self % begin
                
                call forward(current, x_batch)

                ! Now we have a prediction
                current => self % end
                ! Compute the loss
                loss = 0.0
                loss = sum((current % a - y_batch)**2) / real(BS) / real(size(y,1))
                call backward(self % end, 2.0 * (current % a - y_batch) / real(BS))

                current => self % begin
                current % w = current % w - LR * matmul(x_batch, transpose(current % dh))
                current => current % next
                update_pass: do
                    current % w = current % w - LR * matmul(current % previous % da, transpose(current % dh))
                    if(associated(current % next)) then
                        current => current % next
                    else
                        exit update_pass
                    end if
                end do update_pass
            end do
            print *, "loss: ", loss
        end do


        deallocate(x_batch, y_batch)
        current => self % begin
        deallocation: do
            print *, "alloc ",  current % activation_str, current % size,BS
            deallocate(current % h )
            deallocate(current % a )
            deallocate(current % dh)
            deallocate(current % da)
            if(associated(current % next)) then
                current => current % next
            else
                exit deallocation
            end if
        end do deallocation
    end subroutine
end module