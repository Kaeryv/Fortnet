module functions
  contains
  pure function sigmoid(x) result(res)
    real, intent(in) :: x(:,:)
    real :: res(size(x,1), size(x,2))

    res = 1 / (1 + exp(-x))
  end function sigmoid
  
  pure function sigmoid_prime(x) result(res)
    real, intent(in) :: x(:,:)
    real :: res(size(x,1),size(x,2))

    res = sigmoid(x) * (1 - sigmoid(x))
  end function sigmoid_prime
  
  pure function tanhf(x) result(res)
    real, intent(in) :: x(:,:)
    real :: res(size(x,1),size(x,2))

    res = tanh(x)
  end function tanhf

  pure function tanh_prime(x) result(res)
    real, intent(in) :: x(:,:)
    real :: res(size(x,1),size(x,2))

    res = 1 - tanh(x)**2
  end function tanh_prime

  pure function relu(x) result(res)
    real, intent(in) :: x(:,:)
    real :: res(size(x,1),size(x,2))

    where(x < 0.0)
      res = 0.0
    elsewhere
      res = x
    end where
  end function relu

  pure function relu_prime(x) result(res)
    real, intent(in) :: x(:,:)
    real :: res(size(x,1),size(x,2))

    where(x < 0.0)
      res = 0.0
    elsewhere
      res = 1.0
    end where
  end function relu_prime
end module functions
