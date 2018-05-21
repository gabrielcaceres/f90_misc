!!! Simple-pass Procedures for (running) mean & (co)variance
! Implementation of Welford's algorithm
! see https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
! 
module m_moments
  use m_argtype
  implicit none
  
contains

  ! Variance
  function var(x, mean)
    real(wp) :: var
    real(wp), intent(in), dimension(:) :: x
    real(wp), intent(in), optional :: mean
    !
    real(wp) :: m, rn
    integer :: n
    !
    n = size(x)
    rn = real(n,wp)
    if (present(mean)) then
       m = mean
    else
       m = sum(x)/rn
    end if
    !
    var = (sum((x-m)**2))/rn
  end function var
      
  ! Single-pass variance calculation
  function var_onepass(x) result(var)
    real(wp) :: var
    real(wp), intent(in), dimension(:) :: x
    !
    real(wp) :: mean, delta, prod
    integer :: i, n
    !
    n = size(x)
    mean = x(1)
    prod = 0_wp
    do i = 2, n
       delta = x(i) - mean
       mean = mean + delta/i
       prod = prod + delta*(x(i) - mean)
    end do
    var = prod/n
  end function var_onepass    

  ! WIP
  ! ! Single-pass covariance calculation
  ! function cov_onepass(x,y) result(cov)
  !   real(wp) :: var
  !   real(wp), intent(in), dimension(:) :: x, y
  !   !
  !   real(wp) :: meanX, meanY, deltaX, deltaY, prod
  !   integer :: i, n
  !   !
  !   n = size(x)
  !   meanx = x(1)
  !   meany = y(1)
  !   prod = 0_wp
  !   do i = 2, n
  !      delta = x(i) - mean
  !      mean = mean + delta/i
  !      prod = prod + delta*(x(i) - mean)
  !   end do
  !   var = prod/n
  ! end function cov    
  
end module m_moments
