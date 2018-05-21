module m_statdist
  use m_argtype
  use m_randutils
  implicit none
  
contains

  ! Random number from uniform normal distribution in range [lo,hi)
  subroutine get_runif(x, lo, hi)
    real(wp), intent(out), dimension(:) :: x
    real(wp), intent(in), optional :: hi, lo
    ! 
    call get_random(x)
    if ( present(lo) .and. present(hi) ) then
       x = (x * (hi - low)) + lo
    end if
    ! 
  end subroutine get_runif

  ! Random number from normal distribution
  ! subroutine get_rnorm(x, mu, sigma)
  ! end subroutine get_rnorm
  ! Box-Muller transform
  ! see: https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
  subroutine rnorm_boxmuller(x, mu, sigma)
    real(wp), intent(out) :: x
    real(wp), intent(in), optional :: mu, sigma
    real(wp) :: m, sig
    real(wp) :: u1, u2, z1, z2
    !
    call get_random(u1)
    call get_random(u2)
    z1 = sqrt(-2._wp*log(u1))*cos(2._wp*pi*u2)
    z2 = sqrt(-2._wp*log(u1))*sin(2._wp*pi*u2)
    x = z1
    ! x2 = z2
    !
    ! OR??
    real(wp) :: r, th
    r = sqrt( -2._wp * log(u1) )
    th = 2._wp * pi * u2
    x1 = r * sin(theta)
    x2 = r * cos(theta)
    ! 
  end subroutine rnorm_boxmuller
  
  ! Polar method
  ! see: https://en.wikipedia.org/wiki/Marsaglia_polar_method
  subroutine rnorm_polar(x, mu, sigma)
    real(wp), intent(out) :: x
    real(wp), intent(in), optional :: mu, sigma
    real(wp) :: m, sig
    real(wp) :: r
    real(wp) :: u1, u2, z1, z2
    !
    call get_runif(u1, -1._wp, 1._wp)
    call get_runif(u2, -1._wp, 1._wp)
    r = u1**2 + u2**2
    do while ( r >= 1._dp )
       call get_runif(u1, -1._wp, 1._wp)
       call get_runif(u2, -1._wp, 1._wp)
       r = u1**2 + u2**2
    end do
    r = sqrt( -2._wp * log(r) / r )
    z1 = u1 * r
    z2 = u2 * r
    ! 
  end subroutine rnorm_polar
 
  
  
end module m_statdist
