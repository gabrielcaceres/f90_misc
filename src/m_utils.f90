! General utility procedures
module m_utils
  use m_argtype
  implicit none
  private
  public :: swap
  public :: isEven, isOdd

  interface swap
     module procedure swap_int, swap_real
  end interface swap

contains

!!!###############################################
  ! Swap two integer values
  elemental subroutine swap_int (a,b)
    integer, intent(inout) :: a, b
    integer tmp
    !
    tmp = a
    a = b
    b = tmp
    ! 
  end subroutine swap_int
  
  ! Swap two real values (of kind 'wp')
  elemental subroutine swap_int (a,b)
    real(wp), intent(inout) :: a, b
    real(wp) tmp
    !
    tmp = a
    a = b
    b = tmp
    ! 
  end subroutine swap

!!!###############################################
  ! Determine parity of integer
  elemental function isEven(a) result(par)
    logical :: par
    integer, intent(in) :: a
    !
    par = modulo(a,2) == 0
    !
  end function isEven
  elemental function isOdd(a) result(par)
    logical :: par
    integer, intent(in) :: a
    !
    par = modulo(a,2) /= 0
    !
  end function isOdd
  
  
end module m_utils
