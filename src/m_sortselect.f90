module m_SortSelect
  use iso_c_binding
  use m_argtype

  interface insertionSort
     module procedure insertionSort_int, insertionSort_real
  end interface insertionSort

  ! Call to C library qsort
  ! http://stackoverflow.com/questions/20941575/sorting-in-fortran-undefined-reference-to-qsort
  interface
     !standard C library qsort
    subroutine qsort(array,elem_count,elem_size,compare) bind(C,name="qsort")
      import
      type(c_ptr),value       :: array
      integer(c_size_t),value :: elem_count
      integer(c_size_t),value :: elem_size
      type(c_funptr),value    :: compare !int(*compare)(const void *, const void *)
    end subroutine qsort

    ! integer(c_int) function compar( a, b ) bind(C)
    !   use iso_c_binding
    !   integer(c_int) a, b
    !   if ( a .lt. b ) compar = -1
    !   if ( a .eq. b ) compar = 0
    !   if ( a .gt. b ) compar = 1
    ! end function compar
 
  end interface
  
contains
  
  elemental subroutine swap (a,b)
    integer, intent(inout) :: a, b
    integer tmp
    !
    tmp = a
    a = b
    b = tmp
    ! 
  end subroutine swap

  ! subroutine hoarePartition (x, p)
  !   integer, intent(inout), dimension(:) :: x
  !   integer, intent(inout) :: p
  !   integer a, b
  !   !
  !   call swap(x(p),x(1))
  !   a = 1
  !   b = size(x) - 1
  !   loop1: do
  !      loop2: do
  !         if ( a > b ) exit loop1
  !         if ( x(a) >= x(1) ) exit loop2
  !         a = a + 1
  !      end do loop2
  !      do while ( x(1) < x(b) )
  !         b = b - 1
  !      end do
  !      if ( a >= b ) exit loop1
  !      call swap(x(a),x(b))
  !      a = a + 1
  !      b = b - 1
  !   end do loop1
  !   call swap(x(1),x(a-1))
  !   p = a - 1
  !   ! 
  ! end subroutine hoarePartition

  
  pure subroutine hoarePartition (x, pin,p)
    integer, intent(inout), dimension(0:) :: x
    ! integer, intent(inout) :: p
    integer, intent(in), value :: pin
    integer, intent(out) :: p
    integer a, b
    !
    p = pin
    call swap(x(p),x(0))
    a = 1
    b = size(x) - 1
    loop1: do
       loop2: do
          if ( a > b ) exit loop1
          if ( x(a) >= x(0) ) exit loop2
          a = a + 1
       end do loop2
       do while ( x(0) < x(b) )
          b = b - 1
       end do
       if ( a >= b ) exit loop1
       call swap(x(a),x(b))
       a = a + 1
       b = b - 1
    end do loop1
    call swap(x(0),x(a-1))
    p = a - 1
    ! 
  end subroutine hoarePartition

  ! GOOD ONE
  ! subroutine quickselect (x, l, a)
  !   integer, intent(inout), dimension(0:) :: x
  !   integer, intent(in) :: l
  !   integer, intent(out) :: a
  !   integer :: p, n, m, k
  !   !
  !   k = l
  !   n = size(x) - 1
  !   m = lbound(x,dim=1)
  !   do
  !      p = k
  !      call hoarePartition(x(m:n),p)
  !      print *, p, k
  !      print *, x
  !      if ( p == k ) exit
  !      if ( p > k ) then
  !         n = p
  !      else
  !         m = p + 1
  !         k = k - p - 1
  !         ! x(0:(n-k-1)) = x((p+1):n)
  !         ! k = p
  !         ! n = p
  !      end if
  !   end do
  !   a = x(l)
  !   ! 
  ! end subroutine quickselect

  
  ! subroutine quickselect (x, k)
  !   integer, intent(inout), dimension(:) :: x
  !   integer, intent(inout) :: k
  !   integer :: p, n
  !   !
  !   n = size(x)
  !   do
  !      p = k
  !      call hoarePartition(x(1:n),p)
  !      print *, p, k
  !      print *, x
  !      if ( p == k) exit
  !      if ( p > k ) then
  !         n = p
  !      else
  !         k = k - p - 1
  !         x(1:(n-k)) = x((p+1):n)
  !         n = n - k
  !      end if
  !   end do
  !   ! 
  ! end subroutine quickselect


  subroutine insertionSort_mod (x)
    integer, intent(inout), dimension(:) :: x
    integer :: v, minl
    integer :: i, j, n
    !
    n = size(x)
    minl = minloc(x,dim=1)
    call swap(x(1),x(minl))
    do i = 3, n
       v = x(i)
       j = i
       do while ( x(j-1) > v )
          x(j) = x(j-1)
          j = j - 1
          ! if ( j < 2 ) exit
       end do
       x(j) = v
    end do
    ! 
  end subroutine insertionSort_mod

  pure subroutine insertionSort_int (x)
    integer, intent(inout), dimension(:) :: x
    integer :: v
    integer :: i, j, n
    !
    n = size(x)
    do i = 2, n
       v = x(i)
       j = i
       do while ( x(j-1) > v )
          x(j) = x(j-1)
          j = j - 1
          if ( j < 2 ) exit
       end do
       x(j) = v
    end do
    ! 
  end subroutine insertionSort_int

  pure subroutine insertionSort_real (x)
    real(wp), intent(inout), dimension(:) :: x
    real(wp) :: v
    integer :: i, j, n
    !
    n = size(x)
    do i = 2, n
       v = x(i)
       j = i
       do while ( x(j-1) > v )
          x(j) = x(j-1)
          j = j - 1
          if ( j < 2 ) exit
       end do
       x(j) = v
    end do
    ! 
  end subroutine insertionSort_real
  
  pure subroutine selectionsort (u)
    integer, intent(inout), dimension(:) :: u
    integer :: n, min, imin
    integer :: i, j
    !
    n = size(u)
    do i = 1, n
       min = huge(min)
       do j = i, n
          if ( u(j) < min ) then
             imin = j
             min = u(j)
          end if
       end do
       u((i+1):imin) = u(i:(imin-1))
       u(i) = min
    end do
    ! 
  end subroutine selectionsort

  
  pure recursive subroutine quicksort (x)
    integer, intent(inout), dimension(:) :: x
    integer :: p, n, m, k, pin
    integer :: idx1, idx2
    !
    n = size(x)
    ! if (n <= 40) return
    ! if (n == 1) return
    if (n < 85) then
       call insertionSort(x)
       return
    end if
    p = n/2
    pin = p
    call hoarePartition(x,pin,p)
    p = p + 1
    idx1 = max(p-1,1)
    idx2 = min(p+1,n)
    call quicksort(x(1:idx1))
    call quicksort(x(idx2:n))
    ! 
  end subroutine quicksort

end module m_SortSelect
