! General procedures for random numbers
!
! Uses RANDOM_NUMBER intrinsic
module m_randutils
  use m_argtype
  implicit none
  private
  public :: init_random_seed
  public :: get_random

  interface get_random
     module procedure get_random_real, get_random_int
  end interface get_random
  
contains

  ! Wrapper for PRNG for reals in range [0,1)
  subroutine get_random_real(x)
    real(wp), intent(out) :: x
    ! 
    call random_number(x)
    ! 
  end subroutine get_random_real
  
  ! Get random integer in range [lo,hi]
  subroutine get_random_int(x, lo, hi)
    integer, intent(out) :: x
    integer, intent(in) :: hi, lo
    real(wp) :: u
    ! 
    call random_number(u)
    x = int(u * (hi - lo)) + lo
    ! 
  end subroutine get_random_int

  !!!#### Fisher–Yates/Knuth Shuffle
  subroutine knuth_shuffle(x,xna)
    implicit none
    double precision, intent(inout), dimension(:) :: x
    logical, intent(inout), dimension(:) :: xna
    integer n, i, j
    real rnd
    double precision tmp
    logical tmpna

    n = size(x)
    do i = n, 2, -1
       call random_number(rnd)
       j = int(rnd*i)+1
       tmp = x(i)
       x(i) = x(j)
       x(j) = tmp
       tmpna = xna(i)
       xna(i) = xna(j)
       xna(j) = tmpna
    end do
  end subroutine knuth_shuffle

!!!#### Bootstrap series (i.e. random draws with replacement)
  subroutine tsboot(x,xna,wx,wxna)
    implicit none
    double precision, intent(in), dimension(:) :: x
    logical, intent(in), dimension(:) :: xna
    double precision, intent(out), dimension(:) :: wx
    logical, intent(out), dimension(:) :: wxna
    integer n, i, j
    real rnd

    n = size(x)
    do i = 1, n
       call random_number(rnd)
       j = int(rnd*n)+1
       wx(i) = x(j)
       wxna(i) = xna(j)
    end do

  end subroutine tsboot

  
  ! Set seed for random number generator
  ! slightly modified from: https://gcc.gnu.org/onlinedocs/gcc-4.2.4/gfortran/RANDOM_005fSEED.html
  subroutine init_random_seed()
    integer :: i, n, clock
    integer, dimension(:), allocatable :: seed
    ! 
    call random_seed(size = n)
    allocate(seed(n))
    call system_clock(count=clock)
    seed = [ (i - 1, i = 1, n) ]
    seed = clock + 37*seed ! + getpid() !add?
    call random_seed(put = seed)
    deallocate(seed)
    ! 
  end subroutine init_random_seed

  ! ! alternatively, could use:
  ! !!!##### Initialize random seed; taken from gfortran online docs
  ! subroutine init_random_seed()
  !   use iso_fortran_env, only: int64
  !   implicit none
  !   integer, allocatable :: seed(:)
  !   integer :: i, n, un, istat, dt(8), pid
  !   integer(int64) :: t

  !   call random_seed(size = n)
  !   allocate(seed(n))
  !   ! First try if the OS provides a random number generator
  !   open(newunit=un, file="/dev/urandom", access="stream", &
  !        form="unformatted", action="read", status="old", iostat=istat)
  !   if (istat == 0) then
  !      read(un) seed
  !      close(un)
  !   else
  !      ! Fallback to XOR:ing the current time and pid. The PID is
  !      ! useful in case one launches multiple instances of the same
  !      ! program in parallel.
  !      call system_clock(t)
  !      if (t == 0) then
  !         call date_and_time(values=dt)
  !         t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 &
  !              + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 &
  !              + dt(3) * 24_int64 * 60 * 60 * 1000 &
  !              + dt(5) * 60 * 60 * 1000 &
  !              + dt(6) * 60 * 1000 + dt(7) * 1000 &
  !              + dt(8)
  !      end if
  !      pid = getpid()
  !      t = ieor(t, int(pid, kind(t)))
  !      do i = 1, n
  !         seed(i) = lcg(t)
  !      end do
  !   end if
  !   call random_seed(put=seed)
  ! contains
  !   ! This simple PRNG might not be good enough for real work, but is
  !   ! sufficient for seeding a better PRNG.
  !   function lcg(s)
  !     integer :: lcg
  !     integer(int64) :: s
  !     if (s == 0) then
  !        s = 104729
  !     else
  !        s = mod(s, 4294967296_int64)
  !     end if
  !     s = mod(s * 279470273_int64, 4294967291_int64)
  !     lcg = int(mod(s, int(huge(0), int64)), kind(0))
  !   end function lcg
  ! end subroutine init_random_seed

end module m_randutils
