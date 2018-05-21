! Define types to be used in other modules
module m_argtype
  use, intrinsic :: iso_fortran_env, only: real32, real64, real128
  implicit none
  !
  integer, parameter :: sp = real32
  integer, parameter :: dp = real64
  integer, parameter :: qp = real128
  ! OR??
  ! integer, parameter :: sp = selected_real_kind(6, 37)
  ! integer, parameter :: dp = selected_real_kind(15, 307)
  ! integer, parameter :: qp = selected_real_kind(33, 4931)
  !
  ! working precision to use in most functions
  integer, parameter :: wp = dp

end module m_argtype
