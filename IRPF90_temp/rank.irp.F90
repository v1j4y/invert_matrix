! -*- F90 -*-
!
!-----------------------------------------------!
! This file was generated with the irpf90 tool. !
!                                               !
!           DO NOT MODIFY IT BY HAND            !
!-----------------------------------------------!

subroutine provide_rank
  use rank_mod
  implicit none
  character*(12) :: irp_here = 'provide_rank'
  integer                   :: irp_err 
  logical                   :: irp_dimensions_OK
 if (.not.rank_is_built) then
  call bld_rank
  rank_is_built = .True.

 endif
end subroutine provide_rank

subroutine bld_rank
  use rank_mod
    implicit none                                                      ! rank.irp.f:   3
  character*(4) :: irp_here = 'rank'                                   ! rank.irp.f:   1
    rank = 3                                                           ! rank.irp.f:   8
end subroutine bld_rank
