! -*- F90 -*-
!
!-----------------------------------------------!
! This file was generated with the irpf90 tool. !
!                                               !
!           DO NOT MODIFY IT BY HAND            !
!-----------------------------------------------!

subroutine provide_m
  use rank_mod
  use mat_mod
  implicit none
  character*(9) :: irp_here = 'provide_m'
  integer                   :: irp_err 
  logical                   :: irp_dimensions_OK
  if (.not.rank_is_built) then
    call provide_rank
  endif
 if (allocated (m) ) then
  irp_dimensions_OK = .True.
  irp_dimensions_OK = irp_dimensions_OK.AND.(SIZE(m,1)==(rank))
  irp_dimensions_OK = irp_dimensions_OK.AND.(SIZE(m,2)==(rank))
  if (.not.irp_dimensions_OK) then
   deallocate(m,stat=irp_err)
   if (irp_err /= 0) then
     print *, irp_here//': Deallocation failed: m'
     print *, ' size: (rank,rank)'
   endif
   if ((rank>0).and.(rank>0)) then
    allocate(m(rank,rank),stat=irp_err)
    if (irp_err /= 0) then
     print *, irp_here//': Allocation failed: m'
     print *, ' size: (rank,rank)'
    endif
   endif
  endif
 else
   if ((rank>0).and.(rank>0)) then
    allocate(m(rank,rank),stat=irp_err)
    if (irp_err /= 0) then
     print *, irp_here//': Allocation failed: m'
     print *, ' size: (rank,rank)'
    endif
   endif
 endif
 if (.not.m_is_built) then
  call bld_m
  m_is_built = .True.

 endif
end subroutine provide_m

subroutine bld_m
  use rank_mod
  use mat_mod
    implicit none                                                       ! mat.irp.f:   3
  character*(1) :: irp_here = 'm'                                       ! mat.irp.f:   1
    integer :: i,j,k,l                                                  ! mat.irp.f:   8
    M(1,1)=1d0                                                          ! mat.irp.f:  10
    M(1,2)=1d0                                                          ! mat.irp.f:  11
    M(1,3)=2d0                                                          ! mat.irp.f:  12
    M(2,1)=1d0                                                          ! mat.irp.f:  14
    M(2,2)=1d0                                                          ! mat.irp.f:  15
    M(2,3)=0d0                                                          ! mat.irp.f:  16
    M(3,1)=2d0                                                          ! mat.irp.f:  18
    M(3,2)=0d0                                                          ! mat.irp.f:  19
    M(3,3)=1d0                                                          ! mat.irp.f:  20
    print *, 'Matrix:'                                                  ! mat.irp.f:  22
    do i=1,rank                                                         ! mat.irp.f:  23
        do j=1,rank                                                     ! mat.irp.f:  24
            write(6,12)M(j,i)                                           ! mat.irp.f:  25
        enddo                                                           ! mat.irp.f:  26
            write(6,*)                                                  ! mat.irp.f:  27
    enddo                                                               ! mat.irp.f:  28
   12   format((F5.2,'  '),$)                                           ! mat.irp.f:  31
end subroutine bld_m
