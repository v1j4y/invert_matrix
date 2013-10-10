! -*- F90 -*-
!
!-----------------------------------------------!
! This file was generated with the irpf90 tool. !
!                                               !
!           DO NOT MODIFY IT BY HAND            !
!-----------------------------------------------!

program irp_program                                                  ! invert:   0
 call invert                                                         ! invert.irp.f:   0
 call irp_finalize_751899324()                                       ! invert.irp.f:   0
end program                                                          ! invert.irp.f:   0
subroutine invert                                                    ! invert.irp.f:   1
  use rank_mod
  use mat_mod
    implicit none                                                    ! invert.irp.f:   2
  character*(6) :: irp_here = 'invert'                               ! invert.irp.f:   1
          real*8,allocatable :: INV(:,:),PROD(:,:)                   ! invert.irp.f:   3
          double precision, allocatable :: EVAL(:),EVEC(:,:)         ! invert.irp.f:   4
          double precision, allocatable :: A(:,:),WORK(:),AP(:)      ! invert.irp.f:   5
          integer,allocatable :: IPIV(:)                             ! invert.irp.f:   6
          character*1 JOBZ,UPLO                                      ! invert.irp.f:   7
          integer N,LDA,LWORK,i,j,l,k,INFO                           ! invert.irp.f:   8
  if (.not.m_is_built) then
    call provide_m
  endif
  if (.not.rank_is_built) then
    call provide_rank
  endif
          N = rank                                                   ! invert.irp.f:  10
          allocate (INV(N,N))                                        ! invert.irp.f:  12
          allocate (PROD(N,N))                                       ! invert.irp.f:  13
          allocate (A(N,N))                                          ! invert.irp.f:  14
          allocate (AP(N*N))                                         ! invert.irp.f:  15
          allocate (IPIV(N))                                         ! invert.irp.f:  16
          allocate (WORK(N*(2*N)))                                   ! invert.irp.f:  17
          LDA=N                                                      ! invert.irp.f:  18
          UPLO='U'                                                   ! invert.irp.f:  19
          JOBZ='V'                                                   ! invert.irp.f:  20
          K=0                                                        ! invert.irp.f:  22
          do j=1,N                                                   ! invert.irp.f:  23
          do i=1,j                                                   ! invert.irp.f:  24
            AP(i+(j-1)*j/2)=M(i,j)                                   ! invert.irp.f:  25
            K=K+1                                                    ! invert.irp.f:  26
          enddo                                                      ! invert.irp.f:  27
          enddo                                                      ! invert.irp.f:  28
          call DSPTRF( UPLO, N, AP, IPIV, INFO )                     ! invert.irp.f:  31
          if (INFO.ne.0)then                                         ! invert.irp.f:  34
              print*,'Error at dspev'                                ! invert.irp.f:  35
              call exit(1)                                           ! invert.irp.f:  36
          endif                                                      ! invert.irp.f:  37
          call DSPTRI( UPLO, N, AP, IPIV, WORK, INFO )               ! invert.irp.f:  39
          if (INFO.ne.0)then                                         ! invert.irp.f:  44
              print*,'Error at dspev'                                ! invert.irp.f:  45
              call exit(1)                                           ! invert.irp.f:  46
          endif                                                      ! invert.irp.f:  47
          do i=1,rank                                                ! invert.irp.f:  49
            do j=1,i                                                 ! invert.irp.f:  50
                INV(j,i)= AP(j + (i-1)*i/2)                          ! invert.irp.f:  51
                INV(i,j)= INV(j,i)                                   ! invert.irp.f:  52
            enddo                                                    ! invert.irp.f:  53
          enddo                                                      ! invert.irp.f:  54
          print *, 'Matrix Inverse:'                                 ! invert.irp.f:  56
          do i=1,rank                                                ! invert.irp.f:  57
            do j=1,rank                                              ! invert.irp.f:  58
                write(6,12)(INV(j,i))                                ! invert.irp.f:  59
            enddo                                                    ! invert.irp.f:  60
                write(6,*)                                           ! invert.irp.f:  61
          enddo                                                      ! invert.irp.f:  62
          PROD=0d0                                                   ! invert.irp.f:  64
          print *, 'Matrix Inverse*Matrix:'                          ! invert.irp.f:  65
          do i=1,rank                                                ! invert.irp.f:  66
            do j=1,rank                                              ! invert.irp.f:  67
                do k=1,rank                                          ! invert.irp.f:  68
                    PROD(j,i)=PROD(j,i)+(INV(j,k)*M(k,i))            ! invert.irp.f:  69
                enddo                                                ! invert.irp.f:  70
                write(6,12)((PROD(j,i)))                             ! invert.irp.f:  71
            enddo                                                    ! invert.irp.f:  72
                write(6,*)                                           ! invert.irp.f:  73
          enddo                                                      ! invert.irp.f:  74
          deallocate (A)                                             ! invert.irp.f:  76
          deallocate (AP)                                            ! invert.irp.f:  77
          deallocate (IPIV)                                          ! invert.irp.f:  78
          deallocate (WORK)                                          ! invert.irp.f:  79
   12   format((F5.2,'  '),$)                                        ! invert.irp.f:  80
end                                                                  ! invert.irp.f:  83
