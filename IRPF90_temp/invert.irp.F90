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
          LWORK=N                                                    ! invert.irp.f:  19
          UPLO='U'                                                   ! invert.irp.f:  20
          JOBZ='V'                                                   ! invert.irp.f:  21
          do j=1,N                                                   ! invert.irp.f:  23
          do i=1,N                                                   ! invert.irp.f:  24
            A(j,i)=M(j,i)                                            ! invert.irp.f:  25
          enddo                                                      ! invert.irp.f:  26
          enddo                                                      ! invert.irp.f:  27
          call DGETRF( N, N, A, LDA, IPIV, INFO )                    ! invert.irp.f:  38
          write(6,*)'------------'                                   ! invert.irp.f:  39
          write(6,*)INFO                                             ! invert.irp.f:  40
          if (INFO.ne.0)then                                         ! invert.irp.f:  41
              print*,'Error at dspev'                                ! invert.irp.f:  42
              call exit(1)                                           ! invert.irp.f:  43
          endif                                                      ! invert.irp.f:  44
          print *, 'Matrix after DGETRF:'                            ! invert.irp.f:  45
          do i=1,rank                                                ! invert.irp.f:  46
            do j=1,rank                                              ! invert.irp.f:  47
                write(6,12)(A(j,i))                                  ! invert.irp.f:  48
            enddo                                                    ! invert.irp.f:  49
                write(6,*)                                           ! invert.irp.f:  50
          enddo                                                      ! invert.irp.f:  51
          INFO=0                                                     ! invert.irp.f:  52
          call DGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )          ! invert.irp.f:  53
          write(6,*)'------------'                                   ! invert.irp.f:  55
          write(6,*)INFO                                             ! invert.irp.f:  56
          if (INFO.ne.0)then                                         ! invert.irp.f:  58
              print*,'Error at dspev'                                ! invert.irp.f:  59
              call exit(1)                                           ! invert.irp.f:  60
          endif                                                      ! invert.irp.f:  61
          print *, 'Matrix after DGETRI:'                            ! invert.irp.f:  62
          do i=1,rank                                                ! invert.irp.f:  63
            do j=1,rank                                              ! invert.irp.f:  64
                write(6,12)(A(j,i))                                  ! invert.irp.f:  65
            enddo                                                    ! invert.irp.f:  66
                write(6,*)                                           ! invert.irp.f:  67
          enddo                                                      ! invert.irp.f:  68
          do i=1,rank                                                ! invert.irp.f:  70
            do j=1,rank                                              ! invert.irp.f:  71
                INV(i,j)= A(i,j)                                     ! invert.irp.f:  73
            enddo                                                    ! invert.irp.f:  74
          enddo                                                      ! invert.irp.f:  75
          print *, 'Matrix Inverse:'                                 ! invert.irp.f:  77
          do i=1,rank                                                ! invert.irp.f:  78
            do j=1,rank                                              ! invert.irp.f:  79
                write(6,12)(INV(j,i))                                ! invert.irp.f:  80
            enddo                                                    ! invert.irp.f:  81
                write(6,*)                                           ! invert.irp.f:  82
          enddo                                                      ! invert.irp.f:  83
          PROD=0d0                                                   ! invert.irp.f:  85
          print *, 'Matrix Inverse*Matrix:'                          ! invert.irp.f:  86
          do i=1,rank                                                ! invert.irp.f:  87
            do j=1,rank                                              ! invert.irp.f:  88
                do k=1,rank                                          ! invert.irp.f:  89
                    PROD(j,i)=PROD(j,i)+(INV(j,k)*M(k,i))            ! invert.irp.f:  90
                enddo                                                ! invert.irp.f:  91
                write(6,12)((PROD(j,i)))                             ! invert.irp.f:  92
            enddo                                                    ! invert.irp.f:  93
                write(6,*)                                           ! invert.irp.f:  94
          enddo                                                      ! invert.irp.f:  95
          deallocate (A)                                             ! invert.irp.f:  97
          deallocate (AP)                                            ! invert.irp.f:  98
          deallocate (IPIV)                                          ! invert.irp.f:  99
          deallocate (WORK)                                          ! invert.irp.f: 100
   12   format((F8.4,'  '),$)                                        ! invert.irp.f: 101
end                                                                  ! invert.irp.f: 104
