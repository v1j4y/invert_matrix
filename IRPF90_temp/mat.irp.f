BEGIN_PROVIDER [real*8,M,(rank,rank)]

    implicit none
    BEGIN_DOC
    ! give the matrix input
    END_DOC

    integer :: i,j,k,l

    M(1,1)=1d0
    M(1,2)=1d0
    M(1,3)=2d0

    M(2,1)=1d0
    M(2,2)=1d0
    M(2,3)=0d0

    M(3,1)=2d0
    M(3,2)=0d0
    M(3,3)=1d0
!--------------------
!   M(1,1)=-1.043d0
!   M(1,2)=.21d0
!   M(1,3)=.27d0
!   M(1,4)=-.51d0
!   M(1,5)=-.67d0

!   M(2,1)=1.053d0
!   M(2,2)=.42d0
!   M(2,3)=-.61d0
!   M(2,4)=-.39d0
!   M(2,5)=-.16d0

!   M(3,1)=-1.04d0
!   M(3,2)=.49d0
!   M(3,3)=-.36d0
!   M(3,4)=.65d0
!   M(3,5)=-1.23d0

!   M(4,1)=1.056d0
!   M(4,2)=-.21d0
!   M(4,3)=.24d0
!   M(4,4)=.41d0
!   M(4,5)=-.64d0

!   M(5,1)=1.026d0
!   M(5,2)=.70d0
!   M(5,3)=.61d0
!   M(5,4)=.05d0
!   M(5,5)=.26d0

    print *, 'Matrix:'
    do i=1,rank
        do j=1,rank
            write(6,12)M(j,i)
        enddo
            write(6,*)
    enddo


   12   format((F5.2,'  '),$)

END_PROVIDER
