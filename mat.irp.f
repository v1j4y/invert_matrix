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

    print *, 'Matrix:'
    do i=1,rank
        do j=1,rank
            write(6,12)M(j,i)
        enddo
            write(6,*)
    enddo


   12   format((F5.2,'  '),$)

END_PROVIDER
