program invert
    implicit none
          real*8,allocatable :: INV(:,:),PROD(:,:)
          double precision, allocatable :: EVAL(:),EVEC(:,:)
          double precision, allocatable :: A(:,:),WORK(:),AP(:)
          integer,allocatable :: IPIV(:)
          character*1 JOBZ,UPLO
          integer N,LDA,LWORK,i,j,l,k,INFO

          N = rank

          allocate (INV(N,N))
          allocate (PROD(N,N))
          allocate (A(N,N))
          allocate (AP(N*N))
          allocate (IPIV(N))
          allocate (WORK(N*(2*N)))
          LDA=N
          UPLO='U'
          JOBZ='V'

          K=0
          do j=1,N
          do i=1,j
            AP(i+(j-1)*j/2)=M(i,j)
            K=K+1
          enddo
          enddo

!    invert the matrix
          call DSPTRF( UPLO, N, AP, IPIV, INFO )
!         write(6,*)'------------'
!         write(6,*)INFO
          if (INFO.ne.0)then
              print*,'Error at dspev'
              call exit(1)
          endif
!         INFO=0
          call DSPTRI( UPLO, N, AP, IPIV, WORK, INFO )

!         write(6,*)'------------'
!         write(6,*)INFO

          if (INFO.ne.0)then
              print*,'Error at dspev'
              call exit(1)
          endif

          do i=1,rank
            do j=1,i
                INV(j,i)= AP(j + (i-1)*i/2)
                INV(i,j)= INV(j,i)
            enddo
          enddo

          print *, 'Matrix Inverse:'
          do i=1,rank
            do j=1,rank
                write(6,12)(INV(j,i))
            enddo
                write(6,*)
          enddo

          PROD=0d0
          print *, 'Matrix Inverse*Matrix:'
          do i=1,rank
            do j=1,rank
                do k=1,rank
                    PROD(j,i)+=INV(j,k)*M(k,i)
                enddo
                write(6,12)((PROD(j,i)))
            enddo
                write(6,*)
          enddo

          deallocate (A)
          deallocate (AP)
          deallocate (IPIV)
          deallocate (WORK)
   12   format((F5.2,'  '),$)
!contains
    !TODO
end program invert
