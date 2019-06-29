program datatype1
  use mpi_f08
  implicit none

  integer, dimension(8,8) :: array
  integer :: rank, ierr
  !TODO: declare variable for datatype
  integer :: i, j
  type(mpi_datatype) :: rowtype
  type(mpi_status) :: status
  integer, dimension(4) :: stride, nd

  call mpi_init(ierr)
 ! call mpi_comm_size(mpi_comm_world, nstasks, ierr )
  call mpi_comm_rank(MPI_COMM_WORLD, rank ,ierr)

  ! initialize arrays
  if (rank == 0) then
     do i=1,8
        do j=1,8
           array(i,j) = i*10 + j
        end do
     end do
  else
     array(:,:) = 0
  end if

  if (rank == 0) then
     write(*,*) 'Data in rank 0'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if
  
  nd=(/1,2,3,4/)
  
  do i=1,4
     stride(i)= (i-1)+2*8*(i-1)
  end do

  if (rank == 0) then
	write(*,*)'Strides: ', stride
  end if




	call mpi_type_indexed(4, nd, stride, mpi_integer, rowtype, ierr)
	call mpi_type_commit(rowtype, ierr)
  !TODO: create datatype describing one row, use mpi_type_vector
	if (rank == 0) then
		call mpi_send(array, 1, rowtype, 1, 1, mpi_comm_world, ierr)
	else
		call mpi_recv(array, 1,rowtype, 0, 1, mpi_comm_world, status, ierr) 
	end if
	


  !TODO: send first row of matrix from rank 0 to 1

  ! Print out the result
  if (rank == 1) then
     write(*,*) 'Received data'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if

  !TODO free datatype

  call mpi_finalize(ierr)

end program datatype1
