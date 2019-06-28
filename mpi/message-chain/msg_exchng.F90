program exchange
  use mpi_f08
  implicit none
  integer, parameter :: msgsize = 100
  integer :: rc, pid, ntasks
  type(mpi_status) :: status
  integer :: message(msgsize)
  integer :: receiveBuffer(msgsize)

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, pid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  message = pid

  if (pid == 0) then
     call mpi_sendrecv(message, msgsize, mpi_integer, 1, 1, receiveBuffer, msgsize, mpi_integer, ntasks-1, 0, mpi_comm_world, status, rc)
    ! call mpi_send(message, msgsize, MPI_INTEGER, 1, 1, MPI_COMM_WORLD, rc)
    ! call mpi_recv(receiveBuffer, msgsize, MPI_INTEGER, ntasks - 1, 0, MPI_COMM_WORLD, status, rc)
     write(*,*) 'Rank: ', pid,' received message', receiveBuffer(1)
  else if (pid == ntasks-1) then
     call mpi_sendrecv(message, msgsize, mpi_integer, 0, 0, receiveBuffer, msgsize, mpi_integer, pid-1, pid, mpi_comm_world, status, rc)
     !call mpi_send(message, msgsize, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, rc)
     !call mpi_recv(receiveBuffer, msgsize, MPI_INTEGER, pid - 1, pid, MPI_COMM_WORLD, status, rc)
     write(*,*) 'Rank: ', pid, ' received message', receiveBuffer(1)
  else   
     call mpi_sendrecv(message, msgsize, mpi_integer, pid+1, pid+1, receiveBuffer, msgsize, mpi_integer, pid-1, pid, mpi_comm_world, status, rc)
    ! call mpi_send(message, msgsize, MPI_INTEGER, pid +1, pid+1, MPI_COMM_WORLD, rc)
    ! call mpi_recv(receiveBuffer, msgsize, MPI_INTEGER, pid -1, pid, MPI_COMM_WORLD, status, rc)
     write(*,*) 'Rank: ', pid, ' received message', receiveBuffer(1)
     
  end if

  call mpi_finalize(rc)

end program exchange
