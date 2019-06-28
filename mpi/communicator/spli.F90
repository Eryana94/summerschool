program comm_split
	use mpi_f08
	integer :: rc, i, ntasks, rankid, col
	integer, dimension(8) :: sendbuf, recvbuf
	type(mpi_comm) :: comm1
	call mpi_init(rc)
	
	call mpi_comm_size(mpi_comm_world, ntasks, rc)
	call mpi_comm_rank(mpi_comm_world, rankid, rc)	

	do i=1, 8
		sendbuf(i) = (i-1)+ 8*(rankid)	 
	end do
	
	call mpi_barrier(mpi_comm_world, rc)
	
	write(*,*)'Task ', rankid, ': ', sendbuf
	
	if (mod(rankid,2)==0) then
		 col = 1
	else
		col = 2
	end if

	call mpi_comm_split(mpi_comm_world, col, rankid, comm1, rc)

	call mpi_reduce(sendbuf, recvbuf, 8, mpi_integer, mpi_sum, 0, comm1)
	call mpi_barrier(mpi_comm_world, rc)
	write(*,*)'Task ', rankid, ': ', recvbuf	

	call mpi_finalize(rc)
end program comm_split
