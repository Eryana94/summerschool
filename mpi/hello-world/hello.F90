program hello
	use mpi_F08
	integer :: rc, ntasks, rid
	call mpi_init(rc)
	call mpi_comm_size(mpi_comm_world, ntasks, rc)
	call mpi_comm_rank(mpi_comm_world, rid, rc)

	if (rid == 0) then
		write(*,*)'There are ', ntasks, 'processes in the world.'
	end if 

	write(*,*)'Hello, sicerely rank ', rid


	call mpi_finalize()

end program hello
