program hello
  use omp_lib
  use mpi_f08
  implicit none
  integer :: taskid, ntasks, rc, ierr, tid
  integer :: provided, required=MPI_THREAD_FUNNELED

  call mpi_init_thread(required, provided, rc)

  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, taskid, rc)

 !$omp parallel default(shared) private(tid)
	tid = omp_get_thread_num()
	write(*,*) 'Hello! I am thread ', tid, ' of process ', taskid

 !$omp end parallel 

  call MPI_Finalize(rc)
end program hello
