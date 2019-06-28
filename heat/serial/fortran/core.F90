! Main solver routines for heat equation solver
module core

contains

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  subroutine evolve(curr, prev, a, dt)

    use heat
    implicit none

    type(field), intent(inout) :: curr, prev
    real(dp) :: a, dt
    integer :: i, j, nx, ny

    nx = curr%nx
    ny = curr%ny

    ! TODO: implement the heat equation update
  do j = 1, prev%ny
     do i = 1, prev%nx
        curr%data(i,j) = prev%data(i,j) + dt * a * ( (prev%data(i-1,j) - 2.0*prev%data(i,j) + prev%data(i+1,j)) / prev%dx **2 + &
             (prev%data(i,j-1) - 2.0*prev%data(i,j) + prev%data(i,j+1)) / prev%dy **2 )
     end do
  end do
	


  end subroutine evolve
!u^{m+1}(i,j) = u^m(i,j) + \Delta t \alpha \nabla^2 u^m(i,j)
end module core
