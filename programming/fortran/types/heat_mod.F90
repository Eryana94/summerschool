module heat

use iso_fortran_env, only : REAL64
implicit none
integer, parameter :: dp = REAL64


type field
	integer :: nx, ny
	real(dp) :: dx, dy
	real(dp), allocatable :: vals(:,:)	
end type field

contains
subroutine set_field(nx, ny, field0)
	implicit none
	type(field), intent(out) :: field0
	integer, intent(in) :: x, y
	field0%nx = nx
	field0%ny = ny
	field0%dx = 0.01
	field0%dy = 0.01

end subrountine set_field

end module heat
