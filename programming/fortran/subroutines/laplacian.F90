module laplacian_mod
  implicit none
  real, parameter :: dx = 0.01, dy = 0.01

contains

  subroutine initialize(field0)
    ! TODO: implement a subroutine that initializes the input array
	implicit none
	real, dimension(0:,0:), intent(inout) :: field0
	call random_number(field0)
   
   end subroutine initialize

  subroutine laplacian(curr, prev)
        implicit none
        real, dimension(0:,0:), intent(inout) :: curr, prev
        integer :: i, j
	do j = 1, ubound(prev,2)-1
		do i = 1, ubound(prev,1)-1
			curr(i,j)=(prev(i-1,j)-2*prev(i,j)+prev(i+1,j))/(dx**2)+(prev(i,j-1)-2*prev(i,j)+prev(i,j+1))/(dy**2) 
		end do
	end do	
        


  end subroutine laplacian

  subroutine write_field(array)
	implicit none
	real, dimension(0:,0:), intent(inout) :: array
	integer a
	do a=0, ubound(array,2)
		write(*,*)array(:,a)
	end do

  end subroutine write_field

end module laplacian_mod
