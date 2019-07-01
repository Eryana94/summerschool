program main
	
	use setup
	implicit none
	integer, dimension(2) :: n_mols
	type(molecule), allocatable :: mols_arr(:)
	real, dimension(3) :: box
	integer, dimension(2) :: n_atoms!atom numbers in molecule type 1 and molecule tpye 2
	
	integer :: i,j

	box = (/5,5,5/)
	n_mols = (/3,5/)
	n_atoms = (/1,4/)

	call init_box(box, mols_arr, n_mols, n_atoms)

	do i=1, n_mols(1)
		write(*,*)'Molecule ', i, ' Type 1'
		do j = 1, n_atoms(1)
			write(*,*)'Atom ', j, ' Coords:', mols_arr(i)%atoms(j)%coords
		end do
	end do

	do i=1, n_mols(2)
		write(*,*)'Molecule ', i+n_mols(1), ' Type 2'
		do j = 1, n_atoms(2)
			write(*,*)'Atom ', j, ' Coords:', mols_arr(i+n_mols(1))%atoms(j)%coords
		end do
	end do
end program main
