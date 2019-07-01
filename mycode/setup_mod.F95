module setup

!	use iso_fortran_anv, only :: real64, int16
!	integer, parameter :: rp = real64, ip = int16
	
	type atom
		! contains atom type, charge, vdw parameters, and coordinates
		character (len=4) :: a_type
		real :: charge
		real :: vdw_eps
		real :: vdw_sigma
		real, dimension(3) :: coords
	end type atom


	type molecule
		! contains molecule name and atoms
		character (len=3) :: mol_type
		type(atom), allocatable :: atoms(:)
		integer, allocatable :: bonds(:,:)
	end type molecule

	contains

	subroutine init_box(box, mols_arr, n_mols, n_atoms)
	! initialize simulation box using default parameters or parameters from input file (vdwd, atom coordinates)
		implicit none
		type(molecule), allocatable :: mols_arr(:)
		integer, dimension(2) :: n_mols
		integer, dimension(2) :: n_atoms
		real, dimension(3) :: box
		real :: alpha
		integer :: i, j
		real, dimension(3) :: kick
		real, parameter :: bl = 0.2
		 	
		!aalocate array containing molecules in system mols_arr
		allocate(mols_arr(n_mols(1) + n_mols(2)))
	
		do i=1, n_mols(1)
			allocate(mols_arr(i)%atoms(n_atoms(1)))
			do j = 1, n_atoms(1)
				call random_number(kick)
				if (j == 1) then
					mols_arr(i)%atoms(j)%coords = box * kick
				else
					kick = kick / sqrt(kick(1)**2 + kick(2)**2+ kick(3)**2) ! normalize kick vector
					mols_arr(i)%atoms(j)%coords = kick * bl + mols_arr(i)%atoms(j-1)%coords ! calculate position of next atom in molecule based on kick and bond lenght
				end if
			end do
		end do

		do i=1, n_mols(2)
			allocate(mols_arr(i+n_mols(1))%atoms(n_atoms(2)))
			do j = 1, n_atoms(2)
				call random_number(kick)
				if (j == 1) then
					mols_arr(i+n_mols(1))%atoms(j)%coords = box * kick
				else
					kick = kick / sqrt(kick(1)**2 + kick(2)**2+ kick(3)**2) ! normalize kick vector
					mols_arr(i+n_mols(1))%atoms(j)%coords = kick * bl + mols_arr(n_mols(1)+i)%atoms(j-1)%coords ! calculate position of next atom in molecule based on kick and bond lenght
				end if
			end do
		end do
	end subroutine init_box


end module setup
