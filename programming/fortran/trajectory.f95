program trajectory
	!initialize coordinates
	real::  x,y,z
	real :: E
	real, parameter :: kb=1.380649e-5 
	real, allocatable :: old_pos(:,:,:) ! old position matrix
	real, allocatable :: new_pos(:,:,:) ! new position matrix
	x = 10 ! box dimensioan
	y = 10
	z = 10
	n_par = 5 ! number of particles
	real :: r
	
	
