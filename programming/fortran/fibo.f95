program fibo
	real*4 :: n1, n2, n3
	integer :: i
	n1 = 1
	n2 = 1
	write(*,*)n1
	write(*,*)n2
	do i = 2, 100
		n3 = n1 + n2
		write(*,*)n3
		n2 = n1
		n1 = n3
	end do
end program fibo

