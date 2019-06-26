program hello
  implicit none
  integer :: i
  write(*,*)'Syota numero'
  read(*,*)i
  do while(i>0)
	if (mod(i,2)==0) then
		write(*,*)'Parillinen luku'
	else
		write(*,*)'Pariton luku'
	end if
	write(*,*)'Syota numero'
	read(*,*)i
  end do
end program hello
