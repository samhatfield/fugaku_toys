module io
    implicit none

contains
subroutine write_hdata_file(tdata,tddata,nt,nx,ny,tdx,tdy,filename)

!    function: write data array to file


  implicit none

  integer nx,ny,nt
  REAL*8 :: tdata(0:nx,0:ny)
  REAL*8 ::tddata(0:nx,0:ny,0:nt),tdx,tdy
  character(len=*) filename
  integer i,j
  real :: data(0:nx,0:ny)
  real ::ddata(0:nx,0:ny,0:nt),dx,dy

  data = tdata
  ddata = tddata
  dx =tdx
  dy= tdy

  print *,filename
  open(unit=9,file=filename,status='unknown')

  do j=1,ny-1
     do i=1,nx-1
        write(9,*) real(i-1)*dx,real(j-1)*dy,&
             & data(i,j),ddata(i,j,1),ddata(i,j,2)
     end do
  end do

  close(9)

  return
end subroutine write_hdata_file

!    --------------------------------------------------------------------------

subroutine write_udata_file(tdata,tddata,nt,nx,ny,tdx,tdy,filename)

!    function: write data array to file


  implicit none

  integer nx,ny,nt
  REAL*8 :: tdata(0:nx,0:ny)
  REAL*8 ::tddata(0:nx,0:ny,0:nt),tdx,tdy
  character(len=*) filename
  integer i,j
  real :: data(0:nx,0:ny)
  real ::ddata(0:nx,0:ny,0:nt),dx,dy

  data = tdata
  ddata = tddata
  dx =tdx
  dy= tdy

  print *,filename
  open(unit=9,file=filename,status='unknown')

  do j=1,ny-1
     do i=1,nx
        write(9,*) real(i-0.5)*dx,real(j-1)*dy,&
             &data(i,j),ddata(i,j,1),ddata(i,j,2)
     end do
  end do

  close(9)

  return
end subroutine write_udata_file

!    --------------------------------------------------------------------------

subroutine write_vdata_file(tdata,tddata,nt,nx,ny,tdx,tdy,filename)

!    function: write data array to file

  implicit none

  integer nx,ny,nt
  REAL*8 :: tdata(0:nx,0:ny)
  REAL*8 ::tddata(0:nx,0:ny,0:nt),tdx,tdy
  character(len=*) filename
  integer i,j
  real :: data(0:nx,0:ny)
  real ::ddata(0:nx,0:ny,0:nt),dx,dy

  data = tdata
  ddata = tddata
  dx =tdx
  dy= tdy

  print *,filename
  open(unit=9,file=filename,status='unknown')

  do j=1,ny
     do i=1,nx-1
        write(9,*) real(i-1)*dx,real(j-0.5)*dy,data(i,j),&
             &ddata(i,j,1),ddata(i,j,2)
     end do
  end do

  close(9)

  return
end subroutine write_vdata_file

end module io
