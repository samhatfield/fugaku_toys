module dynamics
    implicit none

contains
subroutine rhs(n,nx,ny,nt,u,du,v,dv,h,dh,gp,rdx,rdy,au,taux,tauy,fu,fv,&
          & lrestart,ab,dt,h0)

  !THIS SUBROUTINE WILL CALCULATE THE RIGHT HAND SIDE OF THE SHALLOW WATER EQUATIONS



  implicit none

  INTEGER :: n,nx,ny,nt
  REAL*8 :: h(0:nx,0:ny),u(0:nx,0:ny),v(0:nx,0:ny)
  REAL*8 :: taux(0:ny),tauy(0:nx),b(0:nx,0:ny)
  REAL*8 :: dh(0:nx,0:ny,0:nt),du(0:nx,0:ny,0:nt),dv(0:nx,0:ny,0:nt)
  REAL*8 :: ab(nt)
  REAL*8 :: h0
  integer i,j,k
  character*5 num,crun
  REAL*8 :: fu(0:ny),fv(0:ny)
  REAL*8 :: zeta(0:nx,0:ny)
  REAL*8 :: gp,f0,au,rdx,rdy,dt,slip
  REAL*8 :: r0,r1,r2,r3,r4,r5
  logical :: lrestart

!!!!!!!!!!!!!!!!!!!!!!!!!!!
!This is the grid:
!!!!!!!!!!!!!!!!!!!!!!!!!!!
!------------------------------------------------------------...
!  h(1,ny-1)   u(2,ny-1)   h(2,ny-1)   u(3,ny-1)   h(3,ny-1)   u(4,ny-1)       ... u(nx-1,ny-1)   h(nx-1,ny-1)
!
!--v(1,ny-1)---------------v(1,ny-1)---------------v(1,ny-1)-...                                  v(nx-1,ny-1)
!
!  h(1,ny-2)   u(2,ny-2)   h(2,ny-2)   u(3,ny-2)   h(3,ny-2)   u(4,ny-2)       ... u(nx-1,ny-2)   h(nx-1,ny-2)
!.
!.
!.
!----------------------------------------------...
!
!  h(1,2)   u(2,2)   h(2,2)   u(3,2)   h(3,2)   u(4,2) ... u(nx-1,2)   h(nx-1,2)
!
!--v(1,2))-----------v(2,2)------------v(3,2)--...                     v(nx-1,2)
!
!  h(1,1)   u(2,1)   h(2,1)   u(3,1)   h(3,1)   u(4,1) ... u(nx-1,1)   h(nx-1,1)
!------------------------------------------------------


     r0 = .125
     r1 = 2.0
     r2 = 0.25
     r3 = 0.5
     r4 = 1.0

     ! calculate Bernoulli potential
     ! 1/g*h+0.5(u^2+v^2)
     do j=0,ny-1
        do i=0,nx-1
           b(i,j)=gp*h(i,j)+r0*&
                & ((u(i,j)+u(i+1,j))**2+(v(i,j)+v(i,j+1))**2)
        end do
     end do

     ! calculate relative vorticity
     ! d_x v - d_y u
     do j=1,ny
        do i=1,nx
           zeta(i,j)=(v(i,j)-v(i-1,j))*rdx-(u(i,j)-u(i,j-1))*rdy
        end do
     end do

     ! calculate new time increments for prognostic variables
     do j=1,ny-1
        do i=2,nx-1
           du(i,j,3)=du(i,j,2)  !For Adams Bashforth
           du(i,j,2)=du(i,j,1)  !For Adams Bashforth
           du(i,j,1)=au*(u(i+1,j)+u(i-1,j)-r1*u(i,j))*rdx*rdx+au*(u(i,j+1)+u(i,j-1)-r1*u(i,j))*rdy*rdy &  !\nu (dx dx u + dy dy u)
                & +r2*(fu(j)+r3*(zeta(i,j)+zeta(i,j+1)))*(v(i-1,j)+v(i,j)+v(i-1,j+1)+v(i,j+1)) & ! +(f+zeta)v
                & -(b(i,j)-b(i-1,j))*rdx + taux(j) !-dx b = dx (g*h+0.5(u^2+v^2))
        end do
     end do

    do j=2,ny-1
        do i=1,nx-1
           dv(i,j,3)=dv(i,j,2)  !For Adams Bashforth
           dv(i,j,2)=dv(i,j,1)  !For Adams Bashforth
           dv(i,j,1)=au*(v(i+1,j)+v(i-1,j)-r1*v(i,j))*rdx*rdx+au*(v(i,j+1)+v(i,j-1)-r1*v(i,j))*rdy*rdy & !\nu (dx dx v + dy dy v)
                & -r2*(fv(j)+r3*(zeta(i,j)+zeta(i+1,j)))*(u(i,j-1)+u(i,j)+u(i+1,j-1)+u(i+1,j)) & ! -(f+zeta)u
                & - (b(i,j)-b(i,j-1))*rdy + tauy(i) !-dx b = dx (g*h+0.5(u^2+v^2))
        end do
     end do

     do j=1,ny-1
        do i=1,nx-1
           dh(i,j,3)=dh(i,j,2)  !For Adams Bashforth
           dh(i,j,2)=dh(i,j,1)  !For Adams Bashforth
           dh(i,j,1)= +(h0+r3*(h(i-1,j)+h(i,j)))*u(i,j)*rdx &
                & +(h0+r3*(h(i,j-1)+h(i,j)))*v(i,j)*rdy &
                & -(h0+r3*(h(i+1,j)+h(i,j)))*u(i+1,j)*rdx &
                & -(h0+r3*(h(i,j+1)+h(i,j)))*v(i,j+1)*rdy
        end do
     end do

     !Calculate contribution for prognostic variables for this timestep
     do j=1,ny-1
        do i=2,nx-1
           IF(n.lt.3.and.(.not.lrestart))THEN
              du(i,j,0)=du(i,j,1)*dt
           ELSE
              du(i,j,0)=ab(1)*du(i,j,1)+ab(2)*du(i,j,2)+ab(3)*du(i,j,3)
           END IF
        end do
     end do
     do j=2,ny-1
        do i=1,nx-1
           IF(n.lt.3.and.(.not.lrestart))THEN
              dv(i,j,0)=dv(i,j,1)*dt
           ELSE
              dv(i,j,0)=ab(1)*dv(i,j,1)+ab(2)*dv(i,j,2)+ab(3)*dv(i,j,3)
           END IF
        end do
     end do

     do j=1,ny-1
        do i=1,nx-1
           IF(n.lt.3.and.(.not.lrestart))THEN
              dh(i,j,0)=dh(i,j,1)*dt
           ELSE
              dh(i,j,0)=ab(1)*dh(i,j,1)+ab(2)*dh(i,j,2)+ab(3)*dh(i,j,3)
           END IF
        end do
     end do

   end subroutine rhs



subroutine timeupdate(n,nx,ny,nt,u,du,v,dv,h,dh,slip,ndump,num,nwrite,dx,dy)

  !THIS SUBROUTINE WILL UPDATE THE PROGNOSTIC VARIABELS

  implicit none

  INTEGER :: n,nx,ny,nt
  REAL*8 :: h(0:nx,0:ny),u(0:nx,0:ny),v(0:nx,0:ny)
  REAL*8 :: dh(0:nx,0:ny,0:nt),du(0:nx,0:ny,0:nt),dv(0:nx,0:ny,0:nt)
  integer i,j,k
  character*5 num,crun
  REAL*8 :: slip,r1,r4
  REAL*8 :: mean(3), meandiff(3), std(3)
  integer ndump,nwrite
  REAL*8 :: dx,dy

  r1 = 2.0
  r4 = 1.0

  do j=1,ny-1
     do i=2,nx-1
        u(i,j)=u(i,j)+du(i,j,0)
     end do
  end do
  do j=2,ny-1
     do i=1,nx-1
        v(i,j)=v(i,j)+dv(i,j,0)
     end do
  end do

  do j=1,ny-1
     do i=1,nx-1
        h(i,j)=h(i,j)+dh(i,j,0)
     end do
  end do

  !FIX BOUNDARY CONDITIONS
  do j=1,ny-1
     v(0,j)=(r4-r1*slip)*v(1,j)
     v(nx,j)=(r4-r1*slip)*v(nx-1,j)
     h(0,j)=h(1,j)
     h(nx,j)=h(nx-1,j)
  end do
  do i=1,nx-1
     u(i,0)=(r4-r1*slip)*u(i,1)
     u(i,ny)=(r4-r1*slip)*u(i,ny-1)
     h(i,0)=h(i,1)
     h(i,ny)=h(i,ny-1)
  end do

  !WRITE OUTPUT IF NECESSARY
  if (mod(n,nwrite).eq.0) then
     ndump=ndump+1
     if (ndump.le.9) write(num,'(I1)') ndump
     if (ndump.ge.10.and.ndump.le.99) write(num,'(I2)') ndump
     if (ndump.ge.100.and.ndump.le.999) write(num,'(I3)') ndump
     if (ndump.ge.1000.and.ndump.le.9999) write(num,'(I4)') ndump
     if (ndump.ge.10000) write(num,'(I5)') ndump
     print *,num
     call write_hdata_file(h,dh,nt,nx,ny,dx,dy,'./Output/h.noemulator.'//num)
     call write_udata_file(u,du,nt,nx,ny,dx,dy,'./Output/u.noemulator.'//num)
     call write_vdata_file(v,dv,nt,nx,ny,dx,dy,'./Output/v.noemulator.'//num)
  endif

end subroutine timeupdate




subroutine initialise(nx,ny,nt,lrestart,au,h0,dt,f0,&
     & dx,dy,gp,ab,fu,fv,taux,tauy,h,dh,u,du,v,dv,crun)

  !INTITIALISE MODEL FIELDS FOR THE SHALLOW WATER MODEL SETUP


  implicit none

  INTEGER :: nx,ny,nt
  REAL*8 :: h(0:nx,0:ny),u(0:nx,0:ny),v(0:nx,0:ny)
  REAL*8 :: taux(0:ny),tauy(0:nx)
  REAL*8 :: dh(0:nx,0:ny,0:nt),du(0:nx,0:ny,0:nt),dv(0:nx,0:ny,0:nt)
  REAL*8 :: ab(nt)
  REAL*8 :: h0
  integer i,j,i2,j2,k,l,itest
  character*5 crun
  REAL*8 :: fu(0:ny),fv(0:ny)
  REAL*8 :: vec(2+nt)
  REAL*8 :: gp,pi,f0,beta,au,x0,y0,dx,dy,dt
  logical :: lrestart
  REAL*8 :: field(0:nx,0:ny),dfield(0:nx,0:ny,nt)

  pi=3.14159265358979
  x0=3480000.0
  y0=3480000.0
  au=470.23
  h0=500.
  dt=25.0
  f0=4.46e-5
  beta=2.e-11
  dx=x0/real(nx-1)
  dy=y0/real(ny-1)
  gp=9.81

! Adams-Bashforth parameters
  ab(1)=(23./12.)*dt
  ab(2)=-(16./12.)*dt
  ab(3)=(5./12.)*dt

! Define Coriolis parameter and initial u,v,h,zeta

  do j=0,ny
     fu(j)=f0+beta*y0*(real(j)-0.5)/real(ny-1)
  end do
  do j=0,ny
     fv(j)=f0+beta*y0*real(j-1)/real(ny-1)
  end do

! define the wind forcing:
  do i=0,ny-1
     taux(i) = 0.12*(cos(2.0*pi*((real(i)-0.5)*y0/real(ny-1)-0.5*y0)/y0)&
          & +2.0*sin(pi*((real(i)-0.5)*y0/real(ny-1)-0.5*y0)/y0))/(999.8*h0)
  end do

  do i=0,nx
     tauy(i)= 0.0
  end do

  !Is this a restart simulation?
  IF(.not.lrestart)THEN
     do j=0,ny
        do i=0,nx
           h(i,j)=0.0
           dh(i,j,1)=0.
           dh(i,j,2)=0.
        end do
     end do
     do j=0,ny
        do i=0,nx
           u(i,j)=0.
           du(i,j,1)=0.
           du(i,j,2)=0.
        end do
     end do
     do j=0,ny
        do i=0,nx
           v(i,j)=0.
           dv(i,j,1)=0.
           dv(i,j,2)=0.
        end do
     end do
  ELSE
     open(12,file='./initial/u.dat.'//TRIM(crun), STATUS='OLD', ACTION='read')
     open(13,file='./initial/v.dat.'//TRIM(crun), STATUS='OLD', ACTION='read')
     open(14,file='./initial/h.dat.'//TRIM(crun), STATUS='OLD', ACTION='read')

     do j=1,ny-1
        do i=1,nx-1
           read(14,*) vec
           h(i,j) = vec(3)
           dh(i,j,1:(nt-1)) = vec(4:)
        end do
     end do

     do j=1,ny-1
        do i=1,nx
           read(12,*) vec
           u(i,j) = vec(3)
           du(i,j,1:(nt-1)) = vec(4:)
        end do
     end do
     do j=1,ny
        do i=1,nx-1
           read(13,*) vec
           v(i,j) = vec(3)
           dv(i,j,1:(nt-1)) = vec(4:)
        end do
     end do

     CLOSE(12)
     CLOSE(13)
     CLOSE(14)
  END IF

end subroutine initialise
end module dynamics
