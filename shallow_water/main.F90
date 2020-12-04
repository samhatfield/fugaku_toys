program cgrid_shallow_water
  !Written by Peter Dueben (2014) but based on Fortran 77 code by David Marshall
  implicit none

  integer nx,ny,nt
  parameter(nx=101,ny=101,nt=3)

  !PROGNOSTIC FIELDS:
  REAL*8 :: h(0:nx,0:ny),u(0:nx,0:ny),v(0:nx,0:ny)
  !WIND FORCING:
  REAL*8 :: taux(0:ny),tauy(0:nx)
  !TIME INCREMENTS FOR ADAMS-BASHFORTH TIMESTEPPING SCHEME:
  REAL*8 :: dh(0:nx,0:ny,0:nt),du(0:nx,0:ny,0:nt),dv(0:nx,0:ny,0:nt)
  !COEFFICIENTS OF ADAMS-BASHFORTH TIMESTEPPING SCHEME:
  REAL*8 :: ab(nt)
  ! CORIOLIS PARAMETER AT U AND V GRID-POINTS RESPECTIVELY
  REAL*8 :: fu(0:ny),fv(0:ny)

  REAL*8 :: gp,f0,au,dx,dy,rdx,rdy,dt
  REAL*8 :: slip,g,rho0,h0

  integer :: j,k,n,nstop
  integer :: ndump,nwrite
  character*5 :: num, crun
  logical :: lrestart

! MODEL PARAMETER INITIALISATION
  nstop=  20000 !number of timesteps
  nwrite=   100 !Sets frequency of output
  ndump=0
! CHOOSE BOUNDARY CONDITIONS: free-slip (0.) or no-slip (1.)?
  slip=1._8
  lrestart = .FALSE.

! INITIALISE MODEL FIELDS
  CALL initialise(nx,ny,nt,lrestart,au,h0,dt,f0,&
       & dx,dy,gp,ab,fu,fv,taux,tauy,h,dh,u,du,v,dv,crun)

  rdx = 1.0_8/dx
  rdy = 1.0_8/dy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    MAIN LOOP STARTS HERE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do n=1,nstop
     !CALCULATE RHS OF EQUATIONS
     CALL rhs(n,nx,ny,nt,u,du,v,dv,h,dh,gp,rdx,rdy,au,taux,tauy,fu,fv,&
          & lrestart,ab,dt,h0)
     !UPDATE PROGNOSTIC QUANTITIES
     CALL timeupdate(n,nx,ny,nt,u,du,v,dv,h,dh,slip,ndump,num,nwrite,dx,dy)
  end do

end program cgrid_shallow_water
