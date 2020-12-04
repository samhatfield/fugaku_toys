program cgrid_shallow_water
    use dynamics, only: initialise, rhs, timeupdate
    use params, only: nx, ny, nt, nstop, nwrite

    ! Written by Peter Dueben (2014) but based on Fortran 77 code by David Marshall
    implicit none

    ! PROGNOSTIC FIELDS:
    real(8) :: h(0:nx,0:ny), u(0:nx,0:ny), v(0:nx,0:ny)
    !WIND FORCING:
    real(8) :: taux(0:ny), tauy(0:nx)
    !TIME INCREMENTS FOR ADAMS-BASHFORTH TIMESTEPPING SCHEME:
    real(8) :: dh(0:nx,0:ny,0:nt), du(0:nx,0:ny,0:nt) ,dv(0:nx,0:ny,0:nt)
    ! CORIOLIS PARAMETER AT U AND V GRID-POINTS RESPECTIVELY
    real(8) :: fu(0:ny), fv(0:ny)

    real(8) :: slip, g, rho0

    integer :: j, k, n

    ! INITIALISE MODEL FIELDS
    CALL initialise(fu, fv, taux, tauy, h, dh, u, du, v, dv)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !    MAIN LOOP STARTS HERE
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    do n = 1, nstop
        !CALCULATE RHS OF EQUATIONS
        CALL rhs(n, u, du, v, dv, h, dh, taux, &
            & tauy, fu, fv)

        !UPDATE PROGNOSTIC QUANTITIES
        CALL timeupdate(n, u, du, v, dv, h, dh)
    end do
end program cgrid_shallow_water
