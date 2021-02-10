!===============================================================================
! Shallow water model used for investigating half-precision support on Fugaku
!
! ECMWF/RIKEN-CCS
!
! Contributors:
! Sam Hatfield, ECMWF
! Peter Dueben, ECMWF
! Dave Marshall, University of Oxford
!
! samuel.hatfield@ecmwf.int
!===============================================================================

program main
    use dynamics, only: initialise, rhs, timeupdate
    use params, only: dp, p, nx, ny, nt, nstop, nwrite
    use io, only: write_field, write_restart

    implicit none

    ! Define prognostic fields (height, eastward current, northward current)
    real(p) :: h(0:nx,0:ny), u(0:nx,0:ny), v(0:nx,0:ny)

    ! Define wind stress fields
    real(p) :: taux(0:ny), tauy(0:nx)

    ! Define time increments for Adams-Bashforth timestepping scheme
    real(p) :: dh(0:nx,0:ny,0:nt), du(0:nx,0:ny,0:nt), dv(0:nx,0:ny,0:nt)

    ! Define Coriolis parameters for U and V gridpoints, respectively
    real(p) :: fu(0:ny), fv(0:ny)

    ! Timestep loop index
    integer :: n

    ! Timing variables
    integer :: tic, toc, t_rate

    ! Print working precision
    if (PREC == 8) then
        write (*,*) "Using double-precision"
    else if (PREC == 4) then
        write (*,*) "Using single-precision"
    else
        write (*,*) "Using half-precision"
    end if

    ! Initialise model fields
    call initialise(fu, fv, taux, tauy, h, dh, u, du, v, dv)

    ! Main timestepping loop
    call system_clock(tic)
    do n = 1, nstop
        if (mod(n, 1000) == 0) then
            write (*,*) "Timestep", n
        end if

        ! Calculate right-hand-side of equations
        call rhs(n, h, u, v, taux, tauy, fu, fv, dh, du, dv)

        ! Update prognostic variables
        call timeupdate(dh, du, dv, h, u, v)

        ! Write output
        if (mod(n, nwrite) == 0) then
            call write_field(h, 'h', n)
            call write_field(u, 'u', n)
            call write_field(v, 'v', n)
        endif
    end do
    call system_clock(toc, t_rate)
    write (*,*) "Main loop took", (toc - tic) / real(t_rate,dp), "seconds"

    ! Write restart files
    call write_restart(h, dh, 'h', ny-1, nx-1)
    call write_restart(u, du, 'u', ny-1, nx)
    call write_restart(v, dv, 'v', ny, nx-1)
end program main

