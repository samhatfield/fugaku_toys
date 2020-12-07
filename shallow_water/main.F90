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
    use params, only: p, nx, ny, nt, nstop

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

    ! Initialise model fields
    CALL initialise(fu, fv, taux, tauy, h, dh, u, du, v, dv)

    ! Main timestepping loop
    do n = 1, nstop
        if (mod(n, 1000) == 0) then
            write (*,*) "Timestep", n
        end if

        ! Calculate right-hand-side of equations
        CALL rhs(n, h, u, v, taux, tauy, fu, fv, dh, du, dv)

        ! Update prognostic variables
        CALL timeupdate(n, dh, du, dv, h, u, v)
    end do
end program main
