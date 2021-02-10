!===============================================================================
! Dynamics module for shallow water model, containing timestepping etc.
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

module dynamics
    use params

    implicit none

    private
    public initialise, rhs, timeupdate

contains
    !> Calculates right-hand-side of shallow water equations.
    !> @param[in] n the time step
    !> @param[in] h the height
    !> @param[in] u the eastward current
    !> @param[in] v the northward current
    !> @param[in] taux the eastward wind stress
    !> @param[in] tauy the northward wind stress
    !> @param[in] fu the Coriolis parameter for U gridpoints
    !> @param[in] fv the Coriolis parameter for V gridpoints
    !> @param[inout] dh the Adams-Bashforth time increment array for height
    !> @param[inout] du the Adams-Bashforth time increment array for eastward
    !>                  current
    !> @param[inout] dv the Adams-Bashforth time increment array for northward
    !>                  current
    subroutine rhs(n, h, u, v, taux, tauy, fu, fv, dh, du, dv)
        integer, intent(in) :: n
        real(p), intent(in) :: h(0:nx,0:ny)
        real(p), intent(in) :: u(0:nx,0:ny)
        real(p), intent(in) :: v(0:nx,0:ny)
        real(p), intent(in) :: taux(0:ny)
        real(p), intent(in) :: tauy(0:nx)
        real(p), intent(in) :: fu(0:ny)
        real(p), intent(in) :: fv(0:ny)
        real(p), intent(inout) :: dh(0:nx,0:ny,0:nt)
        real(p), intent(inout) :: du(0:nx,0:ny,0:nt)
        real(p), intent(inout) :: dv(0:nx,0:ny,0:nt)

        integer :: i, j
        real(p) :: b(0:nx,0:ny)
        real(p) :: zeta(0:nx,0:ny)
        real(p) :: r0, r1, r2, r3

        r0 = 0.125_p
        r1 = 2.0_p
        r2 = 0.25_p
        r3 = 0.5_p

        ! Calculate Bernoulli potential
        ! 1/g*h+0.5(u^2+v^2)
        do j = 0, ny-1
            do i = 0, nx-1
                b(i,j) = gp*h(i,j) + r0*( &
                    & (u(i,j) + u(i+1,j))*(u(i,j) + u(i+1,j)) + &
                    & (v(i,j) + v(i,j+1))*(v(i,j) + v(i,j+1)))
            end do
        end do

        ! Calculate relative vorticity
        ! d_x v - d_y u
        do j = 1, ny
            do i = 1, nx
                zeta(i,j) = (v(i,j)-v(i-1,j))*rdx - (u(i,j) - u(i,j-1))*rdy
            end do
        end do

        ! Calculate new time increments for prognostic variables
        do j = 1, ny-1
            do i = 2, nx-1
                ! Shunt Adams-Bashforth dimension along one
                du(i,j,3) = du(i,j,2)
                du(i,j,2) = du(i,j,1)

                ! Calculate new Adams-Bashforth term
                du(i,j,1) = au*(u(i+1,j) + u(i-1,j) - r1*u(i,j))*rdx*rdx + &
                    & au*(u(i,j+1) + u(i,j-1) - r1*u(i,j))*rdy*rdy + &
                    & r2*(fu(j) + r3*(zeta(i,j) + zeta(i,j+1)))* &
                    & (v(i-1,j) + v(i,j) + v(i-1,j+1) + v(i,j+1)) - &
                    & (b(i,j) - b(i-1,j))*rdx + taux(j)
            end do
        end do

        do j = 2, ny-1
            do i = 1, nx-1
                ! Shunt Adams-Bashforth dimension along one
                dv(i,j,3) = dv(i,j,2)
                dv(i,j,2) = dv(i,j,1)

                ! Calculate new Adams-Bashforth term
                dv(i,j,1) = au*(v(i+1,j) + v(i-1,j) - r1*v(i,j))*rdx*rdx + &
                    & au*(v(i,j+1) + v(i,j-1) - r1*v(i,j))*rdy*rdy - &
                    & r2*(fv(j)+r3*(zeta(i,j)+zeta(i+1,j)))* &
                    & (u(i,j-1)+u(i,j)+u(i+1,j-1)+u(i+1,j)) - &
                    & (b(i,j)-b(i,j-1))*rdy + tauy(i)
            end do
        end do

        do j = 1, ny-1
            do i = 1, nx-1
                ! Shunt Adams-Bashforth dimension along one
                dh(i,j,3) = dh(i,j,2)
                dh(i,j,2) = dh(i,j,1)

                ! Calculate new Adams-Bashforth term
                dh(i,j,1) = (h0 + r3*(h(i-1,j) + h(i,j)))*u(i,j)*rdx + &
                    & (h0 + r3*(h(i,j-1) + h(i,j)))*v(i,j)*rdy - &
                    & (h0 + r3*(h(i+1,j) + h(i,j)))*u(i+1,j)*rdx - &
                    & (h0 + r3*(h(i,j+1) + h(i,j)))*v(i,j+1)*rdy
            end do
        end do

        ! Calculate contribution to prognostic variables for this timestep
        do j = 1, ny-1
            do i = 2,nx-1
                if (n < 3 .and. (.not. lrestart)) then
                    du(i,j,0) = du(i,j,1)*dt
                else
                    du(i,j,0) = ab(1)*du(i,j,1) + ab(2)*du(i,j,2) + &
                        & ab(3)*du(i,j,3)
                end if
            end do
        end do
        do j = 2, ny-1
            do i = 1, nx-1
                if (n < 3 .and. (.not.lrestart)) then
                    dv(i,j,0) = dv(i,j,1)*dt
                else
                    dv(i,j,0) = ab(1)*dv(i,j,1) + ab(2)*dv(i,j,2) + &
                        & ab(3)*dv(i,j,3)
                end if
            end do
        end do

        do j = 1, ny-1
            do i = 1, nx-1
                if (n < 3 .and. (.not. lrestart)) then
                    dh(i,j,0) = dh(i,j,1)*dt
                else
                    dh(i,j,0) = ab(1)*dh(i,j,1) + ab(2)*dh(i,j,2) + &
                        & ab(3)*dh(i,j,3)
                end if
            end do
        end do
    end subroutine rhs

    !> Update prognostic variables.
    !> @param[in] n the time step
    !> @param[in] dh the Adams-Bashforth time increment array for height
    !> @param[in] du the Adams-Bashforth time increment array for eastward
    !>               current
    !> @param[in] dv the Adams-Bashforth time increment array for northward
    !>               current
    !> @param[inout] h the height
    !> @param[inout] u the eastward current
    !> @param[inout] v the northward current
    subroutine timeupdate(dh, du, dv, h, u, v)
        real(p), intent(in) :: dh(0:nx,0:ny,0:nt)
        real(p), intent(in) :: du(0:nx,0:ny,0:nt)
        real(p), intent(in) :: dv(0:nx,0:ny,0:nt)
        real(p), intent(inout) :: h(0:nx,0:ny)
        real(p), intent(inout) :: u(0:nx,0:ny)
        real(p), intent(inout) :: v(0:nx,0:ny)

        real(p) :: r1 = 2.0_p, r4 = 1.0_p
        integer :: i, j

        do j = 1, ny-1
            do i = 2, nx-1
                u(i,j) = u(i,j) + du(i,j,0)
            end do
        end do
        do j = 2, ny-1
            do i = 1, nx-1
                v(i,j) = v(i,j) + dv(i,j,0)
            end do
        end do

        do j = 1, ny-1
            do i = 1, nx-1
                h(i,j) = h(i,j) + dh(i,j,0)
            end do
        end do

        ! Fix boundary conditions
        do j = 1, ny-1
            v(0,j) = (r4-r1*slip)*v(1,j)
            v(nx,j) = (r4-r1*slip)*v(nx-1,j)
            h(0,j) = h(1,j)
            h(nx,j) = h(nx-1,j)
        end do
        do i = 1, nx-1
            u(i,0) = (r4-r1*slip)*u(i,1)
            u(i,ny) = (r4-r1*slip)*u(i,ny-1)
            h(i,0) = h(i,1)
            h(i,ny) = h(i,ny-1)
        end do
    end subroutine timeupdate

    !> Initialise model fields.
    !> @param[inout] fu the Coriolis parameter for U gridpoints
    !> @param[inout] fv the Coriolis parameter for V gridpoints
    !> @param[inout] taux the eastward wind stress
    !> @param[inout] tauy the northward wind stress
    !> @param[inout] h the height
    !> @param[inout] dh the Adams-Bashforth time increment array for height
    !> @param[inout] u the eastward current
    !> @param[inout] du the Adams-Bashforth time increment array for eastward
    !>                  current
    !> @param[inout] v the northward current
    !> @param[inout] dv the Adams-Bashforth time increment array for northward
    !>                  current
    subroutine initialise(fu, fv, taux, tauy, h, dh, u, du, v, dv)
        use io, only: read_restart

        real(p), intent(inout) :: fu(0:ny)
        real(p), intent(inout) :: fv(0:ny)
        real(p), intent(inout) :: taux(0:ny)
        real(p), intent(inout) :: tauy(0:nx)
        real(p), intent(inout) :: h(0:nx,0:ny)
        real(p), intent(inout) :: dh(0:nx,0:ny,0:nt)
        real(p), intent(inout) :: u(0:nx,0:ny)
        real(p), intent(inout) :: du(0:nx,0:ny,0:nt)
        real(p), intent(inout) :: v(0:nx,0:ny)
        real(p), intent(inout) :: dv(0:nx,0:ny,0:nt)

        integer :: i, j

        if (nstop > 99999999) then
            write (*,*) "Integration length longer than 99999999 time steps"
            write (*,*) "Output filenames will not be formatted correctly"
            stop
        end if

        ! Define Coriolis parameter for U grid
        do j = 0, ny
            fu(j) = f0 + beta*y0*(real(j,p) - 0.5_p)/real(ny - 1,p)
        end do

        ! Define Coriolis parameter for V grid
        do j = 0, ny
            fv(j) = f0 + beta*y0*real(j - 1,p)/real(ny - 1,p)
        end do

        ! Define the wind forcing:
        do i = 0, ny - 1
            taux(i) = 0.12_dp*( &
                & cos(2.0_dp*pi*((real(i,dp) - 0.5_dp) / real(ny-1,dp) - 0.5_dp)) + &
                & 2.0_dp*sin(pi*((real(i,dp) - 0.5_dp) / real(ny-1,dp) - 0.5_dp)) &
                )/(999.8_dp*h0)
        end do

        do i = 0, nx
            tauy(i) = 0.0
        end do

        ! Initialise fields from rest or restart file
        IF (.not. lrestart) THEN
            do j = 0, ny
                do i = 0, nx
                    h(i,j) = 0.0_p
                    dh(i,j,1) = 0.0_p
                    dh(i,j,2) = 0.0_p
                end do
            end do
            do j =0, ny
                do i = 0, nx
                    u(i,j) = 0.0_p
                    du(i,j,1) = 0.0_p
                    du(i,j,2) = 0.0_p
                end do
            end do
            do j = 0, ny
                do i = 0, nx
                    v(i,j) = 0.0_p
                    dv(i,j,1) = 0.0_p
                    dv(i,j,2) = 0.0_p
                end do
            end do
        else
            call read_restart(h, dh, 'h')
            call read_restart(u, du, 'u')
            call read_restart(v, dv, 'v')
        end if
    end subroutine initialise
end module dynamics

