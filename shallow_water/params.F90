module params
    implicit none

    public

    ! Define precision for all real variables (defined by CPP macro)
    integer, parameter :: p = PREC

    logical, parameter :: lrestart = .false.

    integer, parameter :: nx = 101
    integer, parameter :: ny = 101
    integer, parameter :: nt = 3

    integer, parameter :: nstop = 2000 !number of timesteps
    integer, parameter :: nwrite = 100 !Sets frequency of output

    real(p), parameter :: pi = 3.14159265358979_p
    real(p), parameter :: x0 = 3480000.0_p
    real(p), parameter :: y0 = 3480000.0_p
    real(p), parameter :: au = 470.23_p
    real(p), parameter :: h0 = 500.0_p
    real(p), parameter :: dt = 25.0_p
    real(p), parameter :: f0 = 4.46e-5_p
    real(p), parameter :: beta = 2.0e-11_p
    real(p), parameter :: gp = 9.81_p
    real(p), parameter :: dx = x0/real(nx - 1,p)
    real(p), parameter :: dy = y0/real(ny - 1,p)
    real(p), parameter :: rdx = 1.0_p/dx
    real(p), parameter :: rdy = 1.0_p/dy
    real(p), parameter :: ab(nt) = dt*(/ 23.0_p/12.0_p, -16.0_p/12.0_p, &
        & 5.0_p/12.0_p /)
    real(p), parameter :: slip = 1.0_p
end module params
