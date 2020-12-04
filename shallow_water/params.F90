module params
    implicit none

    public

    logical, parameter :: lrestart = .false.

    integer, parameter :: nx = 101
    integer, parameter :: ny = 101
    integer, parameter :: nt = 3

    integer, parameter :: nstop = 20000 !number of timesteps
    integer, parameter :: nwrite = 100 !Sets frequency of output

    real(8), parameter :: pi = 3.14159265358979
    real(8), parameter :: x0 = 3480000.0
    real(8), parameter :: y0 = 3480000.0
    real(8), parameter :: au = 470.23
    real(8), parameter :: h0 = 500.0
    real(8), parameter :: dt = 25.0
    real(8), parameter :: f0 = 4.46e-5
    real(8), parameter :: beta = 2.e-11
    real(8), parameter :: gp = 9.81
    real(8), parameter :: dx = x0/real(nx - 1)
    real(8), parameter :: dy = y0/real(ny - 1)
    real(8), parameter :: rdx = 1.0_8/dx
    real(8), parameter :: rdy = 1.0_8/dy
    real(8), parameter :: ab(nt) = dt*(/ 23.0/12.0, -16.0/12.0, 5.0/12.0 /)
    real(8), parameter :: slip = 1._8
end module params
