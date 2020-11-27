!===============================================================================
! Lorenz '63 model used for investigating half-precision support on Fugaku
!
! ECMWF/RIKEN-CCS
!
! Contributors:
! Sam Hatfield, ECMWF
! samuel.hatfield@ecmwf.int
!===============================================================================

program main
    implicit none

    ! Define precision for all real variables (defined by CPP macro)
    integer, parameter :: p = PREC

    ! Model parameters
    real(p), parameter :: a = 10.0_p
    real(p), parameter :: b = 8.0_p/3.0_p
    real(p), parameter :: r = 28.0_p

    ! Time step
    real(p), parameter :: dt = 0.01_p

    ! Number of time steps for model spin-up
    integer, parameter :: n_spin = 1000

    ! Number of time steps for actual model trajectory
    integer, parameter :: n_steps = 100

    ! Stores model trajectory
    real(p) :: trajectory(n_steps,3)

    ! Run model with arbitrary initial condition (includes model spin-up)
    trajectory = run_model(n_steps, (/ 1.0_p, 1.0_p, 1.0_p /))

    ! Output trajectory to plaintext file
    call output(trajectory, "output.txt")

contains

    !> Run model using modified Euler scheme.
    !> @param[in] n_steps the length of the integration
    !> @param[in] in the initial condition
    !> @return out the computed trajectory
    function run_model(n_steps, init) result(traj)
        integer, intent(in) :: n_steps
        real(p), intent(in) :: init(3)
        real(p), dimension(n_steps,3) :: traj
        real(p), dimension(3) :: k1, k2
        integer :: i

        ! Set first time step for spin-up
        traj(1,:) = init

        ! Spin up model, discarding intermediate values
        do i = 1, n_spin
            k1 = dt*dRdT(traj(1,:))
            k2 = dt*dRdT(traj(1,:) + k1)
    
            traj(1,:) = traj(1,:) + 0.5_p * (k1 + k2)
        end do

        ! Perform actual model trajectory
        do i = 1, n_steps-1
            k1 = dt*dRdT(traj(i,:))
            k2 = dt*dRdT(traj(i,:) + k1)
    
            traj(i+1,:) = traj(i,:) + 0.5_p * (k1 + k2)
        end do
    end function run_model

    !> The full nonlinear ODE.
    !> @param[in] state the state vector at which to evaluate the ODE
    !> @return dRdT the evaluated ODE
    function dRdT(state)
        real(p), intent(in) :: state(3)
        real(p) :: dRdT(3), x, y, z

        ! Extract vector elements
        x = state(1); y = state(2); z = state(3)

        dRdT = (/ a*(y - x), r*x - y - x*z, x*y - b*z /)
    end function dRdT

    !> Outputs the given 2D array to a plaintext file with one row of values per
    !> time step.
    !> @param[in] time_axis an array of time values corresponding to each row
    !> of the output array
    !> @param[in] output_array the array containing the data to output
    !> to be output
    subroutine output(output_array, filename)
        real(p), intent(in) :: output_array(:,:)
        character(len=*), intent(in) :: filename
        integer :: i

        open(1, file=filename)
        do i = 1, size(output_array, 1)
            write (1,*) output_array(i,:)
        end do
        close(1)
    end subroutine output
end program main

