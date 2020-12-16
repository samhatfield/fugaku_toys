!===============================================================================
! Program for verifying the Reduced Precision Emulator half-precision type
! against Fugaku's native FP16 type
!
! ECMWF/RIKEN-CCS
!
! Contributors:
! Sam Hatfield, ECMWF
! samuel.hatfield@ecmwf.int
!===============================================================================

program main
    use rp_emulator

    implicit none

    ! Define double-precision and half-precision kind parameters
    integer, parameter :: dp = selected_real_kind(13, 300)
    integer, parameter :: hp = 2

    ! Define lower and upper limits of numbers considered
    real(dp) :: lower = -24.0_dp, upper = 17.0_dp

    ! Define number of numbers to generated
    integer, parameter :: n_samples = 1024

    ! Define loop index
    integer :: i

    ! Define container variables for native double-precision, emulated half
    ! precision and native Fugaku half-precision
    real(dp) :: val_dp
    type(rpe_var) :: val_rpe
    real(hp) :: val_fug

    ! Set up rpe to use half-precision
    RPE_ACTIVE = .true.
    RPE_IEEE_HALF = .true.
    RPE_DEFAULT_SBITS = 10

    ! Loop over total number of samples, creating a double-precision variable,
    ! storing this in the rpe and Fugaku half-precision variables, and then
    ! outputting all numbers to file
    open(1, file="output.txt")
    do i = 0, n_samples-1
        val_dp = 2.0_dp**(lower + real(i,dp)*(upper - lower)/real(n_samples,dp))
        val_rpe = val_dp
        val_fug = val_dp
        write(1, '(3d15.7)') val_dp, val_rpe%val, real(val_fug,dp)
    end do
    close(1)
end program main
