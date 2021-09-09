! A simple benchmark for FFTW.
!
! Tests real-to-complex transform. Uses single-precision.
!
! Based on https://gist.github.com/appleparan/c048c44668ede7ef28ba63c660b6dcf3.
program main
    use ISO_C_BINDING
    implicit none

    include 'fftw3.f03.h'

    ! Size of FFTW input/output array
    integer, parameter :: n = 10000

    ! Number of times to repeat FFT cycle
    integer, parameter :: n_repeats = 100000

    ! Storage array for input/output
    real(4) :: test_in(n+2)

    ! FFTW plan variables
    integer(8) :: plan_c2r, plan_r2c

    ! Timing variables
    integer :: tic, toc, t_rate

    ! Loop indices
    integer :: i, j

    ! Create FFTW plans for direct and inverse transforms
    call sfftw_plan_dft_r2c_1d(plan_r2c, n, test_in, test_in, FFTW_MEASURE)
    call sfftw_plan_dft_c2r_1d(plan_c2r, n, test_in, test_in, FFTW_MEASURE)

    ! Initialize input array with random numbers
    do i = 1, n
        call random_number(test_in(i))
    end do

    ! Print slice of input
    print *, "IN(1:5) before: "
    write(*, *) (test_in(i),i = 1,5)
    write(*, *) ""

    ! Run n_repeats direct/inverse FFT cycles
    call system_clock(tic)
    do i = 1, n_repeats
        call sfftw_execute_dft_r2c(plan_r2c, test_in, test_in)
    
        call sfftw_execute_dft_c2r(plan_c2r, test_in, test_in)

        test_in = test_in / n
    end do
    call system_clock(toc, t_rate)

    ! Print slice of output
    print *, "IN(1:5) after: "
    write(*, *) (test_in(i),i = 1,5)
    write(*, *) ""

    ! Print wallclock time
    write (*,*) "Took", (toc - tic) / real(t_rate,8), "seconds"
    write(*, *) ""

    ! Destroy both plans
    call sfftw_destroy_plan(plan_r2c)
    call sfftw_destroy_plan(plan_c2r)
end program
