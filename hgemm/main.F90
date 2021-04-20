!===============================================================================
! Program for testing support for half-precision matrix-matrix multiplication
! (HGEMM).
!
! ECMWF/RIKEN-CCS
!
! Contributors:
! Sam Hatfield, ECMWF
!
! samuel.hatfield@ecmwf.int
!===============================================================================

program main
    implicit none

    integer, parameter :: hp = 2
    integer, parameter :: dp = 8

    integer, parameter :: n_repeat = 100

    character :: transa = "N"
    character :: transb = "N"
    integer, parameter :: m = PROBLEM_SIZE
    integer, parameter :: n = PROBLEM_SIZE
    integer, parameter :: k = PROBLEM_SIZE
    integer, parameter :: lda = m
    integer, parameter :: ldb = k
    integer, parameter :: ldc = m
    real(dp), parameter :: alpha_dp = 1.0
    real(dp), parameter :: beta_dp = 0.0
    real(dp) :: a_dp(lda,k)
    real(dp) :: b_dp(ldb,n)
    real(dp) :: c_dp(ldc,n)
    real(hp), parameter :: alpha_hp = alpha_dp
    real(hp), parameter :: beta_hp = beta_dp
    real(hp) :: a_hp(lda,k)
    real(hp) :: b_hp(ldb,n)
    real(hp) :: c_hp(ldc,n)

    integer :: i, j
    real :: temp
    logical :: write_values = .false.

    ! Timing variables
    integer :: tic, toc, t_rate

    ! Initialise double- and half-precision matrices
    do i = 1, lda
        do j = 1, k
            call random_number(temp)
            a_dp(i,j) = temp
            a_hp(i,j) = temp
        end do
    end do
    do i = 1, ldb
        do j = 1, n
            call random_number(temp)
            b_dp(i,j) = temp
            b_hp(i,j) = temp
        end do
    end do

    write (*,*) "PROBLEM_SIZE", PROBLEM_SIZE

    if (write_values) then
        write (*,*) "Double-precision matrices"
        write (*,*) "A"
        do i = 1, lda
            write (*,*) a_dp(i,:)
        end do
        write (*,*) "B"
        do i = 1, lda
            write (*,*) b_dp(i,:)
        end do
    
        write (*,*) ""
        write (*,*) "Half-precision matrices"
        write (*,*) "A"
        do i = 1, lda
            write (*,*) real(a_hp(i,:),dp)
        end do
        write (*,*) "B"
        do i = 1, lda
            write (*,*) real(b_hp(i,:),dp)
        end do
    end if

    ! Benchmark double-precision
    call system_clock(tic)
    do i = 1, n_repeat
        call dgemm(transa, transb, m, n, k, alpha_dp, a_dp, lda, b_dp, ldb, beta_dp, c_dp, ldc)
    end do
    call system_clock(toc, t_rate)

    write (*,*) "Double-precision"
    write (*,*) (toc - tic) / real(t_rate,dp)

    ! Benchmark half-precision
    call system_clock(tic)
    do i = 1, n_repeat
        call fjblas_gemm_r16(transa, transb, m, n, k, alpha_hp, a_hp, lda, b_hp, ldb, beta_hp, c_hp, ldc)
    end do
    call system_clock(toc, t_rate)

    write (*,*) "Half-precision"
    write (*,*) (toc - tic) / real(t_rate,dp)

    if (write_values) then
        write (*,*) ""
        write (*,*) "Double-precision C"
        do i = 1, ldc
            write (*,*) c_dp(i,:)
        end do
        write (*,*) ""
        write (*,*) "Half-precision C"
        do i = 1, ldc
            write (*,*) real(c_hp(i,:),dp)
        end do
    end if
end program main

