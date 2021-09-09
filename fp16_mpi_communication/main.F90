!===============================================================================
! Program for testing support for half-precision MPI communication
!
! ECMWF/RIKEN-CCS
!
! Contributors:
! Sam Hatfield, ECMWF
!
! samuel.hatfield@ecmwf.int
!===============================================================================

program main
    use mpi

    implicit none

    ! MPI variables
    integer :: world_rank, world_size, error

    ! Number to communicate
    real(2) :: num

    call mpi_init(error)

    call mpi_comm_rank(mpi_comm_world, world_rank, error)
    call mpi_comm_size(mpi_comm_world, world_size, error)

    if (world_size < 2) then
        print "(A)", "World size must be greater than 1"
        call mpi_abort(mpi_comm_world, 1, error)
    end if

    ! If rank #0, send
    if (world_rank == 0) then
        num = 3.141592653589793_8
        print "(A,F10.7,A)", "Process 0 sending", real(num,4), " to process 1"
        call mpi_send(num, 1, mpi_real2, 1, 0, mpi_comm_world, error)
    ! Else if rank #0, receive
    else if (world_rank == 1) then
        call mpi_recv(num, 1, mpi_real2, 0, 0, mpi_comm_world, mpi_status_ignore, error)
        print "(A,F10.7,A)", "Process 1 received", real(num,4), " from process 0"
    end if

    call mpi_finalize(error)
end program main

