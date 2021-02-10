!===============================================================================
! I/O module for shallow water model
!
! ECMWF/RIKEN-CCS
!
! Contributors:
! Sam Hatfield, ECMWF
!
! samuel.hatfield@ecmwf.int
!===============================================================================

module io
    use params, only: dp, p, nx, ny, nt, dx, dy

    implicit none

    private
    public write_field, write_restart, read_restart

contains
    !> Write snapshot of model field to file.
    !> @param[in] field the field to write
    !> @param[in] field_name the name of the field, for the file name
    !> @param[in] timestep the timestep, for the file name
    subroutine write_field(field, field_name, timestep)
        real(p), intent(in) :: field(0:nx,0:ny)
        character, intent(in) :: field_name
        integer, intent(in) :: timestep

        character(len=1024) :: filename
        integer :: j

        ! Define output file name
        write (filename, "(A9,A1,A1,I8.8,A4)") './output/', field_name, '.', &
            & timestep, '.txt'

        ! Write field to file
        open(unit=9, file=filename, status='unknown')
        do j = 1, ny-1
            write(9,*) real(field(:,j),dp)
        end do
        close(9)
    end subroutine write_field

    !> Write restart of model field to file.
    !> @param[in] field the field to write
    !> @param[in] dfield the field to write
    !> @param[in] field_name the name of the field, for the file name
    !> @param[in] timestep the timestep, for the file name
    subroutine write_restart(field, dfield, field_name, uppery, upperx)

        real(p), intent(in) :: field(0:nx,0:ny)
        real(p), intent(in) :: dfield(0:nx,0:ny,0:nt)
        character, intent(in) :: field_name
        integer, intent(in) :: uppery
        integer, intent(in) :: upperx

        character(len=1024) :: filename
        integer :: i, j

        ! Define output file name
        write (filename, "(A1,A16)") field_name, '.restart.out.txt'

        ! Write field to file
        open(unit=9, file=filename, status='replace')
        do j = 1, uppery
            do i = 1, upperx
                write(9,*) real((i-1)*dx,dp), real((j-1)*dy,dp), &
                     & real(field(i,j),dp), real(dfield(i,j,1),dp), real(dfield(i,j,2),dp)
            end do
        end do
        close(9)
    end subroutine write_restart

    !> Write restart of model field to file.
    !> @param[in] field the field to write
    !> @param[in] dfield the field to write
    !> @param[in] field_name the name of the field, for the file name
    !> @param[in] timestep the timestep, for the file name
    subroutine read_restart(field, dfield, field_name, uppery, upperx)
        real(p), intent(out) :: field(0:nx,0:ny)
        real(p), intent(out) :: dfield(0:nx,0:ny,0:nt)
        character, intent(in) :: field_name
        integer, intent(in) :: uppery
        integer, intent(in) :: upperx

        character(len=1024) :: filename
        integer :: i, j
        real(dp) :: invar(2+nt)

        ! Define output file name
        write (filename, "(A1,A15)") field_name, '.restart.in.txt'

        ! Read field from file
        open(unit=9, file=filename, status='old', action='read')
        do j = 1, uppery
            do i = 1, upperx
                read(9,*) invar
                field(i,j) = invar(3)
                dfield(i,j,1:(nt-1)) = invar(4:)
            end do
        end do
        close(9)
    end subroutine read_restart
end module io

