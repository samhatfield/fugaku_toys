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
    implicit none

    private
    public write_field

contains
    !> Write snapshot of model field to file.
    !> @param[in] field the field to write
    !> @param[in] field_name the name of the field, for the file name
    !> @param[in] timestep the timestep, for the file name
    subroutine write_field(field, field_name, timestep)
        use params, only: nx, ny

        real(8), intent(in) :: field(0:nx,0:ny)
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
            write(9,*) field(:,j)
        end do
        close(9)
    end subroutine write_field
end module io
