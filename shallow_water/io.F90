module io
    use params, only: nx, ny, nt

    implicit none

    private
    public write_field

contains
    ! Function: write data array to file
    subroutine write_field(field, field_name, timestep)
        real(8), intent(in) :: field(0:nx,0:ny)
        character, intent(in) :: field_name
        integer, intent(in) :: timestep

        character(len=1024) :: filename
        integer :: i,j

        write (filename, "(A9,A1,A1,I5.5,A4)") './output/', field_name, '.', timestep, '.txt'

        open(unit=9, file=filename, status='unknown')

        do j = 1, ny-1
            write(9,*) field(:,j)
        end do

        close(9)
    end subroutine write_field
end module io
