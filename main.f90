module mod_string

    implicit none

    type t_string
        character(1), allocatable :: char(:)
    contains
        procedure init
        procedure print
    end type

contains

    subroutine init(self, string)

        !! Returns the input "string" in "self" with each letter in the "char" attribute

        class(t_string) :: self
        character(*), intent(in) :: string
        integer :: i

        allocate(self%char(len_trim(string)))

        do i = 1, len_trim(string)
            self%char(i) = string(i:i)
        end do

    end subroutine

    subroutine print(self)

        class(t_string) :: self
        character(256) :: string
        integer :: i

        string = ""
        do i = 1, size(self%char)
            string = trim(string) // self%char(i)
        end do

        write(*,*)trim(string)

    end subroutine

end module

module mod_string_list

    use mod_string
    implicit none

    type t_string_list
        type(t_string), allocatable :: string(:)
    contains
        procedure :: get_size
        procedure :: append
    end type

contains

    subroutine get_size(self, list_size)

        !! Returns the list size

        class(t_string_list) :: self
        integer, intent(out) :: list_size

        list_size = size(self%string)

    end subroutine

    subroutine append(self, string_input)

        class(t_string_list) :: self
        character(*), intent(in) :: string_input

        type(t_string), allocatable :: temp(:)
        integer :: i
        integer :: initial_list_size

        if (allocated(self%string)) then
            !! Create a copy of the current list, increment its size, paste back the list items and add the new string
            call self%get_size(initial_list_size)
            allocate(temp(initial_list_size))
            temp = self%string
            deallocate(self%string)
            allocate(self%string(initial_list_size + 1))
            do i = 1, initial_list_size
                self%string(i) = temp(i)
            end do
            call self%string(initial_list_size + 1)%init(string_input)
        else
            !! Allocate the list and add the string
            allocate(self%string(1))
            call self%string(1)%init(string_input)
        end if

    end subroutine

end module

program word_generator
    use mod_string
    use mod_string_list
    implicit none

    type(t_string_list) :: file_list

    call dir_files("\datasets", "csv", file_list)
    
    read(*,*)
    
end program

subroutine get_active_folder(active_folder)

    !! Returns the project root folder
    implicit none
    integer :: free_id
    character(256) :: active_folder
    character(256) :: active_folder_file
    character(256) :: cmd

    active_folder_file = "active_folder.txt"
    cmd = "cd > " // trim(active_folder_file)
    call execute_command_line(cmd)

    open(newunit=free_id, file = active_folder_file)
    read(free_id,"(A)")active_folder
    close(free_id)

end subroutine

subroutine dir_files(folder, extension, file_list)

    !! Return a file containing the list of the files contained in the specified folder relative to project folder
    use mod_string_list
    implicit none

    character(*), intent(in) :: folder
    character(*), intent(in) :: extension
    type(t_string_list), intent(out) :: file_list

    character(256), parameter :: output_file = "list.txt"
    character(256) :: active_folder
    character(256) :: cmd
    character(256) :: row
    integer :: new_unit
    integer :: row_number
    integer :: i

    call get_active_folder(active_folder)

    cmd = "dir " // trim(active_folder) // trim(folder)//"\*."//trim(extension)//" /b > "//trim(output_file)

    call execute_command_line(cmd)

    call get_file_row_number(output_file, row_number)

    open(newunit = new_unit, file = output_file)

    do i = 1, row_number
        read(new_unit,"(A)") row
        call file_list%append(row)
        call file_list%string(i)%print()
    end do

    close(new_unit)

end subroutine

subroutine get_file_row_number(file, row_number)

    !! Returns the number of adjacent rows in a file
    !! Stops when an empty line is found

    implicit none
    character(256), intent(in) :: file
    integer :: new_unit
    integer :: ioerr
    integer, intent(out) :: row_number

    row_number = 0

    open(newunit = new_unit, file = file)

    do
        read(new_unit, "(A)", iostat = ioerr)
        if (ioerr == 0) then
            row_number = row_number + 1
        else
            exit
        end if
    end do

    close(new_unit)

end subroutine

