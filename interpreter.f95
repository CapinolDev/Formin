program interpreter
    implicit none

    integer :: ios, num
    character(len=256) :: fileName
    character(len=256) :: line
    character(len=256) :: command, value, val
    integer :: pos1, pos2
    character(len=256) :: tokens(10)
    integer :: ntok

    type :: Var
        character(len=32) :: name
        character(len=256) :: value
    end type Var

    type(Var), allocatable :: Vars(:)
    integer :: VarCount

    VarCount = 0
    allocate(Vars(0))


    call get_command_argument(1, fileName)

    if (len_trim(fileName) == 0) then
        write(*,'(A)') "Usage: ./formin <filename>"
        stop
    end if

    open(unit=1, file=trim(fileName), action='read', status='old', iostat=ios)
    if (ios /= 0) then
        write(*,'(A)') "Error opening file!"
        stop
    end if

    do
        read(1, '(A)', iostat=ios) line
        if (ios /= 0) exit

        pos1 = index(line, '#/')
        pos2 = index(line, '/#')

        if (trim(line) == 'bye') then
            exit
        else if (pos1 > 0 .and. pos2 > pos1) then
            command = adjustl(trim(line(1:pos1-1)))
            value   = adjustl(trim(line(pos1+2:pos2-1)))

            call split(value, "|", tokens, ntok)

            select case (command)
            case ('spew')
            if (ntok >= 1) then
                if (any(trim(tokens(1)) == Vars(:)%name)) then
                    write(*,'(A)') trim(getVar(trim(tokens(1))))
                else
                    write(*,'(A)') trim(tokens(1))
                end if
            end if

            case ("create")
                if (ntok >= 2) then
                    read(tokens(2), *, iostat=ios) val
                    if (ios /= 0) then
                        write(*,*) "Error: invalid value for variable ", trim(tokens(1))
                    else
                        call setVar(trim(tokens(1)), val)
                    end if
                else if (ntok == 1) then
                    call setVar(trim(tokens(1)), '')
                end if
            case default
                write(*,'(A)') "Unknown command: "//trim(command)
            end select
        end if
    end do

    close(1)

    contains 

    subroutine extendArray(arr, newSize)
        type(Var), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: newSize
        type(Var), allocatable :: tmp(:)

        allocate(tmp(newSize))
        if (size(arr) > 0) tmp(1:size(arr)) = arr
        call move_alloc(tmp, arr)
    end subroutine extendArray


    subroutine setVar(name, value)
        character(len=*), intent(in) :: name, value
        integer :: i

        do i = 1, VarCount
            if (trim(Vars(i)%name) == trim(name)) then
                Vars(i)%value = value
                return
            end if
        end do

        VarCount = VarCount + 1
        call extendArray(Vars, VarCount)
        Vars(VarCount)%name = trim(name)
        Vars(VarCount)%value = trim(value)
    end subroutine setVar


    subroutine split(input, delimiter, tokens, count)
        character(len=*), intent(in) :: input
        character(len=*), intent(in) :: delimiter
        character(len=256), intent(out) :: tokens(:)
        integer, intent(out) :: count

        integer :: start, pos, lenInput, lenDelim

        count = 0
        lenInput = len_trim(input)
        lenDelim = len_trim(delimiter)
        start = 1

        do
            pos = index(input(start:), delimiter)
            if (pos == 0) then
                count = count + 1
                tokens(count) = adjustl(trim(input(start:lenInput)))
                exit
            else
                count = count + 1
                tokens(count) = adjustl(trim(input(start:start+pos-2)))
                start = start + pos + lenDelim - 1
            end if
        end do
    end subroutine

    function getVar(name) result(val)
        character(len=*), intent(in) :: name
        character(len=256) :: val
        integer :: i

        val = "undefined"
        do i = 1, VarCount
            if (trim(Vars(i)%name) == trim(name)) then
                val = Vars(i)%value
                return
            end if
        end do
    end function


end program interpreter
