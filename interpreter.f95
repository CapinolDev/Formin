program interpreter
    use terminal_colors
    implicit none

    integer :: ios, readFileName
    integer :: nextFileUnit = 20 
    integer :: fileUnit
    character(len=256) :: fileName, tempLine
    character(len=256) :: line
    character(len=256) :: command, value, val
    integer :: pos1, pos2
    character(len=256) :: tokens(10)
    integer :: ntok
    character(len=256) :: outAdd 
    integer :: a, b, sum, ios_local, sub, mult, div
    character(len=256) :: s1, s2, tempRead
    integer :: lineInt
    integer :: lineNumber
    character (len=1024) :: osSeperator
    character (len=5) :: osClear
    integer :: i
    integer, allocatable :: goStack(:)
    integer :: goDepth = 0


    type :: Var
        character(len=32) :: name
        character(len=256) :: value
    end type Var
    type :: Marker
        integer :: pos
        character(len=256) :: name
    end type Marker

    type(Var), allocatable :: Vars(:)
    type(Marker), allocatable :: Markers(:)
    integer :: VarCount, MarkerCount

    call get_environment_variable("PATH", osSeperator)
    if (index(osSeperator, ";") > 0) then
        osClear = 'cls'
    else
        osClear = 'clear'
    end if

    VarCount = 0
    MarkerCount = 0
    allocate(Vars(0))
    allocate(Markers(0))
    lineNumber = 0

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
        lineNumber = lineNumber + 1

        pos1 = index(line, '#/')
        pos2 = index(line, '/#')
        if (pos1 > 0 .and. pos2 > pos1) then
            command = adjustl(trim(line(1:pos1-1)))
            value   = adjustl(trim(line(pos1+2:pos2-1)))
            call split(value, "|", tokens, ntok)
            if (trim(command) == 'mark' .and. ntok == 1) then
                call setMarker(lineNumber, trim(tokens(1)))
            end if
        end if
    end do

    rewind(1)
    lineNumber = 0

    do
        read(1, '(A)', iostat=ios) line
        if (ios /= 0) exit
        lineNumber = lineNumber + 1

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
                do i = 1, ntok
                    write(*,'(A)', advance='no') trim(resolveToken(tokens(i)))//' '
                end do
                write(*,*)
            else
                write(*,'(A)') "Error: spew requires at least one token"
            end if
            case('spewmult')
            if (ntok >= 1) then
                do i = 1, ntok
                    write(*,'(A)', advance='no') trim(resolveToken(tokens(i)))
                end do
                write(*,*)
            else
                write(*,'(A)') "Error: spewmult requires at least one token"
            end if

            case('color')
                call set_color(tokens(1))

            case ("create")
                if (ntok >= 2) then
                    call setVar(trim(tokens(1)), resolveToken(tokens(2)))
                else if (ntok == 1) then
                    call setVar(trim(tokens(1)), '')
                end if

            case ("add")          
                if (ntok == 3) then
                    s1 = resolveToken(tokens(2))
                    s2 = resolveToken(tokens(3))

                    read(s1, *, iostat=ios_local) a
                    if (ios_local /= 0) then
                        write(*,'(A)') "Error: first operand is not an integer"
                        cycle
                    end if
                    read(s2, *, iostat=ios_local) b
                    if (ios_local /= 0) then
                        write(*,'(A)') "Error: second operand is not an integer"
                        cycle
                    end if


                    sum = a + b
                    write(outAdd, '(I0)') sum
                    call setVar(trim(tokens(1)), trim(outAdd))
                else
                    write(*,'(A)') "Error: add requires exactly 3 tokens: name|lhs|rhs"
                end if
            case ("sub")
                if (ntok == 3) then
                    s1 = resolveToken(tokens(2))
                    s2 = resolveToken(tokens(3))

                    read(s1, *, iostat=ios_local) a
                    if (ios_local /= 0) then
                        write(*,'(A)') "Error: first operand is not an integer"
                        cycle
                    end if
                    read(s2, *, iostat=ios_local) b
                    if (ios_local /= 0) then
                        write(*,'(A)') "Error: second operand is not an integer"
                        cycle
                    end if

                    sub = a - b
                    write(outAdd, '(I0)') sub
                    call setVar(trim(tokens(1)), trim(outAdd))
                else
                    write(*,'(A)') "Error: sub requires exactly 3 tokens: name|lhs|rhs"
                end if
            case ("mult")
                if (ntok == 3) then
                    s1 = resolveToken(tokens(2))
                    s2 = resolveToken(tokens(3))

                    read(s1, *, iostat=ios_local) a
                    if (ios_local /= 0) then
                        write(*,'(A)') "Error: first operand is not an integer"
                        cycle
                    end if
                    read(s2, *, iostat=ios_local) b
                    if (ios_local /= 0) then
                        write(*,'(A)') "Error: second operand is not an integer"
                        cycle
                    end if


                    mult = a * b
                    write(outAdd, '(I0)') mult
                    call setVar(trim(tokens(1)), trim(outAdd))
                else
                    write(*,'(A)') "Error: mult requires exactly 3 tokens: name|lhs|rhs"
                end if
            case("div")
                if (ntok == 3) then
                   s1 = resolveToken(tokens(2))
                    s2 = resolveToken(tokens(3))

                    read(s1, *, iostat=ios_local) a
                    if (ios_local /= 0) then
                        write(*,'(A)') "Error: first operand is not an integer"
                        cycle
                    end if
                    read(s2, *, iostat=ios_local) b
                    if (ios_local /= 0) then
                        write(*,'(A)') "Error: second operand is not an integer"
                        cycle
                    end if


                    div = a / b
                    write(outAdd, '(I0)') div
                    call setVar(trim(tokens(1)), trim(outAdd))
                else
                    write(*,'(A)') "Error: div requires exactly 3 tokens: name|lhs|rhs"
                end if
            case ("mark")
                if (ntok == 1) then
                    call setMarker(lineNumber, trim(tokens(1)))
                else
                    write(*,*) "Error: mark requires exactly 1 token (name)"
                end if
            case ("go")
                if (ntok == 1) then
                    lineInt = getMarker(trim(tokens(1)))
                    if (lineInt > 0) then
                        call pushGo(lineNumber)
                        rewind(1)
                        lineNumber = 0
                        do while (lineNumber < lineInt)
                            read(1,'(A)',iostat=ios) line
                            if (ios /= 0) exit
                            lineNumber = lineNumber + 1
                        end do
                    else
                        write(*,*) "Error: marker not found: ", trim(tokens(1))
                    end if
                else
                    write(*,*) "Error: go requires exactly 1 token (name)"
                end if
            case("ifgo")
                if (ntok == 5) then
                    s1 = resolveToken(tokens(1))
                    s2 = resolveToken(tokens(3))

                    if (trim(tokens(2)) == 'is') then
                        if (s1 == s2) then
                            lineInt = getMarker(trim(tokens(4)))
                            if (lineInt > 0) then
                                call pushGo(lineNumber)
                                rewind(1)
                                lineNumber = 0
                                do while (lineNumber < lineInt)
                                    read(1,'(A)',iostat=ios) line
                                    if (ios /= 0) exit
                                    lineNumber = lineNumber + 1
                                end do
                            else
                                if (trim(tokens(5)) /= '_') then
                                    write(*,*) "Error: marker not found: ", trim(tokens(1))
                                end if
                            end if
                        else
                            lineInt = getMarker(trim(tokens(5)))
                            if (lineInt > 0) then
                                call pushGo(lineNumber)
                                rewind(1)
                                lineNumber = 0
                                do while (lineNumber < lineInt)
                                    read(1,'(A)',iostat=ios) line
                                    if (ios /= 0) exit
                                    lineNumber = lineNumber + 1
                                end do
                            else
                                if (trim(tokens(5)) /= '_') then
                                    write(*,*) "Error: marker not found: ", trim(tokens(1))
                                end if
                            end if
                        end if
                    else if(trim(tokens(2)) == 'isnt') then
                        if (s1 /= s2) then
                            lineInt = getMarker(trim(tokens(4)))
                            if (lineInt > 0) then
                                call pushGo(lineNumber)
                                rewind(1)
                                lineNumber = 0
                                do while (lineNumber < lineInt)
                                    read(1,'(A)',iostat=ios) line
                                    if (ios /= 0) exit
                                    lineNumber = lineNumber + 1
                                end do
                            else
                                if (trim(tokens(5)) /= '_') then
                                    write(*,*) "Error: marker not found: ", trim(tokens(1))
                                end if
                            end if
                        else 
                            lineInt = getMarker(trim(tokens(5)))
                            if (lineInt > 0) then
                                call pushGo(lineNumber)
                                rewind(1)
                                lineNumber = 0
                                do while (lineNumber < lineInt)
                                    read(1,'(A)',iostat=ios) line
                                    if (ios /= 0) exit
                                    lineNumber = lineNumber + 1
                                end do
                            else
                                if (trim(tokens(5)) /= '_') then
                                    write(*,*) "Error: marker not found: ", trim(tokens(1))
                                end if
                            end if
                        end if
                    else if(trim(tokens(2)) == '>') then
                        if (s1 > s2) then
                            lineInt = getMarker(trim(tokens(4)))
                            if (lineInt > 0) then
                                call pushGo(lineNumber)
                                rewind(1)
                                lineNumber = 0
                                do while (lineNumber < lineInt)
                                    read(1,'(A)',iostat=ios) line
                                    if (ios /= 0) exit
                                    lineNumber = lineNumber + 1
                                end do
                            else
                                if (trim(tokens(5)) /= '_') then
                                    write(*,*) "Error: marker not found: ", trim(tokens(1))
                                end if
                            end if
                        else 
                            lineInt = getMarker(trim(tokens(5)))
                            if (lineInt > 0) then
                                call pushGo(lineNumber)
                                rewind(1)
                                lineNumber = 0
                                do while (lineNumber < lineInt)
                                    read(1,'(A)',iostat=ios) line
                                    if (ios /= 0) exit
                                    lineNumber = lineNumber + 1
                                end do
                            else
                                if (trim(tokens(5)) /= '_') then
                                    write(*,*) "Error: marker not found: ", trim(tokens(1))
                                end if
                            end if
                        end if
                    else if(trim(tokens(2)) == '<') then
                        if (s1 < s2) then
                            lineInt = getMarker(trim(tokens(4)))
                            if (lineInt > 0) then
                                call pushGo(lineNumber)
                                rewind(1)
                                lineNumber = 0
                                do while (lineNumber < lineInt)
                                    read(1,'(A)',iostat=ios) line
                                    if (ios /= 0) exit
                                    lineNumber = lineNumber + 1
                                end do
                            else
                                if (trim(tokens(5)) /= '_') then
                                    write(*,*) "Error: marker not found: ", trim(tokens(1))
                                end if
                            end if
                        else 
                            lineInt = getMarker(trim(tokens(5)))
                            if (lineInt > 0) then
                                call pushGo(lineNumber)
                                rewind(1)
                                lineNumber = 0
                                do while (lineNumber < lineInt)
                                    read(1,'(A)',iostat=ios) line
                                    if (ios /= 0) exit
                                    lineNumber = lineNumber + 1
                                end do
                            else
                                if (trim(tokens(5)) /= '_') then
                                    write(*,*) "Error: marker not found: ", trim(tokens(1))
                                end if
                            end if
                        end if
                    else if(trim(tokens(2)) == '>=') then
                        if (s1 >= s2) then
                            lineInt = getMarker(trim(tokens(4)))
                            if (lineInt > 0) then
                                rewind(1)
                                lineNumber = 0
                                do while (lineNumber < lineInt)
                                    read(1,'(A)',iostat=ios) line
                                    if (ios /= 0) exit
                                    lineNumber = lineNumber + 1
                                end do
                            else
                                if (trim(tokens(5)) /= '_') then
                                    write(*,*) "Error: marker not found: ", trim(tokens(1))
                                end if
                            end if
                        else 
                            lineInt = getMarker(trim(tokens(5)))
                            if (lineInt > 0) then
                                rewind(1)
                                lineNumber = 0
                                do while (lineNumber < lineInt)
                                    read(1,'(A)',iostat=ios) line
                                    if (ios /= 0) exit
                                    lineNumber = lineNumber + 1
                                end do
                            else
                                if (trim(tokens(5)) /= '_') then
                                    write(*,*) "Error: marker not found: ", trim(tokens(1))
                                end if
                            end if
                        end if
                    else if(trim(tokens(2)) == '<=') then
                        if (s1 <= s2) then
                            lineInt = getMarker(trim(tokens(4)))
                            if (lineInt > 0) then
                                rewind(1)
                                lineNumber = 0
                                do while (lineNumber < lineInt)
                                    read(1,'(A)',iostat=ios) line
                                    if (ios /= 0) exit
                                    lineNumber = lineNumber + 1
                                end do
                            else
                                if (trim(tokens(5)) /= '_') then
                                    write(*,*) "Error: marker not found: ", trim(tokens(1))
                                end if
                            end if
                        else 
                            lineInt = getMarker(trim(tokens(5)))
                            if (lineInt > 0) then
                                rewind(1)
                                lineNumber = 0
                                do while (lineNumber < lineInt)
                                    read(1,'(A)',iostat=ios) line
                                    if (ios /= 0) exit
                                    lineNumber = lineNumber + 1
                                end do
                            else
                                if (trim(tokens(5)) /= '_') then
                                    write(*,*) "Error: marker not found: ", trim(tokens(1))
                                end if
                            end if
                        end if
                    
                    else 
                        write(*,*) "Error: comparison should either be is, isnt, >, <, >=, <="
                    end if
                else
                    write(*,*) "Error: ifgo requires 5 tokens (var1|comparison|var2|marker|marker (goes if not true))"
                end if
            case ("ask")
                if (ntok==2) then
                    write(*,*) trim(resolveToken(tokens(1)))
                    read(*,*) tempRead
                    call setVar(trim(tokens(2)), tempRead)
                else 
                    print*,'Error: ask requires 2 tokens: question|var (where the answer is stored)'
                end if
            case("clear")
                call system(osClear)
            case ("open")
                if (ntok == 2) then
                    
                    s1 = trim(tokens(1))
                    s2 = resolveToken(tokens(2))

                    fileUnit = nextFileUnit
                    nextFileUnit = nextFileUnit + 1

                    open(unit=fileUnit, file=s2, status='old', action='read', iostat=ios)
                    if (ios /= 0) then
                        write(*,*) "Error: cannot open file ", trim(s2)
                        cycle  
                    else
                        call setVar(s1, trim(toString(fileUnit)))
                    end if
                else
                    write(*,*) "Error: open requires 2 tokens: handleName|filePath"
                end if


            case ("read")
            if (ntok == 2) then
                s1 = resolveToken(tokens(1)) 
                s2 = trim(tokens(2))

                read(s1, *, iostat=ios_local) fileUnit
                if (ios_local /= 0) then
                    write(*,*) "Error: invalid file handle variable"
                    cycle
                end if
                read(fileUnit, '(A)', iostat=ios_local) tempLine
                if (ios_local /= 0) then
                    call setVar(trim(s2), "")  
                else
                    call setVar(trim(s2), trim(tempLine))
                end if
            else
                write(*,*) "Error: read requires 2 tokens: handleName|lineVar"
            end if


            case ("close")
                if (ntok == 1) then
                    s1 = resolveToken(tokens(1))

                    read(s1, *, iostat=ios_local) fileUnit
                    if (ios_local /= 0) then
                        write(*,*) "Error: invalid file handle variable"
                    else
                        close(fileUnit, iostat=ios_local)
                        if (ios_local /= 0) then
                            write(*,*) "Error closing file handle ", trim(s1)
                        end if
                    end if
                else
                    write(*,*) "Error: close requires 1 token: handleName"
                end if
            case("goback")
                lineInt = popGo()
                if (lineInt > 0) then
                    rewind(1)
                    lineNumber = 0
                    do while (lineNumber < lineInt)
                        read(1,'(A)',iostat=ios) line
                        if (ios /= 0) exit
                        lineNumber = lineNumber + 1
                    end do
                else
                    write(*,*) "Error: no previous go to return to"
                end if
 
            case default
                write(*,'(A)') "Unknown command: "//trim(command)
            end select
        end if
    end do

    close(1)

    contains 

    function toString(i) result(s)
        integer, intent(in) :: i
        character(len=256) :: s
        write(s, '(I0)') i
    end function toString


    subroutine extendArrayVar(arr, newSize)
        type(Var), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: newSize
        type(Var), allocatable :: tmp(:)

        allocate(tmp(newSize))
        if (size(arr) > 0) tmp(1:size(arr)) = arr
        call move_alloc(tmp, arr)
    end subroutine extendArrayVar

        subroutine extendArrayMark(arr, newSize)
        type(Marker), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: newSize
        type(Marker), allocatable :: tmp(:)

        allocate(tmp(newSize))
        if (size(arr) > 0) tmp(1:size(arr)) = arr
        call move_alloc(tmp, arr)
    end subroutine extendArrayMark


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
        call extendArrayVar(Vars, VarCount)
        Vars(VarCount)%name = trim(name)
        Vars(VarCount)%value = trim(value)
    end subroutine setVar
    subroutine setMarker(position, name)
        character(len=*), intent(in) :: name
        integer, intent(in) :: position
        integer :: i

        do i = 1, MarkerCount
            if (trim(Markers(i)%name) == trim(name)) then
                Markers(i)%pos = position
                return
            end if
        end do
        MarkerCount = MarkerCount + 1
        call extendArrayMark(Markers, MarkerCount)
        Markers(MarkerCount)%pos = position
        Markers(MarkerCount)%name = trim(name)

    end subroutine setMarker

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
    function getMarker(name) result(pos)
        character(len=*), intent(in) :: name
        integer :: pos, i

        pos = -1
        do i = 1, MarkerCount
            if (trim(Markers(i)%name) == trim(name)) then
                pos = Markers(i)%pos
                return
            end if
        end do
    end function
    function resolveToken(tok) result(res)
        character(len=*), intent(in) :: tok
        character(len=256) :: res
        character(len=256) :: trimmed

        trimmed = trim(tok)

        if (len_trim(trimmed) >= 2) then
            if (trimmed(1:1) == "'" .and. trimmed(len_trim(trimmed):len_trim(trimmed)) == "'") then
                res = trimmed(2:len_trim(trimmed)-1)
                return
            end if
        end if

        if (any(trimmed == Vars(:)%name)) then
            res = getVar(trimmed)
            return
        end if

        res = trimmed
    end function resolveToken

    subroutine pushGo(pos)
        integer, intent(in) :: pos
        integer, allocatable :: tmp(:)

        if (.not.allocated(goStack)) then
            allocate(goStack(1))
            goStack(1) = pos
            goDepth = 1
        else
            allocate(tmp(goDepth))
            tmp = goStack
            deallocate(goStack)
            allocate(goStack(goDepth + 1))
            goStack(1:goDepth) = tmp
            goStack(goDepth + 1) = pos
            goDepth = goDepth + 1
            deallocate(tmp)
        end if
    end subroutine pushGo


    function popGo() result(pos)
        integer :: pos
        integer, allocatable :: tmp(:)

        if (.not.allocated(goStack)) then
            pos = -1
            return
        end if

        if (goDepth <= 0) then
            pos = -1
            return
        end if

        pos = goStack(goDepth)

        if (goDepth == 1) then
            deallocate(goStack)
            goDepth = 0
        else
            allocate(tmp(goDepth - 1))
            tmp = goStack(1:goDepth - 1)
            deallocate(goStack)
            allocate(goStack(goDepth - 1))
            goStack = tmp
            deallocate(tmp)
            goDepth = goDepth - 1
        end if
    end function popGo

end program interpreter
