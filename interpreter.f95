program interpreter
    use terminal_colors
    implicit none
    character(len=5) :: version
    integer :: ios
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
    character(len=256) :: s1, s2, s3, tempRead, localhelp
    integer :: lineInt
    integer :: lineNumber
    character (len=1024) :: osSeperator, strSave
    character (len=5) :: osClear
    integer :: i, finalValInt
    integer, allocatable :: goStack(:)
    integer :: goDepth = 0
    character(len=256) :: finalValStr
    character(len=256) :: userOs
    real :: r
    
    type :: Var
        character(len=32)  :: name
        character(len=256) :: value
        character(len=8)   :: vartype 
    end type Var
    type :: Marker
        integer :: pos
        character(len=256) :: name
    end type Marker

    type(Var), allocatable    :: Vars(:)
    type(Marker), allocatable :: Markers(:)
    integer :: VarCount, MarkerCount

    call get_environment_variable("PATH", osSeperator)
    if (index(osSeperator, ";") > 0) then
        osClear = 'cls'
        userOs = 'Windows'
    else
        osClear = 'clear'
        userOs = 'Unix'
    end if

    version = '1.0.7'

    VarCount = 0
    MarkerCount = 0
    allocate(Vars(0))
    allocate(Markers(0))
    lineNumber = 0




    call get_command_argument(1, fileName)
    if (trim(fileName) == 'ver') then
        write(*,*) "Version ", trim(version)

    else if (len_trim(fileName) == 0) then
        write(*,'(A)') "Usage: ./formin <filename>"
        stop
    end if
if(trim(fileName) /= 'ver') then
    open(unit=1, file=trim(fileName), action='read', status='old', iostat=ios)
    if (ios /= 0) then
        write(*,'(A)') "Error opening file!"
        stop
    end if
    call init_random()

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

            select case (trim(command))

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
                    if (ntok == 3) then
                        call createVar(trim(tokens(1)), resolveToken(tokens(2)), trim(tokens(3)))
                    else
                        call createVar(trim(tokens(1)), resolveToken(tokens(2)))
                    end if
                else if (ntok == 1) then
                    call createVar(trim(tokens(1)), '', 'str')
                else
                    write(*,'(A)') "Error: create requires 1-3 tokens: name|[value]|[type]"
                end if

            case ("add")
                call numeric_op(tokens, ntok, "+")

            case ("sub")
                call numeric_op(tokens, ntok, "-")
            case ("mult")
                call numeric_op(tokens, ntok, "*")
            case("div")
                call numeric_op(tokens, ntok, "/")
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
                    if (cmp_values(s1, s2, trim(tokens(2)))) then
                        call jump_to(tokens(4))
                    else
                        call jump_to(tokens(5))
                    end if
                else
                    write(*,*) "Error: ifgo requires 5 tokens (var1|comparison|var2|marker|marker)"
                end if

            case ("ask")
                if (ntok==2) then
                    write(*,*) trim(resolveToken(tokens(1)))
                    read(*,*) tempRead
                    call setVar(trim(tokens(2)), trim(tempRead))
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
                        call setVar(s1, trim(toString(fileUnit)), 'int')
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
                        call setVar(trim(s2), "", 'str')
                    else
                        call setVar(trim(s2), trim(tempLine), 'str')
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
                    !write(*,*) "Error: no previous go to return to"
                end if
            case("str")
                select case(trim(tokens(1)))

                case("cat")
                    if (ntok >= 4) then
                        s1 = resolveToken(tokens(3))
                        s2 = resolveToken(tokens(4))
                        strSave = ''

                        if (ntok == 5) then
                            if (trim(lower(tokens(5))) == 'sp') then
                                strSave = trim(s1) // ' ' // trim(s2)
                            else
                                strSave = trim(s1) // trim(s2)
                            end if
                        else
                            strSave = trim(s1) // trim(s2)
                        end if

                        call setVar(trim(tokens(2)), trim(strSave), 'str')
                    else
                        write(*,*) "Error: cat requires at least 3 tokens: var|string1|string2|[space]"
                    end if
                case("rev")
                    if(ntok==3) then
                        s1 = resolveToken(tokens(3))
                        strSave = ''
                        strSave = trim(reverse(trim(s1)))
                        call setVar(trim(tokens(2)),trim(strSave))
                    else
                        write(*,*) "Error: rev requires 2 tokens: var|string"
                    end if
                case("low")
                    if(ntok==3) then
                        s1 = resolveToken(tokens(3))
                        strSave = ''
                        strSave = trim(lower(trim(s1)))
                        call setVar(trim(tokens(2)),trim(strSave))
                    else
                        write(*,*) "Error: low requires 2 tokens: var|string"
                    end if
                case("up")
                    if(ntok==3) then
                        s1 = resolveToken(tokens(3))
                        strSave = ''
                        strSave = trim(upper(trim(s1)))
                        call setVar(trim(tokens(2)),trim(strSave))
                    else
                        write(*,*) "Error: up requires 2 tokens: var|string"
                    end if
                case("len")
                    if(ntok==3) then
                        s1 = trim(tokens(2))
                        s2 = resolveToken(tokens(3))

                        i = len_trim(s2)               
                        write(strSave, '(I0)') i       

                        call setVar(s1, trim(strSave), 'int')
                    else
                        write(*,*) "Error: len requires 2 tokens: var|string "
                    end if
                
                end select
            case("type")
                if(ntok==2) then
                    s1 = trim(tokens(1))
                    s2 = trim(tokens(2))

                    call setVar(s1, getVarType(trim(s2)), 'str') 
                else
                    write(*,*) "Error: type requires 2 tokens: varToStore|var"
                end if
            case("set")
                if (ntok == 2) then
                    call modifyVar(trim(tokens(1)), resolveToken(tokens(2)))
                else if (ntok == 3) then
                    call modifyVar(trim(tokens(1)), resolveToken(tokens(2)), trim(tokens(3)))
                else                        
                    write(*,*) "Error: set requires 2 or 3 tokens: var|value|[type]"
                end if
            case("mod")
                if(ntok==3) then
                    s1 = trim(tokens(1))
                    s2 = resolveToken(tokens(2))
                    s3 = trim(tokens(3))
                    read(s2,*) a
                    read(s3,*) b

                    finalValInt = modulo(a, b)
                    write(finalValStr, '(I0)') finalValInt

                    call setVar(s1, finalValStr, 'int')
                else
                    write(*,*) "Error: mod requires 3 tokens: var|value to mod|value to mod by"
                end if
            case("getos")
                if(ntok==1) then
                    s1 = trim(tokens(1))

                    call setVar(s1, trim(userOs))
                else 
                    write(*,*) "Error: getos requires 1 token: var"
                end if
            case("randi")
                if (ntok==2) then
                    s1 = trim(tokens(1))
                    s2 = resolveToken(tokens(2))

                    read(s2,*) a
                    call random_number(r)
                    i = floor(r * a)
                    write(s2, '(I0)') i
                    call setVar(s1, s2)
                
                else 
                    write(*,*) "Error: randi requires 2 tokens: var|multiplier"
                end if
            case default
                write(*,'(A)') "Unknown command: "//trim(command)
            end select
        
        end if
    end do

    close(1)
end if
contains

        subroutine numeric_op(tokens, ntok, op)
            character(len=*), intent(in) :: tokens(:)
            integer, intent(in) :: ntok
            character(len=*), intent(in) :: op
            character(len=256) :: lhs, rhs, resultStr
            real(kind=8) :: a, b, res
            integer :: ios1, ios2
            logical :: intA, intB, anyFloat

            if (ntok /= 3) then
                write(*,'(A)') "Error: arithmetic requires 3 tokens: name|lhs|rhs"
                return
            end if

            lhs = resolveToken(tokens(2))
            rhs = resolveToken(tokens(3))

            read(lhs, *, iostat=ios1) a
            read(rhs, *, iostat=ios2) b

            if (ios1 /= 0 .or. ios2 /= 0) then
                write(*,'(A)') "Error: non-numeric operands"
                return
            end if

            intA = (index(lhs, ".") == 0)
            intB = (index(rhs, ".") == 0)
            anyFloat = .not.(intA .and. intB)

            select case (op)
            case ("+"); res = a + b
            case ("-"); res = a - b
            case ("*"); res = a * b
            case ("/")
                if (b == 0.0d0) then
                    write(*,'(A)') "Error: division by zero"
                    return
                end if
                res = a / b
            end select

            if (anyFloat) then
                write(resultStr, '(F15.8)') res
                call setVar(trim(tokens(1)), adjustl(trim(resultStr)), 'float')
            else
                write(resultStr, '(I0)') int(res)
                call setVar(trim(tokens(1)), adjustl(trim(resultStr)), 'int')
            end if
        end subroutine numeric_op


    subroutine jump_to(tokMarker)
        character(len=*), intent(in) :: tokMarker
        integer :: tgt
        if (trim(tokMarker) == '_') return
        tgt = getMarker(trim(tokMarker))
        if (tgt > 0) then
            call pushGo(lineNumber)
            rewind(1)
            lineNumber = 0
            do while (lineNumber < tgt)
                read(1,'(A)',iostat=ios) line
                if (ios /= 0) exit
                lineNumber = lineNumber + 1
            end do
        else
            write(*,*) "Error: marker not found: ", trim(tokMarker)
        end if
    end subroutine jump_to

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

    subroutine setVar(name, value, vartype)
        character(len=*), intent(in) :: name, value
        character(len=*), intent(in), optional :: vartype
        integer :: i, ios_num
        real(kind=8) :: tempReal
        integer :: tempInt
        character(len=8) :: typeToSet

        do i = 1, VarCount
            if (trim(Vars(i)%name) == trim(name)) then
                Vars(i)%value   = trim(value)
                if (present(vartype)) then
                Vars(i)%vartype = adjustl(trim(vartype))
                end if
                return
            end if
        end do
        
        if (present(vartype)) then
            typeToSet = adjustl(trim(vartype))
        else
            read(value, *, iostat=ios_num) tempInt
            if (ios_num == 0) then
                typeToSet = 'int'
            else
                read(value, *, iostat=ios_num) tempReal
                if (ios_num == 0) then
                    typeToSet = 'float'
                else
                    typeToSet = 'str'
                end if
            end if
        end if

        

        VarCount = VarCount + 1
        call extendArrayVar(Vars, VarCount)
        Vars(VarCount)%name    = trim(name)
        Vars(VarCount)%value   = trim(value)
        Vars(VarCount)%vartype = trim(typeToSet)
    end subroutine setVar

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
    end function getVar

    function getVarType(name) result(vtype)
        character(len=*), intent(in) :: name
        character(len=8) :: vtype
        integer :: i

        vtype = "undefined"
        do i = 1, VarCount
            if (trim(Vars(i)%name) == trim(name)) then
                vtype = Vars(i)%vartype
                return
            end if
        end do
    end function getVarType

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
    end subroutine split

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
    end function getMarker

    function resolveToken(tok) result(res)
        character(len=*), intent(in) :: tok
        character(len=256) :: res
        character(len=256) :: trimmed
        integer :: i

        trimmed = trim(tok)

        if (len_trim(trimmed) >= 2) then
            if (trimmed(1:1) == "'" .and. trimmed(len_trim(trimmed):len_trim(trimmed)) == "'") then
                res = trimmed(2:len_trim(trimmed)-1)
                return
            end if
        end if

        do i = 1, VarCount
            if (trimmed == trim(Vars(i)%name)) then
                res = Vars(i)%value
                return
            end if
        end do

        res = trimmed
    end function resolveToken

    logical function try_parse_real(s, x)
        character(len=*), intent(in) :: s
        real(kind=8),    intent(out) :: x
        integer :: iosn
        read(s, *, iostat=iosn) x
        try_parse_real = (iosn == 0)
    end function try_parse_real

    logical function cmp_values(sleft, sright, op)
        character(len=*), intent(in) :: sleft, sright, op
        real(kind=8) :: a, b, eps
        logical :: left_num, right_num
        character(len=16) :: cop

        eps = 1.0d-9
        cop = adjustl(trim(op))

        left_num  = try_parse_real(trim(sleft),  a)
        right_num = try_parse_real(trim(sright), b)

        if (left_num .and. right_num) then
            select case (cop)
            case ('is')
                cmp_values = (abs(a - b) <= eps)
            case ('isnt')
                cmp_values = (abs(a - b) > eps)
            case ('>')
                cmp_values = (a > b + eps)
            case ('<')
                cmp_values = (a < b - eps)
            case ('>=')
                cmp_values = (a > b - eps)
            case ('<=')
                cmp_values = (a < b + eps)
            case default
                cmp_values = .false.
            end select
        else
            select case (cop)
            case ('is')
                cmp_values = (trim(sleft) == trim(sright))
            case ('isnt')
                cmp_values = (trim(sleft) /= trim(sright))
            case ('>')
                cmp_values = (trim(sleft) >  trim(sright))
            case ('<')
                cmp_values = (trim(sleft) <  trim(sright))
            case ('>=')
                cmp_values = (trim(sleft) >= trim(sright))
            case ('<=')
                cmp_values = (trim(sleft) <= trim(sright))
            case default
                cmp_values = .false.
            end select
        end if
    end function cmp_values

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
    pure function lower(str) result(out)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: out
        integer :: i
        out = str
        do i = 1, len(str)
            select case (str(i:i))
            case ('A':'Z')
                out(i:i) = achar(iachar(str(i:i)) + 32)
            end select
        end do
    end function lower
    pure function reverse(str) result(out)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: out
        integer :: i
        do i = 1, len(str)
            out(i:i) = str(len(str)-i+1:len(str)-i+1)
        end do
    end function reverse

    pure function upper(str) result(out)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: out
        integer :: i
        out = str
        do i = 1, len(str)
            if (str(i:i) >= 'a' .and. str(i:i) <= 'z') then
                out(i:i) = achar(iachar(str(i:i)) - 32)
            end if
        end do
    end function upper

    subroutine createVar(name, value, vartype)
        character(len=*), intent(in) :: name, value
        character(len=*), intent(in), optional :: vartype
        integer :: i

        do i = 1, VarCount
            if (trim(Vars(i)%name) == trim(name)) then
                write(*,*) "Error: variable already exists: ", trim(name)
                return
            end if
        end do

        if (present(vartype)) then
            call setVar(name, value, vartype)
        else
            call setVar(name, value)
        end if
    end subroutine createVar


    subroutine modifyVar(name, value, vartype)
        character(len=*), intent(in) :: name, value
        character(len=*), intent(in), optional :: vartype
        integer :: i, found

        found = 0
        do i = 1, VarCount
            if (trim(Vars(i)%name) == trim(name)) then
                found = 1
                exit
            end if
        end do

        if (found == 0) then
            write(*,*) "Error: variable not found: ", trim(name)
            return
        end if

        if (present(vartype)) then
            call setVar(name, value, vartype)
        else
            call setVar(name, value)
        end if
    end subroutine modifyVar


    subroutine init_random()
        integer, allocatable :: seed(:)
        integer :: n, i, j
        call random_seed(size=n)
        allocate(seed(n))
        call system_clock(count=i)
        seed = i + 37 * [(j, j=1,n)]
        call random_seed(put=seed)
        deallocate(seed)
    end subroutine init_random

end program interpreter
