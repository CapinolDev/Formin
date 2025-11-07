program interpreter
    use terminal_colors
    use formin_lists

    implicit none

    integer, parameter :: OP_NONE=0, OP_SPEW=1, OP_SPEWMULT=2, OP_COLOR=3, OP_CREATE=4, &
                      OP_ADD=5, OP_SUB=6, OP_MULT=7, OP_DIV=8, OP_MARK=9, OP_GO=10,  &
                      OP_IFGO=11, OP_ASK=12, OP_CLEAR=13, OP_OPEN=14, OP_READ=15,    &
                      OP_CLOSE=16, OP_GOBACK=17, OP_STR=18, OP_TYPE=19, OP_SET=20,   &
                      OP_MOD=21, OP_GETOS=22, OP_RANDI=23, OP_SQRT=24, OP_LIST=25,   &
                      OP_SYS=26, OP_CPUTIME=27

    character(len=5) :: version
    integer :: ios
    integer :: nextFileUnit = 20
    integer :: fileUnit
    character(len=256) :: fileName, tempLine, verbose
    character(len=256) :: line
    character(len=256) :: command, value
    integer :: pos1, pos2
    character(len=256) :: tokens(10)
    integer :: ntok
    integer :: a, b, ios_local
    character(len=256) :: s1, s2, s3, tempRead
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
    integer :: jdx
    character(len=256), allocatable :: tmp(:)
    character(len=256) :: tmpStr
    character(len=256) :: suffix
    real :: timerStart, timerEnd

    
    
    type :: Var
        character(len=32)  :: name
        character(len=256) :: value
        character(len=8)   :: vartype 
        real(kind=8)       :: numVal = 0.0d0
        logical            :: isNum = .false.
    end type Var
    type :: Marker
        integer :: pos
        character(len=256) :: name
    end type Marker
    type :: ParsedLine
        character(len=64) :: command
        character(len=256) :: tokens(10)
        integer :: ntok
        character(len=4) :: suffix
        integer :: listIndex = -1
        integer :: cmdId = OP_NONE
    end type ParsedLine
    


    type(Var), allocatable    :: Vars(:)
    type(Marker), allocatable :: Markers(:)
    type(ParsedLine), allocatable :: Parsed(:)
    type(List), allocatable :: Lists(:)
    integer :: VarCount, MarkerCount, ListCount
    character(len=256), allocatable :: fileLines(:)
    integer, allocatable :: usedLines(:)
    

    
    integer :: numLines = 0
    integer :: numLinesCalled = 0
    logical :: foundLine = .false.

    call get_environment_variable("PATH", osSeperator)
    if (index(osSeperator, ";") > 0) then
        osClear = 'cls'
        userOs = 'Windows'
    else
        osClear = 'clear'
        userOs = 'Unix'
    end if

    version = '1.1.2'

    VarCount = 0
    MarkerCount = 0
    ListCount = 0
    allocate(Vars(0))
    allocate(Markers(0))
    allocate(Lists(0))
    allocate(usedLines(0))
    lineNumber = 0




    call get_command_argument(1, fileName)
    call get_command_argument(2, verbose)
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
    if(verbose=='loud') then
        call cpu_time(timerStart)
    end if



    allocate(fileLines(0))
    numLines = 0
    do
        read(1, '(A)', iostat=ios) line
        if (ios /= 0) exit
        numLines = numLines + 1
        call extendArrayStr(fileLines, numLines)
        fileLines(numLines) = trim(line)
    end do
    close(1)

    allocate(Parsed(numLines))

    do lineNumber = 1, numLines
        line = trim(fileLines(lineNumber))

        Parsed(lineNumber)%command = ""
        Parsed(lineNumber)%suffix  = ""
        Parsed(lineNumber)%ntok    = 0
        Parsed(lineNumber)%tokens  = ""
        Parsed(lineNumber)%listIndex = -1
        Parsed(lineNumber)%cmdId   = OP_NONE

        pos1 = index(line, "#/")
        pos2 = index(line, "/#")

        if (pos1 > 0 .and. pos2 > pos1) then
            Parsed(lineNumber)%command = adjustl(trim(line(1:pos1-1)))
            value = adjustl(trim(line(pos1+2:pos2-1)))
            Parsed(lineNumber)%suffix = adjustl(trim(line(pos2+2:pos2+3)))
            call split(value, "|", Parsed(lineNumber)%tokens, Parsed(lineNumber)%ntok)
            do i = 1, Parsed(lineNumber)%ntok
                Parsed(lineNumber)%tokens(i) = trim(Parsed(lineNumber)%tokens(i))
            end do
        end if

        select case (trim(Parsed(lineNumber)%command))
            case ('spew');      Parsed(lineNumber)%cmdId = OP_SPEW
            case ('spewmult');  Parsed(lineNumber)%cmdId = OP_SPEWMULT
            case ('color');     Parsed(lineNumber)%cmdId = OP_COLOR
            case ('create');    Parsed(lineNumber)%cmdId = OP_CREATE
            case ('add');       Parsed(lineNumber)%cmdId = OP_ADD
            case ('sub');       Parsed(lineNumber)%cmdId = OP_SUB
            case ('mult');      Parsed(lineNumber)%cmdId = OP_MULT
            case ('div');       Parsed(lineNumber)%cmdId = OP_DIV
            case ('mark');      Parsed(lineNumber)%cmdId = OP_MARK
            case ('go');        Parsed(lineNumber)%cmdId = OP_GO
            case ('ifgo');      Parsed(lineNumber)%cmdId = OP_IFGO
            case ('ask');       Parsed(lineNumber)%cmdId = OP_ASK
            case ('clear');     Parsed(lineNumber)%cmdId = OP_CLEAR
            case ('open');      Parsed(lineNumber)%cmdId = OP_OPEN
            case ('read');      Parsed(lineNumber)%cmdId = OP_READ
            case ('close');     Parsed(lineNumber)%cmdId = OP_CLOSE
            case ('goback');    Parsed(lineNumber)%cmdId = OP_GOBACK
            case ('str');       Parsed(lineNumber)%cmdId = OP_STR
            case ('type');      Parsed(lineNumber)%cmdId = OP_TYPE
            case ('set');       Parsed(lineNumber)%cmdId = OP_SET
            case ('mod');       Parsed(lineNumber)%cmdId = OP_MOD
            case ('getos');     Parsed(lineNumber)%cmdId = OP_GETOS
            case ('randi');     Parsed(lineNumber)%cmdId = OP_RANDI
            case ('sqrt');      Parsed(lineNumber)%cmdId = OP_SQRT
            case ('list');      Parsed(lineNumber)%cmdId = OP_LIST
            case ('sys');       Parsed(lineNumber)%cmdId = OP_SYS
            case ('cputime');   Parsed(lineNumber)%cmdId = OP_CPUTIME
            case default;       Parsed(lineNumber)%cmdId = OP_NONE
        end select

        if (Parsed(lineNumber)%command == 'list') then
            if (Parsed(lineNumber)%ntok >= 2) then
                Parsed(lineNumber)%listIndex = findList(Lists, ListCount, trim(Parsed(lineNumber)%tokens(2)))
            end if
        end if
    end do

    do lineNumber = 1, numLines
        if (trim(Parsed(lineNumber)%command) == 'mark' .and. Parsed(lineNumber)%ntok == 1) then
            call setMarker(lineNumber, trim(Parsed(lineNumber)%tokens(1)))
        end if
    end do

    lineNumber = 0
    do
        lineNumber = lineNumber + 1
        if (lineNumber > numLines) then
            call cpu_time(timerEnd)
            if (verbose=='loud') print*, 'Execution time: ', timerEnd - timerStart
            exit
        end if

        if (trim(fileLines(lineNumber)) == 'bye') then
            if (verbose=='loud') then
                call cpu_time(timerEnd)
                print*, 'Execution time: ', timerEnd - timerStart, 's'
            end if
            exit
        end if

        if (len_trim(Parsed(lineNumber)%command) > 0) then
            suffix = Parsed(lineNumber)%suffix
            if (suffix == '' .or. suffix == '!') then
                call execute_command(Parsed(lineNumber)%cmdId, Parsed(lineNumber)%tokens, Parsed(lineNumber)%ntok, lineNumber)
            else if (suffix == '?') then
                foundLine = .false.
                do i = 1, numLinesCalled
                    if (usedLines(i) == lineNumber) then
                        foundLine = .true.
                    end if
                end do
                if (.not. foundLine) then
                    numLinesCalled = numLinesCalled + 1
                    call extendArrayLines(usedLines, numLinesCalled)
                    usedLines(numLinesCalled) = lineNumber
                    call execute_command(Parsed(lineNumber)%cmdId, Parsed(lineNumber)%tokens, Parsed(lineNumber)%ntok, lineNumber)
                end if
            end if
        end if
    end do
end if
contains

    subroutine numeric_op(tokens, ntok, op)
        character(len=*), intent(in) :: tokens(:)
        integer, intent(in) :: ntok
        character(len=*), intent(in) :: op
        real(kind=8) :: a, b, res
        integer :: i, ios1, ios2
        logical :: isNumA, isNumB
        character(len=256) :: lhs, rhs, tmp

        if (ntok /= 3) then
            write(*,'(A)') "Error: arithmetic requires 3 tokens: name|lhs|rhs"
            if (suffix=='!') call exit(1)
            return
        end if

        lhs = trim(tokens(2))
        rhs = trim(tokens(3))

        isNumA = .false.; isNumB = .false.; a = 0d0; b = 0d0

        do i = 1, VarCount
            if (lhs == Vars(i)%name .and. Vars(i)%isNum) then
                a = Vars(i)%numVal
                isNumA = .true.
                exit
            end if
        end do

        do i = 1, VarCount
            if (rhs == Vars(i)%name .and. Vars(i)%isNum) then
                b = Vars(i)%numVal
                isNumB = .true.
                exit
            end if
        end do

        if (.not.isNumA) then
            read(lhs,*,iostat=ios1) a
            if (ios1 /= 0) then
                tmp = resolveToken_fast(tokens(2))
                read(tmp,*,iostat=ios1) a
                if (ios1 /= 0) then
                    write(*,'(A)') "Error: non-numeric lhs"
                    if (suffix=='!') call exit(1)
                    return
                end if
            end if
        end if

        if (.not.isNumB) then
            read(rhs,*,iostat=ios2) b
            if (ios2 /= 0) then
                tmp = resolveToken_fast(tokens(3))
                read(tmp,*,iostat=ios2) b
                if (ios2 /= 0) then
                    write(*,'(A)') "Error: non-numeric rhs"
                    if (suffix=='!') call exit(1)
                    return
                end if
            end if
        end if

        select case(op)
        case("+"); res = a + b
        case("-"); res = a - b
        case("*"); res = a * b
        case("/")
            if (b == 0d0) then
                write(*,'(A)') "Error: division by zero"
                if (suffix=='!') call exit(1)
                return
            end if
            res = a / b
        end select

        call setVar(trim(tokens(1)), trim(toStringReal(res)), 'float')
    end subroutine numeric_op

    subroutine jump_to(tokMarker)
        character(len=*), intent(in) :: tokMarker
        integer :: tgt
        if (trim(tokMarker) == '_') return
        tgt = getMarker(trim(tokMarker))
        if (tgt > 0) then
            call pushGo(lineNumber)
            lineNumber = tgt - 1

        else
            write(*,*) "Error: marker not found: ", trim(tokMarker)
            if (suffix=='!') then
                call exit(1)
            end if
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

    subroutine extendArrayLines(arr, newSize)
        integer, allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: newSize
        integer, allocatable :: tmp(:)

        allocate(tmp(newSize))
        if (size(arr)>0) tmp(1:size(arr)) = arr
        call move_alloc(tmp,arr)

    end subroutine extendArrayLines

    subroutine extendArrayMark(arr, newSize)
        type(Marker), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: newSize
        type(Marker), allocatable :: tmp(:)

        allocate(tmp(newSize))
        if (size(arr) > 0) tmp(1:size(arr)) = arr
        call move_alloc(tmp, arr)
    end subroutine extendArrayMark

    subroutine extendArrayList(arr, newSize)
        type(List), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: newSize
        type(List), allocatable :: tmp(:)

        allocate(tmp(newSize))
        if (size(arr) > 0) tmp(1:size(arr)) = arr
        call move_alloc(tmp,arr)
    end subroutine extendArrayList

    
    subroutine setVar(name, value, vartype)
        character(len=*), intent(in) :: name, value
        character(len=*), intent(in), optional :: vartype
        integer :: i, idx, iosn
        real(kind=8) :: tmp
        character(len=8) :: typeToSet
        integer, save :: cacheCount = 0
        character(len=32), save :: cacheNames(4096)
        integer, save :: cacheIdx(4096)

        idx = -1
        do i = 1, cacheCount
            if (trim(name) == cacheNames(i)) then
                idx = cacheIdx(i)
                exit
            end if
        end do

        if (idx > 0 .and. idx <= VarCount) then
            Vars(idx)%value = trim(value)
            if (present(vartype)) Vars(idx)%vartype = trim(vartype)
            read(value, *, iostat=iosn) tmp
            if (iosn == 0) then
                Vars(idx)%numVal = tmp
                Vars(idx)%isNum  = .true.
            else
                Vars(idx)%isNum  = .false.
            end if
            return
        end if

        do i = 1, VarCount
            if (trim(Vars(i)%name) == trim(name)) then
                Vars(i)%value = trim(value)
                if (present(vartype)) Vars(i)%vartype = trim(vartype)
                read(value, *, iostat=iosn) tmp
                if (iosn == 0) then
                    Vars(i)%numVal = tmp
                    Vars(i)%isNum  = .true.
                else
                    Vars(i)%isNum  = .false.
                end if
                cacheCount = cacheCount + 1
                cacheNames(cacheCount) = trim(name)
                cacheIdx(cacheCount)   = i
                return
            end if
        end do

        if (present(vartype)) then
            typeToSet = trim(vartype)
        else
            typeToSet = 'str'
        end if

        VarCount = VarCount + 1
        call extendArrayVar(Vars, VarCount)
        Vars(VarCount)%name = trim(name)
        Vars(VarCount)%value = trim(value)
        Vars(VarCount)%vartype = trim(typeToSet)
        read(value, *, iostat=iosn) tmp
        if (iosn == 0) then
            Vars(VarCount)%numVal = tmp
            Vars(VarCount)%isNum  = .true.
        else
            Vars(VarCount)%isNum  = .false.
        end if

        cacheCount = cacheCount + 1
        cacheNames(cacheCount) = trim(name)
        cacheIdx(cacheCount)   = VarCount
    end subroutine setVar

    function getVarVar(name) result(returnVar)
        character(len=*), intent(in) :: name
        type(Var) :: returnVar
        
        do i =1, VarCount
            if (trim(Vars(i)%name) == trim(name)) then
                returnVar = Vars(i)
            end if
        end do
    end function getVarVar

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

    pure function toStringReal(x) result(s)
        real(kind=8), intent(in) :: x
        character(len=64) :: s
        write(s,'(G0)') x
    end function toStringReal


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
        character(len=256) :: trimmed, base
        integer :: idx, listIdx, varIdx
        logical :: ok
        integer :: j

      
        integer, save :: listCacheCount = 0, varCacheCount = 0
        character(len=64), save :: listCacheNames(1024), varCacheNames(2048)
        integer, save :: listCacheIdx(1024), varCacheIdx(2048)

        trimmed = trim(tok)

       
        if (len_trim(trimmed) >= 2) then
            if (trimmed(1:1) == "'" .and. trimmed(len_trim(trimmed):len_trim(trimmed)) == "'") then
                res = trimmed(2:len_trim(trimmed)-1)
                return
            end if
        end if

       
        ok = try_parse_indexed_token(trimmed, base, idx)
        if (ok) then
            
            listIdx = -1
            do j = 1, listCacheCount
                if (trim(base) == listCacheNames(j)) then
                    listIdx = listCacheIdx(j)
                    exit
                end if
            end do
            if (listIdx < 1) then
                listIdx = findList(Lists, ListCount, trim(base))
                if (listIdx > 0) then
                    listCacheCount = listCacheCount + 1
                    listCacheNames(listCacheCount) = trim(base)
                    listCacheIdx(listCacheCount) = listIdx
                end if
            end if
            if (listIdx > 0) then
                res = listGetIdx(Lists, listIdx, idx)
            else
                res = 'undefined'
            end if
            return
        end if

       
        varIdx = -1
        do j = 1, varCacheCount
            if (trimmed == varCacheNames(j)) then
                varIdx = varCacheIdx(j)
                exit
            end if
        end do
        if (varIdx > 0) then
            res = Vars(varIdx)%value
            return
        end if

        do j = 1, VarCount
            if (trimmed == trim(Vars(j)%name)) then
                res = Vars(j)%value
                varCacheCount = varCacheCount + 1
                varCacheNames(varCacheCount) = trim(Vars(j)%name)
                varCacheIdx(varCacheCount) = j
                return
            end if
        end do

      
        res = trimmed
    end function resolveToken

    function resolveToken_fast(tok) result(res)
        character(len=*), intent(in) :: tok
        character(len=256) :: res
        integer :: i
        character(len=256) :: t

        integer, save :: cacheCount = 0
        character(len=32), save :: cacheNames(2048)
        integer, save :: cacheIdx(2048)

        t = trim(tok)

        if (len_trim(t) >= 2) then
            if (t(1:1) == "'" .and. t(len_trim(t):len_trim(t)) == "'") then
                res = t(2:len_trim(t)-1)
                return
            end if
        end if

        do i = 1, cacheCount
            if (t == cacheNames(i)) then
                res = Vars(cacheIdx(i))%value
                return
            end if
        end do

        do i = 1, VarCount
            if (t == Vars(i)%name) then
                res = Vars(i)%value
                cacheCount = cacheCount + 1
                cacheNames(cacheCount) = t
                cacheIdx(cacheCount)   = i
                return
            end if
        end do

        res = resolveToken(t)
    end function resolveToken_fast



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
                if (suffix=='!') then
                    call exit(1)
                end if
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
            if (suffix=='!') then
                call exit(1)
            end if
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

    function try_parse_indexed_token(tok, base, idx) result(ok)
        character(len=*), intent(in) :: tok
        character(len=256), intent(out) :: base
        integer, intent(out) :: idx
        logical :: ok
        integer :: lb, rb, iostat_local
        character(len=256) :: idxstr, val

        ok = .false.
        base = ""
        idx  = -1

        lb = index(tok, "[")
        if (lb == 0) return
        rb = index(tok, "]")
        if (rb == 0 .or. rb < lb) return

        base   = adjustl(tok(1:lb-1))
        idxstr = adjustl(tok(lb+1:rb-1))

        
        val = getVar(trim(idxstr))
        if (trim(val) == 'undefined') val = trim(idxstr)

        read(val, *, iostat=iostat_local) idx
        ok = (iostat_local == 0 .and. len_trim(base) > 0)
    end function try_parse_indexed_token

    subroutine extendArrayStr(arr, newSize)
        character(len=256), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: newSize
        character(len=256), allocatable :: tmp(:)
        allocate(tmp(newSize))
        if (size(arr) > 0) tmp(1:size(arr)) = arr
        call move_alloc(tmp, arr)
    end subroutine extendArrayStr


    subroutine execute_command(cmdId, tokens, ntok, lineNumber)
        integer, intent(in) :: cmdId
        character(len=256), intent(in) :: tokens(:)
        integer, intent(inout) :: ntok, lineNumber

            select case(trim(suffix))

                case('?')
                    


                
            end select

           select case (cmdId)


            case (OP_SPEW)
                if (ntok >= 1) then
                    do i = 1, ntok
                        write(*,'(A)', advance='no') trim(resolveToken_fast(tokens(i)))//' '
                    end do
                    write(*,*)
                else
                    write(*,'(A)') "Error: spew requires at least one token"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case(OP_SPEWMULT)
                if (ntok >= 1) then
                    do i = 1, ntok
                        write(*,'(A)', advance='no') trim(resolveToken_fast(tokens(i)))
                    end do
                    write(*,*)
                else
                    write(*,'(A)') "Error: spewmult requires at least one token"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case(OP_COLOR)
                call set_color(tokens(1))

            case (OP_CREATE)
                if (ntok >= 2) then
                    if (ntok == 3) then
                        call createVar(trim(tokens(1)), resolveToken_fast(tokens(2)), trim(tokens(3)))
                    else
                        call createVar(trim(tokens(1)), resolveToken_fast(tokens(2)))
                    end if
                else if (ntok == 1) then
                    call createVar(trim(tokens(1)), '', 'str')
                else
                    write(*,'(A)') "Error: create requires 1-3 tokens: name|[value]|[type]"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case (OP_ADD)
                call numeric_op(tokens, ntok, "+")

            case (OP_SUB)
                call numeric_op(tokens, ntok, "-")
            case (OP_MULT)
                call numeric_op(tokens, ntok, "*")
            case(OP_DIV)
                call numeric_op(tokens, ntok, "/")
            case (OP_MARK)
                if (ntok == 1) then
                    call setMarker(lineNumber, trim(tokens(1)))
                else
                    write(*,*) "Error: mark requires exactly 1 token (name)"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case (OP_GO)
                if (ntok == 1) then
                    lineInt = getMarker(trim(tokens(1)))
                    if (lineInt > 0) then
                        call pushGo(lineNumber)
                        lineNumber = lineint - 1
                    else
                        write(*,*) "Error: marker not found: ", trim(tokens(1))
                        if (suffix=='!') then
                            call exit(1)
                        end if
                    end if
                else
                    write(*,*) "Error: go requires exactly 1 token (name)"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case(OP_IFGO)
                if (ntok == 5) then
                    s1 = resolveToken_fast(tokens(1))
                    s2 = resolveToken_fast(tokens(3))
                    if (cmp_values(s1, s2, trim(tokens(2)))) then
                        call jump_to(tokens(4))
                    else
                        call jump_to(tokens(5))
                    end if
                else
                    write(*,*) "Error: ifgo requires 5 tokens (var1|comparison|var2|marker|marker)"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case (OP_ASK)
                if (ntok==2) then
                    write(*,*) trim(resolveToken_fast(tokens(1)))
                    read(*,*) tempRead
                    call setVar(trim(tokens(2)), trim(tempRead))
                else
                    print*,'Error: ask requires 2 tokens: question|var (where the answer is stored)'
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case(OP_CLEAR)
                call system(osClear)

            case (OP_OPEN)
                if (ntok == 2) then
                    s1 = trim(tokens(1))
                    s2 = resolveToken_fast(tokens(2))

                    fileUnit = nextFileUnit
                    nextFileUnit = nextFileUnit + 1

                    open(unit=fileUnit, file=s2, status='old', action='read', iostat=ios)
                    if (ios /= 0) then
                        write(*,*) "Error: cannot open file ", trim(s2)
                        if (suffix=='!') then
                            call exit(1)
                        end if
                        return
                    else
                        call setVar(s1, trim(toString(fileUnit)), 'int')
                    end if
                else
                    write(*,*) "Error: open requires 2 tokens: handleName|filePath"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case (OP_READ)
                if (ntok == 2) then
                    s1 = resolveToken_fast(tokens(1))
                    s2 = trim(tokens(2))

                    read(s1, *, iostat=ios_local) fileUnit
                    if (ios_local /= 0) then
                        write(*,*) "Error: invalid file handle variable"
                        if (suffix=='!') then
                            call exit(1)
                        end if
                        return
                    end if
                    read(fileUnit, '(A)', iostat=ios_local) tempLine
                    if (ios_local /= 0) then
                        call setVar(trim(s2), "", 'str')
                    else
                        call setVar(trim(s2), trim(tempLine), 'str')
                    end if
                else
                    write(*,*) "Error: read requires 2 tokens: handleName|lineVar"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case (OP_CLOSE)
                if (ntok == 1) then
                    s1 = resolveToken_fast(tokens(1))
                    read(s1, *, iostat=ios_local) fileUnit
                    if (ios_local /= 0) then
                        write(*,*) "Error: invalid file handle variable"
                        if (suffix=='!') then
                            call exit(1)
                        end if
                    else
                        close(fileUnit, iostat=ios_local)
                        if (ios_local /= 0) then
                            write(*,*) "Error closing file handle ", trim(s1)
                            if (suffix=='!') then
                                call exit(1)
                            end if
                        end if
                    end if
                else
                    write(*,*) "Error: close requires 1 token: handleName"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case(OP_GOBACK)
                lineInt = popGo()
                if (lineInt > 0) then
                    lineNumber = lineInt - 1
                end if
            case(OP_STR)
                select case(trim(tokens(1)))

                case("cat")
                    if (ntok >= 4) then
                        s1 = resolveToken_fast(tokens(3))
                        s2 = resolveToken_fast(tokens(4))
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
                        if (suffix=='!') then
                            call exit(1)
                        end if
                    end if
                case("rev")
                    if(ntok==3) then
                        s1 = resolveToken_fast(tokens(3))
                        strSave = ''
                        strSave = trim(reverse(trim(s1)))
                        call setVar(trim(tokens(2)),trim(strSave))
                    else
                        write(*,*) "Error: rev requires 2 tokens: var|string"
                        if (suffix=='!') then
                            call exit(1)
                        end if
                    end if
                case("low")
                    if(ntok==3) then
                        s1 = resolveToken_fast(tokens(3))
                        strSave = ''
                        strSave = trim(lower(trim(s1)))
                        call setVar(trim(tokens(2)),trim(strSave))
                    else
                        write(*,*) "Error: low requires 2 tokens: var|string"
                        if (suffix=='!') then
                            call exit(1)
                        end if
                    end if
                case("up")
                    if(ntok==3) then
                        s1 = resolveToken_fast(tokens(3))
                        strSave = ''
                        strSave = trim(upper(trim(s1)))
                        call setVar(trim(tokens(2)),trim(strSave))
                    else
                        write(*,*) "Error: up requires 2 tokens: var|string"
                        if (suffix=='!') then
                            call exit(1)
                        end if
                    end if
                case("len")
                    if(ntok==3) then
                        s1 = trim(tokens(2))
                        s2 = resolveToken_fast(tokens(3))

                        i = len_trim(s2)               
                        write(strSave, '(I0)') i       

                        call setVar(s1, trim(strSave), 'int')
                    else
                        write(*,*) "Error: len requires 2 tokens: var|string "
                        if (suffix=='!') then
                            call exit(1)
                        end if
                    end if
                
                end select
            case(OP_TYPE)
                if(ntok==2) then
                    s1 = trim(tokens(1))
                    s2 = trim(tokens(2))

                    call setVar(s1, getVarType(trim(s2)), 'str') 
                else
                    write(*,*) "Error: type requires 2 tokens: varToStore|var"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if
            case(OP_SET)
                if (ntok == 2) then
                    call modifyVar(trim(tokens(1)), resolveToken_fast(tokens(2)))
                else if (ntok == 3) then
                    call modifyVar(trim(tokens(1)), resolveToken_fast(tokens(2)), trim(tokens(3)))
                else                        
                    write(*,*) "Error: set requires 2 or 3 tokens: var|value|[type]"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if
            case(OP_MOD)
                if(ntok==3) then
                    s1 = trim(tokens(1))
                    s2 = resolveToken_fast(tokens(2))
                    s3 = resolveToken_fast(tokens(3))
                    read(s2,*) a
                    read(s3,*) b

                    finalValInt = modulo(a, b)
                    write(finalValStr, '(I0)') finalValInt

                    call setVar(s1, finalValStr, 'int')
                else
                    write(*,*) "Error: mod requires 3 tokens: var|value to mod|value to mod by"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if
            case(OP_GETOS)
                if(ntok==1) then
                    s1 = trim(tokens(1))

                    call setVar(s1, trim(userOs))
                else 
                    write(*,*) "Error: getos requires 1 token: var"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if
            case(OP_RANDI)
                if (ntok==2) then
                    s1 = trim(tokens(1))
                    s2 = resolveToken_fast(tokens(2))

                    read(s2,*) a
                    call random_number(r)
                    i = floor(r * a)
                    write(s2, '(I0)') i
                    call setVar(s1, s2)
                
            
                else 
                    write(*,*) "Error: randi requires 2 tokens: var|multiplier"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case(OP_SQRT)
                if (ntok==2) then
                    s1 = trim(tokens(1))
                    s2 = resolveToken_fast(tokens(2))
                    
                    read(s2,*) r
                    r = sqrt(r)
                    write(s2, '(F5.3)') r
                    call setVar(trim(s1),trim(s2))
                else
                    write(*,*) "Error: sqrt requires 2 tokens: var|number"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if
            case (OP_LIST)
                if (ntok < 2) then
                    write(*,*) "Error: list requires a subcommand"
                    if (suffix=='!') call exit(1)
                else
                    select case (trim(lower(tokens(1))))

                    case ("create","new")
                        if (ntok /= 2) then
                            write(*,*) "Error: list create|name"
                            if (suffix=='!') call exit(1)
                        else
                            call createList(Lists, ListCount, trim(tokens(2)))
                        end if

                    case ("push")
                        if (ntok /= 3) then
                            write(*,*) "Error: list push|name|value"
                            if (suffix=='!') call exit(1)
                        else
                            if (Parsed(lineNumber)%listIndex < 1) then
                                Parsed(lineNumber)%listIndex = findList(Lists, ListCount, trim(tokens(2)))
                                if (Parsed(lineNumber)%listIndex < 1) then
                                    call createList(Lists, ListCount, trim(tokens(2)))
                                    Parsed(lineNumber)%listIndex = ListCount
                                end if
                            end if

                            call listPushCached(Lists(Parsed(lineNumber)%listIndex), trim(resolveToken_fast(tokens(3))))
                        end if


                    case ("get")
                        if (ntok /= 4) then
                            write(*,*) "Error: list get|outVar|name|index"
                            if (suffix=='!') call exit(1)
                        else
                            tmpStr = resolveToken_fast(tokens(4))
                            read(tmpStr, *, iostat=ios_local) i
                            if (ios_local /= 0) then
                                write(*,*) "Error: index must be integer"
                                if (suffix=='!') call exit(1)
                            else
                                if (Parsed(lineNumber)%listIndex < 1) then
                                    Parsed(lineNumber)%listIndex = findList(Lists, ListCount, trim(tokens(3)))
                                end if
                                if (Parsed(lineNumber)%listIndex > 0) then
                                    call setVar(trim(tokens(2)), listGetIdx(Lists, Parsed(lineNumber)%listIndex, i), 'str')
                                else
                                    write(*,*) "Error: list not found: ", trim(tokens(3))
                                end if

                            end if
                        end if

                    case ("set")
                        if (ntok /= 4) then
                            write(*,*) "Error: list set|name|index|value"
                            if (suffix=='!') call exit(1)
                        else
                            tmpStr = resolveToken_fast(tokens(3))
                            read(tmpStr, *, iostat=ios_local) i
                            if (ios_local /= 0) then
                                write(*,*) "Error: index must be integer"
                                if (suffix=='!') call exit(1)
                            else
                                if (Parsed(lineNumber)%listIndex < 1) then
                                    Parsed(lineNumber)%listIndex = findList(Lists, ListCount, trim(tokens(2)))
                                end if
                                if (Parsed(lineNumber)%listIndex > 0) then
                                    call listSetIdx(Lists, Parsed(lineNumber)%listIndex, i, trim(resolveToken_fast(tokens(4))))
                                else
                                    write(*,*) "Error: list not found: ", trim(tokens(2))
                                end if

                            end if
                        end if

                    case ("len")
                        if (ntok /= 3) then
                            write(*,*) "Error: list len|outVar|name"
                            if (suffix=='!') call exit(1)
                        else
                            write(finalValStr,'(I0)') listLen(Lists, ListCount, trim(tokens(3)))
                            call setVar(trim(tokens(2)), finalValStr, 'int')
                        end if

                    case ("pop")
                        if (ntok /= 3) then
                            write(*,*) "Error: list pop|outVar|name"
                            if (suffix=='!') call exit(1)
                        else
                            call listPop(Lists, ListCount, trim(tokens(3)), tmpStr)
                            call setVar(trim(tokens(2)), trim(tmpStr), 'str')
                        end if

                    case ("clear")
                        if (ntok /= 2) then
                            write(*,*) "Error: list clear|name"
                            if (suffix=='!') call exit(1)
                        else
                            call listClear(Lists, ListCount, trim(tokens(2)))
                        end if
                    case ("reserve")
                    if (ntok /= 3) then
                        write(*,*) "Error: list reserve|name|capacity"
                        if (suffix=='!') call exit(1)
                    else
                        tmpStr = resolveToken_fast(tokens(3))
                        read(tmpStr, *, iostat=ios_local) i
                        if (ios_local /= 0) then
                            write(*,*) "Error: capacity must be an integer"
                            if (suffix=='!') call exit(1)
                        else
                            jdx = findList(Lists, ListCount, trim(tokens(2)))
                            if (jdx > 0) call resizeList(Lists(jdx), i)
                        end if
                    end if

                    case default
                        write(*,*) "Error: unknown list subcommand"
                        if (suffix=='!') call exit(1)
                    end select
                end if

            case(OP_SYS)
                if (ntok==1) then
                    s1 = resolveToken_fast(tokens(1))
                    call system(s1)
                else
                    write(*,*) 'Error: sys requires 1 token: str'
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case(OP_CPUTIME)
                if(ntok==1) then
                    s1= trim(tokens(1))

                    call cpu_time(r)

                    write(s2,'(F5.3)') r

                    call setVar(trim(s1), trim(s2))


                else 

                    write(*,*) 'Error: cputime requires 1 token: var'
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case default
                write(*,'(A)') "Unknown command: "//trim(command)
                if (suffix=='!') then
                    call exit(1)
                end if
            end select
    end subroutine execute_command


end program interpreter
