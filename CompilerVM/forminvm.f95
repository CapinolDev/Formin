program forminvm
    use formin_lists
    use formin_opcodes
    use formin_keyboard

    implicit none
    
    character(len=5) :: version
    integer :: ios
    integer :: nextFileUnit = 20
    integer :: fileUnit
    character(len=256) :: fileName, tempLine, verbose
    type :: Instr
        sequence
        integer :: op
        integer :: ntok
        character(len=256) :: tokens(10)
        character(len=1) :: suffix
    end type Instr
    integer :: a, b, ios_local
    character(len=256) :: s1, s2, s3, tempRead
    integer :: lineInt
    integer :: lineNumber
    character (len=1024) :: osSeperator, strSave
    character (len=5) :: osClear
    integer :: i, finalValInt
    integer :: lastKeyCode = 0
    integer, allocatable :: goStack(:)
    integer :: goDepth = 0
    character(len=256) :: finalValStr
    character(len=256) :: userOs
    real :: r, r2
    integer :: jdx, intVal, subId, skipCount
    character(len=256), allocatable :: tmp(:)
    character(len=256) :: tmpStr
    character(len=256) :: suffix
    real :: timerStart, timerEnd
    character(len=1) :: ch
    
    
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
        character(len=256) :: tokens(10)
        integer :: tokenInternIds(10) = 0
        integer :: ntok
        character(len=4) :: suffix
        integer :: listIndex = -1
        integer :: cmdId = OP_NONE
        integer :: jumpA = 0
        integer :: jumpB = 0
    end type ParsedLine
    


    integer, parameter :: VAR_HASH_SIZE = 16384
    integer, parameter :: INTERN_HASH_SIZE = 65536

    type(Var), allocatable    :: Vars(:)
    type(Marker), allocatable :: Markers(:)
    type(ParsedLine), allocatable :: Parsed(:)
    type(Instr), allocatable :: Program(:)
    type(List), allocatable :: Lists(:)
    integer :: VarCount, MarkerCount, ListCount
    integer :: VarCapacity
    integer, allocatable :: usedLines(:)
    integer, allocatable :: varHash(:)
    character(len=256), allocatable :: internValues(:)
    integer, allocatable :: internHash(:)
    integer :: internCount = 0
    integer :: internCapacity = 0
    character(len=256), allocatable :: stdinQueue(:)
    integer :: stdinQueueCount = 0
    integer :: stdinQueueCapacity = 0
    integer :: suffixSkipCounters(0:255)
    integer :: INTERN_LIST_CREATE, INTERN_LIST_NEW, INTERN_LIST_PUSH, INTERN_LIST_GET
    integer :: INTERN_LIST_SET, INTERN_LIST_LEN, INTERN_LIST_POP, INTERN_LIST_CLEAR
    integer :: INTERN_LIST_RESERVE
    integer :: INTERN_STR_CAT, INTERN_STR_REV, INTERN_STR_LOW, INTERN_STR_UP, INTERN_STR_LEN
    

    
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

    version = '1.1.7'

    VarCount = 0
    MarkerCount = 0
    ListCount = 0
    allocate(Vars(0))
    VarCapacity = 0
    allocate(Markers(0))
    allocate(Lists(0))
    allocate(usedLines(0))
    allocate(varHash(VAR_HASH_SIZE))
    varHash = 0
    call init_intern_pool()
    INTERN_LIST_CREATE = intern_string("create")
    INTERN_LIST_NEW = intern_string("new")
    INTERN_LIST_PUSH = intern_string("push")
    INTERN_LIST_GET = intern_string("get")
    INTERN_LIST_SET = intern_string("set")
    INTERN_LIST_LEN = intern_string("len")
    INTERN_LIST_POP = intern_string("pop")
    INTERN_LIST_CLEAR = intern_string("clear")
    INTERN_LIST_RESERVE = intern_string("reserve")
    INTERN_STR_CAT = intern_string("cat")
    INTERN_STR_REV = intern_string("rev")
    INTERN_STR_LOW = intern_string("low")
    INTERN_STR_UP  = intern_string("up")
    INTERN_STR_LEN = intern_string("len")
    lineNumber = 0
    suffixSkipCounters = 0




    call get_command_argument(1, fileName)
    call get_command_argument(2, verbose)
    if (len_trim(fileName) == 0) then
        write(*,'(A)') "Usage: forminvm <program.fbc>"
        stop
    end if
    if (trim(fileName) == 'ver') then
        write(*,*) "Version ", trim(version)
        stop
    end if

    open(unit=20, file=trim(fileName), form='unformatted', access='stream', iostat=ios)
    if (ios /= 0) then
        write(*,'(A)') "Error opening file!"
        stop
    end if
    read(20) numLines
    allocate(Program(numLines))
    read(20) Program
    close(20)

    if (verbose == 'loud') then
        call cpu_time(timerStart)
    end if

    allocate(Parsed(numLines))

    do lineNumber = 1, numLines
        Parsed(lineNumber)%tokens    = Program(lineNumber)%tokens
        Parsed(lineNumber)%tokenInternIds = 0
        Parsed(lineNumber)%ntok      = Program(lineNumber)%ntok
        Parsed(lineNumber)%suffix    = Program(lineNumber)%suffix
        Parsed(lineNumber)%listIndex = -1
        Parsed(lineNumber)%cmdId     = Program(lineNumber)%op
        if (Parsed(lineNumber)%ntok > 0) then
            do i = 1, Parsed(lineNumber)%ntok
                Parsed(lineNumber)%tokenInternIds(i) = intern_string(Parsed(lineNumber)%tokens(i))
            end do
        end if
    end do

    do lineNumber = 1, numLines
        if (Parsed(lineNumber)%cmdId == OP_MARK .and. Parsed(lineNumber)%ntok == 1) then
            call setMarker(lineNumber, trim(Parsed(lineNumber)%tokens(1)))
        end if
    end do

    do lineNumber = 1, numLines
        select case (Parsed(lineNumber)%cmdId)
        case (OP_GO)
            if (Parsed(lineNumber)%ntok >= 1) then
                Parsed(lineNumber)%jumpA = getMarker(trim(Parsed(lineNumber)%tokens(1)))
                if (Parsed(lineNumber)%jumpA <= 0) then
                    write(*,*) "Warning: unknown marker in GO: ", trim(Parsed(lineNumber)%tokens(1))
                end if
            end if
        case (OP_IFGO)
            if (Parsed(lineNumber)%ntok >= 5) then
                if (trim(Parsed(lineNumber)%tokens(4)) /= '_') then
                    Parsed(lineNumber)%jumpA = getMarker(trim(Parsed(lineNumber)%tokens(4)))
                    if (Parsed(lineNumber)%jumpA <= 0) then
                        write(*,*) "Warning: unknown marker in IFGO true branch: ", trim(Parsed(lineNumber)%tokens(4))
                    end if
                else
                    Parsed(lineNumber)%jumpA = 0
                end if
                if (trim(Parsed(lineNumber)%tokens(5)) /= '_') then
                    Parsed(lineNumber)%jumpB = getMarker(trim(Parsed(lineNumber)%tokens(5)))
                    if (Parsed(lineNumber)%jumpB <= 0) then
                        write(*,*) "Warning: unknown marker in IFGO false branch: ", trim(Parsed(lineNumber)%tokens(5))
                    end if
                else
                    Parsed(lineNumber)%jumpB = 0
                end if
            end if
        case default
            cycle
        end select
    end do

    lineNumber = 0
    do
        lineNumber = lineNumber + 1
        if (lineNumber > numLines) then
            call cpu_time(timerEnd)
            if (verbose=='loud') print*, 'Execution time: ', timerEnd - timerStart
            exit
        end if

        if (Parsed(lineNumber)%cmdId /= OP_NONE) then
            suffix = Parsed(lineNumber)%suffix
            if (should_skip_suffix_line(suffix)) cycle
               
            if (trim(suffix) == '?') then
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
            else
                 call execute_command(Parsed(lineNumber)%cmdId, Parsed(lineNumber)%tokens, Parsed(lineNumber)%ntok, lineNumber)
            end if

        end if
    end do
contains

    subroutine numeric_op(tokens, ntok, op)
        character(len=*), intent(in) :: tokens(:)
        integer, intent(in) :: ntok
        character(len=*), intent(in) :: op
        real(kind=8) :: a, b, res

        if (ntok /= 3) then
            write(*,'(A)') "Error: arithmetic requires 3 tokens: name|lhs|rhs"
            if (suffix=='!') call exit(1)
            return
        end if

        if (.not. getNumericValueFast(tokens(2), a)) then
            write(*,'(A)') "Error: non-numeric lhs"
            if (suffix=='!') call exit(1)
            return
        end if

        if (.not. getNumericValueFast(tokens(3), b)) then
            write(*,'(A)') "Error: non-numeric rhs"
            if (suffix=='!') call exit(1)
            return
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

        call setVarNumericFast(trim(tokens(1)), res)
    end subroutine numeric_op

    subroutine enqueue_stdin(value)
        character(len=*), intent(in) :: value

        call ensure_stdin_capacity(stdinQueueCount + 1)
        stdinQueueCount = stdinQueueCount + 1
        stdinQueue(stdinQueueCount) = trim(value)
    end subroutine enqueue_stdin

    subroutine ensure_stdin_capacity(required)
        integer, intent(in) :: required
        character(len=256), allocatable :: tmp(:)
        integer :: newCap

        if (stdinQueueCapacity >= required) return

        newCap = max(8, stdinQueueCapacity)
        if (newCap <= 0) newCap = 8
        do while (newCap < required)
            newCap = newCap * 2
        end do

        allocate(tmp(newCap))
        if (stdinQueueCount > 0 .and. allocated(stdinQueue)) then
            tmp(1:stdinQueueCount) = stdinQueue(1:stdinQueueCount)
        end if
        call move_alloc(tmp, stdinQueue)
        stdinQueueCapacity = newCap
    end subroutine ensure_stdin_capacity

    subroutine run_command_with_ins(command)
        character(len=*), intent(in) :: command
        character(len=512) :: tmpFile
        character(len=1024) :: pipedCommand
        integer :: tmpUnit, ios_local_tmp, idx

        call build_temp_input_path(tmpFile)
        open(newunit=tmpUnit, file=trim(tmpFile), status='replace', action='write', iostat=ios_local_tmp)
        if (ios_local_tmp /= 0) then
            write(*,*) "Warning: unable to prepare ins input; running sys without injected stdin."
            call system(trim(command))
            call clear_stdin_queue()
            return
        end if

        do idx = 1, stdinQueueCount
            write(tmpUnit,'(A)') trim(stdinQueue(idx))
        end do
        close(tmpUnit)

        pipedCommand = trim(command)//' < "'//trim(tmpFile)//'"'
        call system(trim(pipedCommand))

        call delete_temp_file(trim(tmpFile))
        call clear_stdin_queue()
    end subroutine run_command_with_ins

    subroutine build_temp_input_path(path)
        character(len=*), intent(out) :: path
        character(len=256) :: tmpDir
        character(len=1) :: sepChar
        integer :: clockVal
        character(len=64) :: suffix

        tmpDir = ''
        call get_environment_variable('TMPDIR', tmpDir)
        if (len_trim(tmpDir) == 0) then
            if (trim(userOs) == 'Windows') then
                call get_environment_variable('TEMP', tmpDir)
                if (len_trim(tmpDir) == 0) call get_environment_variable('TMP', tmpDir)
                if (len_trim(tmpDir) == 0) tmpDir = '.'
            else
                tmpDir = '/tmp'
            end if
        end if

        sepChar = '/'
        if (trim(userOs) == 'Windows') sepChar = '\'
        if (len_trim(tmpDir) == 0) then
            tmpDir = '.'//sepChar
        else
            if (tmpDir(len_trim(tmpDir):len_trim(tmpDir)) /= sepChar) then
                tmpDir = trim(tmpDir)//sepChar
            end if
        end if

        call system_clock(clockVal)
        write(suffix,'(I0)') abs(clockVal)
        path = trim(tmpDir)//'formin_ins_'//trim(suffix)//'.tmp'
    end subroutine build_temp_input_path

    subroutine delete_temp_file(path)
        character(len=*), intent(in) :: path
        logical :: exists
        integer :: tmpUnit, ios_local_tmp

        inquire(file=trim(path), exist=exists)
        if (.not. exists) return

        open(newunit=tmpUnit, file=trim(path), status='old', action='readwrite', iostat=ios_local_tmp)
        if (ios_local_tmp == 0) then
            close(tmpUnit, status='delete')
        else
            if (trim(userOs) == 'Windows') then
                call system('del "'//trim(path)//'"')
            else
                call system('rm -f "'//trim(path)//'"')
            end if
        end if
    end subroutine delete_temp_file

    subroutine clear_stdin_queue()
        stdinQueueCount = 0
    end subroutine clear_stdin_queue

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

    subroutine jump_to_pc(tgt)
        integer, intent(in) :: tgt
        if (tgt <= 0) then
            write(*,*) "Error: invalid jump target"
            if (suffix=='!') then
                call exit(1)
            end if
            return
        end if
        call pushGo(lineNumber)
        lineNumber = tgt - 1
    end subroutine jump_to_pc

    function toString(i) result(s)
        integer, intent(in) :: i
        character(len=256) :: s
        write(s, '(I0)') i
    end function toString

    subroutine extendArrayVar(arr, newSize)
        type(Var), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: newSize
        type(Var), allocatable :: tmp(:)
        integer :: newCap, copyCount, curCap

        if (allocated(arr)) then
            curCap = size(arr)
        else
            curCap = 0
        end if

        if (newSize <= VarCapacity) return
        newCap = max(8, VarCapacity)
        if (newCap <= 0) newCap = 8
        do while (newCap < newSize)
            newCap = newCap * 2
        end do
        allocate(tmp(newCap))
        copyCount = min(curCap, max(VarCount-1, 0))
        if (copyCount > 0) tmp(1:copyCount) = arr(1:copyCount)
        call move_alloc(tmp, arr)
        VarCapacity = newCap
    end subroutine extendArrayVar

    subroutine extendArrayLines(arr, newSize)
        integer, allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: newSize
        integer, allocatable :: tmp(:)
        integer :: curCap, newCap

        if (allocated(arr)) then
            curCap = size(arr)
        else
            curCap = 0
        end if
        if (newSize <= curCap) return
        newCap = max(8, curCap)
        if (newCap <= 0) newCap = 8
        do while (newCap < newSize)
            newCap = newCap * 2
        end do
        allocate(tmp(newCap))
        if (curCap > 0) tmp(1:curCap) = arr(1:curCap)
        call move_alloc(tmp,arr)
    end subroutine extendArrayLines

    subroutine extendArrayMark(arr, newSize)
        type(Marker), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: newSize
        type(Marker), allocatable :: tmp(:)
        integer :: curCap, newCap

        if (allocated(arr)) then
            curCap = size(arr)
        else
            curCap = 0
        end if
        if (newSize <= curCap) return
        newCap = max(8, curCap)
        if (newCap <= 0) newCap = 8
        do while (newCap < newSize)
            newCap = newCap * 2
        end do
        allocate(tmp(newCap))
        if (curCap > 0) tmp(1:curCap) = arr(1:curCap)
        call move_alloc(tmp, arr)
    end subroutine extendArrayMark

    subroutine extendArrayList(arr, newSize)
        type(List), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: newSize
        type(List), allocatable :: tmp(:)
        integer :: curCap, newCap

        if (allocated(arr)) then
            curCap = size(arr)
        else
            curCap = 0
        end if
        if (newSize <= curCap) return
        newCap = max(8, curCap)
        if (newCap <= 0) newCap = 8
        do while (newCap < newSize)
            newCap = newCap * 2
        end do
        allocate(tmp(newCap))
        if (curCap > 0) tmp(1:curCap) = arr(1:curCap)
        call move_alloc(tmp,arr)
    end subroutine extendArrayList

    
    subroutine setVar(name, value, vartype)
        character(len=*), intent(in) :: name, value
        character(len=*), intent(in), optional :: vartype
        integer :: idx, iosn
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
            call insertVarHash(trim(name), idx)
            return
        end if

        i = findVarIdx(name)
        if (i > 0 .and. i <= VarCount) then
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
            call insertVarHash(trim(name), i)
            return
        end if

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
        call insertVarHash(trim(name), VarCount)
    end subroutine setVar

    function getVarVar(name) result(returnVar)
        character(len=*), intent(in) :: name
        type(Var) :: returnVar
        integer :: idx

        idx = findVarIdx(name)
        if (idx > 0) returnVar = Vars(idx)
    end function getVarVar

    function getVar(name) result(val)
        character(len=*), intent(in) :: name
        character(len=256) :: val
        integer :: idx

        val = "undefined"
        idx = findVarIdx(name)
        if (idx > 0) val = Vars(idx)%value
    end function getVar

    function getVarType(name) result(vtype)
        character(len=*), intent(in) :: name
        character(len=8) :: vtype
        integer :: idx

        vtype = "undefined"
        idx = findVarIdx(name)
        if (idx > 0) vtype = Vars(idx)%vartype
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

        varIdx = findVarIdx(trimmed)
        if (varIdx > 0) then
            res = Vars(varIdx)%value
            varCacheCount = varCacheCount + 1
            varCacheNames(varCacheCount) = trimmed
            varCacheIdx(varCacheCount) = varIdx
            return
        end if

      
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

        i = findVarIdx(t)
        if (i > 0) then
            res = Vars(i)%value
            cacheCount = cacheCount + 1
            cacheNames(cacheCount) = t
            cacheIdx(cacheCount)   = i
            return
        end if

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

    logical function should_skip_suffix_line(rawSuffix)
        character(len=*), intent(in) :: rawSuffix
        integer :: idx, code, lb, ub
        character(len=1) :: ch
        logical :: found

        should_skip_suffix_line = .false.
        ch = achar(0)
        found = .false.

        do idx = 1, len(rawSuffix)
            if (rawSuffix(idx:idx) /= ' ') then
                ch = rawSuffix(idx:idx)
                found = .true.
                exit
            end if
        end do

        if (.not. found) return

        lb = lbound(suffixSkipCounters,1)
        ub = ubound(suffixSkipCounters,1)
        code = iachar(ch)

        if (code < lb .or. code > ub) return

        if (suffixSkipCounters(code) > 0) then
            suffixSkipCounters(code) = suffixSkipCounters(code) - 1
            should_skip_suffix_line = .true.
        end if
    end function should_skip_suffix_line

    logical function register_suffix_skips(mask, count)
        character(len=*), intent(in) :: mask
        integer, intent(in) :: count
        integer :: idx, code, lb, ub, limit
        logical :: seen(0:255)
        character(len=1) :: ch

        register_suffix_skips = .false.
        if (count <= 0) return

        limit = len_trim(mask)
        if (limit <= 0) return

        lb = lbound(suffixSkipCounters,1)
        ub = ubound(suffixSkipCounters,1)
        seen = .false.

        do idx = 1, limit
            ch = mask(idx:idx)
            if (ch == ' ') cycle
            code = iachar(ch)
            if (code < lb .or. code > ub) cycle
            if (seen(code)) cycle
            suffixSkipCounters(code) = suffixSkipCounters(code) + count
            seen(code) = .true.
            register_suffix_skips = .true.
        end do
    end function register_suffix_skips

    logical function parse_int_string(str, value)
        character(len=*), intent(in) :: str
        integer, intent(out) :: value
        integer :: ios_local
        real(kind=8) :: tmpReal

        parse_int_string = .false.
        read(str, *, iostat=ios_local) value
        if (ios_local == 0) then
            parse_int_string = .true.
            return
        end if

        read(str, *, iostat=ios_local) tmpReal
        if (ios_local == 0) then
            value = int(tmpReal)
            parse_int_string = .true.
        end if
    end function parse_int_string

    subroutine init_intern_pool()
        internCount = 0
        internCapacity = 0
        allocate(internValues(0))
        allocate(internHash(INTERN_HASH_SIZE))
        internHash = 0
    end subroutine init_intern_pool

    subroutine ensureInternCapacity(newSize)
        integer, intent(in) :: newSize
        character(len=256), allocatable :: tmp(:)
        integer :: newCap

        if (newSize <= internCapacity) return
        newCap = max(8, internCapacity)
        if (newCap <= 0) newCap = 8
        do while (newCap < newSize)
            newCap = newCap * 2
        end do
        allocate(tmp(newCap))
        if (internCapacity > 0) tmp(1:internCount) = internValues(1:internCount)
        call move_alloc(tmp, internValues)
        internCapacity = newCap
    end subroutine ensureInternCapacity

    pure integer function hashInternString(str) result(bucket)
        character(len=*), intent(in) :: str
        integer :: i, c, h, L
        character(len=256) :: trimmed

        trimmed = trim(str)
        h = 5381
        L = len_trim(trimmed)
        do i = 1, L
            c = iachar(trimmed(i:i))
            h = ishft(h, 5) + h + c
        end do
        if (h < 0) h = -h
        bucket = modulo(h, INTERN_HASH_SIZE) + 1
    end function hashInternString

    integer function intern_string(str) result(id)
        character(len=*), intent(in) :: str
        character(len=256) :: trimmed
        integer :: bucket, steps

        trimmed = adjustl(trim(str))
        if (len_trim(trimmed) == 0) then
            id = 0
            return
        end if

        if (.not.allocated(internHash)) call init_intern_pool()

        bucket = hashInternString(trimmed)
        steps = 0
        do while (steps < INTERN_HASH_SIZE)
            if (internHash(bucket) == 0) then
                internCount = internCount + 1
                call ensureInternCapacity(internCount)
                internValues(internCount) = trimmed
                internHash(bucket) = internCount
                id = internCount
                return
            else if (internValues(internHash(bucket)) == trimmed) then
                id = internHash(bucket)
                return
            end if
            bucket = bucket + 1
            if (bucket > INTERN_HASH_SIZE) bucket = 1
            steps = steps + 1
        end do
        id = 0
    end function intern_string

    function getInternString(id) result(str)
        integer, intent(in) :: id
        character(len=256) :: str
        if (id >= 1 .and. id <= internCount) then
            str = internValues(id)
        else
            str = ""
        end if
    end function getInternString

    pure integer function hashVarName(name) result(bucket)
        character(len=*), intent(in) :: name
        integer :: i, c, h, L
        character(len=256) :: trimmed

        trimmed = trim(name)
        h = 5381
        L = len_trim(trimmed)
        do i = 1, L
            c = iachar(trimmed(i:i))
            h = ishft(h, 5) + h + c
        end do
        if (h < 0) h = -h
        bucket = modulo(h, VAR_HASH_SIZE) + 1
    end function hashVarName

    subroutine insertVarHash(name, idx)
        character(len=*), intent(in) :: name
        integer, intent(in) :: idx
        integer :: bucket, steps
        character(len=256) :: trimmed

        if (.not.allocated(varHash)) return
        trimmed = trim(name)
        if (len_trim(trimmed) == 0) return
        bucket = hashVarName(trimmed)
        steps = 0
        do while (steps < VAR_HASH_SIZE)
            if (varHash(bucket) == 0) then
                varHash(bucket) = idx
                return
            else if (trim(Vars(varHash(bucket))%name) == trimmed) then
                varHash(bucket) = idx
                return
            end if
            bucket = bucket + 1
            if (bucket > VAR_HASH_SIZE) bucket = 1
            steps = steps + 1
        end do
    end subroutine insertVarHash

    integer function findVarIdx(name) result(idx)
        character(len=*), intent(in) :: name
        integer :: bucket, steps, existing
        character(len=256) :: trimmed

        idx = 0
        if (.not.allocated(varHash)) return
        trimmed = trim(name)
        if (len_trim(trimmed) == 0) return
        bucket = hashVarName(trimmed)
        steps = 0
        do while (steps < VAR_HASH_SIZE)
            existing = varHash(bucket)
            if (existing == 0) return
            if (trim(Vars(existing)%name) == trimmed) then
                idx = existing
                return
            end if
            bucket = bucket + 1
            if (bucket > VAR_HASH_SIZE) bucket = 1
            steps = steps + 1
        end do
    end function findVarIdx

    integer function ensureVarIdx(name) result(idx)
        character(len=*), intent(in) :: name
        character(len=256) :: trimmed

        trimmed = trim(name)
        idx = findVarIdx(trimmed)
        if (idx == 0) then
            VarCount = VarCount + 1
            call extendArrayVar(Vars, VarCount)
            idx = VarCount
            Vars(idx)%name = trimmed
            Vars(idx)%value = ''
            Vars(idx)%vartype = 'str'
            Vars(idx)%numVal = 0.0d0
            Vars(idx)%isNum  = .false.
            call insertVarHash(trimmed, idx)
        end if
    end function ensureVarIdx

    subroutine setVarNumericFast(name, value)
        character(len=*), intent(in) :: name
        real(kind=8), intent(in) :: value
        character(len=256) :: buf
        integer :: idx

        idx = ensureVarIdx(name)
        write(buf, '(G0)') value
        buf = adjustl(buf)
        Vars(idx)%value = trim(buf)
        Vars(idx)%vartype = 'float'
        Vars(idx)%numVal = value
        Vars(idx)%isNum  = .true.
    end subroutine setVarNumericFast

    logical function getNumericValueFast(tok, value)
        character(len=*), intent(in) :: tok
        real(kind=8), intent(out) :: value
        integer :: idx
        character(len=256) :: resolved

        idx = findVarIdx(trim(tok))
        if (idx > 0 .and. Vars(idx)%isNum) then
            value = Vars(idx)%numVal
            getNumericValueFast = .true.
            return
        end if
        resolved = resolveToken_fast(tok)
        getNumericValueFast = try_parse_real(resolved, value)
    end function getNumericValueFast

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
        integer :: existing

        existing = findVarIdx(name)
        if (existing > 0) then
            write(*,*) "Error: variable already exists: ", trim(name)
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
    end subroutine createVar


    subroutine modifyVar(name, value, vartype)
        character(len=*), intent(in) :: name, value
        character(len=*), intent(in), optional :: vartype
        integer :: idx

        idx = findVarIdx(name)
        if (idx == 0) then
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

        
        iostat_local = findVarIdx(trim(idxstr))
        if (iostat_local > 0) then
            val = Vars(iostat_local)%value
        else
            val = trim(idxstr)
        end if

        if (.not. parse_int_string(trim(val), idx)) then
            ok = .false.
            return
        end if
        ok = (len_trim(base) > 0)
    end function try_parse_indexed_token

    subroutine execute_command(cmdId, tokens, ntok, lineNumber)
        use formin_opcodes, only: OP_SPEW, OP_SPEWMULT, OP_COLOR, OP_CREATE, &
            OP_ADD, OP_SUB, OP_MULT, OP_DIV, OP_MARK, OP_GO, OP_IFGO, OP_ASK, &
            OP_CLEAR, OP_OPEN, OP_READ, OP_CLOSE, OP_GOBACK, OP_STR, OP_TYPE, &
            OP_SET, OP_MOD, OP_GETOS, OP_RANDI, OP_SQRT, OP_LIST, OP_SYS, &
            OP_CPUTIME, OP_INS, OP_KEY, OP_IFSKIP, OP_ACHAR
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
            case(OP_SYS)
                if (ntok==1) then
                    s1 = resolveToken_fast(trim(tokens(1)))
                    if (stdinQueueCount > 0) then
                        call run_command_with_ins(trim(s1))
                    else
                        call system(trim(s1))
                    end if

                else
                    write(*,*) 'Error: sys requires 1 token: str'
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
                    lineInt = Parsed(lineNumber)%jumpA
                    if (lineInt <= 0) lineInt = getMarker(trim(tokens(1)))
                    if (lineInt > 0) then
                        call jump_to_pc(lineInt)
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
                        lineInt = Parsed(lineNumber)%jumpA
                        if (lineInt <= 0 .and. trim(tokens(4)) /= '_') lineInt = getMarker(trim(tokens(4)))
                    else
                        lineInt = Parsed(lineNumber)%jumpB
                        if (lineInt <= 0 .and. trim(tokens(5)) /= '_') lineInt = getMarker(trim(tokens(5)))
                    end if
                    if (lineInt > 0) call jump_to_pc(lineInt)
                else
                    write(*,*) "Error: ifgo requires 5 tokens (var1|comparison|var2|marker|marker)"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if
            case (OP_IFSKIP)
                if (ntok == 5) then
                    s1 = resolveToken_fast(tokens(1))
                    s2 = resolveToken_fast(tokens(3))
                    if (cmp_values(s1, s2, trim(tokens(2)))) then
                        s3 = resolveToken_fast(tokens(4))
                        tempRead = resolveToken_fast(tokens(5))
                        read(tempRead, *, iostat=ios_local) skipCount
                        if (ios_local /= 0 .or. skipCount <= 0) then
                            write(*,*) "Error: ifskip requires a positive numeric skip count"
                            if (suffix=='!') then
                                call exit(1)
                            end if
                        else
                            if (.not. register_suffix_skips(s3, skipCount)) then
                                write(*,*) "Error: ifskip could not find any suffix characters to skip"
                                if (suffix=='!') then
                                    call exit(1)
                                end if
                            end if
                        end if
                    end if
                else
                    write(*,*) "Error: ifskip requires 5 tokens: lhs|op|rhs|suffixes|count"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case (OP_ASK)
                if (ntok==2) then
                    write(*,*) trim(resolveToken_fast(tokens(1)))
                    tempRead = ''
                    read(*,'(A)', iostat=ios_local) tempRead
                    if (ios_local /= 0) then
                        if (ios_local < 0) then
                            write(*,*) "Warning: ask reached end of input; terminating program."
                            call setVar(trim(tokens(2)), '')
                            if (suffix=='!') then
                                call exit(1)
                            end if
                            lineNumber = numLines
                            return
                        else
                            write(*,*) "Error: ask failed to read input."
                            if (suffix=='!') then
                                call exit(1)
                            end if
                            call setVar(trim(tokens(2)), '')
                        end if
                    else
                        call setVar(trim(tokens(2)), trim(tempRead))
                    end if
                else
                    print*,'Error: ask requires 2 tokens: question|var (where the answer is stored)'
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if
            case (OP_KEY)
                if (ntok == 1) then
                    intVal = key_pressed()
                    if (intVal > 0) then
                        lastKeyCode = intVal
                    else if (lastKeyCode > 0) then
                        if (.not. key_held(lastKeyCode)) lastKeyCode = 0
                    end if
                    call setVar(trim(tokens(1)), trim(toString(lastKeyCode)), 'int')
                else
                    write(*,*) "Error: key requires 1 token: var"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if
            case (OP_INS)
                if (ntok >= 1) then
                    do i = 1, ntok
                        s1 = trim(resolveToken_fast(tokens(i)))
                        call enqueue_stdin(s1)
                    end do
                else
                    write(*,*) "Error: ins requires at least 1 token: value"
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
                subId = Parsed(lineNumber)%tokenInternIds(1)
                if (subId == INTERN_STR_CAT) then
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
                else if (subId == INTERN_STR_REV) then
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
                else if (subId == INTERN_STR_LOW) then
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
                else if (subId == INTERN_STR_UP) then
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
                else if (subId == INTERN_STR_LEN) then
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
                else
                    write(*,*) "Error: str requires a valid subcommand"
                    if (suffix=='!') call exit(1)
                end if
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
                    write(s2, '(G0)') r
                    call setVar(trim(s1),trim(s2))
                else
                    write(*,*) "Error: sqrt requires 2 tokens: var|number"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if
            case(OP_POW)
                if (ntok==3) then
                    s1 = trim(tokens(1))
                    s2 = resolveToken_fast(tokens(2))
                    s3 = resolveToken_fast(tokens(3))

                    read(s2,*) r
                    read(s3, *) r2
                    r = r**r2
                    write(s2, '(G0)') r
                    call setVar (trim(s1), trim(s2))
                else
                    write(*,*) "Error: pow requires 3 tokens: var|number1|number2"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case(OP_SIN)
                if (ntok==3) then
                    s1 = trim(tokens(1))
                    s2 = trim(tokens(2))
                    s3 = resolveToken_fast(tokens(3))

                    read(s3,*) r
                    if (s2 == 'dg') then
                        r = r * (ACOS(-1.0)/180.0)
                        r = sin(r)
                        write(s2, '(G0)') r 
                    else if (s2=='rad') then
                        r = sin(r)
                        write(s2, '(G0)') r 
                    else 
                        write(*,*) "Error: unknown rad/dg type"
                        if (suffix=='!') then
                            call exit(1)
                        end if
                    end if
                    
                    call setVar (trim(s1), trim(s2))

                else
                    write(*,*) "Error: sin requires 3 tokens: var|dg/rad|number"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case(OP_COS)
                if (ntok==3) then
                    s1 = trim(tokens(1))
                    s2 = trim(tokens(2))
                    s3 = resolveToken_fast(tokens(3))

                    read(s3,*) r
                    if (s2 == 'dg') then
                        r = r * (ACOS(-1.0)/180.0)
                        r = cos(r)
                        write(s2, '(G0)') r 
                    else if (s2=='rad') then
                        r = cos(r)
                        write(s2, '(G0)') r 
                    else 
                        write(*,*) "Error: unknown rad/dg type"
                        if (suffix=='!') then
                            call exit(1)
                        end if
                    end if
                    
                    call setVar (trim(s1), trim(s2))

                else
                    write(*,*) "Error: cos requires 3 tokens: var|dg/rad|number"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case(OP_TAN)
                if (ntok==3) then
                    s1 = trim(tokens(1))
                    s2 = trim(tokens(2))
                    s3 = resolveToken_fast(tokens(3))

                    read(s3,*) r
                    if (s2 == 'dg') then
                        r = r * (ACOS(-1.0)/180.0)
                        r = tan(r)
                        write(s2, '(G0)') r 
                    else if (s2=='rad') then
                        r = tan(r)
                        write(s2, '(G0)') r 
                    else 
                        write(*,*) "Error: unknown rad/dg type"
                        if (suffix=='!') then
                            call exit(1)
                        end if
                    end if
                    
                    call setVar (trim(s1), trim(s2))

                else
                    write(*,*) "Error: tan requires 3 tokens: var|dg/rad|number"
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if

            case (OP_LIST)
                if (ntok < 2) then
                    write(*,*) "Error: list requires a subcommand"
                    if (suffix=='!') call exit(1)
                else
                    subId = Parsed(lineNumber)%tokenInternIds(1)
                    if (subId == INTERN_LIST_CREATE .or. subId == INTERN_LIST_NEW) then
                        if (ntok /= 2) then
                            write(*,*) "Error: list create|name"
                            if (suffix=='!') call exit(1)
                        else
                            call createList(Lists, ListCount, trim(tokens(2)))
                        end if

                    else if (subId == INTERN_LIST_PUSH) then
                        if (ntok /= 3) then
                            write(*,*) "Error: list push|name|value"
                            if (suffix=='!') call exit(1)
                        else
                            if (Parsed(lineNumber)%listIndex < 1) then
                                Parsed(lineNumber)%listIndex = findList(Lists, ListCount, resolveToken_fast(tokens(2)))
                                if (Parsed(lineNumber)%listIndex < 1) then
                                    call createList(Lists, ListCount, trim(tokens(2)))
                                    Parsed(lineNumber)%listIndex = ListCount
                                end if
                            end if

                            call listPushCached(Lists(Parsed(lineNumber)%listIndex), trim(resolveToken_fast(tokens(3))))
                        end if


                    else if (subId == INTERN_LIST_GET) then
                        if (ntok /= 4) then
                            write(*,*) "Error: list get|outVar|name|index"
                            if (suffix=='!') call exit(1)
                        else
                        tmpStr = resolveToken_fast(tokens(4))
                        if (.not. parse_int_string(tmpStr, i)) then
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

                    else if (subId == INTERN_LIST_SET) then
                        if (ntok /= 4) then
                            write(*,*) "Error: list set|name|index|value"
                            if (suffix=='!') call exit(1)
                    else
                        tmpStr = resolveToken_fast(tokens(3))
                        if (.not. parse_int_string(tmpStr, i)) then
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

                    else if (subId == INTERN_LIST_LEN) then
                        if (ntok /= 3) then
                            write(*,*) "Error: list len|outVar|name"
                            if (suffix=='!') call exit(1)
                        else
                            write(finalValStr,'(I0)') listLen(Lists, ListCount, trim(tokens(3)))
                            call setVar(trim(tokens(2)), finalValStr, 'int')
                        end if

                    else if (subId == INTERN_LIST_POP) then
                        if (ntok /= 3) then
                            write(*,*) "Error: list pop|outVar|name"
                            if (suffix=='!') call exit(1)
                        else
                            call listPop(Lists, ListCount, trim(tokens(3)), tmpStr)
                            call setVar(trim(tokens(2)), trim(tmpStr), 'str')
                        end if

                    else if (subId == INTERN_LIST_CLEAR) then
                        if (ntok /= 2) then
                            write(*,*) "Error: list clear|name"
                            if (suffix=='!') call exit(1)
                        else
                            call listClear(Lists, ListCount, trim(tokens(2)))
                        end if
                    else if (subId == INTERN_LIST_RESERVE) then
                    if (ntok /= 3) then
                        write(*,*) "Error: list reserve|name|capacity"
                        if (suffix=='!') call exit(1)
                    else
                        tmpStr = resolveToken_fast(tokens(3))
                        if (.not. parse_int_string(tmpStr, i)) then
                            write(*,*) "Error: capacity must be an integer"
                            if (suffix=='!') call exit(1)
                        else
                            jdx = findList(Lists, ListCount, trim(tokens(2)))
                            if (jdx > 0) call resizeList(Lists(jdx), i)
                        end if
                    end if

                    else
                        write(*,*) "Error: unknown list subcommand"
                        if (suffix=='!') call exit(1)
                    end if
                end if

            

            case(OP_CPUTIME)
                if(ntok==1) then
                    s1= trim(tokens(1))

                    call cpu_time(r)

                    write(s2,'(G0)') r

                    call setVar(trim(s1), trim(s2))


                else 

                    write(*,*) 'Error: cputime requires 1 token: var'
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if
            case(OP_BYE)
                if (trim(verbose) == 'loud') then
                    call cpu_time(timerEnd)
                    write(*,*) 'Execution time: ', timerEnd - timerStart, 's'
                end if
                call exit(0)
            case(OP_ABS)
                if(ntok==2) then
                    s1 = trim(tokens(1))
                    s2 = resolveToken_fast(tokens(2))

                    read(s2, *) r 
                    r = abs(r)
                    write(s2,'(G0)') r
                    call setVar(trim(s1), trim(s2))
                else 
                    write(*,*) 'Error: abs requires 2 tokens: var|num'
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if
            case(OP_FLOOR)
                if(ntok==2) then
                    s1=trim(tokens(1))
                    s2 = resolveToken_fast(tokens(2))

                    read(s2,*) r
                    r = floor(r)
                    write(s2,'(G0)') r
                    call setVar(trim(s1), trim(s2))
                end if
            case(OP_CEILING)
                if(ntok==2) then
                    s1=trim(tokens(1))
                    s2 = resolveToken_fast(tokens(2))

                    read(s2,*) r
                    r = ceiling(r)
                    write(s2,'(G0)') r
                    call setVar(trim(s1), trim(s2))
                end if
            case(OP_ACHAR)
                if (ntok==2) then
                    s1=trim(tokens(1))
                    s2 = resolveToken_fast(tokens(2))
                    read(s2,*) a
                    s2 = achar(a)
                    call setVar(trim(s1),trim(s2))
                else 
                    write(*,*) 'Error: achar requires 2 tokens: var|num'
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if
            case(OP_ICHAR)
                if (ntok==2) then
                    s1=trim(tokens(1))
                    s2 = resolveToken_fast(tokens(2))
                    ch = trim(s2)
                    i = ichar(ch)
                    write(s2,*) i
                    call setVar(trim(s1),trim(s2))
                else 
                    write(*,*) 'Error:ichar requires 2 tokens: var|char'
                    if (suffix=='!') then
                        call exit(1)
                    end if
                end if
            case default
                write(*,'(A,I0)') "Unknown opcode: ", cmdId
                if (suffix=='!') then
                    call exit(1)
                end if
            end select
    end subroutine execute_command

    subroutine set_color(color)
        character(len=*), intent(in) :: color
        character(len=64) :: normalized

        normalized = adjustl(trim(color))
        select case (normalized)
        case ("reset")
            write(*,'(A)', advance="no") char(27)//"[0m"
        case ("black")
            write(*,'(A)', advance="no") char(27)//"[30m"
        case ("red")
            write(*,'(A)', advance="no") char(27)//"[31m"
        case ("green")
            write(*,'(A)', advance="no") char(27)//"[32m"
        case ("yellow")
            write(*,'(A)', advance="no") char(27)//"[33m"
        case ("blue")
            write(*,'(A)', advance="no") char(27)//"[34m"
        case ("magenta")
            write(*,'(A)', advance="no") char(27)//"[35m"
        case ("cyan")
            write(*,'(A)', advance="no") char(27)//"[36m"
        case ("white")
            write(*,'(A)', advance="no") char(27)//"[37m"
        case ("bright_red")
            write(*,'(A)', advance="no") char(27)//"[1;31m"
        case ("bright_green")
            write(*,'(A)', advance="no") char(27)//"[1;32m"
        case ("bright_yellow")
            write(*,'(A)', advance="no") char(27)//"[1;33m"
        case ("bright_blue")
            write(*,'(A)', advance="no") char(27)//"[1;34m"
        case ("bright_magenta")
            write(*,'(A)', advance="no") char(27)//"[1;35m"
        case ("bright_cyan")
            write(*,'(A)', advance="no") char(27)//"[1;36m"
        case ("bright_white")
            write(*,'(A)', advance="no") char(27)//"[1;37m"
        case default
            continue
        end select
    end subroutine set_color


end program forminvm
