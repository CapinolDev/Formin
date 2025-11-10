program forminc
    use formin_opcodes
    implicit none

    
    type :: Instr
        sequence
        integer :: op
        integer :: ntok
        character(len=256) :: tokens(10)
        character(len=1)  :: suffix  
    end type Instr


    type(Instr), allocatable :: Program(:)
    integer :: ProgCapacity
    character(len=256) :: line, cmd, rawTokens, tail
    character(len=256) :: tok(10)
    integer :: i, n, ios, pos1, pos2, lineNo
    character(len=128) :: outFile, inFile
    integer :: ProgCount

   
    call get_command_argument(1, inFile)
    if (len_trim(inFile) == 0) then
        print *, "Usage: forminc <source.fmn>"
        stop
    end if

    outFile = trim(inFile)//".fbc"
    open(unit=10, file=trim(inFile), status='old', action='read', iostat=ios)
    if (ios /= 0) stop "Cannot open source file."

    allocate(Program(0))
    ProgCapacity = 0
    ProgCount = 0
    lineNo = 0

    
    do
        read(10,'(A)',iostat=ios) line
        if (ios /= 0) exit
        lineNo = lineNo + 1

        line = trim(line)
        if (line == "") cycle
        if (line(1:1) == "!") cycle  

        if (lower_str(line) == "bye") then
            ProgCount = ProgCount + 1
            call extendArrayInstr(Program, ProgCount)
            call clearInstr(Program(ProgCount))
            Program(ProgCount)%op = OP_BYE
            cycle
        end if

        pos1 = index(line, "#/")
        pos2 = index(line, "/#")
        if (pos1 == 0 .or. pos2 == 0 .or. pos2 <= pos1) then
            
            cycle
        end if

        cmd       = adjustl(trim(line(1:pos1-1)))
        rawTokens = adjustl(trim(line(pos1+2:pos2-1)))
        tail      = ""
        if (pos2 + 2 <= len_trim(line)) tail = trim(line(pos2+2:))

        
        tok = ""
        call split(rawTokens, "|", tok, n)

        
        ProgCount = ProgCount + 1
        call extendArrayInstr(Program, ProgCount)
        call clearInstr(Program(ProgCount))

        
        call lower_inplace(cmd)

        select case (trim(cmd))
        case ("create")
            Program(ProgCount)%op = OP_CREATE
            if (n < 1 .or. n > 3) then
                call warn(lineNo, "create requires 1 to 3 tokens: name|[value]|[type]")
            else
                call assign_tokens(n)
            end if

        case ("add")
            Program(ProgCount)%op = OP_ADD
            if (n < 3) then
                call warn(lineNo, "add requires 3 tokens: out|lhs|rhs")
            else
                call assign_tokens(n)
            end if

        case ("sub")
            Program(ProgCount)%op = OP_SUB
            if (n < 3) then
                call warn(lineNo, "sub requires 3 tokens: out|lhs|rhs")
            else
                call assign_tokens(n)
            end if
        case ("mult")
            Program(ProgCount)%op = OP_MULT
            if (n < 3) then
                call warn(lineNo, "mult requires 3 tokens: out|lhs|rhs")
            else
                call assign_tokens(n)
            end if

        case ("spew")
            Program(ProgCount)%op = OP_SPEW
            if (n < 1) then
                call warn(lineNo, "spew requires at least 1 token")
            else
                call assign_tokens(n)
            end if

        case ("mark")
            Program(ProgCount)%op = OP_MARK
            if (n < 1) then
                call warn(lineNo, "mark requires 1 token: label")
            else
                call assign_tokens(1)
            end if

        case ("go")
            Program(ProgCount)%op = OP_GO
            if (n < 1) then
                call warn(lineNo, "go requires 1 token: label")
            else
                call assign_tokens(1)
            end if

        case ("ifgo")
            Program(ProgCount)%op = OP_IFGO
            if (n < 5) then
                call warn(lineNo, "ifgo requires 5 tokens: lhs|op|rhs|trueLabel|falseLabel")
            else
                call assign_tokens(n)
            end if
        case ("ask")
            Program(ProgCount)%op = OP_ASK
            if (n < 2) then
                call warn(lineNo, "ask requires 2 tokens: question|var")
            else
                call assign_tokens(n)
            end if
        case ("spewmult")
            Program(ProgCount)%op = OP_SPEWMULT
            if (n < 1) then
                call warn(lineNo, "spewmult requires at least 1 token")
            else
                call assign_tokens(n)
            end if
        case ("div")
            Program(ProgCount)%op = OP_DIV
            if (n < 3) then
                call warn(lineNo, "div requires 3 tokens: out|lhs|rhs")
            else
                call assign_tokens(n)
            end if
        case ("color")
            Program(ProgCount)%op = OP_COLOR
            if (n < 1) then
                call warn(lineNo, "color requires 1 token")
            else
                call assign_tokens(1)
            end if
        case ("clear")
            Program(ProgCount)%op = OP_CLEAR
            if (n > 0) call warn(lineNo, "clear does not take parameters")
        case ("open")
            Program(ProgCount)%op = OP_OPEN
            if (n < 2) then
                call warn(lineNo, "open requires 2 tokens: handleVar|filePath")
            else
                call assign_tokens(n)
            end if
        case ("read")
            Program(ProgCount)%op = OP_READ
            if (n < 2) then
                call warn(lineNo, "read requires 2 tokens: handleVar|lineVar")
            else
                call assign_tokens(n)
            end if
        case ("close")
            Program(ProgCount)%op = OP_CLOSE
            if (n < 1) then
                call warn(lineNo, "close requires 1 token: handleVar")
            else
                call assign_tokens(1)
            end if
        case ("goback")
            Program(ProgCount)%op = OP_GOBACK
            if (n > 0) call warn(lineNo, "goback does not take parameters")
        case ("set")
            Program(ProgCount)%op = OP_SET
            if (n < 2) then
                call warn(lineNo, "set requires 2 or 3 tokens: var|value|[type]")
            else
                call assign_tokens(n)
            end if
        case ("str")
            Program(ProgCount)%op = OP_STR
            if (n < 1) then
                call warn(lineNo, "str requires a subcommand")
            else
                call assign_tokens(n)
            end if
        case ("type")
            Program(ProgCount)%op = OP_TYPE
            if (n < 2) then
                call warn(lineNo, "type requires 2 tokens: outVar|var")
            else
                call assign_tokens(n)
            end if
        case ("mod")
            Program(ProgCount)%op = OP_MOD
            if (n < 3) then
                call warn(lineNo, "mod requires 3 tokens: out|value|divisor")
            else
                call assign_tokens(n)
            end if
        case ("getos")
            Program(ProgCount)%op = OP_GETOS
            if (n < 1) then
                call warn(lineNo, "getos requires 1 token: var")
            else
                call assign_tokens(1)
            end if
        case ("randi")
            Program(ProgCount)%op = OP_RANDI
            if (n < 2) then
                call warn(lineNo, "randi requires 2 tokens: var|max")
            else
                call assign_tokens(n)
            end if
        case ("sqrt")
            Program(ProgCount)%op = OP_SQRT
            if (n < 2) then
                call warn(lineNo, "sqrt requires 2 tokens: var|value")
            else
                call assign_tokens(n)
            end if
        case ("pow")
            Program(ProgCount)%op = OP_POW
                if (n < 3) then
                    call warn(lineNo, "pow requires 3 tokens: var|value1|value2")
                else
                    call assign_tokens(n)
                end if
        case ("sys")
            Program(ProgCount)%op = OP_SYS
            if (n < 1) then
                call warn(lineNo, "sys requires 1 token: command")
            else
                call assign_tokens(n)
            end if
        case ("cputime")
            Program(ProgCount)%op = OP_CPUTIME
            if (n < 1) then
                call warn(lineNo, "cputime requires 1 token: var")
            else
                call assign_tokens(1)
            end if
            
        case ("list")
            Program(ProgCount)%op = OP_LIST
            if (n < 1) then
                call warn(lineNo, "list requires a subcommand")
            else
                call assign_tokens(n)
            end if
        case ("sin")
            Program(ProgCount)%op = OP_SIN
                if (n<3) then 
                    call warn(lineNo, "sin requires 3 tokens: var|degrees/rads|num")
                else
                    call assign_tokens(n)
                end if
        case ("cos")
            Program(ProgCount)%op = OP_COS
                if (n<3) then 
                    call warn(lineNo, "cos requires 3 tokens: var|degrees/rads|num")
                else
                    call assign_tokens(n)
                end if
        case ("tan")
            Program(ProgCount)%op = OP_TAN
                if (n<3) then 
                    call warn(lineNo, "tan requires 3 tokens: var|degrees/rads|num")
                else
                    call assign_tokens(n)
                end if
        case("abs")
            Program(ProgCount)%op = OP_ABS
            if (n<2) then
                call warn(lineNo, "abs requires 2 tokens: var|num")
            else 
                call assign_tokens(n)
            end if
        case("floor")
            Program(ProgCount)%op = OP_FLOOR
            if (n<2) then
                call warn(lineNo, "floor requires 2 tokens: var|num")
            else
                call assign_tokens(n)
            end if
        case("ceiling")
            Program(ProgCount)%op = OP_CEILING
            if (n<2) then
                call warn(lineNo, "ceiling requires 2 tokens: var|num")
            else
                call assign_tokens(n)
            end if
        

        case default
          
            call warn(lineNo, "unknown command '"//trim(cmd)//"' (skipped)")
        end select

      
        Program(ProgCount)%suffix = first_suffix_char(tail)
    end do
    close(10)

    open(unit=20, file=trim(outFile), form='unformatted', access='stream', iostat=ios)
    if (ios /= 0) stop "Cannot open output file."
    write(20) ProgCount
    write(20) Program
    close(20)

    print *, "Compiled", ProgCount, "instructions →", trim(outFile)

contains

    subroutine split(input, delimiter, tokens, count)
        character(len=*), intent(in)  :: input, delimiter
        character(len=*), intent(out) :: tokens(:)
        integer,          intent(out) :: count
        integer :: start, pos, lenInput, lenDelim

        count = 0
        lenInput = len_trim(input)
        if (lenInput == 0) return
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
                if (start > lenInput) exit
            end if
            if (count >= size(tokens)) exit
        end do
    end subroutine split

    subroutine extendArrayInstr(arr, newSize)
        type(Instr), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: newSize
        type(Instr), allocatable :: tmp(:)
        integer :: curCap, newCap

        if (allocated(arr)) then
            curCap = size(arr)
        else
            curCap = 0
        end if

        if (newSize <= ProgCapacity) return
        newCap = max(8, ProgCapacity)
        if (newCap <= 0) newCap = 8
        do while (newCap < newSize)
            newCap = newCap * 2
        end do
        allocate(tmp(newCap))
        if (curCap > 0) tmp(1:curCap) = arr(1:curCap)
        call move_alloc(tmp, arr)
        ProgCapacity = newCap
    end subroutine extendArrayInstr

    subroutine clearInstr(x)
        type(Instr), intent(inout) :: x
        x%op = OP_NONE
        x%ntok = 0
        x%tokens = ""
        x%suffix = ' '
    end subroutine clearInstr

    subroutine lower_inplace(s)
        character(len=*), intent(inout) :: s
        integer :: k, L, code
        L = len_trim(s)
        do k = 1, L
            code = iachar(s(k:k))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                s(k:k) = achar(code + 32)
            end if
        end do
    end subroutine lower_inplace

    function lower_str(s) result(t)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: t
        integer :: k, L, code
        t = s
        L = len_trim(t)
        do k = 1, L
            code = iachar(t(k:k))
            if (code >= iachar('A') .and. code <= iachar('Z')) t(k:k) = achar(code + 32)
        end do
    end function lower_str

    function first_suffix_char(tail) result(ch)
        character(len=*), intent(in) :: tail
        character(len=1) :: ch
        integer :: L, k
        L = len_trim(tail)
        ch = ' '
        if (L == 0) return
        do k = 1, L
            if (tail(k:k) /= ' ') then
                ch = tail(k:k)
                return
            end if
        end do
    end function first_suffix_char

    subroutine warn(ln, msg)
        integer, intent(in) :: ln
        character(len=*), intent(in) :: msg
        write(*,'(A,I0,A,A)') "forminc:", ln, ": ", trim(msg)
    end subroutine warn

    subroutine dump_program(P, N)
        type(Instr), intent(in) :: P(:)
        integer,    intent(in) :: N
        integer :: i, j
        do i = 1, N
            write(*,'(I5,2X,I3,2X)', advance='no') i, P(i)%op
            do j = 1, P(i)%ntok
                write(*,'(A,1X)', advance='no') trim(P(i)%tokens(j))
            end do
            write(*,'(A1)') P(i)%suffix
        end do
    end subroutine dump_program

    subroutine assign_tokens(count)
        integer, intent(in) :: count
        integer :: j, limit
        Program(ProgCount)%tokens = ""
        Program(ProgCount)%ntok = 0
        limit = min(count, size(Program(ProgCount)%tokens))
        do j = 1, limit
            Program(ProgCount)%ntok = Program(ProgCount)%ntok + 1
            Program(ProgCount)%tokens(Program(ProgCount)%ntok) = adjustl(trim(tok(j)))
        end do
    end subroutine assign_tokens

end program forminc
