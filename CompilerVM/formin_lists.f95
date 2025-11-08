    module formin_lists
        implicit none
        private

        integer, parameter :: DEFAULT_ITEM_LEN = 32

        public :: List
        public :: createList, findList, listPush, listSet, listGet, listLen, listClear, listPop
        public :: resizeList, listPushCached
        ! New/optimized APIs:
        public :: listReserveLazy
        public :: findListIdx, listPushIdx, listSetIdx, listGetIdx
        public :: createListIfMissing

        type :: List
            character(len=256) :: name
            character(:), allocatable :: items(:)
            integer :: size = 0
            integer :: capacity = 0
            integer :: item_len = DEFAULT_ITEM_LEN
        end type List

    contains

        subroutine ensureItemsAllocated(this)
            type(List), intent(inout) :: this
            if (.not.allocated(this%items)) then
                if (this%capacity <= 0) this%capacity = 10
                allocate(character(len=this%item_len) :: this%items(this%capacity))
            end if
        end subroutine ensureItemsAllocated

        subroutine resizeList(this, newCap)
            type(List), intent(inout) :: this
            integer, intent(in)       :: newCap
            character(len=:), allocatable :: tmp(:)
            integer :: copyN

            if (newCap <= 0) then
                if (allocated(this%items)) deallocate(this%items)
                this%capacity = 0
                this%size     = 0
                return
            end if

            if (.not.allocated(this%items)) then
                allocate(character(len=this%item_len) :: this%items(newCap))
                this%capacity = newCap
                return
            end if

            allocate(character(len=this%item_len) :: tmp(newCap))
            copyN = min(this%size, newCap)
            if (copyN > 0) tmp(1:copyN) = this%items(1:copyN)
            call move_alloc(tmp, this%items)
            this%capacity = newCap
            this%size     = copyN
        end subroutine resizeList

        subroutine listReserveLazy(this, newCap)
            type(List), intent(inout) :: this
            integer, intent(in)       :: newCap
            if (newCap > this%capacity) then
                this%capacity = newCap
            end if
        end subroutine listReserveLazy

        pure integer function findList(listArr, listCount, name) result(idx)
            type(List), intent(in) :: listArr(:)
            integer,  intent(in)   :: listCount
            character(len=*), intent(in) :: name
            integer :: j
            idx = 0
            do j = 1, listCount
                if (trim(listArr(j)%name) == trim(name)) then
                    idx = j
                    return
                end if
            end do
        end function findList

        pure integer function findListIdx(listArr, listCount, name) result(idx)
            type(List), intent(in) :: listArr(:)
            integer,  intent(in)   :: listCount
            character(len=*), intent(in) :: name
            idx = findList(listArr, listCount, name)
        end function findListIdx

        subroutine extendArrayListGeometric(arr, neededCount)
            type(List), allocatable, intent(inout) :: arr(:)
            integer, intent(in) :: neededCount
            type(List), allocatable :: tmp(:)
            integer :: curCap, newCap

            if (allocated(arr)) then
                curCap = size(arr)
            else
                curCap = 0
            end if
            if (neededCount <= curCap) return

            newCap = merge(1, curCap, curCap <= 0)
            if (newCap <= 0) newCap = 1
            do while (newCap < neededCount)
                newCap = max(1, newCap * 2)
            end do

            allocate(tmp(newCap))
            if (curCap > 0) tmp(1:curCap) = arr(1:curCap)
            call move_alloc(tmp, arr)
        end subroutine extendArrayListGeometric


        subroutine createList(listArr, listCount, name)
            type(List), allocatable, intent(inout) :: listArr(:)
            integer, intent(inout) :: listCount
            character(len=*), intent(in) :: name
            integer :: j
            j = findList(listArr, listCount, name)
            if (j > 0) then
                write(*,*) "Error: list already exists: ", trim(name)
                return
            end if
            call extendArrayListGeometric(listArr, listCount + 1)
            listCount = listCount + 1
            listArr(listCount)%name     = trim(name)
            listArr(listCount)%item_len = DEFAULT_ITEM_LEN
            listArr(listCount)%capacity = 10
            listArr(listCount)%size     = 0
        end subroutine createList

        subroutine createListIfMissing(listArr, listCount, name)
            type(List), allocatable, intent(inout) :: listArr(:)
            integer, intent(inout) :: listCount
            character(len=*), intent(in) :: name
            if (findList(listArr, listCount, name) == 0) then
                call createList(listArr, listCount, name)
            end if
        end subroutine createListIfMissing

        subroutine listPush(listArr, listCount, name, value)
            type(List), allocatable, intent(inout) :: listArr(:)
            integer, intent(in) :: listCount
            character(len=*), intent(in) :: name, value
            integer :: j
            j = findList(listArr, listCount, name)
            if (j == 0) then
                write(*,*) "Error: list not found: ", trim(name)
                return
            end if
            call listPushCached(listArr(j), value)
        end subroutine listPush

        subroutine listPushIdx(listArr, idx, value)
            type(List), allocatable, intent(inout) :: listArr(:)
            integer, intent(in) :: idx
            character(len=*), intent(in) :: value
            if (idx < 1 .or. idx > size(listArr)) then
                write(*,*) "Error: list index out of range in listPushIdx"
                return
            end if
            call listPushCached(listArr(idx), value)
        end subroutine listPushIdx

        subroutine listPushCached(this, value)
            type(List), intent(inout) :: this
            character(len=*), intent(in) :: value
            if (this%size >= this%capacity) then
                if (.not.allocated(this%items)) then
                    call ensureItemsAllocated(this)
                else
                    call resizeList(this, max(1, this%capacity * 2))
                end if
            end if
            if (.not.allocated(this%items)) call ensureItemsAllocated(this)
            this%size = this%size + 1
            
            this%items(this%size) = value(1:min(len(value), this%item_len))
        end subroutine listPushCached

        subroutine listSet(listArr, listCount, name, idx, value)
            type(List), allocatable, intent(inout) :: listArr(:)
            integer, intent(in) :: listCount, idx
            character(len=*), intent(in) :: name, value
            integer :: j
            j = findList(listArr, listCount, name)
            if (j == 0) then
                write(*,*) "Error: list not found: ", trim(name)
                return
            end if
            call listSetIdx(listArr, j, idx, value)
        end subroutine listSet

        subroutine listSetIdx(listArr, listIdx, idx, value)
            type(List), allocatable, intent(inout) :: listArr(:)
            integer, intent(in) :: listIdx, idx
            character(len=*), intent(in) :: value
            if (listIdx < 1 .or. listIdx > size(listArr)) then
                write(*,*) "Error: list index out of range in listSetIdx"
                return
            end if
            if (idx < 1 .or. idx > listArr(listIdx)%size) then
                write(*,*) "Error: index out of bounds"
                return
            end if
            listArr(listIdx)%items(idx) = value(1:min(len(value), listArr(listIdx)%item_len))
        end subroutine listSetIdx

        function listGet(listArr, listCount, name, idx) result(val)
            type(List), allocatable, intent(in) :: listArr(:)
            integer, intent(in) :: listCount, idx
            character(len=256) :: val
            integer :: j
            character(len=*), intent(in) :: name
            val = ""
            j = findList(listArr, listCount, name)
            if (j == 0) return
            if (idx < 1 .or. idx > listArr(j)%size) return
            val = listArr(j)%items(idx)(1:min(256, len(listArr(j)%items(idx))))
        end function listGet

        function listGetIdx(listArr, listIdx, idx) result(val)
            type(List), allocatable, intent(in) :: listArr(:)
            integer, intent(in) :: listIdx, idx
            character(len=256) :: val
            val = ""
            if (listIdx < 1 .or. listIdx > size(listArr)) return
            if (idx < 1 .or. idx > listArr(listIdx)%size) return
            val = listArr(listIdx)%items(idx)(1:min(256, len(listArr(listIdx)%items(idx))))
        end function listGetIdx

        pure integer function listLen(listArr, listCount, name) result(n)
            type(List), allocatable, intent(in) :: listArr(:)
            integer, intent(in) :: listCount
            character(len=*), intent(in) :: name
            integer :: j
            n = 0
            j = findList(listArr, listCount, name)
            if (j == 0) return
            n = listArr(j)%size
        end function listLen

        subroutine listClear(listArr, listCount, name)
            type(List), allocatable, intent(inout) :: listArr(:)
            integer, intent(in) :: listCount
            character(len=*), intent(in) :: name
            integer :: j
            j = findList(listArr, listCount, name)
            if (j == 0) return
            listArr(j)%size = 0
        end subroutine listClear

        subroutine listPop(listArr, listCount, name, outVar)
            type(List), allocatable, intent(inout) :: listArr(:)
            integer, intent(in) :: listCount
            character(len=256), intent(out) :: outVar
            character(len=:), allocatable :: tmpVal
            character(len=*), intent(in) :: name
            integer :: j
            outVar = ""
            j = findList(listArr, listCount, name)
            if (j == 0 .or. listArr(j)%size <= 0) return
            tmpVal = listArr(j)%items(listArr(j)%size)
            outVar = tmpVal(1:min(256, len(tmpVal)))
            listArr(j)%size = listArr(j)%size - 1
        end subroutine listPop

    end module formin_lists
