module formin_opcodes
    implicit none
    integer, parameter:: OP_NONE=0, OP_SPEW=1, OP_SPEWMULT=2, OP_COLOR=3, &
                          OP_CREATE=4, OP_ADD=5, OP_SUB=6, OP_MULT=7, OP_DIV=8, &
                          OP_MARK=9, OP_GO=10, OP_IFGO=11, OP_ASK=12, OP_CLEAR=13, &
                          OP_OPEN=14, OP_READ=15, OP_CLOSE=16, OP_GOBACK=17, &
                          OP_STR=18, OP_TYPE=19, OP_SET=20, OP_MOD=21, &
                          OP_GETOS=22, OP_RANDI=23, OP_SQRT=24, OP_LIST=25, &
                          OP_SYS=26, OP_CPUTIME=27, OP_POW = 28, OP_SIN = 29, &
                          OP_COS = 30, OP_TAN = 31, OP_BYE = 32, OP_ABS = 33, &
                          OP_FLOOR=34, OP_CEILING = 35, OP_EXIT = 36, OP_INS = 37, &
                          OP_KEY = 38, OP_IFSKIP = 39, OP_ACHAR = 40, OP_ICHAR=41
end module formin_opcodes
