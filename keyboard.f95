module formin_keyboard
    use, intrinsic :: iso_c_binding
    implicit none

    interface
        integer(c_int) function key_is_held(key_code) bind(C, name="key_is_held")
            import :: c_int
            integer(c_int), value :: key_code
        end function key_is_held

        integer(c_int) function key_get_pressed() bind(C, name="key_get_pressed")
            import :: c_int
        end function key_get_pressed
    end interface

contains

    function key_held(key) result(isHeld)
        integer, intent(in) :: key
        logical :: isHeld
        isHeld = (key_is_held(key) /= 0)
    end function key_held

    function key_pressed() result(code)
        integer :: code
        code = key_get_pressed()
    end function key_pressed

end module formin_keyboard
