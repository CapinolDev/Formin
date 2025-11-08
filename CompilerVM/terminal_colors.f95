module terminal_colors
    implicit none
contains
    subroutine set_color(color)
        character(len=*), intent(in) :: color
        character(len=64) :: normalized

        normalized = adjustl(trim(color))
        select case(normalized)
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
end module terminal_colors
