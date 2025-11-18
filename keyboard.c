
#include <stdio.h>

#ifdef _WIN32
#include <windows.h>


int key_is_held(int key_code) {
    SHORT state = GetAsyncKeyState(key_code);
    return (state & 0x8000) ? 1 : 0;
}


int key_get_pressed() {
    for (int i = 8; i < 256; i++) {
        SHORT state = GetAsyncKeyState(i);
        if (state & 0x0001) return i;
    }
    return 0;
}

#else
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/select.h>

int key_get_pressed() {
    struct termios oldt, newt;
    int ch;
    int oldf;

    tcgetattr(STDIN_FILENO, &oldt);
    newt = oldt;
    newt.c_lflag &= ~(ICANON | ECHO);
    tcsetattr(STDIN_FILENO, TCSANOW, &newt);
    oldf = fcntl(STDIN_FILENO, F_GETFL, 0);
    fcntl(STDIN_FILENO, F_SETFL, oldf | O_NONBLOCK);

    ch = getchar();

    tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
    fcntl(STDIN_FILENO, F_SETFL, oldf);

    if (ch != EOF)
        return ch;
    else
        return 0;
}


int key_is_held(int key_code) {
    
    
    return 0;
}
#endif
