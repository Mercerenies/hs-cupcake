#define _WIN32_WINNT 0x0501

#include <windows.h>

typedef unsigned int uint_t;
typedef long long int64_t;
typedef unsigned long long uint64_t;
typedef int64_t (*h_wndProc)(HWND, uint_t, uint64_t, int64_t);

h_wndProc haskellWndPtr = NULL;

void setWndPtr(h_wndProc f) {
    haskellWndPtr = f;
}

int64_t defWndProc(HWND hwnd, uint_t msg, uint64_t wparam, int64_t lparam) {
    return DefWindowProc(hwnd, msg, wparam, lparam);
}

int messageBox(char* text, char* caption, int type) {
    return MessageBox(NULL, text, caption, type);
}

LRESULT CALLBACK wndProc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam) {
    switch (msg) {
    case WM_CLOSE:
        DestroyWindow(hwnd);
        break;
    case WM_DESTROY:
        PostQuitMessage(0);
        break;
    default:
        if (haskellWndPtr) {
            return haskellWndPtr(hwnd, msg, wparam, lparam);
        } else {
            return DefWindowProc(hwnd, msg, wparam, lparam);
        }
    }
    return 0;
}

HWND makeWindow(char* titlebar) {

    static const char className[] = "Window";

    WNDCLASSEX wc;
    HWND hwnd;

    wc.cbSize = sizeof(WNDCLASSEX);
    wc.style = 0;
    wc.lpfnWndProc = wndProc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.hInstance = GetModuleHandle(NULL);
    wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
    wc.lpszMenuName = NULL;
    wc.lpszClassName = className;
    wc.hIconSm = LoadIcon(NULL, IDI_APPLICATION);
    if (!RegisterClassEx(&wc))
        return NULL;
    hwnd = CreateWindowEx(WS_EX_CLIENTEDGE,
                          className,
                          titlebar,
                          WS_OVERLAPPEDWINDOW,
                          CW_USEDEFAULT,
                          CW_USEDEFAULT,
                          CW_USEDEFAULT,
                          CW_USEDEFAULT,
                          NULL,
                          NULL,
                          GetModuleHandle(NULL),
                          NULL);
    if (hwnd == NULL)
        return NULL;
    return hwnd;
}

HWND makeCtrl(char* cls, char* text, HWND parent) {

    HWND hwnd = CreateWindowEx(0,
                               cls,
                               text,
                               WS_CHILD,
                               0,
                               0,
                               0,
                               0,
                               parent,
                               NULL,
                               GetModuleHandle(NULL),
                               NULL);

    if (hwnd == NULL)
        return NULL;
    return hwnd;
}

void setPos(HWND hwnd, int x, int y, int w, int h) {
    SetWindowPos(hwnd, NULL, x, y, w, h, SWP_NOZORDER);
}

void showWindow(HWND hwnd) {
    ShowWindow(hwnd, SW_SHOW);
    UpdateWindow(hwnd);
}

void hideWindow(HWND hwnd) {
    ShowWindow(hwnd, SW_HIDE);
    UpdateWindow(hwnd);
}

void setWindowTitle(HWND hwnd, char* title) {
    SetWindowText(hwnd, title);
}

char* getWindowTitle(HWND hwnd) { // Free the char* when you're done with it :)
    int n = GetWindowTextLength(hwnd);
    char* ch = calloc(n + 1, sizeof(char));
    GetWindowText(hwnd, ch, n + 1);
    return ch;
}

void messageLoop() {
    MSG msg;
    while (GetMessage(&msg, NULL, 0, 0) > 0) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
}
