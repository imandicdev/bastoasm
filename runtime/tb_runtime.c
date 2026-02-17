#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>

#define TB_MEM_SIZE 65536
#define TB_FB_W 320
#define TB_FB_H 200
#define TB_FB_SCALE 3

static int32_t tb_mem[TB_MEM_SIZE];

static const int32_t *tb_data_ptr = NULL;
static int32_t tb_data_count = 0;
static int32_t tb_data_index = 0;

static HWND tb_hwnd = NULL;
static uint32_t tb_framebuffer[TB_FB_W * TB_FB_H];
static BITMAPINFO tb_bmi;
static int tb_window_ready = 0;

static LRESULT CALLBACK tb_wndproc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam) {
    (void)wparam;
    (void)lparam;
    if (msg == WM_DESTROY) {
        PostQuitMessage(0);
        return 0;
    }
    return DefWindowProcA(hwnd, msg, wparam, lparam);
}

static void tb_pump_messages(void) {
    MSG msg;
    while (PeekMessageA(&msg, NULL, 0, 0, PM_REMOVE)) {
        if (msg.message == WM_QUIT) {
            exit(0);
        }
        TranslateMessage(&msg);
        DispatchMessageA(&msg);
    }
}

static void tb_present(void) {
    if (!tb_window_ready || tb_hwnd == NULL) {
        return;
    }
    HDC dc = GetDC(tb_hwnd);
    if (dc != NULL) {
        StretchDIBits(
            dc,
            0,
            0,
            TB_FB_W * TB_FB_SCALE,
            TB_FB_H * TB_FB_SCALE,
            0,
            0,
            TB_FB_W,
            TB_FB_H,
            tb_framebuffer,
            &tb_bmi,
            DIB_RGB_COLORS,
            SRCCOPY);
        ReleaseDC(tb_hwnd, dc);
    }
}

static void tb_ensure_window(void) {
    if (tb_window_ready) {
        return;
    }

    static int class_registered = 0;
    if (!class_registered) {
        WNDCLASSEXA wc;
        memset(&wc, 0, sizeof(wc));
        wc.cbSize = sizeof(wc);
        wc.lpfnWndProc = tb_wndproc;
        wc.hInstance = GetModuleHandleA(NULL);
        wc.lpszClassName = "BstoAsmTbWindow";
        wc.hCursor = LoadCursorA(NULL, IDC_ARROW);
        wc.style = CS_HREDRAW | CS_VREDRAW;
        RegisterClassExA(&wc);
        class_registered = 1;
    }

    RECT r;
    r.left = 0;
    r.top = 0;
    r.right = TB_FB_W * TB_FB_SCALE;
    r.bottom = TB_FB_H * TB_FB_SCALE;
    AdjustWindowRect(&r, WS_OVERLAPPEDWINDOW, FALSE);

    tb_hwnd = CreateWindowExA(
        0,
        "BstoAsmTbWindow",
        "TinyBASIC Native",
        WS_OVERLAPPEDWINDOW | WS_VISIBLE,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        r.right - r.left,
        r.bottom - r.top,
        NULL,
        NULL,
        GetModuleHandleA(NULL),
        NULL);

    memset(&tb_bmi, 0, sizeof(tb_bmi));
    tb_bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    tb_bmi.bmiHeader.biWidth = TB_FB_W;
    tb_bmi.bmiHeader.biHeight = -TB_FB_H;
    tb_bmi.bmiHeader.biPlanes = 1;
    tb_bmi.bmiHeader.biBitCount = 32;
    tb_bmi.bmiHeader.biCompression = BI_RGB;

    memset(tb_framebuffer, 0, sizeof(tb_framebuffer));
    tb_window_ready = 1;
    tb_pump_messages();
    tb_present();
}

static uint32_t tb_gray(int32_t value) {
    if (value <= 0) {
        return 0x00000000u;
    }
    if (value > 255) {
        value = 255;
    }
    return ((uint32_t)value << 16) | ((uint32_t)value << 8) | (uint32_t)value;
}

void tb_print_i32(int32_t value) {
    printf("%d", value);
}

void tb_print_cstr(const char *text) {
    if (text == NULL) {
        return;
    }
    fputs(text, stdout);
}

void tb_print_newline(void) {
    fputc('\n', stdout);
}

void tb_data_init(const int32_t *values, int32_t count) {
    tb_data_ptr = values;
    tb_data_count = count;
    tb_data_index = 0;
}

int32_t tb_read_data(void) {
    if (tb_data_ptr == NULL || tb_data_count <= 0) {
        return 0;
    }
    if (tb_data_index < 0 || tb_data_index >= tb_data_count) {
        return 0;
    }
    return tb_data_ptr[tb_data_index++];
}

void tb_poke(int32_t addr, int32_t value) {
    if (addr < 0 || addr >= TB_MEM_SIZE) {
        return;
    }
    tb_mem[addr] = value;
}

int32_t tb_peek(int32_t addr) {
    if (addr < 0 || addr >= TB_MEM_SIZE) {
        return 0;
    }
    return tb_mem[addr];
}

void tb_call(int32_t call_id) {
    tb_pump_messages();

    if (call_id == 2) {
        tb_ensure_window();
        int32_t x = tb_peek(0);
        int32_t y = tb_peek(1);
        int32_t w = tb_peek(2);
        int32_t h = tb_peek(3);
        if (w <= 0 || h <= 0 || w > TB_FB_W || h > TB_FB_H) {
            return;
        }
        for (int32_t row = 0; row < h; row++) {
            for (int32_t col = 0; col < w; col++) {
                int32_t sx = x + col;
                int32_t sy = y + row;
                if (sx < 0 || sx >= TB_FB_W || sy < 0 || sy >= TB_FB_H) {
                    continue;
                }
                int32_t src_index = 4 + row * w + col;
                int32_t pix = tb_peek(src_index);
                if (pix <= 0) {
                    continue;
                }
                tb_framebuffer[sy * TB_FB_W + sx] = tb_gray(pix);
            }
        }
        return;
    }

    if (call_id == 4) {
        int32_t vkey = tb_peek(0);
        SHORT state = GetAsyncKeyState(vkey);
        tb_poke(1, (state & 0x8000) ? 1 : 0);
        return;
    }

    if (call_id == 5) {
        tb_ensure_window();
        tb_present();
        int32_t delay_ms = tb_peek(0);
        if (delay_ms < 0) {
            delay_ms = 0;
        }
        Sleep((DWORD)delay_ms);
        return;
    }

    if (call_id == 6) {
        tb_ensure_window();
        memset(tb_framebuffer, 0, sizeof(tb_framebuffer));
        return;
    }

    if (call_id == 7) {
        tb_ensure_window();
        return;
    }
}

void tb_exit(int32_t code) {
    exit(code);
}
