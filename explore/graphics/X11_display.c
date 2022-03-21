#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <stdio.h>

#define DUMP_ERR printf

static void draw(Display *display, Window win, GC gc,
    int width, int height);

int main()
{
    Display *display;
    int screennum;
    int width;
    int height;
    Atom protocols;

    const char *strdraw = "hello world";

    Window win;
    GC gc;

    display = XOpenDisplay(NULL);
    if (!display) {
        DUMP_ERR("call XOpenDisplay(%s) fail\n", XDisplayName(NULL));
        return 1;
    }
    screennum = DefaultScreen(display);
    printf("Screen:%s\tScreenCount:%d\n", DisplayString(display), ScreenCount(display));
    win = RootWindow(display, screennum);
    char name[256];
    name[255] = 0;
    XFetchName(display, win, (char**)&name);
    printf("Root Window:%s\n", name);
    XRaiseWindow(display, win);

    Window ewin;
    unsigned int ewidth, eheight; // Window size
    int x, y; // Window position
    ewidth = 600, eheight = 400;
    ewin = XCreateSimpleWindow(display, RootWindow(display, screennum),
           x, y, ewidth, eheight, 4,
           BlackPixel(display, screennum), WhitePixel(display, screennum));
    XMapWindow(display, ewin);
    XFlush(display);

    getchar();
}
