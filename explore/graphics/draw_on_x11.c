/* ---- XGraph.c ---- */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <stdio.h>

int main() {
    Display *display;
    Window window;
    XSetWindowAttributes attr;
    Colormap colormap;
    XColor color1, color2;
    XGCValues gcvalue;
    GC gc;
    XSizeHints *sz;

    display = XOpenDisplay(":0.0");

    // get default colormap
    colormap = DefaultColormap(display, DefaultScreen(display));

    // get colorcell
    color1.red = color1.blue = 0xff;
    color1.green = 0;
    color2.red = color2.green = color2.blue = 0xff;
    color1.flags = color2.flags = DoRed | DoGreen | DoBlue;
    XAllocColor(display, colormap, &color1);
    XAllocColor(display, colormap, &color2);

    // Window attributes and creating
    attr.background_pixel = color2.pixel; // background color
    window = XCreateWindow(display,
             XDefaultRootWindow(display), 100, 100, 300, 300,
             2, XDefaultDepth(display, 0), InputOutput,
             CopyFromParent, CWBackPixel, &attr);
    Window w2 = XDefaultRootWindow(display);
    // talk to window manager
    XStoreName(display, window, "Hello x11!");
    sz = XAllocSizeHints();
    sz->x = 100;
    sz->y = 100;
    sz->width = 300;
    sz->height = 300;
    sz->flags = USPosition | USSize;
    XSetNormalHints(display, window, sz);

    // show the windows
    printf("Map window\n");
    XMapWindow(display, window);
    XFlush(display);
    getchar();

    // create GC
    gc = XCreateGC(display, w2, 0, &gcvalue);
    XSetForeground(display, gc, color1.pixel);
    XSetBackground(display, gc, color2.pixel);

    // draw a rectangle
    printf("Draw rectangle\n");
    XDrawRectangle(display, w2, gc, 10, 10, 100, 100);
    XFlush(display);
    getchar();

    // clear window
    XClearWindow(display, window);

    // set line style
    XSetLineAttributes(display, gc, 5, LineOnOffDash,
        CapButt, JoinRound);
    // draw the line (200, 10) - (200, 290)
    printf("Draw line\n");
    XDrawLine(display, window, gc, 200, 10, 200, 290);
    XFlush(display);
    getchar();

    // close the Window
    printf("Destory Window\n");
    XDestroyWindow(display, window);
    XFlush(display);
    getchar();

    printf("close display\n");
    XCloseDisplay(display);
    getchar();

    return 0;
}
