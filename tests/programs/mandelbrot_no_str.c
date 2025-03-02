int putchar(int ch);

void set_color(int v) {
  putchar(27);
  putchar('[');

  putchar((v & 15) + '0');
  v >>= 4;
  putchar((v & 15) + '0');
  v >>= 4;
  putchar(';');

  putchar((v & 15) + '0');
  v >>= 4;
  putchar((v & 15) + '0');
  v >>= 4;
  putchar('m');
}

void putcharblock() {
  putchar(0xE2);
  putchar(0x96);
  putchar(0x88);
}

void reset_color() {
  putchar(27);
  putchar('[');
  putchar('0');
  putchar('m');
}

int mandelbrot(int real, int imag) {
  int zReal = real;
  int zImag = imag;

  for (int i = 0; i < 100; ++i) {
    int r2 = (zReal * zReal) / 10000;
    int i2 = (zImag * zImag) / 10000;

    if (r2 + i2 > 40000)
      return i;

    int newZImag = (2 * zReal * zImag) / 10000 + imag;
    int newZReal = r2 - i2 + real;

    zReal = newZReal;
    zImag = newZImag;
  }
  return 100;
}

// can use an enum if compiler supports it
// typedef enum {
//   BLACK = 802,
//   RED = 4898,
//   L_RED = 4880,
//   GREEN = 8994,
//   L_GREEN = 8976,
//   ORANGE = 13090,
//   YELLOW = 13072,
//   BLUE = 17186,
//   L_BLUE = 17168,
//   MAGENTA = 21282,
//   L_MAGENTA = 21264,
//   CYAN = 25378,
//   L_CYAN = 25360,
//   GRAY = 29474,
//   WHITE = 29456
// } Color;


int main() {
  int BLACK = 802;
  int RED = 4898;
  int L_RED = 4880;
  int GREEN = 8994;
  int L_GREEN = 8976;
  int ORANGE = 13090;
  int YELLOW = 13072;
  int BLUE = 17186;
  int L_BLUE = 17168;
  int MAGENTA = 21282;
  int L_MAGENTA = 21264;
  int CYAN = 25378;
  int L_CYAN = 25360;
  int GRAY = 29474;
  int WHITE = 2945;

  int x_start = -20000, x_fin = 10000;
  int y_start = -10000, y_fin = 10000;

  int dx = (x_fin - x_start) / (171 - 1);
  int dy = (y_fin - y_start) / (45 - 1);

  for (int i = 0; i < 45; i++) {
    for (int j = 0; j < 171; j++) {
      int x = x_start + j * dx;
      int y = y_fin - i * dy;
      int value = mandelbrot(x, y);

      if (value == 100) {
        putchar(' ');
      } else if (value > 90) {
        set_color(RED);
        putcharblock();
      } else if (value > 70) {
        set_color(L_RED);
        putcharblock();
      } else if (value > 50) {
        set_color(ORANGE);
        putcharblock();
      } else if (value > 30) {
        set_color(YELLOW);
        putcharblock();
      } else if (value > 20) {
        set_color(L_GREEN);
        putcharblock();
      } else if (value > 10) {
        set_color(GREEN);
        putcharblock();
      } else if (value > 5) {
        set_color(L_CYAN);
        putcharblock();
      } else if (value > 4) {
        set_color(CYAN);
        putcharblock();
      } else if (value > 3) {
        set_color(L_BLUE);
        putcharblock();
      } else if (value > 2) {
        set_color(BLUE);
        putcharblock();
      } else if (value > 1) {
        set_color(MAGENTA);
        putcharblock();
      } else {
        set_color(L_MAGENTA);
        putcharblock();
      }

      reset_color();
    }
    putchar('\n');
  }
  return 0;
}
