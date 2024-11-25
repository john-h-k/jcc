
int printf(const char *, ...);

int mandelbrot(double real, double imag) {
	int limit = 100;
	double zReal = real;
	double zImag = imag;

	for (int i = 0; i < limit; ++i) {
		double r2 = zReal * zReal;
		double i2 = zImag * zImag;
		
		if (r2 + i2 > 4.0) return i;

		zImag = 2.0 * zReal * zImag + imag;
		zReal = r2 - i2 + real;
	}

	return limit;
}

int main() {
	int width = 379; //number of characters fitting horizontally on my screen 
	int heigth = 98; //number of characters fitting vertically on my screen
		
	double x_start = -2.0;
	double x_fin = 1.0;
	double y_start = -1.0;
	double y_fin = 1.0;


	//~ double x_start = -0.25;
	//~ double x_fin = 0.05;
	//~ double y_start = -0.95;
	//~ double y_fin = -0.75;
	
	//~ double x_start = -0.13;
	//~ double x_fin = -0.085;
	//~ double y_start = -0.91;
	//~ double y_fin = -0.88;
	
	double dx = (x_fin - x_start)/(width - 1);
	double dy = (y_fin - y_start)/(heigth - 1);

	// const char *char_ = "\u2588";

	// const char *black = "\033[22;30m";
	// const char *red = "\033[22;31m";
	// const char *l_red = "\033[01;31m";
	// const char *green = "\033[22;32m";
	// const char *l_green = "\033[01;32m";
	// const char *orange = "\033[22;33m";
	// const char *yellow = "\033[01;33m";
	// const char *blue = "\033[22;34m";
	// const char *l_blue = "\033[01;34m";
	// const char *magenta = "\033[22;35m";
	// const char *l_magenta = "\033[01;35m";
	// const char *cyan = "\033[22;36m";
	// const char *l_cyan = "\033[01;36m";
	// const char *gray = "\033[22;37m";
	// const char *white = "\033[01;37m";
	// const char *char_ = "";

	// const char *black = "A";
	// const char *red = "B";
	// const char *l_red = "C";
	// const char *green = "D";
	// const char *l_green = "E";
	// const char *orange = "F";
	// const char *yellow = "G";
	// const char *blue = "H";
	// const char *l_blue = "I";
	// const char *magenta = "J";
	// const char *l_magenta = "K";
	// const char *cyan = "L";
	// const char *l_cyan = "M";
	// const char *gray = "\033[22;37m";
	// const char *white = "\033[01;37m";


	for (int i = 0; i < heigth; i++) {
		for (int j = 0; j < width; j++) {
			double x = x_start + j*dx; // current real value
			double y = y_fin - i*dy; // current imaginary value

			int value = mandelbrot(x,y);
			
			// printf("%d,%d - %d\n", i, j, value);
			// if (value == 100) {printf("A");}
			// else if (value > 90) {printf("B");}
			// else if (value > 70) {printf("C");}
			// else if (value > 50) {printf("D");}
			// else if (value > 30) {printf("E");}
			// else if (value > 20) {printf("F");}
			// else if (value > 10) {printf("G");}
			// else if (value > 5) {printf("H");}
			// else if (value > 4) {printf("I");}
			// else if (value > 3) {printf("J");}
			// else if (value > 2) {printf("K");}
			// else if (value > 1) {printf("L");}
			// else {printf("M");}
			// if (value == 100) {printf("%s", " ");}
			// else if (value > 90) {printf("%s%s", red, char_);}
			// else if (value > 70) {printf("%s%s", l_red, char_);}
			// else if (value > 50) {printf("%s%s", orange, char_);}
			// else if (value > 30) {printf("%s%s", yellow, char_);}
			// else if (value > 20) {printf("%s%s", l_green, char_);}
			// else if (value > 10) {printf("%s%s", green, char_);}
			// else if (value > 5) {printf("%s%s", l_cyan, char_);}
			// else if (value > 4) {printf("%s%s", cyan, char_);}
			// else if (value > 3) {printf("%s%s", l_blue, char_);}
			// else if (value > 2) {printf("%s%s", blue, char_);}
			// else if (value > 1) {printf("%s%s", magenta, char_);}
			// else {printf("%s%s", l_magenta, char_);}
			
			// printf("\033[0m");
		}

		printf("\n");
	}

	return 0;
}
