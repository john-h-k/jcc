#undef  line
#define line 1000

int a = __LINE__;
#line line
int a = __LINE__;
#if 1000 != __LINE__
	// #error "  # line line" not work as expected
#endif

int
main()
{
	return 0;
}
