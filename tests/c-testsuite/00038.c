int
main()
{
	int x, *p;

	if (sizeof(0) < 2)
		return 1;
	if (sizeof 0 < 2)
		return 2;
	if (sizeof(char) < 1)
		return 3;
	if (sizeof(int) - 2 < 0)
		return 4;
	if (sizeof(&x) != sizeof p)
		return 5;
	return 0;
}
