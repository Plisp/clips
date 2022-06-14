int main(void) {
    int i;
    int numbers[10] = { 0 };
    int swapped;
	int x,y;

    i = 0;
    while (i < 10) {
        scanf("%d", &numbers[i]);
        ++i;
    }

	swapped = 1;
    while (swapped == 1) {
        swapped = 0;
        i = 1;
        while (i < 10) {
            x = numbers[i];
            y = numbers[i - 1];
            if (x < y) {
                numbers[i] = y;
                numbers[i - 1] = x;
                swapped = 1;
            }
            ++i;
        }
    }

    i = 0;
    while (i < 10) {
        printf("%d\n", numbers[i]);
        ++i;
    }
}
