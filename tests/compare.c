// expected value: 0

int lt(int a, int b) {
    return a < b;
}

int lte(int a, int b) {
    return a <= b;
}

int gt(int a, int b) {
    return a > b;
}

int gte(int a, int b) {
    return a >= b;
}

int main() {
  /* lt */

  if (lt(1, -1)) {
    return 1;
  }

  if (lt(1, 1)) {
    return 2;
  }

  if (!lt(1, 3)) {
    return 3;
  }

  /* lte */

  if (lte(1, -1)) {
    return 4;
  }

  if (!lte(1, 1)) {
    return 5;
  }

  if (!lte(1, 3)) {
    return 6;
  }

  /* gt */

  if (!gt(1, -1)) {
    return 7;
  }

  if (gt(1, 1)) {
    return 8;
  }

  if (gt(1, 3)) {
    return 9;
  }

  /* gte */

  if (!gte(1, -1)) {
    return 10;
  }

  if (!gte(1, 1)) {
    return 11;
  }

  if (gte(1, 3)) {
    return 12;
  }
}
