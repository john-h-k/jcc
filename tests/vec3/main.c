#include "../include.h"

typedef float scalar;

#ifdef USENEON

typedef struct {
  float x, y, z;
} vec3;
static vec3 zero = {0};

scalar v3x(vec3 v) { return v[0]; }
scalar v3y(vec3 v) { return v[1]; }
scalar v3z(vec3 v) { return v[2]; }

vec3 v3(scalar x, scalar y, scalar z) {
  float32_t ar[4];
  ar[0] = x;
  ar[1] = y;
  ar[2] = z;
  ar[3] = 0;
  return vld1q_f32(ar);
}

vec3 v3add(vec3 v, vec3 w) { return vaddq_f32(v, w); }
vec3 v3sub(vec3 v, vec3 w) { return vsubq_f32(v, w); }
vec3 v3mul(vec3 v, vec3 w) { return vmulq_f32(v, w); }
vec3 v3scale(vec3 v, scalar c) { return vmulq_n_f32(v, c); }
scalar v3dot(vec3 v, vec3 w) { return vaddvq_f32(vmulq_f32(v, w)); }

#elifdef USESSE

typedef struct {
  float x, y, z;
} vec3;
static vec3 zero = {0};

scalar v3x(vec3 v) { return v[0]; }
scalar v3y(vec3 v) { return v[1]; }
scalar v3z(vec3 v) { return v[2]; }

vec3 v3(scalar x, scalar y, scalar z) {
  scalar ar[4];
  ar[0] = x;
  ar[1] = y;
  ar[2] = z;
  ar[3] = 0;
  return _mm_load_ps(ar);
}

vec3 v3add(vec3 v, vec3 w) { return _mm_add_ps(v, w); }
vec3 v3sub(vec3 v, vec3 w) { return _mm_sub_ps(v, w); }
vec3 v3mul(vec3 v, vec3 w) { return _mm_mul_ps(v, w); }
vec3 v3scale(vec3 v, scalar c) { return _mm_mul_ps(v, _mm_load_ps1(&c)); }
scalar v3dot(vec3 v, vec3 w) {
  v = _mm_mul_ps(v, w);
  v = _mm_hadd_ps(v, v);
  v = _mm_hadd_ps(v, v);
  return v[0];
}

#else

typedef struct {
  float x, y, z;
} vec3;
static vec3 zero = {0};

scalar v3x(vec3 v) { return v.x; }
scalar v3y(vec3 v) { return v.y; }
scalar v3z(vec3 v) { return v.z; }

vec3 v3(scalar x, scalar y, scalar z) {
  vec3 v;
  v.x = x;
  v.y = y;
  v.z = z;
  return v;
}

vec3 v3add(vec3 v, vec3 w) {
  vec3 cpy;
  cpy.x = v.x + w.x;
  cpy.y = v.y + w.y;
  cpy.z = v.z + w.z;
  return cpy;
}

vec3 v3sub(vec3 v, vec3 w) {
  vec3 cpy;
  cpy.x = v.x - w.x;
  cpy.y = v.y - w.y;
  cpy.z = v.z - w.z;
  return cpy;
}

vec3 v3mul(vec3 v, vec3 w) {
  vec3 cpy;
  cpy.x = v.x * w.x;
  cpy.y = v.y * w.y;
  cpy.z = v.z * w.z;
  return cpy;
}

vec3 v3scale(vec3 v, scalar c) {
  vec3 cpy;
  cpy.x = v.x * c;
  cpy.y = v.y * c;
  cpy.z = v.z * c;
  return cpy;
}

scalar v3dot(vec3 v, vec3 w) { return v.x * w.x + v.y * w.y + v.z * w.z; }

#endif

scalar randomscalar(void) { return (float)rand() / (float)2147483647; }

vec3 v3neg(vec3 v) {
  vec3 l = {0};
  vec3 r = v;
  vec3 tmp = v3sub(l, r);
  return tmp;
}

scalar v3length(vec3 v) { return sqrtf(v3dot(v, v)); }

vec3 v3unit(vec3 v) {
  scalar len = v3length(v);
  scalar scale = 1.0 / len;
  vec3 norm = v3scale(v, scale);
  return norm;
}

vec3 v3cross(vec3 v, vec3 w) {
  return v3(v3y(v) * v3z(w) - v3z(v) * v3y(w),
            v3z(v) * v3x(w) - v3x(v) * v3z(w),
            v3x(v) * v3y(w) - v3y(v) * v3x(w));
}

vec3 v3random(void) {
  vec3 tmp = v3(randomscalar(), randomscalar(), randomscalar());
  return tmp;
}

vec3 v3randominterval(scalar min, scalar max) {
  vec3 l = v3(min, min, min);
  vec3 r = v3random();
  vec3 scaled = v3scale(r, max - min);
  vec3 res = v3add(l, scaled);

  return res;
}

vec3 v3randomunit(void) {
  while (1) {
    vec3 v = v3randominterval(-1, 1);
    if (v3dot(v, v) < 1) {
      vec3 u = v3unit(v);
      return u;
    }
  }
}

vec3 v3randominunitdisk(void) {
  while (1) {
    vec3 v = v3(-1.0 + 2.0 * randomscalar(), -1.0 + 2.0 * randomscalar(), 0);
    if (v3dot(v, v) < 1) {
      return v;
    }
  }
}

#define EPSILON 1e-6

int main(void) {
  {
    vec3 v = v3(1.0, 2.0, 3.0);
    if (fabs(v3x(v) - 1.0) > EPSILON || fabs(v3y(v) - 2.0) > EPSILON ||
        fabs(v3z(v) - 3.0) > EPSILON) {
      fprintf(
          stderr,
          "Error: v3 or accessors failed. Expected (1,2,3), got (%f,%f,%f).\n",
          v3x(v), v3y(v), v3z(v));
      return 1;
    }
  }

  {
    vec3 a = v3(1, 2, 3);
    vec3 b = v3(4, 5, 6);
    vec3 s = v3add(a, b);
    if (fabs(v3x(s) - 5.0) > EPSILON || fabs(v3y(s) - 7.0) > EPSILON ||
        fabs(v3z(s) - 9.0) > EPSILON) {
      fprintf(stderr,
              "Error: v3add failed. Expected (5,7,9), got (%f,%f,%f).\n",
              v3x(s), v3y(s), v3z(s));
      return 2;
    }
  }

  {
    vec3 a = v3(4, 5, 6);
    vec3 b = v3(1, 2, 3);
    vec3 d = v3sub(a, b);
    if (fabs(v3x(d) - 3.0) > EPSILON || fabs(v3y(d) - 3.0) > EPSILON ||
        fabs(v3z(d) - 3.0) > EPSILON) {
      fprintf(stderr,
              "Error: v3sub failed. Expected (3,3,3), got (%f,%f,%f).\n",
              v3x(d), v3y(d), v3z(d));
      return 3;
    }
  }

  {
    vec3 a = v3(2, 3, 4);
    vec3 b = v3(3, 4, 5);
    vec3 p = v3mul(a, b);
    if (fabs(v3x(p) - 6.0) > EPSILON || fabs(v3y(p) - 12.0) > EPSILON ||
        fabs(v3z(p) - 20.0) > EPSILON) {
      fprintf(stderr,
              "Error: v3mul failed. Expected (6,12,20), got (%f,%f,%f).\n",
              v3x(p), v3y(p), v3z(p));
      return 4;
    }
  }

  {
    vec3 a = v3(1, 2, 3);
    vec3 s = v3scale(a, 3);
    if (fabs(v3x(s) - 3.0) > EPSILON || fabs(v3y(s) - 6.0) > EPSILON ||
        fabs(v3z(s) - 9.0) > EPSILON) {
      fprintf(stderr,
              "Error: v3scale failed. Expected (3,6,9), got (%f,%f,%f).\n",
              v3x(s), v3y(s), v3z(s));
      return 5;
    }
  }

  {
    vec3 a = v3(1, 2, 3);
    vec3 b = v3(4, 5, 6);
    scalar d = v3dot(a, b);
    if (fabs(d - 32.0) > EPSILON) {
      fprintf(stderr, "Error: v3dot failed. Expected 32, got %f.\n", d);
      return 6;
    }
  }

  {
    vec3 a = v3(1, -2, 3);
    vec3 n = v3neg(a);
    if (fabs(v3x(n) + 1.0) > EPSILON || fabs(v3y(n) - 2.0) > EPSILON ||
        fabs(v3z(n) + 3.0) > EPSILON) {
      fprintf(stderr,
              "Error: v3neg failed. Expected (-1,2,-3), got (%f,%f,%f).\n",
              v3x(n), v3y(n), v3z(n));
      return 7;
    }
  }

  {
    vec3 a = v3(3, 4, 0);
    scalar len = v3length(a);
    if (fabs(len - 5.0) > EPSILON) {
      fprintf(stderr, "Error: v3length failed. Expected 5, got %f.\n", len);
      return 8;
    }
  }

  {
    vec3 a = v3(3, 4, 0);
    vec3 u = v3unit(a);
    if (fabs(v3length(u) - 1.0) > EPSILON || fabs(v3x(u) - 0.6) > EPSILON ||
        fabs(v3y(u) - 0.8) > EPSILON || fabs(v3z(u) - 0.0) > EPSILON) {
      fprintf(stderr,
              "Error: v3unit failed. Expected (0.6,0.8,0), got (%f,%f,%f).\n",
              v3x(u), v3y(u), v3z(u));
      return 9;
    }
  }

  {
    vec3 a = v3(1, 0, 0);
    vec3 b = v3(0, 1, 0);
    vec3 c = v3cross(a, b);
    if (fabs(v3x(c) - 0.0) > EPSILON || fabs(v3y(c) - 0.0) > EPSILON ||
        fabs(v3z(c) - 1.0) > EPSILON) {
      fprintf(stderr,
              "Error: v3cross failed. Expected (0,0,1), got (%f,%f,%f).\n",
              v3x(c), v3y(c), v3z(c));
      return 10;
    }
  }

  {
    for (int i = 0; i < 10; i++) {
      vec3 r = v3random();
      if (v3x(r) < 0.0 || v3x(r) > 1.0 || v3y(r) < 0.0 || v3y(r) > 1.0 ||
          v3z(r) < 0.0 || v3z(r) > 1.0) {
        fprintf(stderr,
                "Error: v3random failed. Got (%f,%f,%f), components should be "
                "in [0,1].\n",
                v3x(r), v3y(r), v3z(r));
        return 11;
      }
    }
  }

  {
    scalar min = -5.0, max = 5.0;
    for (int i = 0; i < 10; i++) {
      vec3 r = v3randominterval(min, max);
      if (v3x(r) < min || v3x(r) > max || v3y(r) < min || v3y(r) > max ||
          v3z(r) < min || v3z(r) > max) {
        fprintf(stderr,
                "Error: v3randominterval failed. Got (%f,%f,%f), expected "
                "components in [%f,%f].\n",
                v3x(r), v3y(r), v3z(r), min, max);
        return 12;
      }
    }
  }

  {
    for (int i = 0; i < 10; i++) {
      vec3 u = v3randomunit();
      if (fabs(v3length(u) - 1.0) > EPSILON) {
        fprintf(stderr,
                "Error: v3randomunit failed. Got vector with length %f, "
                "expected 1.\n",
                v3length(u));
        return 13;
      }
    }
  }

  {
    for (int i = 0; i < 10; i++) {
      vec3 d = v3randominunitdisk();
      if (fabs(v3z(d)) > EPSILON ||
          (v3x(d) * v3x(d) + v3y(d) * v3y(d)) >= 1.0) {
        fprintf(stderr,
                "Error: v3randominunitdisk failed. Got (%f,%f,%f), expected "
                "z==0 and x^2+y^2 < 1.\n",
                v3x(d), v3y(d), v3z(d));
        return 14;
      }
    }
  }

  return 0;
}
