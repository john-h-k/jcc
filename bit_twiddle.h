#ifndef BIT_TWIDDLE_H
#define BIT_TWIDDLE_H

  // _Generic((value), int                                                        \
  //          : ((abs((value)) & ~((1 << (bitc)) - 1u)) == 0), long               \
  //          : ((labs((value)) & ~((1l << (bitc)) - 1l)) == 0), long long        \
  //          : ((llabs((value)) & ~((1ll << (bitc)) - 1l)) == 0),                \
  //                                                                              \
  //            unsigned int                                                      \
  //          : (((value) & ~((1u << (bitc + 1)) - 1u)) == 0), unsigned long      \
  //          : (((value) & ~((1ul << (bitc + 1)) - 1ul)) == 0),                  \
  //            unsigned long long                                                \
  //          : (((value) & ~((1ull << (bitc + 1)) - 1ull)) == 0))

#define CLAMP_BITS(value, bitc) ((value) & ((1 << (bitc)) - 1))

#define NTH_BIT(value, bitn) (((value) & (1ull << (bitn))) >> (bitn))

#define MASK(bitc) (1 << ((bitc)-1))
#define SIGN_EXT(value, bitc) (((value) ^ MASK(bitc)) - MASK(bitc))

#endif
