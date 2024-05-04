#ifndef BIT_TWIDDLE_H
#define BIT_TWIDDLE_H

#define SIG_FITS_IN_BITS(value, bitc)                                          \
  ((abs((value)) & ~((1 << (bitc)) - 1)) == 0)
#define UNS_FITS_IN_BITS(value, bitc)                                          \
  (((value) & ~((1 << (bitc)) - 1)) == 0)

#define CLAMP_BITS(value, bitc) ((value) & ((1 << (bitc)) - 1))

#define NTH_BIT(value, bitn) (((value) & (1 << (bitn))) >> (bitn))

#define MASK(bitc) (1 << ((bitc) - 1))
#define SIGN_EXT(value, bitc) (((value) ^ MASK(bitc)) - MASK(bitc))

#endif
