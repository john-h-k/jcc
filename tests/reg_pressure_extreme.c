// expected value: 50
// stdout: 44850

int printf(const char *format, ...);

int main() {
  int a00 = 0, a01 = 1, a02 = 2, a03 = 3, a04 = 4, a05 = 5, a06 = 6, a07 = 7,
      a08 = 8, a09 = 9, a10 = 10, a11 = 11, a12 = 12, a13 = 13, a14 = 14,
      a15 = 15, a16 = 16, a17 = 17, a18 = 18, a19 = 19, a20 = 20, a21 = 21,
      a22 = 22, a23 = 23, a24 = 24, a25 = 25, a26 = 26, a27 = 27, a28 = 28,
      a29 = 29, a30 = 30, a31 = 31, a32 = 32, a33 = 33, a34 = 34, a35 = 35,
      a36 = 36, a37 = 37, a38 = 38, a39 = 39, a40 = 40, a41 = 41, a42 = 42,
      a43 = 43, a44 = 44, a45 = 45, a46 = 46, a47 = 47, a48 = 48, a49 = 49,
      a50 = 50, a51 = 51, a52 = 52, a53 = 53, a54 = 54, a55 = 55, a56 = 56,
      a57 = 57, a58 = 58, a59 = 59, a60 = 60, a61 = 61, a62 = 62, a63 = 63,
      a64 = 64, a65 = 65, a66 = 66, a67 = 67, a68 = 68, a69 = 69, a70 = 70,
      a71 = 71, a72 = 72, a73 = 73, a74 = 74, a75 = 75, a76 = 76, a77 = 77,
      a78 = 78, a79 = 79, a80 = 80, a81 = 81, a82 = 82, a83 = 83, a84 = 84,
      a85 = 85, a86 = 86, a87 = 87, a88 = 88, a89 = 89, a90 = 90, a91 = 91,
      a92 = 92, a93 = 93, a94 = 94, a95 = 95, a96 = 96, a97 = 97, a98 = 98,
      a99 = 99, a100 = 100, a101 = 101, a102 = 102, a103 = 103, a104 = 104,
      a105 = 105, a106 = 106, a107 = 107, a108 = 108, a109 = 109, a110 = 110,
      a111 = 111, a112 = 112, a113 = 113, a114 = 114, a115 = 115, a116 = 116,
      a117 = 117, a118 = 118, a119 = 119, a120 = 120, a121 = 121, a122 = 122,
      a123 = 123, a124 = 124, a125 = 125, a126 = 126, a127 = 127, a128 = 128,
      a129 = 129, a130 = 130, a131 = 131, a132 = 132, a133 = 133, a134 = 134,
      a135 = 135, a136 = 136, a137 = 137, a138 = 138, a139 = 139, a140 = 140,
      a141 = 141, a142 = 142, a143 = 143, a144 = 144, a145 = 145, a146 = 146,
      a147 = 147, a148 = 148, a149 = 149, a150 = 150, a151 = 151, a152 = 152,
      a153 = 153, a154 = 154, a155 = 155, a156 = 156, a157 = 157, a158 = 158,
      a159 = 159, a160 = 160, a161 = 161, a162 = 162, a163 = 163, a164 = 164,
      a165 = 165, a166 = 166, a167 = 167, a168 = 168, a169 = 169, a170 = 170,
      a171 = 171, a172 = 172, a173 = 173, a174 = 174, a175 = 175, a176 = 176,
      a177 = 177, a178 = 178, a179 = 179, a180 = 180, a181 = 181, a182 = 182,
      a183 = 183, a184 = 184, a185 = 185, a186 = 186, a187 = 187, a188 = 188,
      a189 = 189, a190 = 190, a191 = 191, a192 = 192, a193 = 193, a194 = 194,
      a195 = 195, a196 = 196, a197 = 197, a198 = 198, a199 = 199, a200 = 200,
      a201 = 201, a202 = 202, a203 = 203, a204 = 204, a205 = 205, a206 = 206,
      a207 = 207, a208 = 208, a209 = 209, a210 = 210, a211 = 211, a212 = 212,
      a213 = 213, a214 = 214, a215 = 215, a216 = 216, a217 = 217, a218 = 218,
      a219 = 219, a220 = 220, a221 = 221, a222 = 222, a223 = 223, a224 = 224,
      a225 = 225, a226 = 226, a227 = 227, a228 = 228, a229 = 229, a230 = 230,
      a231 = 231, a232 = 232, a233 = 233, a234 = 234, a235 = 235, a236 = 236,
      a237 = 237, a238 = 238, a239 = 239, a240 = 240, a241 = 241, a242 = 242,
      a243 = 243, a244 = 244, a245 = 245, a246 = 246, a247 = 247, a248 = 248,
      a249 = 249, a250 = 250, a251 = 251, a252 = 252, a253 = 253, a254 = 254,
      a255 = 255, a256 = 256, a257 = 257, a258 = 258, a259 = 259, a260 = 260,
      a261 = 261, a262 = 262, a263 = 263, a264 = 264, a265 = 265, a266 = 266,
      a267 = 267, a268 = 268, a269 = 269, a270 = 270, a271 = 271, a272 = 272,
      a273 = 273, a274 = 274, a275 = 275, a276 = 276, a277 = 277, a278 = 278,
      a279 = 279, a280 = 280, a281 = 281, a282 = 282, a283 = 283, a284 = 284,
      a285 = 285, a286 = 286, a287 = 287, a288 = 288, a289 = 289, a290 = 290,
      a291 = 291, a292 = 292, a293 = 293, a294 = 294, a295 = 295, a296 = 296,
      a297 = 297, a298 = 298, a299 = 299;

  int r = a00 + a01 + a02 + a03 + a04 + a05 + a06 + a07 + a08 + a09 + a10 + a11 +
         a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 +
         a24 + a25 + a26 + a27 + a28 + a29 + a30 + a31 + a32 + a33 + a34 + a35 +
         a36 + a37 + a38 + a39 + a40 + a41 + a42 + a43 + a44 + a45 + a46 + a47 +
         a48 + a49 + a50 + a51 + a52 + a53 + a54 + a55 + a56 + a57 + a58 + a59 +
         a60 + a61 + a62 + a63 + a64 + a65 + a66 + a67 + a68 + a69 + a70 + a71 +
         a72 + a73 + a74 + a75 + a76 + a77 + a78 + a79 + a80 + a81 + a82 + a83 +
         a84 + a85 + a86 + a87 + a88 + a89 + a90 + a91 + a92 + a93 + a94 + a95 +
         a96 + a97 + a98 + a99 + a100 + a101 + a102 + a103 + a104 + a105 +
         a106 + a107 + a108 + a109 + a110 + a111 + a112 + a113 + a114 + a115 +
         a116 + a117 + a118 + a119 + a120 + a121 + a122 + a123 + a124 + a125 +
         a126 + a127 + a128 + a129 + a130 + a131 + a132 + a133 + a134 + a135 +
         a136 + a137 + a138 + a139 + a140 + a141 + a142 + a143 + a144 + a145 +
         a146 + a147 + a148 + a149 + a150 + a151 + a152 + a153 + a154 + a155 +
         a156 + a157 + a158 + a159 + a160 + a161 + a162 + a163 + a164 + a165 +
         a166 + a167 + a168 + a169 + a170 + a171 + a172 + a173 + a174 + a175 +
         a176 + a177 + a178 + a179 + a180 + a181 + a182 + a183 + a184 + a185 +
         a186 + a187 + a188 + a189 + a190 + a191 + a192 + a193 + a194 + a195 +
         a196 + a197 + a198 + a199 + a200 + a201 + a202 + a203 + a204 + a205 +
         a206 + a207 + a208 + a209 + a210 + a211 + a212 + a213 + a214 + a215 +
         a216 + a217 + a218 + a219 + a220 + a221 + a222 + a223 + a224 + a225 +
         a226 + a227 + a228 + a229 + a230 + a231 + a232 + a233 + a234 + a235 +
         a236 + a237 + a238 + a239 + a240 + a241 + a242 + a243 + a244 + a245 +
         a246 + a247 + a248 + a249 + a250 + a251 + a252 + a253 + a254 + a255 +
         a256 + a257 + a258 + a259 + a260 + a261 + a262 + a263 + a264 + a265 +
         a266 + a267 + a268 + a269 + a270 + a271 + a272 + a273 + a274 + a275 +
         a276 + a277 + a278 + a279 + a280 + a281 + a282 + a283 + a284 + a285 +
         a286 + a287 + a288 + a289 + a290 + a291 + a292 + a293 + a294 + a295 +
         a296 + a297 + a298 + a299;

  printf("%d\n", r);
  return r;
}
