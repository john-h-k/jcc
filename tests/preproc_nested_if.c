// expected value: 0

// #if defined(__has_builtin)
//  #if __has_builtin(a)
//   #if __has_builtin(b)
//    #if __has_builtin(c)
//     #if __has_builtin(e)
//      #if __has_builtin(f)
//       #if __has_builtin(g)
//         #define BAD 0
//       #endif
//      #endif
//     #endif
//    #endif
//   #endif
//  #endif
// #endif

#if defined(__has_builtin)
 #if __has_builtin(__is_target_arch)
  #if __has_builtin(__is_target_vendor)
   #if __has_builtin(__is_target_os)
    #if __has_builtin(__is_target_environment)
     #if __has_builtin(__is_target_variant_os)
      #if __has_builtin(__is_target_variant_environment)
       #if (__is_target_arch(x86_64) && __is_target_vendor(apple) && ((__is_target_os(ios) && __is_target_environment(macabi)) || (__is_target_variant_os(ios) && __is_target_variant_environment(macabi))))
         #define __ENABLE_LEGACY_IPHONE_AVAILABILITY 1
         #define __ENABLE_LEGACY_MAC_AVAILABILITY 1
         #define BAD 1
       #endif /* # if __is_target_arch... */
      #endif /* #if __has_builtin(__is_target_variant_environment) */
     #endif /* #if __has_builtin(__is_target_variant_os) */
    #endif /* #if __has_builtin(__is_target_environment) */
   #endif /* #if __has_builtin(__is_target_os) */
  #endif /* #if __has_builtin(__is_target_vendor) */
 #endif /* #if __has_builtin(__is_target_arch) */
#endif /* #if defined(__has_builtin) */


int main() {
#ifdef BAD
  return 1;
#else
  return 0;
#endif
}
