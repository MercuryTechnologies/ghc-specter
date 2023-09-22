#ifdef __MACOS__
#include <Cocoa/Cocoa.h>
#endif

extern "C" {

float detectScaleFactor( void ) {
#ifdef __MACOS__
  return NSScreen.mainScreen.backingScaleFactor;
#else
  return 1.0;
#endif
}

}

