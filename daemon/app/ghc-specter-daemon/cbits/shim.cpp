extern "C" {

int scale( void ) {
#ifdef __MACOSX__
  return 2.0;
#else
  return 1.0;
#endif
}

}

