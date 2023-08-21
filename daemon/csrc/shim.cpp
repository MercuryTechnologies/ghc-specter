#include "imgui.h"

extern "C" {

// operator () is not supported yet.
ImU32 toImU32(const ImColor& x) {
  return ImU32(x);
}

}
