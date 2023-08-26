#include "imgui.h"

extern "C" {

  // int <-> enum should be explicit. fficxx limitation yet
  void addKeyEvent(ImGuiIO* io, int key, bool down) {
    io->AddKeyEvent(ImGui::GetKeyIndex((ImGuiKey)key), down);
  }
  
  bool isKeyDown(int key) {
    return ImGui::IsKeyDown(ImGui::GetKeyIndex((ImGuiKey)key));
  }
  
  bool isKeyPressed(int key, bool repeat) {
    return ImGui::IsKeyPressed(ImGui::GetKeyIndex((ImGuiKey)key), repeat);
  }
  
  bool isKeyReleased(int key) {
    return ImGui::IsKeyReleased(ImGui::GetKeyIndex((ImGuiKey)key));
  }

}
