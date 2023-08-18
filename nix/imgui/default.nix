{
  stdenv,
  lib,
  fetchFromGitHub,
  pkg-config,
  frameworks,
  glfw,
}:
stdenv.mkDerivation rec {
  pname = "imgui";
  version = "1.89.5";

  src = fetchFromGitHub {
    owner = "ocornut";
    repo = "imgui";
    rev = "v${version}";
    sha256 = "sha256-Ha70CTSBpyF9S+/qG9lAhUlUT4vY0crOoi3vFsy65H8=";
  };

  nativeBuildInputs = [pkg-config];

  buildInputs = [
    glfw
    frameworks.Cocoa
    frameworks.Metal
    frameworks.MetalKit
  ];

  buildPhase = ''
    $CXX -std=c++11 -I. -I./backends -c imgui.cpp
    $CXX -std=c++11 -I. -I./backends -c imgui_demo.cpp
    $CXX -std=c++11 -I. -I./backends -c imgui_draw.cpp
    $CXX -std=c++11 -I. -I./backends -c imgui_tables.cpp
    $CXX -std=c++11 -I. -I./backends -c imgui_widgets.cpp
    $CXX -std=c++11 -I. -I./backends -c backends/imgui_impl_glfw.cpp
    $CXX -std=c++11 -I. -I./backends -c backends/imgui_impl_opengl3.cpp
    $CXX -dynamiclib -undefined suppress -flat_namespace -install_name $out/lib/libimgui.dylib -o libimgui.dylib imgui.o imgui_demo.o imgui_draw.o imgui_tables.o imgui_widgets.o imgui_impl_glfw.o imgui_impl_opengl3.o
  '';

  installPhase = ''
    mkdir -p $out/include/imgui
    mkdir -p $out/lib
    cp *.h $out/include/imgui
    cp -a backends/*.h $out/include/imgui/
    cp -a misc $out/include/imgui/
    cp libimgui.dylib $out/lib

    mkdir -p $out/lib/pkgconfig
    cat >> $out/lib/pkgconfig/libimgui.pc << EOF
    Name: libimgui
    Description: Dear ImGui
    Version: ${version}
    Libs: -L$out/lib -limgui
    Cflags: -I$out/include/imgui
    EOF
  '';

  meta = with lib; {
    description = "Bloat-free Graphical User interface for C++ with minimal dependencies";
    homepage = "https://github.com/ocornut/imgui";
    license = licenses.mit;
    maintainers = with maintainers; [wolfangaukang];
    platforms = platforms.all;
  };
}
