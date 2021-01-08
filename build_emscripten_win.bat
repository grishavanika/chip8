@echo off

:: How to install emscripten
:: 
::cd /d C:\dev\_opensource
::git clone https://github.com/emscripten-core/emsdk.git
::cd emsdk
::emsdk install latest
::emsdk activate latest
::
:: Open NEW cmd and then:
::cd /d C:\dev\_opensource\emsdk
::emsdk_env.bat
:: 

:: Docs: https://emscripten.org/docs/api_reference/emscripten.h.html
:: SDL2 example: https://main.lv/writeup/web_assembly_sdl_example.md
:: 


set em_root=C:\dev\_opensource\emsdk

cd /d %em_root%
call emsdk_env.bat
cd /d %~dp0

echo Building ...

call emcc chip8.cpp ^
	-O2 ^
	-s USE_SDL=2 ^
	--embed-file assets ^
	-DNDEBUG ^
	-std=c++20 ^
	-o chip8_win.js

echo Running...
::emrun --no_browser --port 8080 .
::chip8_win.html
emrun chip8_win.html
