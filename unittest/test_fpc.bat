@taskkill /im MiniRESTTestFPC.exe -F
@IF NOT [%1]==[] CD %1
@del MiniRESTTestFPC.exe /q
@lazbuild.exe --build-all MiniRESTTestFPC.lpi
@MiniRESTTestFPC.exe
@node verificaMemoryLeakFPC.js
