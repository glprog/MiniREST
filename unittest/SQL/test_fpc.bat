@taskkill /im MiniRESTSQLTest.exe -F
@IF NOT [%1]==[] CD %1
@del MiniRESTSQLTest.exe /q
@lazbuild.exe --build-all MiniRESTSQLTest.lpi
@call MiniRESTSQLTest.exe
