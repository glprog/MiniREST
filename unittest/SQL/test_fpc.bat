@taskkill /im MiniRESTSQLTest.exe -F
@IF NOT [%1]==[] CD %1
@lazbuild.exe MiniRESTSQLTest.lpi
@call MiniRESTSQLTest.exe
