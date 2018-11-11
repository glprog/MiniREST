@taskkill /im MiniRESTSQLTest.exe -F
@call "%BDS_MINIREST%rsvars.bat"
@IF NOT [%1]==[] CD %1
@set DIR_TEMP_EXEC=%cd%
@cd Win32/Debug/
@del MiniRESTSQLTest.exe /q
@cd %DIR_TEMP_EXEC%
@msbuild MiniRESTSQLTest.dproj /t:Build
@cd Win32/Debug/
@call MiniRESTSQLTest.exe
