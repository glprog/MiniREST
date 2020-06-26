program MiniRESTSQLTest;

{$mode objfpc}{$H+}

uses
  Classes, TextTestRunner, Test.SQL.Firebird, Test.SQL.PostgreSQL, SysUtils;

begin
  {$DEFINE XMLLISTENER}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');

  RunRegisteredTests;
end.
