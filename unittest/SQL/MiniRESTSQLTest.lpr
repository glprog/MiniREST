program MiniRESTSQLTest;

{$mode objfpc}{$H+}

uses
  Classes, TextTestRunner, Test.SQL.SQLDb, SysUtils;

begin
  {$DEFINE XMLLISTENER}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');

  RunRegisteredTests;
end.
