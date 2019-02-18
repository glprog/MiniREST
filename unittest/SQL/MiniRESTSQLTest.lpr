program MiniRESTSQLTest;

{$mode objfpc}{$H+}

uses
  Classes, TextTestRunner, Test.SQL.SQLDb, SysUtils;

begin
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');

  RunRegisteredTests;
end.
