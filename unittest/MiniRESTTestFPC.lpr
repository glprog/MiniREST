program MiniRESTTestFPC;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, TextTestRunner, TestCase1, uTest.Indy.FPC;

begin
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');

  RunRegisteredTests;
end.
