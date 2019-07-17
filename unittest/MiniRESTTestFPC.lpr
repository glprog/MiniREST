program MiniRESTTestFPC;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, TextTestRunner, TestCase1, uTest.FPWeb.FPC;

begin
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');

  RunRegisteredTests;
end.
