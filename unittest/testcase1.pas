unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework;

type

  TTestCase1= class(TTestCase)
  published
    procedure TestHookUp;
  end;

implementation

procedure TTestCase1.TestHookUp;
begin
  Fail('Write your own test');
end;

initialization
  RegisterTest(TTestCase1.Suite);
end.

