{$IFDEF FPC}
  {$mode DELPHI}
{$IFEND}
unit MiniREST.Common.Utils;

interface

uses SysUtils;

type
  TMiniRESTCommonUtils{$IFDEF FPC}<T>{$IFEND} = class
  public
    class procedure AddToArray{$IFNDEF FPC}<T>{$IFEND}(AValue: T; var AArray: TArray<T>);
  end;

implementation

{ TMiniRESTCommonUtils }

class procedure TMiniRESTCommonUtils{$IFDEF FPC}<T>{$IFEND}.AddToArray{$IFNDEF FPC}<T>{$IFEND}(AValue: T;
  var AArray: TArray<T>);
begin
  SetLength(AArray, Length(AArray) + 1);
  AArray[Length(AArray) - 1] := AValue;
end;

end.
