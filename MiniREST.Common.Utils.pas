unit MiniREST.Common.Utils;

interface

type
  TMiniRESTCommonUtils = class
  public
    class procedure AddToArray<T>(AValue: T; var AArray: TArray<T>);
  end;

implementation

{ TMiniRESTCommonUtils }

class procedure TMiniRESTCommonUtils.AddToArray<T>(AValue: T;
  var AArray: TArray<T>);
begin
  SetLength(AArray, Length(AArray) + 1);
  AArray[Length(AArray) - 1] := AValue;
end;

end.
