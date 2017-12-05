unit MiniREST.RequestInfo;

interface

uses Classes, SysUtils, MiniREST.Intf, MiniREST.Common, IdCustomHTTPServer;

type
  TMiniRESTRequestInfo = class(TInterfacedObject, IMiniRESTRequestInfo)
  private
    FPathLength : Integer;
    FRequestMethod : TMiniRESTRequestMethod;
    FPathParams : TArray<string>;
    function ParseMappingtoPathParams(AMapping : string) : TArray<string>;
  public
    constructor Create(AMapping : string; ARequestMethod : TMiniRESTRequestMethod);
    function GetPathLength: Integer;
    function GetRequestMethod: TMiniRESTRequestMethod;
    function IsMatch(AMapping: string; ARequestMethod : TMiniRESTRequestMethod): Boolean;
  end;

implementation

{ TMiniRESTRequestInfo }

constructor TMiniRESTRequestInfo.Create(AMapping: string;
  ARequestMethod: TMiniRESTRequestMethod);
begin
  FRequestMethod := ARequestMethod;
  FPathParams := ParseMappingtoPathParams(AMapping);
  FPathLength := Length(FPathParams);
end;

function TMiniRESTRequestInfo.GetPathLength: Integer;
begin
  Result := FPathLength;
end;

function TMiniRESTRequestInfo.GetRequestMethod: TMiniRESTRequestMethod;
begin
  Result := FRequestMethod;
end;

function TMiniRESTRequestInfo.IsMatch(AMapping: string; ARequestMethod : TMiniRESTRequestMethod): Boolean;
var LPath : TStringList;
    LPathParams : TArray<string>;
    I : Integer;
begin
  Result := True;
  if FRequestMethod <> ARequestMethod then
    Exit(False);
  LPathParams := ParseMappingtoPathParams(AMapping);
  if Length(LPathParams) <> FPathLength then
    Exit(False);
  for I := 0 to FPathLength - 1 do
  begin
    if (FPathParams[I] = LPathParams[I]) or (Pos('{', LPathParams[I]) > 0 {LPathParams[I].IndexOf('{') > -1}) then
      Continue
    else
      Exit(False);
  end;
end;

function TMiniRESTRequestInfo.ParseMappingtoPathParams(
  AMapping: string): TArray<string>;
var LPath : TStringList;
    I : Integer;
    S : string;
begin
  SetLength(Result, 0);
  LPath := TStringList.Create; { TODO : Refatorar: Mandar para classe utilitária }
  LPath.StrictDelimiter := True;
  LPath.Delimiter := '/';
  try
    LPath.DelimitedText := AMapping;
    for I := 0 to LPath.Count - 1 do
      if Trim(LPath[I]) <> '' then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := LPath[I];
        //Result := Result + [LPath[I]];
      end;
  finally
    LPath.Free;
  end;
end;

end.
