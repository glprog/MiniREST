unit MiniRest.RequestInfo;

interface

uses Classes, SysUtils, MiniRest.Intf, MiniRest.Common, IdCustomHTTPServer;

type
  TMiniRestRequestInfo = class(TInterfacedObject, IMiniRestRequestInfo)
  private
    FPathLength : Integer;
    FRequestMethod : TMiniRestRequestMethod;
    FPathParams : TArray<string>;
    function ParseMappingtoPathParams(AMapping : string) : TArray<string>;
  public
    constructor Create(AMapping : string; ARequestMethod : TMiniRestRequestMethod);
    function GetPathLength: Integer;
    function GetRequestMethod: TMiniRestRequestMethod;
    function IsMatch(AMapping: string; ARequestMethod : TMiniRestRequestMethod): Boolean;
  end;

implementation

{ TMiniRestRequestInfo }

constructor TMiniRestRequestInfo.Create(AMapping: string;
  ARequestMethod: TMiniRestRequestMethod);
begin
  FRequestMethod := ARequestMethod;
  FPathParams := ParseMappingtoPathParams(AMapping);
  FPathLength := Length(FPathParams);
end;

function TMiniRestRequestInfo.GetPathLength: Integer;
begin
  Result := FPathLength;
end;

function TMiniRestRequestInfo.GetRequestMethod: TMiniRestRequestMethod;
begin
  Result := FRequestMethod;
end;

function TMiniRestRequestInfo.IsMatch(AMapping: string; ARequestMethod : TMiniRestRequestMethod): Boolean;
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

function TMiniRestRequestInfo.ParseMappingtoPathParams(
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
