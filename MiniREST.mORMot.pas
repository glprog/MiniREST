unit MiniREST.mORMot;

interface

uses Classes, MiniREST.Intf, MiniREST.Common, MiniREST.Server.Base, SynCrtSock,
  SysUtils, IdHeaderList, IdGlobalProtocols, IdGlobal, IdURI;

type
  TMiniRESTServermORMot = class(TMiniRESTServerBase)
  private
    FServer: THttpApiServer;
    FPort: Integer;
    function Process(Ctxt: THttpServerRequest): cardinal;
  public
    constructor Create;
    function Start: Boolean; override;
    function Stop: Boolean; override;
    function GetPort: Integer; override;
    procedure SetPort(APort: Integer); override;
  end;

  TMiniRESTActionContextmORMot = class(TInterfacedObject, IMiniRESTActionContext)
  private
    FRequest: THttpServerRequest;
    FActionInfo: IMiniRESTActionInfo;
    FResponseStatusCode: Integer;
    FHeaders: TIdHeaderList;
    FParams: TStringList;
    FParamsLoaded: Boolean;
    FResponseContentType: TMiniRESTResponseType;
    procedure DecodeAndSetParams;
  public
    constructor Create(ARequest: THttpServerRequest);
    destructor Destroy; override;
    procedure AppendHeader(AName: string; AValue: string);
    function GetActionInfo: IMiniRESTActionInfo;
    function GetAuthToken: string;
    function GetCommandType: TMiniRESTRequestMethod;
    function GetHeader(AName: string): string;
    function GetPathVariable(AVariable: string): string;
    function GetQueryParam(AQueryParam: string): IMiniRESTQueryParam;
    function GetQueryParams: System.TArray<MiniREST.Intf.IMiniRESTQueryParam>;
    function GetRequestContentAsString: string;
    function GetURI: string;
    procedure SendRedirect(ALocation: string);
    procedure ServeFile(AFilePath: string);
    procedure SetActionInfo(AActionInfo: IMiniRESTActionInfo);
    procedure SetHeader(AName: string; AValue: string);
    procedure SetResponseStream(AStream: TStream);
    function GetResponseContent: string;
    procedure SetResponseContent(const AContent: string);
    function GetResponseContentType: TMiniRESTResponseType;
    procedure SetResponseContentType(const AContentType: TMiniRESTResponseType);
    function GetResponseStatusCode: Integer;
    procedure SetResponseStatusCode(const AStatusCode: Integer);
  end;

  TMiniRESTQueryParammORMot = class(TMiniRESTQueryParamBase)
  end;

implementation

uses StrUtils, MiniREST.Util;

{ TMiniRESTServermORMot }

function SockStringToString(const ASockString: SockString): string;
begin
  Result := string(ASockString);
end;

function StringToSockString(const AString: string): SockString;
begin
  Result := SockString(AString);
end;

constructor TMiniRESTServermORMot.Create;
begin
  inherited;
  FServer := THttpApiServer.Create(True);
  FPort := 8090;
//  FServer.RegisterCompress(CompressDeflate); // our server will deflate html :)
  FServer.OnRequest := Process;
  FServer.Clone(31); // will use a thread pool of 32 threads in total
end;

{function IdemPChar(p, up: pAnsiChar): boolean;
// if the beginning of p^ is same as up^ (ignore case - up^ must be already Upper)
var c: AnsiChar;
begin
  result := false;
  if p=nil then
    exit;
  if (up<>nil) and (up^<>#0) then
    repeat
      c := p^;
      if up^<>c then
        if c in ['a'..'z'] then begin
          dec(c,32);
          if up^<>c then
            exit;
        end else exit;
      inc(up);
      inc(p);
    until up^=#0;
  result := true;
end;

function GetHeaderValue(var headers: SockString; const upname: SockString;
  deleteInHeaders: boolean): SockString;
var i,j,k: integer;
begin
  result := '';
  if (headers='') or (upname='') then
    exit;
  i := 1;
  repeat
    k := length(headers)+1;
    for j := i to k-1 do
      if headers[j]<' ' then begin
        k := j;
        break;
      end;
    if IdemPChar(@headers[i],pointer(upname)) then begin
      j := i;
      inc(i,length(upname));
      while headers[i]=' ' do inc(i);
      result := copy(headers,i,k-i);
      if deleteInHeaders then begin
        while true do
          if (headers[k]=#0) or (headers[k]>=' ') then
            break else
            inc(k);
        delete(headers,j,k-j);
      end;
      exit;
    end;
    i := k;
    while headers[i]<' ' do
      if headers[i]=#0 then
        exit else
        inc(i);
  until false;
end;}

function TMiniRESTServermORMot.GetPort: Integer;
begin
  Result := FPort;
end;

function TMiniRESTServermORMot.Process(Ctxt: THttpServerRequest): cardinal;
var
  LContext: IMiniRESTActionContext;
begin
  LContext := TMiniRESTActionContextmORMot.Create(Ctxt);
  FindController(LContext);
  Result := LContext.GetResponseStatusCode;
end;

procedure TMiniRESTServermORMot.SetPort(APort: Integer);
begin
  FPort := APort;
end;

function TMiniRESTServermORMot.Start: Boolean;
begin
  if FServer.Started then
    FServer.Shutdown;  
  FServer.RemoveUrl(StringToSockString(''), StringToSockString(IntToStr(GetPort)));
  FServer.AddUrl(StringToSockString(''), StringToSockString(IntToStr(GetPort)), false, StringToSockString('+'),true);
  Result := True;
end;

function TMiniRESTServermORMot.Stop: Boolean;
begin
  FServer.Shutdown;
  Result := True;
end;

{ TMiniRESTActionContextmORMot }

procedure TMiniRESTActionContextmORMot.AppendHeader(AName, AValue: string);
begin
  FHeaders.Text := SockStringToString(FRequest.OutCustomHeaders);
  FHeaders.AddValue(AName, AValue);
  FRequest.OutCustomHeaders := StringToSockString(FHeaders.Text);
end;

constructor TMiniRESTActionContextmORMot.Create(ARequest: THttpServerRequest);
begin
  FRequest := ARequest;
  FHeaders := TIdHeaderList.Create(QuoteHTTP);
  FHeaders.FoldLines := True;
  FHeaders.UnfoldLines := True;
  FParams := TStringList.Create;
  FParamsLoaded := False;  
  SetResponseStatusCode(200);
end;

function TMiniRESTActionContextmORMot.GetActionInfo: IMiniRESTActionInfo;
begin
  Result := FActionInfo;
end;

function TMiniRESTActionContextmORMot.GetAuthToken: string;
begin
  Result := GetHeader('MRestToken');
end;

function TMiniRESTActionContextmORMot.GetCommandType: TMiniRESTRequestMethod;
begin
  if FRequest.Method = 'GET' then
    Result := rmGet
  else
  if FRequest.Method = 'POST' then
    Result := rmPost
  else
  if FRequest.Method = 'DELETE' then
    Result := rmDelete
  else
  if FRequest.Method = 'PUT' then
    Result := rmPut
  else
  if FRequest.Method = 'OPTIONS' then
    Result := rmOptions
  else
    raise Exception.Create('Não implementado');
end;

function TMiniRESTActionContextmORMot.GetHeader(AName: string): string;
begin  
  FHeaders.Text := SockStringToString(FRequest.InHeaders);
  Result := FHeaders.Values[AName];
end;

function TMiniRESTActionContextmORMot.GetPathVariable(AVariable: string): string;
begin
  Result := TMiniRESTUtil.GetPathVariable(AVariable, SockStringToString(FRequest.URL), Self);
end;

function TMiniRESTActionContextmORMot.GetQueryParam(AQueryParam: string): IMiniRESTQueryParam;
begin
  if not FParamsLoaded then
    DecodeAndSetParams;
  Result := nil;
  if FParams.IndexOfName(AQueryParam) > -1 then
    Result := TMiniRESTQueryParammORMot.Create(AQueryParam, FParams.Values[AQueryParam]);
end;

function TMiniRESTActionContextmORMot.GetQueryParams: System.TArray<MiniREST.Intf.IMiniRESTQueryParam>;
var I : Integer;
begin
  if not FParamsLoaded then
    DecodeAndSetParams;
  SetLength(Result, 0);
  for I := 0 to FParams.Count - 1 do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := TMiniRESTQueryParammORMot.Create(FParams.Names[I], FParams.ValueFromIndex[I]);
  end;
end;

function TMiniRESTActionContextmORMot.GetRequestContentAsString: string;
begin
  Result := SockStringToString(FRequest.InContent);
end;

function TMiniRESTActionContextmORMot.GetResponseContent: string;
begin
  Result := SockStringToString(FRequest.OutContent);
end;

function TMiniRESTActionContextmORMot.GetResponseContentType: TMiniRESTResponseType;
begin
  Result := FResponseContentType;
end;

function TMiniRESTActionContextmORMot.GetResponseStatusCode: Integer;
begin
  Result := FResponseStatusCode;
end;

function TMiniRESTActionContextmORMot.GetURI: string;
var
  LUri: TURI;
begin
  LUri.From(FRequest.URL);
  Result := SockStringToString(LUri.Root);
end;

procedure TMiniRESTActionContextmORMot.SendRedirect(ALocation: string);
begin
  SetHeader('Location', ALocation);
  SetResponseStatusCode(302);
end;

procedure TMiniRESTActionContextmORMot.ServeFile(AFilePath: string);
begin

end;

procedure TMiniRESTActionContextmORMot.SetActionInfo(AActionInfo: IMiniRESTActionInfo);
begin
  FActionInfo := AActionInfo;
end;

procedure TMiniRESTActionContextmORMot.SetHeader(AName, AValue: string);
begin
  FRequest.OutCustomHeaders := FRequest.OutCustomHeaders + #13#10 +
  AName + ': ' + AValue;
end;

procedure TMiniRESTActionContextmORMot.SetResponseContent(const AContent: string);
begin
  FRequest.OutContent := StringToSockString(AContent);
end;

procedure TMiniRESTActionContextmORMot.SetResponseContentType(
  const AContentType: TMiniRESTResponseType);
begin  
  FResponseContentType := AContentType;
  case AContentType of
    rtTextHtml, rtApplicationJson: FRequest.OutContentType := MiniRESTResponseTypes[AContentType] + '; charset=utf-8';  
  end;   
end;

procedure TMiniRESTActionContextmORMot.SetResponseStatusCode(const AStatusCode: Integer);
begin
  FResponseStatusCode := AStatusCode;
end;

procedure TMiniRESTActionContextmORMot.SetResponseStream(AStream: TStream);
begin

end;

procedure TMiniRESTActionContextmORMot.DecodeAndSetParams;
var
  i, j, posSep : Integer;
  s, LCharSet, LValue: string;
  LEncoding: IIdTextEncoding;
begin
  // Convert special characters
  // ampersand '&' separates values    {Do not Localize}
  FParams.BeginUpdate;
  try
    FParams.Clear;
    { TODO : CHANGE THIS }
    FHeaders.Text := FRequest.InHeaders;
    LCharSet := FHeaders.Params['Content-Type', 'charset'];
    if LCharSet = '' then
      LCharSet := 'utf-8';
    // which charset to use for decoding query string parameters.  We
    // should not be using the 'Content-Type' charset for that.  For
    // 'application/x-www-form-urlencoded' forms, we should be, though...
    posSep := Pos('?', FRequest.URL);
    if posSep > 0 then
      LValue := Copy(FRequest.URL, posSep + 1);
    LEncoding := CharsetToEncoding(LCharSet);
    i := 1;
    while i <= Length(LValue) do
    begin
      j := i;
      while (j <= Length(LValue)) and (LValue[j] <> '&') do {do not localize}
      begin
        Inc(j);
      end;
      s := Copy(LValue, i, j-i);
      // See RFC 1866 section 8.2.1. TP
      s := ReplaceAll(s, '+', ' ');  {do not localize}
      FParams.Add(TIdURI.URLDecode(s, LEncoding));
      i := j + 1;
    end;
  finally
    FParamsLoaded := True;
    FParams.EndUpdate;
  end;  
end;

destructor TMiniRESTActionContextmORMot.Destroy;
begin
  FHeaders.Free;
  FParams.Free;
  inherited;  
end;

end.
