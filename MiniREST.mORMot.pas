unit MiniREST.mORMot;

interface

uses Classes, MiniREST.Intf, MiniREST.Common, MiniREST.Server.Base, SynCrtSock, SysUtils;

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
  public
    constructor Create(ARequest: THttpServerRequest);
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

implementation

uses MiniREST.Util;

{ TMiniRESTServermORMot }

constructor TMiniRESTServermORMot.Create;
begin
  inherited;
  FServer := THttpApiServer.Create(True);
  FPort := 8090;
//  FServer.RegisterCompress(CompressDeflate); // our server will deflate html :)
  FServer.OnRequest := Process;
  FServer.Clone(31); // will use a thread pool of 32 threads in total
end;

function IdemPChar(p, up: pAnsiChar): boolean;
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
end;

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
  FServer.RemoveUrl('', IntToStr(GetPort));
  FServer.AddUrl('', IntToStr(GetPort),false,'+',true);
end;

function TMiniRESTServermORMot.Stop: Boolean;
begin
  FServer.Shutdown
end;

{ TMiniRESTActionContextmORMot }

procedure TMiniRESTActionContextmORMot.AppendHeader(AName, AValue: string);
begin

end;

constructor TMiniRESTActionContextmORMot.Create(ARequest: THttpServerRequest);
begin
  FRequest := ARequest;
end;

function TMiniRESTActionContextmORMot.GetActionInfo: IMiniRESTActionInfo;
begin
  Result := FActionInfo;
end;

function TMiniRESTActionContextmORMot.GetAuthToken: string;
begin

end;

function TMiniRESTActionContextmORMot.GetCommandType: TMiniRESTRequestMethod;
begin

end;

function TMiniRESTActionContextmORMot.GetHeader(AName: string): string;
var
  LHeaders: string;
begin
  LHeaders := FRequest.OutCustomHeaders;
  Result := GetHeaderValue(LHeaders, UpperCase(AName), False);
end;

function TMiniRESTActionContextmORMot.GetPathVariable(AVariable: string): string;
begin
  Result := TMiniRESTUtil.GetPathVariable(AVariable, FRequest.URL, Self);
end;

function TMiniRESTActionContextmORMot.GetQueryParam(AQueryParam: string): IMiniRESTQueryParam;
begin

end;

function TMiniRESTActionContextmORMot.GetQueryParams: System.TArray<MiniREST.Intf.IMiniRESTQueryParam>;
begin

end;

function TMiniRESTActionContextmORMot.GetRequestContentAsString: string;
begin

end;

function TMiniRESTActionContextmORMot.GetResponseContent: string;
begin
  Result := FRequest.OutContent;
end;

function TMiniRESTActionContextmORMot.GetResponseContentType: TMiniRESTResponseType;
begin

end;

function TMiniRESTActionContextmORMot.GetResponseStatusCode: Integer;
begin
  Result := FResponseStatusCode;
end;

function TMiniRESTActionContextmORMot.GetURI: string;
begin
  Result := FRequest.URL;
end;

procedure TMiniRESTActionContextmORMot.SendRedirect(ALocation: string);
begin

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
  FRequest.OutContent := AContent;
end;

procedure TMiniRESTActionContextmORMot.SetResponseContentType(
  const AContentType: TMiniRESTResponseType);
begin

end;

procedure TMiniRESTActionContextmORMot.SetResponseStatusCode(const AStatusCode: Integer);
begin
  FResponseStatusCode := AStatusCode;
end;

procedure TMiniRESTActionContextmORMot.SetResponseStream(AStream: TStream);
begin

end;

end.