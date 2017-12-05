unit MiniREST.Indy;

interface

uses Classes, SysUtils, {$IF DEFINED(VER310) OR DEFINED(VER290)} JSON {$ELSE} DBXJSON {$IFEND}, MiniREST.Intf, MiniREST.Common,
  MiniREST.Server.Base, IdContext, IdCustomHTTPServer, IdHTTPServer, IdGlobal,
  IdGlobalProtocols, IdSchedulerOfThreadPool;

type
  TMiniRESTServerIndy = class(TMiniRESTServerBase)
  private
    FHttpServer : TIdHTTPServer;
    FThreadPool : TIdSchedulerOfThreadPool;
    procedure FindController(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); overload;
    function IndyToMiniRESTRequestType(ACommandType : THTTPCommandType) : TMiniRESTRequestMethod;
    procedure OnCommandError(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
  AException: Exception);
  public
    constructor Create;
    destructor Destroy; override;
    function Start: Boolean; override;
    function Stop: Boolean; override;
    function GetPort: Integer; override;
    procedure SetPort(APort: Integer); override;
  end;

  TMiniRESTActionContextIndy = class(TInterfacedObject, IMiniRESTActionContext)
  private
    FActionInfo : IMiniRESTActionInfo;
    FIndyContext : TIdContext;
    FRequestInfo : TIdHTTPRequestInfo;
    FResponseInfo : TIdHTTPResponseInfo;
    FRequestContentString : string;
  public
    class function New(AActionInfo : IMiniRESTActionInfo; AIndyContext : TIdContext;
    ARequestInfo : TIdHTTPRequestInfo; AResponseInfo : TIdHTTPResponseInfo) : IMiniRESTActionContext;
    constructor Create(AActionInfo : IMiniRESTActionInfo; AIndyContext : TIdContext;
    ARequestInfo : TIdHTTPRequestInfo; AResponseInfo : TIdHTTPResponseInfo);
    function GetActionInfo: IMiniRESTActionInfo;
    procedure SetActionInfo(AActionInfo: IMiniRESTActionInfo);
    function GetIndyContext: TIdContext;
    function GetRequestInfo: TIdHTTPRequestInfo;
    function GetResponseInfo: TIdHTTPResponseInfo;
    function GetRequestContentAsString: string;
    function GetPathVariable(AVariable: string): string;
    function GetAuthToken: string;
    procedure SetResponseContent(AContent: string; AContentType : TMiniRESTResponseType = rtTextHtml; AStatusCode: Integer = 200);
    function GetURI: string;
    function GetCommandType: TMiniRESTRequestMethod;
    function GetHeader(AName: string): string;
    procedure SetHeader(AName: string; AValue: string);
    procedure AppendHeader(AName: string; AValue: string);
    procedure ServeFile(AFilePath: string);
    procedure SendRedirect(ALocation: string);
    procedure SetResponseStream(AStream: TStream);
    function GetQueryParam(AQueryParam: string): IMiniRESTQueryParam;
    function GetQueryParams: System.TArray<MiniREST.Intf.IMiniRESTQueryParam>;
  end;

  TMiniRESTQueryParamIndy = class(TMiniRESTQueryParamBase)
  end;

implementation

uses MiniREST.Util;

{ TMiniRESTServerIndy }

constructor TMiniRESTServerIndy.Create;
begin
  inherited;
  FHttpServer := TIdHTTPServer.Create(nil);
  FThreadPool := TIdSchedulerOfThreadPool.Create(nil);
  FThreadPool.PoolSize := 50;
  FThreadPool.MaxThreads := 100;
  FHttpServer.Scheduler := FThreadPool;
  FHttpServer.DefaultPort := 8080;
  FHttpServer.ServerSoftware := 'MiniREST 0.1';
  FHttpServer.ListenQueue := 50;
  FHttpServer.OnCommandError := OnCommandError;
  FHttpServer.OnCommandGet := FindController;
  FHttpServer.OnCommandOther := FindController;
end;

destructor TMiniRESTServerIndy.Destroy;
begin
  FHttpServer.Free;
  FThreadPool.Free;
  inherited;
end;

procedure TMiniRESTServerIndy.FindController(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  {$IF DEFINED(VER310) OR DEFINED(VER290)}
  AContext.Connection.IOHandler.DefStringEncoding := IndyTextEncoding_UTF8;
  {$ELSE}
  AContext.Connection.IOHandler.DefStringEncoding := TEncoding.UTF8;
  {$IFEND}
  AResponseInfo.ContentType := 'text/html; charset=utf-8';
  ARequestInfo.ContentEncoding := 'utf-8';
  FindController(TMiniRESTActionContextIndy.New(nil, AContext, ARequestInfo, AResponseInfo));
end;

function TMiniRESTServerIndy.GetPort: Integer;
begin
  Result := FHttpServer.DefaultPort;
end;

function TMiniRESTServerIndy.IndyToMiniRESTRequestType(
  ACommandType: THTTPCommandType): TMiniRESTRequestMethod;
begin
  case ACommandType of
    hcGET: Result := rmGet;
    hcPOST: Result := rmPost;
    hcDELETE: Result := rmDelete;
    hcPUT: Result := rmPut;
    hcOPTION: Result := rmOptions;
    else
      raise Exception.Create('Não implementado');
  end;
end;

procedure TMiniRESTServerIndy.OnCommandError(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
  AException: Exception);
var LJson : TJSONObject;
begin
  LJson := TJSONObject.Create;
  try
    if GetLogger <> nil then
    try
      GetLogger.Exception(AException);
    except
    end;
    LJson.AddPair('erro', TJSONString.Create(AException.ToString));
    {$IF DEFINED(VER310) OR DEFINED(VER290)}
    AResponseInfo.ContentText := LJson.ToJSON;
    {$ELSE}
    AResponseInfo.ContentText := LJson.ToString;
    {$IFEND}
    AResponseInfo.CharSet := 'utf-8';
    AResponseInfo.ContentType := 'application/json;';
    AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Origin', '*');
  finally
    LJson.Free;
  end;
end;

procedure TMiniRESTServerIndy.SetPort(APort: Integer);
begin
  FHttpServer.DefaultPort := APort;
end;

function TMiniRESTServerIndy.Start: Boolean;
begin
  FHttpServer.Active := True;
  Result := True; { TODO : Refat }
end;

function TMiniRESTServerIndy.Stop: Boolean;
begin
  FHttpServer.Active := False;
  Result := True; { TODO : Refat }
end;

{ TMiniRESTActionContext }

procedure TMiniRESTActionContextIndy.AppendHeader(AName, AValue: string);
begin
  FResponseInfo.CustomHeaders.AddValue(AName, AValue);
end;

constructor TMiniRESTActionContextIndy.Create(AActionInfo : IMiniRESTActionInfo; AIndyContext : TIdContext;
    ARequestInfo : TIdHTTPRequestInfo; AResponseInfo : TIdHTTPResponseInfo);
begin
  FActionInfo := AActionInfo;
  FIndyContext := AIndyContext;
  FRequestInfo := ARequestInfo;
  FResponseInfo := AResponseInfo;
end;

function TMiniRESTActionContextIndy.GetActionInfo: IMiniRESTActionInfo;
begin
  Result := FActionInfo;
end;

function TMiniRESTActionContextIndy.GetAuthToken: string;
begin
  if FRequestInfo.RawHeaders.IndexOfName('MRestToken') > -1 then
    Result := FRequestInfo.RawHeaders.Values['MRestToken'];
end;

function TMiniRESTActionContextIndy.GetCommandType: TMiniRESTRequestMethod;
begin
  case FRequestInfo.CommandType of
    hcGET: Result := rmGet;
    hcPOST: Result := rmPost;
    hcDELETE: Result := rmDelete ;
    hcPUT: Result := rmPut;
    hcOPTION: Result := rmOptions;
    else
      raise Exception.Create('Não implementado');
  end;
end;

function TMiniRESTActionContextIndy.GetHeader(AName: string): string;
begin
  Result := FRequestInfo.RawHeaders.Values[AName];
end;

function TMiniRESTActionContextIndy.GetIndyContext: TIdContext;
begin
  Result := FIndyContext;
end;

function TMiniRESTActionContextIndy.GetPathVariable(AVariable: string): string;
begin
  Result := TMiniRESTUtil.GetPathVariable(AVariable,FRequestInfo.Document, Self);
end;

function TMiniRESTActionContextIndy.GetQueryParam(
  AQueryParam: string): IMiniRESTQueryParam;
begin
  Result := nil;
  if FRequestInfo.Params.IndexOfName(AQueryParam) > -1 then
    Result := TMiniRESTQueryParamIndy.Create(AQueryParam, FRequestInfo.Params.Values[AQueryParam]);
end;

function TMiniRESTActionContextIndy.GetQueryParams: System.TArray<MiniREST.Intf.IMiniRESTQueryParam>;
var I : Integer;
begin
  SetLength(Result, 0);
  for I := 0 to FRequestInfo.Params.Count - 1 do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := TMiniRESTQueryParamIndy.Create(FRequestInfo.Params.Names[I], FRequestInfo.Params.ValueFromIndex[I]);
  end;
end;

function TMiniRESTActionContextIndy.GetRequestContentAsString: string;
begin
  if FRequestContentString = '' then
  {$IF DEFINED(VER310) OR DEFINED(VER290)}
    FRequestContentString := ReadStringFromStream(FRequestInfo.PostStream, FRequestInfo.PostStream.Size, IndyTextEncoding_UTF8);
  {$ELSE}
    FRequestContentString := ReadStringFromStream(FRequestInfo.PostStream, FRequestInfo.PostStream.Size, TEncoding.UTF8);
  {$IFEND}
  Result := FRequestContentString;
end;

function TMiniRESTActionContextIndy.GetRequestInfo: TIdHTTPRequestInfo;
begin
  Result := FRequestInfo;
end;

function TMiniRESTActionContextIndy.GetResponseInfo: TIdHTTPResponseInfo;
begin
  Result := FResponseInfo;
end;

function TMiniRESTActionContextIndy.GetURI: string;
begin
  Result := FRequestInfo.URI;
end;

class function TMiniRESTActionContextIndy.New(
  AActionInfo : IMiniRESTActionInfo; AIndyContext : TIdContext;
    ARequestInfo : TIdHTTPRequestInfo; AResponseInfo : TIdHTTPResponseInfo): IMiniRESTActionContext;
begin
  Result := Create(AActionInfo, AIndyContext, ARequestInfo, AResponseInfo);
end;

procedure TMiniRESTActionContextIndy.SendRedirect(ALocation: string);
begin
  FResponseInfo.Redirect(ALocation);
end;

procedure TMiniRESTActionContextIndy.ServeFile(AFilePath: string);
var LFileStream : TFileStream;
  LFileName : string;
begin
  LFileName := ProcessPath(ExtractFilePath(ParamStr(0)), AFilePath);
  FResponseInfo.ContentDisposition := ' ';
  FResponseInfo.CharSet := 'utf-8';
  FResponseInfo.ContentEncoding := 'utf-8';
  if FileExists(LFileName) then
  begin
    FResponseInfo.ContentType := 'text/html; charset=utf-8';
    FResponseInfo.ContentType := FResponseInfo.HTTPServer.MIMETable.GetFileMIMEType(LFileName);
//      AResponseInfo.ContentType := AResponseInfo.ContentType + '; charset=utf-8';
    FResponseInfo.CharSet := 'utf-8';
    FResponseInfo.ServeFile(FIndyContext, LFileName);
  end;
end;

procedure TMiniRESTActionContextIndy.SetActionInfo(
  AActionInfo: IMiniRESTActionInfo);
begin
  FActionInfo := AActionInfo;
end;

procedure TMiniRESTActionContextIndy.SetHeader(AName, AValue: string);
begin
  FResponseInfo.CustomHeaders.Values[AName] := AValue;
end;

procedure TMiniRESTActionContextIndy.SetResponseContent(AContent: string; AContentType : TMiniRESTResponseType; AStatusCode: Integer);
begin
  FResponseInfo.ContentText := AContent;
  case AContentType of
    rtTextHtml: FResponseInfo.ContentType := 'text/html; charset=utf-8';
    else
      FResponseInfo.ContentType := MiniRESTResponseTypes[AContentType]; //'text/html; charset=utf-8';
  end;
  FResponseInfo.ResponseNo := AStatusCode;
end;

procedure TMiniRESTActionContextIndy.SetResponseStream(AStream: TStream);
begin
  FResponseInfo.ContentStream := AStream;
end;

end.
