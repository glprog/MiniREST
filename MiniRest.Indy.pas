unit MiniRest.Indy;

interface

uses Classes, SysUtils, {$IF DEFINED(VER310) OR DEFINED(VER290)} JSON {$ELSE} DBXJSON {$IFEND}, MiniRest.Intf, MiniRest.Common,
  MiniRest.Server.Base, IdContext, IdCustomHTTPServer, IdHTTPServer, IdGlobal,
  IdGlobalProtocols, IdSchedulerOfThreadPool;

type
  TMiniRestServerIndy = class(TMiniRestServerBase)
  private
    FHttpServer : TIdHTTPServer;
    FThreadPool : TIdSchedulerOfThreadPool;
    procedure FindController(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); overload;
    function IndyToMiniRestRequestType(ACommandType : THTTPCommandType) : TMiniRestRequestMethod;
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

  TMiniRestActionContextIndy = class(TInterfacedObject, IMiniRestActionContext)
  private
    FActionInfo : IMiniRestActionInfo;
    FIndyContext : TIdContext;
    FRequestInfo : TIdHTTPRequestInfo;
    FResponseInfo : TIdHTTPResponseInfo;
    FRequestContentString : string;
  public
    class function New(AActionInfo : IMiniRestActionInfo; AIndyContext : TIdContext;
    ARequestInfo : TIdHTTPRequestInfo; AResponseInfo : TIdHTTPResponseInfo) : IMiniRestActionContext;
    constructor Create(AActionInfo : IMiniRestActionInfo; AIndyContext : TIdContext;
    ARequestInfo : TIdHTTPRequestInfo; AResponseInfo : TIdHTTPResponseInfo);
    function GetActionInfo: IMiniRestActionInfo;
    procedure SetActionInfo(AActionInfo: IMiniRestActionInfo);
    function GetIndyContext: TIdContext;
    function GetRequestInfo: TIdHTTPRequestInfo;
    function GetResponseInfo: TIdHTTPResponseInfo;
    function GetRequestContentAsString: string;
    function GetPathVariable(AVariable: string): string;
    function GetAuthToken: string;
    procedure SetResponseContent(AContent: string; AContentType : TMiniRestResponseType = rtTextHtml; AStatusCode: Integer = 200);
    function GetURI: string;
    function GetCommandType: TMiniRestRequestMethod;
    function GetHeader(AName: string): string;
    procedure SetHeader(AName: string; AValue: string);
    procedure AppendHeader(AName: string; AValue: string);
    procedure ServeFile(AFilePath: string);
    procedure SendRedirect(ALocation: string);
    procedure SetResponseStream(AStream: TStream);
    function GetQueryParam(AQueryParam: string): IMiniRestQueryParam;
    function GetQueryParams: System.TArray<MiniRest.Intf.IMiniRestQueryParam>;
  end;

  TMiniRestQueryParamIndy = class(TMiniRestQueryParamBase)
  end;

implementation

uses MiniRest.Util;

{ TMiniRestServerIndy }

constructor TMiniRestServerIndy.Create;
begin
  inherited;
  FHttpServer := TIdHTTPServer.Create(nil);
  FThreadPool := TIdSchedulerOfThreadPool.Create(nil);
  FThreadPool.PoolSize := 50;
  FThreadPool.MaxThreads := 100;
  FHttpServer.Scheduler := FThreadPool;
  FHttpServer.DefaultPort := 8080;
  FHttpServer.ServerSoftware := 'MiniRest 0.1';
  FHttpServer.ListenQueue := 50;
  FHttpServer.OnCommandError := OnCommandError;
  FHttpServer.OnCommandGet := FindController;
  FHttpServer.OnCommandOther := FindController;
end;

destructor TMiniRestServerIndy.Destroy;
begin
  FHttpServer.Free;
  FThreadPool.Free;
  inherited;
end;

procedure TMiniRestServerIndy.FindController(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  {$IF DEFINED(VER310) OR DEFINED(VER290)}
  AContext.Connection.IOHandler.DefStringEncoding := IndyTextEncoding_UTF8;
  {$ELSE}
  AContext.Connection.IOHandler.DefStringEncoding := TEncoding.UTF8;
  {$IFEND}
  AResponseInfo.ContentType := 'text/html; charset=utf-8';
  ARequestInfo.ContentEncoding := 'utf-8';
  FindController(TMiniRestActionContextIndy.New(nil, AContext, ARequestInfo, AResponseInfo));
end;

function TMiniRestServerIndy.GetPort: Integer;
begin
  Result := FHttpServer.DefaultPort;
end;

function TMiniRestServerIndy.IndyToMiniRestRequestType(
  ACommandType: THTTPCommandType): TMiniRestRequestMethod;
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

procedure TMiniRestServerIndy.OnCommandError(AContext: TIdContext;
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

procedure TMiniRestServerIndy.SetPort(APort: Integer);
begin
  FHttpServer.DefaultPort := APort;
end;

function TMiniRestServerIndy.Start: Boolean;
begin
  FHttpServer.Active := True;
  Result := True; { TODO : Refat }
end;

function TMiniRestServerIndy.Stop: Boolean;
begin
  FHttpServer.Active := False;
  Result := True; { TODO : Refat }
end;

{ TMiniRestActionContext }

procedure TMiniRestActionContextIndy.AppendHeader(AName, AValue: string);
begin
  FResponseInfo.CustomHeaders.AddValue(AName, AValue);
end;

constructor TMiniRestActionContextIndy.Create(AActionInfo : IMiniRestActionInfo; AIndyContext : TIdContext;
    ARequestInfo : TIdHTTPRequestInfo; AResponseInfo : TIdHTTPResponseInfo);
begin
  FActionInfo := AActionInfo;
  FIndyContext := AIndyContext;
  FRequestInfo := ARequestInfo;
  FResponseInfo := AResponseInfo;
end;

function TMiniRestActionContextIndy.GetActionInfo: IMiniRestActionInfo;
begin
  Result := FActionInfo;
end;

function TMiniRestActionContextIndy.GetAuthToken: string;
begin
  if FRequestInfo.RawHeaders.IndexOfName('MRestToken') > -1 then
    Result := FRequestInfo.RawHeaders.Values['MRestToken'];
end;

function TMiniRestActionContextIndy.GetCommandType: TMiniRestRequestMethod;
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

function TMiniRestActionContextIndy.GetHeader(AName: string): string;
begin
  Result := FRequestInfo.RawHeaders.Values[AName];
end;

function TMiniRestActionContextIndy.GetIndyContext: TIdContext;
begin
  Result := FIndyContext;
end;

function TMiniRestActionContextIndy.GetPathVariable(AVariable: string): string;
begin
  Result := TMiniRestUtil.GetPathVariable(AVariable,FRequestInfo.Document, Self);
end;

function TMiniRestActionContextIndy.GetQueryParam(
  AQueryParam: string): IMiniRestQueryParam;
begin
  Result := nil;
  if FRequestInfo.Params.IndexOfName(AQueryParam) > -1 then
    Result := TMiniRestQueryParamIndy.Create(AQueryParam, FRequestInfo.Params.Values[AQueryParam]);
end;

function TMiniRestActionContextIndy.GetQueryParams: System.TArray<MiniRest.Intf.IMiniRestQueryParam>;
var I : Integer;
begin
  SetLength(Result, 0);
  for I := 0 to FRequestInfo.Params.Count - 1 do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := TMiniRestQueryParamIndy.Create(FRequestInfo.Params.Names[I], FRequestInfo.Params.ValueFromIndex[I]);
  end;
end;

function TMiniRestActionContextIndy.GetRequestContentAsString: string;
begin
  if FRequestContentString = '' then
  {$IF DEFINED(VER310) OR DEFINED(VER290)}
    FRequestContentString := ReadStringFromStream(FRequestInfo.PostStream, FRequestInfo.PostStream.Size, IndyTextEncoding_UTF8);
  {$ELSE}
    FRequestContentString := ReadStringFromStream(FRequestInfo.PostStream, FRequestInfo.PostStream.Size, TEncoding.UTF8);
  {$IFEND}
  Result := FRequestContentString;
end;

function TMiniRestActionContextIndy.GetRequestInfo: TIdHTTPRequestInfo;
begin
  Result := FRequestInfo;
end;

function TMiniRestActionContextIndy.GetResponseInfo: TIdHTTPResponseInfo;
begin
  Result := FResponseInfo;
end;

function TMiniRestActionContextIndy.GetURI: string;
begin
  Result := FRequestInfo.URI;
end;

class function TMiniRestActionContextIndy.New(
  AActionInfo : IMiniRestActionInfo; AIndyContext : TIdContext;
    ARequestInfo : TIdHTTPRequestInfo; AResponseInfo : TIdHTTPResponseInfo): IMiniRestActionContext;
begin
  Result := Create(AActionInfo, AIndyContext, ARequestInfo, AResponseInfo);
end;

procedure TMiniRestActionContextIndy.SendRedirect(ALocation: string);
begin
  FResponseInfo.Redirect(ALocation);
end;

procedure TMiniRestActionContextIndy.ServeFile(AFilePath: string);
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

procedure TMiniRestActionContextIndy.SetActionInfo(
  AActionInfo: IMiniRestActionInfo);
begin
  FActionInfo := AActionInfo;
end;

procedure TMiniRestActionContextIndy.SetHeader(AName, AValue: string);
begin
  FResponseInfo.CustomHeaders.Values[AName] := AValue;
end;

procedure TMiniRestActionContextIndy.SetResponseContent(AContent: string; AContentType : TMiniRestResponseType; AStatusCode: Integer);
begin
  FResponseInfo.ContentText := AContent;
  case AContentType of
    rtTextHtml: FResponseInfo.ContentType := 'text/html; charset=utf-8';
    else
      FResponseInfo.ContentType := MiniRestResponseTypes[AContentType]; //'text/html; charset=utf-8';
  end;
  FResponseInfo.ResponseNo := AStatusCode;
end;

procedure TMiniRestActionContextIndy.SetResponseStream(AStream: TStream);
begin
  FResponseInfo.ContentStream := AStream;
end;

end.
