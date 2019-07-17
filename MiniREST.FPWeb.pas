unit MiniREST.FPWeb;

interface

uses Classes, SysUtils, MiniREST.Intf, MiniREST.Common, MiniREST.Server.Base;

type
  TMiniRESTServerFPWeb = class(TMiniRESTServerBase)
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

  TMiniRESTActionContextFPWeb = class(TInterfacedObject, IMiniRESTActionContext)
  private
    FActionInfo : IMiniRESTActionInfo;
    FIndyContext : TIdContext;
    FRequestInfo : TIdHTTPRequestInfo;
    FResponseInfo : TIdHTTPResponseInfo;
    FRequestContentString : string;
    FResponseContentType: TMiniRESTResponseType;
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
    //procedure SetResponseContent(AContent: string; AContentType : TMiniRESTResponseType = rtTextHtml; AStatusCode: Integer = 200);
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
    function GetResponseContent: string;
    procedure SetResponseContent(const AContent: string);
    function GetResponseContentType: TMiniRESTResponseType;
    procedure SetResponseContentType(const AContentType: TMiniRESTResponseType);
    function GetResponseStatusCode: Integer;
    procedure SetResponseStatusCode(const AStatusCode: Integer);
  end;

  TMiniRESTQueryParamIndy = class(TMiniRESTQueryParamBase)
  end;

implementation

uses MiniREST.Util;

{ TMiniRESTServerFPWeb }

constructor TMiniRESTServerFPWeb.Create;
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

destructor TMiniRESTServerFPWeb.Destroy;
begin
  FHttpServer.Free;
  FThreadPool.Free;
  inherited;
end;

procedure TMiniRESTServerFPWeb.FindController(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  {$IF CompilerVersion >= 31}
  AContext.Connection.IOHandler.DefStringEncoding := IndyTextEncoding_UTF8;
  {$ELSE}
  AContext.Connection.IOHandler.DefStringEncoding := TEncoding.UTF8;
  {$IFEND}
  AResponseInfo.ContentType := 'text/html; charset=utf-8';
  ARequestInfo.ContentEncoding := 'utf-8';
  FindController(TMiniRESTActionContextFPWeb.New(nil, AContext, ARequestInfo, AResponseInfo));
end;

function TMiniRESTServerFPWeb.GetPort: Integer;
begin
  Result := FHttpServer.DefaultPort;
end;

function TMiniRESTServerFPWeb.IndyToMiniRESTRequestType(
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

procedure TMiniRESTServerFPWeb.OnCommandError(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
  AException: Exception);
var LJson : JsonDataObjects.TJSONObject;
begin
  LJson := JsonDataObjects.TJSONObject.Create;
  try
    if GetLogger <> nil then
    try
      GetLogger.Exception(ARequestInfo.URI, AException);
    except
    end;
    LJson.S['message'] := AException.ToString;
    AResponseInfo.ContentText := LJson.ToJSON();
    AResponseInfo.CharSet := 'utf-8';
    AResponseInfo.ContentType := 'application/json;';
  finally
    LJson.Free;
  end;
end;

procedure TMiniRESTServerFPWeb.SetPort(APort: Integer);
begin
  FHttpServer.DefaultPort := APort;
end;

function TMiniRESTServerFPWeb.Start: Boolean;
begin
  FHttpServer.Active := True;
  Result := True; { TODO : Refat }
end;

function TMiniRESTServerFPWeb.Stop: Boolean;
begin
  FHttpServer.Active := False;
  Result := True; { TODO : Refat }
end;

{ TMiniRESTActionContext }

procedure TMiniRESTActionContextFPWeb.AppendHeader(AName, AValue: string);
begin
  FResponseInfo.CustomHeaders.AddValue(AName, AValue);
end;

constructor TMiniRESTActionContextFPWeb.Create(AActionInfo : IMiniRESTActionInfo; AIndyContext : TIdContext;
    ARequestInfo : TIdHTTPRequestInfo; AResponseInfo : TIdHTTPResponseInfo);
begin
  FActionInfo := AActionInfo;
  FIndyContext := AIndyContext;
  FRequestInfo := ARequestInfo;
  FResponseInfo := AResponseInfo;  
end;

function TMiniRESTActionContextFPWeb.GetActionInfo: IMiniRESTActionInfo;
begin
  Result := FActionInfo;
end;

function TMiniRESTActionContextFPWeb.GetAuthToken: string;
begin
  if FRequestInfo.RawHeaders.IndexOfName('MRestToken') > -1 then
    Result := FRequestInfo.RawHeaders.Values['MRestToken'];
end;

function TMiniRESTActionContextFPWeb.GetCommandType: TMiniRESTRequestMethod;
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

function TMiniRESTActionContextFPWeb.GetHeader(AName: string): string;
begin
  Result := FRequestInfo.RawHeaders.Values[AName];
end;

function TMiniRESTActionContextFPWeb.GetIndyContext: TIdContext;
begin
  Result := FIndyContext;
end;

function TMiniRESTActionContextFPWeb.GetPathVariable(AVariable: string): string;
begin
  Result := TMiniRESTUtil.GetPathVariable(AVariable, FRequestInfo.Document, Self);
end;

function TMiniRESTActionContextFPWeb.GetQueryParam(
  AQueryParam: string): IMiniRESTQueryParam;
begin
  Result := nil;
  if FRequestInfo.Params.IndexOfName(AQueryParam) > -1 then
    Result := TMiniRESTQueryParamIndy.Create(AQueryParam, FRequestInfo.Params.Values[AQueryParam]);
end;

function TMiniRESTActionContextFPWeb.GetQueryParams: System.TArray<MiniREST.Intf.IMiniRESTQueryParam>;
var I : Integer;
begin
  SetLength(Result, 0);
  for I := 0 to FRequestInfo.Params.Count - 1 do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := TMiniRESTQueryParamIndy.Create(FRequestInfo.Params.Names[I], FRequestInfo.Params.ValueFromIndex[I]);
  end;
end;

function TMiniRESTActionContextFPWeb.GetRequestContentAsString: string;
begin
  if FRequestContentString = '' then
  {$IF CompilerVersion >= 31}
    FRequestContentString := ReadStringFromStream(FRequestInfo.PostStream, FRequestInfo.PostStream.Size, IndyTextEncoding_UTF8);
  {$ELSE}
    FRequestContentString := ReadStringFromStream(FRequestInfo.PostStream, FRequestInfo.PostStream.Size, TEncoding.UTF8);
  {$IFEND}
  Result := FRequestContentString;
end;

function TMiniRESTActionContextFPWeb.GetRequestInfo: TIdHTTPRequestInfo;
begin
  Result := FRequestInfo;
end;

function TMiniRESTActionContextFPWeb.GetResponseContent: string;
begin
  Result := FResponseInfo.ContentText;
end;

function TMiniRESTActionContextFPWeb.GetResponseContentType: TMiniRESTResponseType;
begin
  Result := FResponseContentType;
end;

function TMiniRESTActionContextFPWeb.GetResponseInfo: TIdHTTPResponseInfo;
begin
  Result := FResponseInfo;
end;

function TMiniRESTActionContextFPWeb.GetResponseStatusCode: Integer;
begin
  Result := FResponseInfo.ResponseNo;
end;

function TMiniRESTActionContextFPWeb.GetURI: string;
begin
  Result := FRequestInfo.URI;
end;

class function TMiniRESTActionContextFPWeb.New(
  AActionInfo : IMiniRESTActionInfo; AIndyContext : TIdContext;
    ARequestInfo : TIdHTTPRequestInfo; AResponseInfo : TIdHTTPResponseInfo): IMiniRESTActionContext;
begin
  Result := Create(AActionInfo, AIndyContext, ARequestInfo, AResponseInfo);
end;

procedure TMiniRESTActionContextFPWeb.SendRedirect(ALocation: string);
begin
  FResponseInfo.Redirect(ALocation);
end;

procedure TMiniRESTActionContextFPWeb.ServeFile(AFilePath: string);
var 
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

procedure TMiniRESTActionContextFPWeb.SetActionInfo(
  AActionInfo: IMiniRESTActionInfo);
begin
  FActionInfo := AActionInfo;
end;

procedure TMiniRESTActionContextFPWeb.SetHeader(AName, AValue: string);
begin
  FResponseInfo.CustomHeaders.Values[AName] := AValue;
end;

procedure TMiniRESTActionContextFPWeb.SetResponseContent(const AContent: string);
begin
  FResponseInfo.ContentText := AContent;
end;

procedure TMiniRESTActionContextFPWeb.SetResponseContentType(
  const AContentType: TMiniRESTResponseType);
begin
  case AContentType of
    rtTextHtml: FResponseInfo.ContentType := 'text/html; charset=utf-8';
  else
    FResponseInfo.ContentType := MiniRESTResponseTypes[AContentType]; //'text/html; charset=utf-8';
  end;
  FResponseContentType := AContentType;
end;

procedure TMiniRESTActionContextFPWeb.SetResponseStatusCode(const AStatusCode: Integer);
begin
  FResponseInfo.ResponseNo := AStatusCode;
end;

procedure TMiniRESTActionContextFPWeb.SetResponseStream(AStream: TStream);
begin
  FResponseInfo.ContentStream := AStream;  
end;

end.
