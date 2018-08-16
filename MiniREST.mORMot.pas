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

{ TMiniRESTServermORMot }

constructor TMiniRESTServermORMot.Create;
begin
  FServer := THttpApiServer.Create(True);
  FPort := 8090;
//  FServer.RegisterCompress(CompressDeflate); // our server will deflate html :)
  FServer.OnRequest := Process;
  FServer.Clone(31); // will use a thread pool of 32 threads in total
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
  Result := 200;
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

end;

function TMiniRESTActionContextmORMot.GetAuthToken: string;
begin

end;

function TMiniRESTActionContextmORMot.GetCommandType: TMiniRESTRequestMethod;
begin

end;

function TMiniRESTActionContextmORMot.GetHeader(AName: string): string;
begin

end;

function TMiniRESTActionContextmORMot.GetPathVariable(AVariable: string): string;
begin

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

end;

function TMiniRESTActionContextmORMot.GetResponseContentType: TMiniRESTResponseType;
begin

end;

function TMiniRESTActionContextmORMot.GetResponseStatusCode: Integer;
begin

end;

function TMiniRESTActionContextmORMot.GetURI: string;
begin

end;

procedure TMiniRESTActionContextmORMot.SendRedirect(ALocation: string);
begin

end;

procedure TMiniRESTActionContextmORMot.ServeFile(AFilePath: string);
begin

end;

procedure TMiniRESTActionContextmORMot.SetActionInfo(AActionInfo: IMiniRESTActionInfo);
begin

end;

procedure TMiniRESTActionContextmORMot.SetHeader(AName, AValue: string);
begin

end;

procedure TMiniRESTActionContextmORMot.SetResponseContent(const AContent: string);
begin

end;

procedure TMiniRESTActionContextmORMot.SetResponseContentType(
  const AContentType: TMiniRESTResponseType);
begin

end;

procedure TMiniRESTActionContextmORMot.SetResponseStatusCode(const AStatusCode: Integer);
begin

end;

procedure TMiniRESTActionContextmORMot.SetResponseStream(AStream: TStream);
begin

end;

end.
