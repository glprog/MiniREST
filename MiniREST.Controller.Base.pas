unit MiniREST.Controller.Base;

interface

uses SysUtils, MiniREST.Intf, MiniREST.Common, MiniREST.Controller.Intf;

type
  TMiniRESTControllerBase = class(TInterfacedObject, IMiniRESTController)
  protected
    FActionContext : IMiniRESTActionContext;
  public
    function GetActionContext: IMiniRESTActionContext;
    procedure SetActionContext(AContext: IMiniRESTActionContext);
    procedure Response(AContent : string; AContentType : TMiniRESTResponseType = rtTextHtml; AStatusCode : Integer = 200);
    procedure ResponseJson(AJson: string; AStatusCode : Integer = 200);
    procedure ResponseErro(AMsgErro: string; AStatusCode : Integer = 500);
    function PathVariableAsInteger(AVariable : string) : Integer;
    function PathVariable(AVariable : string) : string;
    function QueryParam(AQueryParam : string) : string;
    function QueryParams : TArray<IMiniRESTQueryParam>;
  end;

  TMiniRESTControllerFactoryBase = class(TInterfacedObject, IMiniRESTControllerFactory)
  private
    FClass : TClass;
    FFactory: TFunc<IMiniRESTController>;
    FIntf : IInterface;
  public
    constructor Create(AClass: TClass; AFactory : TFunc<IMiniRESTController>);
    function GetClass: TClass;
    function GetController: TObject;
    procedure ClearFactory;
  end;

implementation

{ TMiniRESTControllerBase }

function TMiniRESTControllerBase.GetActionContext: IMiniRESTActionContext;
begin
  Result := FActionContext;
end;

function TMiniRESTControllerBase.PathVariable(AVariable: string): string;
begin
  Result := GetActionContext.GetPathVariable(AVariable);
end;

function TMiniRESTControllerBase.PathVariableAsInteger(
  AVariable: string): Integer;
begin
  Result := StrToInt(PathVariable(AVariable));
end;

function TMiniRESTControllerBase.QueryParam(AQueryParam: string): string;
var LQueryParam : IMiniRESTQueryParam;
begin
  LQueryParam := FActionContext.GetQueryParam(AQueryParam);
  if LQueryParam <> nil then
    Result := LQueryParam.GetValue
  else
    Result := '';
end;

function TMiniRESTControllerBase.QueryParams: TArray<IMiniRESTQueryParam>;
begin
  Result := FActionContext.GetQueryParams;
end;

procedure TMiniRESTControllerBase.Response(AContent: string;
  AContentType: TMiniRESTResponseType; AStatusCode : Integer);
begin
  GetActionContext.SetResponseContent(AContent, AContentType);
end;

procedure TMiniRESTControllerBase.ResponseErro(AMsgErro: string; AStatusCode : Integer);
begin
  Response('{"erro":"' + AMsgErro + '"}', rtApplicationJson, AStatusCode);
end;

procedure TMiniRESTControllerBase.ResponseJson(AJson: string; AStatusCode : Integer);
begin
  Response(AJson, rtApplicationJson, AStatusCode);
end;

procedure TMiniRESTControllerBase.SetActionContext(
  AContext: IMiniRESTActionContext);
begin
  FActionContext := AContext;
end;

{ TMiniRESTControllerFactoryBase }

procedure TMiniRESTControllerFactoryBase.ClearFactory;
begin
  FIntf := nil;
end;

constructor TMiniRESTControllerFactoryBase.Create(AClass: TClass;
  AFactory: TFunc<IMiniRESTController>);
begin
  FClass := AClass;
  FFactory := AFactory;
end;

function TMiniRESTControllerFactoryBase.GetClass: TClass;
begin
  Result := FClass;
end;

function TMiniRESTControllerFactoryBase.GetController: TObject;
begin
  FIntf := FFactory();
  Result := TObject(FIntf);
end;

end.
