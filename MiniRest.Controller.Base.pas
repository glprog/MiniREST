unit MiniRest.Controller.Base;

interface

uses SysUtils, MiniRest.Intf, MiniRest.Common, MiniRest.Controller.Intf;

type
  TMiniRestControllerBase = class(TInterfacedObject, IMiniRestController)
  protected
    FActionContext : IMiniRestActionContext;
  public
    function GetActionContext: IMiniRestActionContext;
    procedure SetActionContext(AContext: IMiniRestActionContext);
    procedure Response(AContent : string; AContentType : TMiniRestResponseType = rtTextHtml; AStatusCode : Integer = 200);
    procedure ResponseJson(AJson: string; AStatusCode : Integer = 200);
    procedure ResponseErro(AMsgErro: string; AStatusCode : Integer = 500);
    function PathVariableAsInteger(AVariable : string) : Integer;
    function PathVariable(AVariable : string) : string;
    function QueryParam(AQueryParam : string) : string;
    function QueryParams : TArray<IMiniRestQueryParam>;
  end;

  TMiniRestControllerFactoryBase = class(TInterfacedObject, IMiniRestControllerFactory)
  private
    FClass : TClass;
    FFactory: TFunc<IMiniRestController>;
    FIntf : IInterface;
  public
    constructor Create(AClass: TClass; AFactory : TFunc<IMiniRestController>);
    function GetClass: TClass;
    function GetController: TObject;
    procedure ClearFactory;
  end;

implementation

{ TMiniRestControllerBase }

function TMiniRestControllerBase.GetActionContext: IMiniRestActionContext;
begin
  Result := FActionContext;
end;

function TMiniRestControllerBase.PathVariable(AVariable: string): string;
begin
  Result := GetActionContext.GetPathVariable(AVariable);
end;

function TMiniRestControllerBase.PathVariableAsInteger(
  AVariable: string): Integer;
begin
  Result := StrToInt(PathVariable(AVariable));
end;

function TMiniRestControllerBase.QueryParam(AQueryParam: string): string;
var LQueryParam : IMiniRestQueryParam;
begin
  LQueryParam := FActionContext.GetQueryParam(AQueryParam);
  if LQueryParam <> nil then
    Result := LQueryParam.GetValue
  else
    Result := '';
end;

function TMiniRestControllerBase.QueryParams: TArray<IMiniRestQueryParam>;
begin
  Result := FActionContext.GetQueryParams;
end;

procedure TMiniRestControllerBase.Response(AContent: string;
  AContentType: TMiniRestResponseType; AStatusCode : Integer);
begin
  GetActionContext.SetResponseContent(AContent, AContentType);
end;

procedure TMiniRestControllerBase.ResponseErro(AMsgErro: string; AStatusCode : Integer);
begin
  Response('{"erro":"' + AMsgErro + '"}', rtApplicationJson, AStatusCode);
end;

procedure TMiniRestControllerBase.ResponseJson(AJson: string; AStatusCode : Integer);
begin
  Response(AJson, rtApplicationJson, AStatusCode);
end;

procedure TMiniRestControllerBase.SetActionContext(
  AContext: IMiniRestActionContext);
begin
  FActionContext := AContext;
end;

{ TMiniRestControllerFactoryBase }

procedure TMiniRestControllerFactoryBase.ClearFactory;
begin
  FIntf := nil;
end;

constructor TMiniRestControllerFactoryBase.Create(AClass: TClass;
  AFactory: TFunc<IMiniRestController>);
begin
  FClass := AClass;
  FFactory := AFactory;
end;

function TMiniRestControllerFactoryBase.GetClass: TClass;
begin
  Result := FClass;
end;

function TMiniRestControllerFactoryBase.GetController: TObject;
begin
  FIntf := FFactory();
  Result := TObject(FIntf);
end;

end.
