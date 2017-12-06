unit MiniREST.Server.Base;

interface

uses SysUtils, Rtti, Generics.Defaults, MiniREST.Intf, MiniREST.Server.Intf,
  MiniREST.Controller.Intf, MiniREST.Controller.Base, MiniREST.Common, MiniREST.Attribute,
  Generics.Collections, SyncObjs, MiniREST.Controller.Security.Intf;

type
  TMiniRESTServerBase = class(TInterfacedObject, IMiniRESTServer) // Mover para outra unit
  strict private
    type
      TMiniRESTActionInfo = class(TInterfacedObject, IMiniRESTActionInfo)
      private
        FMapping : string;
        FMethod : TRttiMethod;
        FClass : TClass;
        FRequestMethod : TMiniRESTRequestMethod;
        FPermission : string;
        FIsFactory : Boolean;
        FFactory : IMiniRESTControllerFactory;
      public
        constructor Create(AMapping, APermission : string; AMethod : TRttiMethod; ARequestMethod : TMiniRESTRequestMethod; AClass : TClass; AFactory: IMiniRESTControllerFactory = nil);
        function GetClass: TClass;
        function GetMapping: string;
        function GetMethod: TRttiMethod;
        function GetRequestMethod: TMiniRESTRequestMethod;
        function GetPermission: string;
        function GetIsFactory: Boolean;
        function GetFactory: IMiniRESTControllerFactory;
      end;
  protected
    FControllerOtherwise : TClass;
    FControllers : TObjectDictionary<string,  IMiniRESTActionInfo>;
    FMiddlewares : TList<IMiniRESTMiddleware>;
    FRttiContext : TRttiContext;
    FLock : TObject;
    FSecurityController : TFunc<IMiniRESTSecurityController>;
    FLogger : IMiniRESTLogger;
    procedure Lock;
    procedure Unlock;
    procedure FindController(AContext : IMiniRESTActionContext);
    procedure InternalAddController(AClass: TClass; AControllerFactory: IMiniRESTControllerFactory);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddController(AController: TClass); overload;
    procedure AddController(AControllerFactory: IMiniRESTControllerFactory); overload;
    procedure SetControllerOtherwise(AController: TClass);
    procedure SetSecurityController(AController: TFunc<MiniREST.Controller.Security.Intf.IMiniRESTSecurityController>);
    function GetPort: Integer; virtual; abstract;
    procedure SetPort(APort: Integer); virtual; abstract;
    function Start: Boolean; virtual; abstract;
    function Stop: Boolean; virtual; abstract;
    procedure AddMiddleware(AMiddleware: IMiniRESTMiddleware);
    function GetLogger: IMiniRESTLogger;
    procedure SetLogger(ALogger: IMiniRESTLogger);
  end;

  TMiniRESTQueryParamBase = class(TInterfacedObject, IMiniRESTQueryParam)
  private
    FName : string;
    FValue : string;
  public
    constructor Create(AName, AValue : string);
    function GetName: string;
    function GetValue: string;
  end;

implementation

uses MiniREST.Util, MiniREST.RequestInfo, MiniREST.ControllerOtherwise.Intf,
  MiniREST.JSON;

{ TMiniRESTServer }

procedure TMiniRESTServerBase.AddController(AController: TClass);
begin
  InternalAddController(AController, nil);
end;

procedure TMiniRESTServerBase.AddController(
  AControllerFactory: IMiniRESTControllerFactory);
begin
  InternalAddController(AControllerFactory.GetClass, AControllerFactory);
end;

procedure TMiniRESTServerBase.AddMiddleware(AMiddleware: IMiniRESTMiddleware);
begin
  FMiddlewares.Add(AMiddleware);
end;

constructor TMiniRESTServerBase.Create;
begin
  FLock := TObject.Create;
  FRttiContext := TRttiContext.Create;
  {$IFDEF VER310}
  FRttiContext.KeepContext;
  {$ENDIF}
  FControllers := TObjectDictionary<string, IMiniRESTActionInfo>.Create;
  FMiddlewares := TList<IMiniRESTMiddleware>.Create;
end;

destructor TMiniRESTServerBase.Destroy;
begin
  FControllers.Free;
  FMiddlewares.Free;
  {$IFDEF VER310}
  FRttiContext.DropContext;
  {$ENDIF}
  FLock.Free;
  inherited;
end;

procedure TMiniRESTServerBase.FindController(AContext: IMiniRESTActionContext);
var LRequestInfo : IMiniRESTRequestInfo;
    LMiniRESTActionInfo : IMiniRESTActionInfo;
    LController : TObject;
    LControllerIntf : IMiniRESTController;
    LControllerOtherwise : IMiniRESTControllerOtherwise; //{ TODO : Refatorar - POG!!}
    LActionContext : IMiniRESTActionContext;
    LMiddleware : IMiniRESTMiddleware;
    LObject : TObject;
    LIntfTemp : IInterface;
    LSecurityResponse : IMiniRESTSecurityResponse;
begin
  { TODO : Implementar / Remover Dependencia Indy/ ServerBase }
  Lock;
  LController := nil;
  try
    for LMiddleware in FMiddlewares do
    begin
      if not LMiddleware.Process(AContext) then
        Exit;
    end;
    if Assigned(FSecurityController) and (FSecurityController <> nil) then  { TODO : Remover }
    begin
      //teste
      (*if (not FSecurityController.IsExcludedPath(AContext.GetURI)) and (not FSecurityController.IsValidToken(AContext)) then
      begin
        AContext.SendRedirect('/login');
        Exit;
      end
      else  *)
      (*if (not FSecurityController.HasPermission(AContext)) then
      begin
        AContext.SetResponseContent('{"erro":403}');
        Exit;
      end;*)
    end;
    LRequestInfo := TMiniRESTRequestInfo.Create(AContext.GetURI, AContext.GetCommandType);
    for LMiniRESTActionInfo in FControllers.Values do
    begin
      //LController := LControllerClass.Create;
      { TODO : Refatorar: Otimizar }
      if LRequestInfo.IsMatch(LMiniRESTActionInfo.Mapping, LMiniRESTActionInfo.RequestMethod) then
      begin
        //LController.Action(AContext, ARequestInfo, AResponseInfo);
        AContext.ActionInfo := LMiniRESTActionInfo;
        if Assigned(FSecurityController) and (FSecurityController <> nil) {and (not FSecurityController.HasPermission(AContext))} then
        begin
          LSecurityResponse := FSecurityController.HasPermission(AContext);
          if not LSecurityResponse.HasPermission then
          begin
            AContext.SetResponseContent('{"erro":"' + TMiniRESTJson.TratarJsonString(LSecurityResponse.PermissionErrorMessage) + '"}', rtApplicationJson, 403); { TODO : Mudar msg / obter de outro lugar }
            Exit;
          end;
        end;
        if LMiniRESTActionInfo.IsFactory then
          LController := LMiniRESTActionInfo.Factory.GetController
        else
          LController := LMiniRESTActionInfo.&Class.Create;
        if Supports(LController, IMiniRESTController, LControllerIntf) then
        begin
          LControllerIntf.SetActionContext(AContext);
          if (Length(LMiniRESTActionInfo.Method.GetParameters) = 1) and (LMiniRESTActionInfo.Method.GetParameters[0].ParamType.QualifiedName = 'MiniREST.Intf.IMiniRESTActionContext') then
            LMiniRESTActionInfo.Method.Invoke(TObject(LControllerIntf),[TValue.From<IMiniRESTActionContext>(AContext)])
          else
            LMiniRESTActionInfo.Method.Invoke(TObject(LControllerIntf),[]);
          if LMiniRESTActionInfo.IsFactory then
            LMiniRESTActionInfo.Factory.ClearFactory;
          { TODO : Implementar }
          Exit;
        end
        else
        begin
          try
            if (Length(LMiniRESTActionInfo.Method.GetParameters) = 1) and (LMiniRESTActionInfo.Method.GetParameters[0].ParamType.QualifiedName = 'MiniREST.Intf.IMiniRESTActionContext') then
              LMiniRESTActionInfo.Method.Invoke(TObject(LController),[TValue.From<IMiniRESTActionContext>(AContext)])
            else
              raise Exception.Create('Método ' + LMiniRESTActionInfo.Method.Parent.Name + '.'+ LMiniRESTActionInfo.Method.Name + ' sem parâmetro IMiniRESTActionContext.'); { TODO : Add logger }
            Exit;
          finally
            LController.Free;
          end;
        end;
      end;
    end;
    if FControllerOtherwise <> nil then
    begin
      LObject := FControllerOtherwise.Create;
      if Supports(LObject, IMiniRESTControllerOtherwise, LControllerOtherwise) then
        LControllerOtherwise.Action(AContext)
      else
      begin
        LObject.Free;
        raise Exception.Create('Classe ' + FControllerOtherwise.ClassName + ' não suporta interface IMiniRESTControllerOtherwise');
      end;
    end;
  finally
    Unlock;
  end;
end;

function TMiniRESTServerBase.GetLogger: IMiniRESTLogger;
begin
  Result := FLogger;
end;

procedure TMiniRESTServerBase.InternalAddController(AClass: TClass;
  AControllerFactory: IMiniRESTControllerFactory);
var LType : TRttiType;
    LMethod : TRttiMethod;
    LAttribute : TCustomAttribute;
    LRequestAttribute : RequestMappingAttribute;
begin
  LType := FRttiContext.GetType(AClass);
  for LMethod in LType.GetMethods do
  begin
    for LAttribute in LMethod.GetAttributes do
    begin
      if LAttribute.ClassType = RequestMappingAttribute then
      begin
        LRequestAttribute := RequestMappingAttribute(LAttribute);
        FControllers.Add(LRequestAttribute.Mapping + '|' + IntToStr(Integer(LRequestAttribute.RequestMethod)), TMiniRESTActionInfo.Create(
        LRequestAttribute.Mapping, LRequestAttribute.Permission, LMethod, LRequestAttribute.RequestMethod, AClass, AControllerFactory));
      end;
    end;
  end;
end;

procedure TMiniRESTServerBase.Lock;
begin
  TMonitor.Enter(FLock);
end;

procedure TMiniRESTServerBase.SetControllerOtherwise(
  AController: TClass);
begin
  FControllerOtherwise := AController;
end;

procedure TMiniRESTServerBase.SetLogger(ALogger: IMiniRESTLogger);
begin
  FLogger := ALogger;
end;

procedure TMiniRESTServerBase.SetSecurityController(
  AController: TFunc<MiniREST.Controller.Security.Intf.IMiniRESTSecurityController>);
begin
  FSecurityController := AController;
end;

procedure TMiniRESTServerBase.Unlock;
begin
  TMonitor.Exit(FLock);
end;

{ TMiniRESTServer.TMiniRESTActionInfo }

constructor TMiniRESTServerBase.TMiniRESTActionInfo.Create(AMapping, APermission : string;
  AMethod: TRttiMethod; ARequestMethod : TMiniRESTRequestMethod; AClass: TClass;
  AFactory: IMiniRESTControllerFactory);
begin
  FMapping := AMapping;
  FMethod := AMethod;
  FClass := AClass;
  FRequestMethod := ARequestMethod;
  FPermission := APermission;
  FFactory := AFactory;
  FIsFactory := AFactory <> nil;
end;

function TMiniRESTServerBase.TMiniRESTActionInfo.GetClass: TClass;
begin
  Result := FClass;
end;

function TMiniRESTServerBase.TMiniRESTActionInfo.GetFactory: IMiniRESTControllerFactory;
begin
  Result := FFactory;
end;

function TMiniRESTServerBase.TMiniRESTActionInfo.GetIsFactory: Boolean;
begin
  Result := FIsFactory;
end;

function TMiniRESTServerBase.TMiniRESTActionInfo.GetMapping: string;
begin
  Result := FMapping;
end;

function TMiniRESTServerBase.TMiniRESTActionInfo.GetMethod: TRttiMethod;
begin
  Result := FMethod;
end;

function TMiniRESTServerBase.TMiniRESTActionInfo.GetPermission: string;
begin
  Result := FPermission;
end;

function TMiniRESTServerBase.TMiniRESTActionInfo.GetRequestMethod: TMiniRESTRequestMethod;
begin
  Result := FRequestMethod;
end;

{ TMiniRESTQueryParamBase }

constructor TMiniRESTQueryParamBase.Create(AName, AValue: string);
begin
  FName := AName;
  FValue := AValue;
end;

function TMiniRESTQueryParamBase.GetName: string;
begin
  Result := FName;
end;

function TMiniRESTQueryParamBase.GetValue: string;
begin
  Result := FValue;
end;

end.
