unit MiniRest.Server.Base;

interface

uses SysUtils, Rtti, Generics.Defaults, MiniRest.Intf, MiniRest.Server.Intf,
  MiniRest.Controller.Intf, MiniRest.Controller.Base, MiniRest.Common, MiniRest.Attribute,
  Generics.Collections, SyncObjs, MiniRest.Controller.Security.Intf;

type
  TMiniRestServerBase = class(TInterfacedObject, IMiniRestServer) // Mover para outra unit
  strict private
    type
      TMiniRestActionInfo = class(TInterfacedObject, IMiniRestActionInfo)
      private
        FMapping : string;
        FMethod : TRttiMethod;
        FClass : TClass;
        FRequestMethod : TMiniRestRequestMethod;
        FPermission : string;
        FIsFactory : Boolean;
        FFactory : IMiniRestControllerFactory;
      public
        constructor Create(AMapping, APermission : string; AMethod : TRttiMethod; ARequestMethod : TMiniRestRequestMethod; AClass : TClass; AFactory: IMiniRestControllerFactory = nil);
        function GetClass: TClass;
        function GetMapping: string;
        function GetMethod: TRttiMethod;
        function GetRequestMethod: TMiniRestRequestMethod;
        function GetPermission: string;
        function GetIsFactory: Boolean;
        function GetFactory: IMiniRestControllerFactory;
      end;
  protected
    FControllerOtherwise : TClass;
    FControllers : TObjectDictionary<string,  IMiniRestActionInfo>;
    FMiddlewares : TList<IMiniRestMiddleware>;
    FRttiContext : TRttiContext;
    FLock : TObject;
    FSecurityController : TFunc<IMiniRestSecurityController>;
    FLogger : IMiniRestLogger;
    procedure Lock;
    procedure Unlock;
    procedure FindController(AContext : IMiniRestActionContext);
    procedure InternalAddController(AClass: TClass; AControllerFactory: IMiniRestControllerFactory);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddController(AController: TClass); overload;
    procedure AddController(AControllerFactory: IMiniRestControllerFactory); overload;
    procedure SetControllerOtherwise(AController: TClass);
    procedure SetSecurityController(AController: TFunc<MiniRest.Controller.Security.Intf.IMiniRestSecurityController>);
    function GetPort: Integer; virtual; abstract;
    procedure SetPort(APort: Integer); virtual; abstract;
    function Start: Boolean; virtual; abstract;
    function Stop: Boolean; virtual; abstract;
    procedure AddMiddleware(AMiddleware: IMiniRestMiddleware);
    function GetLogger: IMiniRestLogger;
    procedure SetLogger(ALogger: IMiniRestLogger);
  end;

  TMiniRestQueryParamBase = class(TInterfacedObject, IMiniRestQueryParam)
  private
    FName : string;
    FValue : string;
  public
    constructor Create(AName, AValue : string);
    function GetName: string;
    function GetValue: string;
  end;

  TMiniRestSecurityResponse = class(TInterfacedObject, IMiniRestSecurityResponse)
  private
    FHasPermission: Boolean;
    FPermissionErrorMessage: string;
  public
    constructor Create(const AHasPermission: Boolean; const APermissionErrorMessage: string = '');
    function GetHasPermission: Boolean;
    function GetPermissionErrorMessage: string;
  end;

implementation

uses MiniRest.Util, MiniRest.RequestInfo, MiniRest.ControllerOtherwise.Intf,
  MiniRest.JSON;

{ TMiniRestServer }

procedure TMiniRestServerBase.AddController(AController: TClass);
begin
  InternalAddController(AController, nil);
end;

procedure TMiniRestServerBase.AddController(
  AControllerFactory: IMiniRestControllerFactory);
begin
  InternalAddController(AControllerFactory.GetClass, AControllerFactory);
end;

procedure TMiniRestServerBase.AddMiddleware(AMiddleware: IMiniRestMiddleware);
begin
  FMiddlewares.Add(AMiddleware);
end;

constructor TMiniRestServerBase.Create;
begin
  FLock := TObject.Create;
  FRttiContext := TRttiContext.Create;
  {$IFDEF VER310}
  FRttiContext.KeepContext;
  {$ENDIF}
  FControllers := TObjectDictionary<string, IMiniRestActionInfo>.Create;
  FMiddlewares := TList<IMiniRestMiddleware>.Create;
end;

destructor TMiniRestServerBase.Destroy;
begin
  FControllers.Free;
  FMiddlewares.Free;
  {$IFDEF VER310}
  FRttiContext.DropContext;
  {$ENDIF}
  FLock.Free;
  inherited;
end;

procedure TMiniRestServerBase.FindController(AContext: IMiniRestActionContext);
var LRequestInfo : IMiniRestRequestInfo;
    LMiniRestActionInfo : IMiniRestActionInfo;
    LController : TObject;
    LControllerIntf : IMiniRestController;
    LControllerOtherwise : IMiniRestControllerOtherwise; //{ TODO : Refatorar - POG!!}
    LActionContext : IMiniRestActionContext;
    LMiddleware : IMiniRestMiddleware;
    LObject : TObject;
    LIntfTemp : IInterface;
    LSecurityResponse : IMiniRestSecurityResponse;
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
    LRequestInfo := TMiniRestRequestInfo.Create(AContext.GetURI, AContext.GetCommandType);
    for LMiniRestActionInfo in FControllers.Values do
    begin
      //LController := LControllerClass.Create;
      { TODO : Refatorar: Otimizar }
      if LRequestInfo.IsMatch(LMiniRestActionInfo.Mapping, LMiniRestActionInfo.RequestMethod) then
      begin
        //LController.Action(AContext, ARequestInfo, AResponseInfo);
        AContext.ActionInfo := LMiniRestActionInfo;
        if Assigned(FSecurityController) and (FSecurityController <> nil) {and (not FSecurityController.HasPermission(AContext))} then
        begin
          LSecurityResponse := FSecurityController.HasPermission(AContext);
          if not LSecurityResponse.HasPermission then
          begin
            AContext.SetResponseContent('{"erro":"' + TMiniRestJson.TratarJsonString(LSecurityResponse.PermissionErrorMessage) + '"}', rtApplicationJson, 403); { TODO : Mudar msg / obter de outro lugar }
            Exit;
          end;
        end;
        if LMiniRestActionInfo.IsFactory then
          LController := LMiniRestActionInfo.Factory.GetController
        else
          LController := LMiniRestActionInfo.&Class.Create;
        if Supports(LController, IMiniRestController, LControllerIntf) then
        begin
          LControllerIntf.SetActionContext(AContext);
          if (Length(LMiniRestActionInfo.Method.GetParameters) = 1) and (LMiniRestActionInfo.Method.GetParameters[0].ParamType.QualifiedName = 'MiniRest.Intf.IMiniRestActionContext') then
            LMiniRestActionInfo.Method.Invoke(TObject(LControllerIntf),[TValue.From<IMiniRestActionContext>(AContext)])
          else
            LMiniRestActionInfo.Method.Invoke(TObject(LControllerIntf),[]);
          if LMiniRestActionInfo.IsFactory then
            LMiniRestActionInfo.Factory.ClearFactory;
          { TODO : Implementar }
          Exit;
        end
        else
        begin
          try
            if (Length(LMiniRestActionInfo.Method.GetParameters) = 1) and (LMiniRestActionInfo.Method.GetParameters[0].ParamType.QualifiedName = 'MiniRest.Intf.IMiniRestActionContext') then
              LMiniRestActionInfo.Method.Invoke(TObject(LController),[TValue.From<IMiniRestActionContext>(AContext)])
            else
              raise Exception.Create('Método ' + LMiniRestActionInfo.Method.Parent.Name + '.'+ LMiniRestActionInfo.Method.Name + ' sem parâmetro IMiniRestActionContext.'); { TODO : Add logger }
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
      if Supports(LObject, IMiniRestControllerOtherwise, LControllerOtherwise) then
        LControllerOtherwise.Action(AContext)
      else
      begin
        LObject.Free;
        raise Exception.Create('Classe ' + FControllerOtherwise.ClassName + ' não suporta interface IMiniRestControllerOtherwise');
      end;
    end;
  finally
    Unlock;
  end;
end;

function TMiniRestServerBase.GetLogger: IMiniRestLogger;
begin
  Result := FLogger;
end;

procedure TMiniRestServerBase.InternalAddController(AClass: TClass;
  AControllerFactory: IMiniRestControllerFactory);
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
        FControllers.Add(LRequestAttribute.Mapping + '|' + IntToStr(Integer(LRequestAttribute.RequestMethod)), TMiniRestActionInfo.Create(
        LRequestAttribute.Mapping, LRequestAttribute.Permission, LMethod, LRequestAttribute.RequestMethod, AClass, AControllerFactory));
      end;
    end;
  end;
end;

procedure TMiniRestServerBase.Lock;
begin
  TMonitor.Enter(FLock);
end;

procedure TMiniRestServerBase.SetControllerOtherwise(
  AController: TClass);
begin
  FControllerOtherwise := AController;
end;

procedure TMiniRestServerBase.SetLogger(ALogger: IMiniRestLogger);
begin
  FLogger := ALogger;
end;

procedure TMiniRestServerBase.SetSecurityController(
  AController: TFunc<MiniRest.Controller.Security.Intf.IMiniRestSecurityController>);
begin
  FSecurityController := AController;
end;

procedure TMiniRestServerBase.Unlock;
begin
  TMonitor.Exit(FLock);
end;

{ TMiniRestServer.TMiniRestActionInfo }

constructor TMiniRestServerBase.TMiniRestActionInfo.Create(AMapping, APermission : string;
  AMethod: TRttiMethod; ARequestMethod : TMiniRestRequestMethod; AClass: TClass;
  AFactory: IMiniRestControllerFactory);
begin
  FMapping := AMapping;
  FMethod := AMethod;
  FClass := AClass;
  FRequestMethod := ARequestMethod;
  FPermission := APermission;
  FFactory := AFactory;
  FIsFactory := AFactory <> nil;
end;

function TMiniRestServerBase.TMiniRestActionInfo.GetClass: TClass;
begin
  Result := FClass;
end;

function TMiniRestServerBase.TMiniRestActionInfo.GetFactory: IMiniRestControllerFactory;
begin
  Result := FFactory;
end;

function TMiniRestServerBase.TMiniRestActionInfo.GetIsFactory: Boolean;
begin
  Result := FIsFactory;
end;

function TMiniRestServerBase.TMiniRestActionInfo.GetMapping: string;
begin
  Result := FMapping;
end;

function TMiniRestServerBase.TMiniRestActionInfo.GetMethod: TRttiMethod;
begin
  Result := FMethod;
end;

function TMiniRestServerBase.TMiniRestActionInfo.GetPermission: string;
begin
  Result := FPermission;
end;

function TMiniRestServerBase.TMiniRestActionInfo.GetRequestMethod: TMiniRestRequestMethod;
begin
  Result := FRequestMethod;
end;

{ TMiniRestQueryParamBase }

constructor TMiniRestQueryParamBase.Create(AName, AValue: string);
begin
  FName := AName;
  FValue := AValue;
end;

function TMiniRestQueryParamBase.GetName: string;
begin
  Result := FName;
end;

function TMiniRestQueryParamBase.GetValue: string;
begin
  Result := FValue;
end;

{ TMiniRestSecurityResponse }

constructor TMiniRestSecurityResponse.Create(const AHasPermission: Boolean;
  const APermissionErrorMessage: string);
begin
  FHasPermission:= AHasPermission;
  FPermissionErrorMessage:= APermissionErrorMessage;
end;

function TMiniRestSecurityResponse.GetHasPermission: Boolean;
begin
  Result:= FHasPermission;
end;

function TMiniRestSecurityResponse.GetPermissionErrorMessage: string;
begin
  Result:= FPermissionErrorMessage;
end;

end.
