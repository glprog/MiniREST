{$mode DELPHI}
unit MiniREST.SQL.SQLDb;

interface

uses Classes, SysUtils, MiniREST.SQL.Intf, MiniREST.SQL.Base, MiniREST.SQL.Common, DB,
  sqldb, IBConnection, fgl;

type
  TLogEvent = procedure (Sender : TSQLConnection; EventType : TDBEventType; Const Msg : String);
  IMiniRESTSQLConnectionFactoryParamsSQLDb = interface(IMiniRESTSQLConnectionFactoryParams)
  ['{F2FB358A-6369-4FCE-AA9B-75FFECA18E88}']    
    function GetConnectionString: string;
    procedure SetConnectionString(const AConnectionString: string);
    function GetUserName: string;
    procedure SetUserName(const AUserName: string);
    function GetPassword: string;
    procedure SetPassword(const APassword: string);
    function GetDatabaseType: TMiniRESTSQLDatabaseType;
    procedure SetDatabseType(const ADatabaseType: TMiniRESTSQLDatabaseType);
    function GetDatabaseName: string;
    procedure SetDatabaseName(const ADatabaseName: string);
    function GetLogEvent: TLogEvent;
    procedure SetLogEvent(const ALogEvent: TLogEvent);
    function GetServerHostName: string;
    procedure SetServerHostName(const AServerHostName: string);
  end;

  TMiniRESTSQLConnectionParamsSQLDb = class(TMiniRESTSQLConnectionFactoryParams, IMiniRESTSQLConnectionFactoryParamsSQLDb)
  private
    FConnectionString: string;
    FUserName: string;
    FPassword: string;
    FDatabaseType: TMiniRESTSQLDatabaseType;
    FDatabaseName: string;
    FLogEvent: TLogEvent;
    FServerHostName: string;
  public
    function GetConnectionString: string;
    procedure SetConnectionString(const AConnectionString: string);
    function GetUserName: string;
    procedure SetUserName(const AUserName: string);
    function GetPassword: string;
    procedure SetPassword(const APassword: string);
    function GetDatabaseType: TMiniRESTSQLDatabaseType;
    procedure SetDatabseType(const ADatabaseType: TMiniRESTSQLDatabaseType);
    function GetDatabaseName: string;
    procedure SetDatabaseName(const ADatabaseName: string);
    function GetLogEvent: TLogEvent;
    procedure SetLogEvent(const ALogEvent: TLogEvent);
    function GetServerHostName: string;
    procedure SetServerHostName(const AServerHostName: string);
  end;

  TMiniRESTSQLConnectionFactorySQLDb = class(TMiniRESTSQLConnectionFactoryBase)
  protected
    FConnectionString: string;
    FConnectionParams: IMiniRESTSQLConnectionFactoryParamsSQLDb;
    procedure ReleaseConnection(AConnection: IMiniRESTSQLConnection); override;
    function InternalGetconnection: IMiniRESTSQLConnection; override;
  public
    constructor Create(AParams: IMiniRESTSQLConnectionFactoryParamsSQLDb); reintroduce;
    function GetConnectionsCount: Integer; override;
    function GetQueueCount: Integer; override;
  end;

  { TMiniRESTSQLConnectionSQLDb }

  TMiniRESTSQLConnectionSQLDb = class(TMiniRESTSQLConnectionBase)
  private
    function GetConnectorType(const ADatabaseType: TMiniRESTSQLDatabaseType): String;
    procedure OnBeforeConnect(Sender: TObject);
  protected
    FSQLConnection: TSQLConnector;
    //FTransaction: TDBXTransaction;
    FConnectionParams: IMiniRESTSQLConnectionFactoryParamsSQLDb;
    FTransaction: TSQLTransaction;
    FInExplicitTransaction: Boolean;
    FLogEvent: TLogEvent;
    function GetObject: TObject; override;
    function GetDriverName(const ADatabaseType: TMiniRESTSQLDatabaseType): string;
    procedure Log(Sender : TSQLConnection; EventType : TDBEventType; Const Msg : String);
    procedure SetMiniRESTSQLParamToSQLParam(AMiniRESTSQLParam: IMiniRESTSQLParam; ASQLParam: TParam);
    procedure CheckConnectionIsValid;
    procedure SetConnectionParams;
  public
    constructor Create(AOwner: IMiniRESTSQLConnectionFactory; AParams: IMiniRESTSQLConnectionFactoryParamsSQLDb);
    destructor Destroy; override;
    procedure Connect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function GetQuery: IMiniRESTSQLQuery; override;
    function GetQuery(const ASQL: string;
    AParams: array of IMiniRESTSQLParam): IMiniRESTSQLQuery; override;
    function GetQuery(const ASQL: string): IMiniRESTSQLQuery; override;
    function Execute(const ACommand: string; AParams: array of IMiniRESTSQLParam): Integer; override;
    function GetDatabaseInfo: IMiniRESTSQLDatabaseInfo; override;
    function InTransaction: Boolean; override;
    procedure Invalidate; override;
  end;

  { TMiniRESTSQLQuerySQLDb }

  TMiniRESTSQLQuerySQLDb = class(TInterfacedObject, IMiniRESTSQLQuery)
  protected
    FConnection: IMiniRESTSQLConnection;
    FQry: TSQLQuery;
    FTransaction: TSQLTransaction;
    FSQL: string;
    //FParams: TFPGInterfacedObjectList<string, IMiniRESTSQLParam>;
    FParams: TFPGInterfacedObjectList<IMiniRESTSQLParam>;
    procedure BeforeOpenMiniRESTDataSet(DataSet: TDataSet);
  public
    constructor Create(AConnection: IMiniRESTSQLConnection);
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    function GetSQL: string;
    procedure SetSQL(const ASQL: string);
    function ParamByName(const AParamName: string): IMiniRESTSQLParam;
    function AddParam(AParam: IMiniRESTSQLParam): IMiniRESTSQLQuery;
    function ApplyUpdates(const AMaxErrors: Integer): Integer;
    function GetDataSet: TDataSet;    
  end;

implementation

uses MiniREST.SQL.Firebird;

type
  TMiniRESTSQLConnectionBaseCrack = class(TMiniRESTSQLConnectionBase);

  { TSQLQuery }

  TSQLQueryMiniREST = class(TSQLQuery)
  private
    FInternalMiniRESTSQLBeforeOpenDataSet: TDataSetNotifyEvent;
  protected
    procedure DoBeforeOpen; override;
  end;

{ TSQLQueryMiniREST }

procedure TSQLQueryMiniREST.DoBeforeOpen;
begin
  inherited DoBeforeOpen;
  if Assigned(FInternalMiniRESTSQLBeforeOpenDataSet) then
    FInternalMiniRESTSQLBeforeOpenDataSet(Self);
end;


function TMiniRESTSQLConnectionParamsSQLDb.GetConnectionString: string;
begin
  Result := FConnectionString;
end;

procedure TMiniRESTSQLConnectionParamsSQLDb.SetConnectionString(const AConnectionString: string);
begin
  FConnectionString := AConnectionString;
end;

function TMiniRESTSQLConnectionParamsSQLDb.GetUserName: string;
begin
  Result := FUserName;
end;

procedure TMiniRESTSQLConnectionParamsSQLDb.SetUserName(const AUserName: string);
begin
  FUserName := AUserName;
end;

function TMiniRESTSQLConnectionParamsSQLDb.GetPassword: string;
begin
  Result := FPassword;
end;

procedure TMiniRESTSQLConnectionParamsSQLDb.SetPassword(const APassword: string);
begin
  FPassword := APassword;
end;

function TMiniRESTSQLConnectionParamsSQLDb.GetDatabaseType: TMiniRESTSQLDatabaseType;
begin
  Result := FDatabaseType;
end;

procedure TMiniRESTSQLConnectionParamsSQLDb.SetDatabseType(const ADatabaseType: TMiniRESTSQLDatabaseType);
begin
  FDatabaseType := ADatabaseType;
end;

function TMiniRESTSQLConnectionFactorySQLDb.InternalGetconnection: IMiniRESTSQLConnection;
begin
  Result := TMiniRESTSQLConnectionSQLDb.Create(Self, FConnectionParams);
end;

constructor TMiniRESTSQLConnectionFactorySQLDb.Create(AParams: IMiniRESTSQLConnectionFactoryParamsSQLDb);
begin
  inherited Create(AParams);
  FConnectionParams := AParams;
  FConnectionsCount := AParams.GetConnectionsCount;  
end;

constructor TMiniRESTSQLConnectionSQLDb.Create(AOwner: IMiniRESTSQLConnectionFactory; AParams: IMiniRESTSQLConnectionFactoryParamsSQLDb);
begin
  if AParams = nil then
    raise Exception.Create('Não foram passados os parâmetros');
  FSQLConnection := TSQLConnector.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  if (AParams.GetDatabaseType = dbtFirebird) then
    FTransaction.Params.Text := 'isc_tpb_read_committed';
  //FTransaction.Options := [stoUseImplicit];
  FSQLConnection.Transaction := FTransaction;
  FSQLConnection.OnLog := Log;
  FTransaction.Action := caCommit;
  FConnectionParams := AParams;
  FInExplicitTransaction := False;
  FLogEvent := AParams.GetLogEvent;
  FSQLConnection.BeforeConnect := OnBeforeConnect;
  inherited Create(AOwner);
end;

destructor TMiniRESTSQLConnectionSQLDb.Destroy;
begin  
  FSQLConnection.Free;
  FTransaction.Free;
  inherited Destroy;
end;

procedure TMiniRESTSQLConnectionSQLDb.Connect;
begin
  CheckConnectionIsValid;
  if FSQLConnection.Connected then
    Exit;
  FSQLConnection.Connected := True;
end;

procedure TMiniRESTSQLConnectionSQLDb.StartTransaction;
begin
  CheckConnectionIsValid;
  if FTransaction.Active then
    FTransaction.Rollback;  
  FTransaction.StartTransaction;
  FInExplicitTransaction := True;
end;

procedure TMiniRESTSQLConnectionSQLDb.Commit;
begin  
  CheckConnectionIsValid;
  FTransaction.Commit;
  FInExplicitTransaction := False;
end;

procedure TMiniRESTSQLConnectionSQLDb.Rollback;
begin
  CheckConnectionIsValid;
  FTransaction.Rollback;
  FInExplicitTransaction := False;
end;

function TMiniRESTSQLConnectionSQLDb.GetQuery: IMiniRESTSQLQuery;
begin
  CheckConnectionIsValid;
  Result := TMiniRESTSQLQuerySQLDb.Create(Self);
end;

function TMiniRESTSQLConnectionSQLDb.GetQuery(const ASQL: string; AParams: array of IMiniRESTSQLParam): IMiniRESTSQLQuery;
var
  LParam: IMiniRESTSQLParam;
begin
  Result := GetQuery(ASQL);
  for LParam in AParams do
  begin
    Result.AddParam(LParam);
  end;
end;

function TMiniRESTSQLConnectionSQLDb.GetQuery(const ASQL: string): IMiniRESTSQLQuery;
begin
  Result := GetQuery();
  Result.SQL := ASQL;
end;

function TMiniRESTSQLConnectionSQLDb.Execute(const ACommand: string; AParams: array of IMiniRESTSQLParam): Integer;
var
  LQry: TSQLQuery;
  LParam: TParam;
  LMiniRESTSQLParam: IMiniRESTSQLParam;
begin
  CheckConnectionIsValid;
  Self.Connect;  
  LQry := TSQLQuery.Create(nil);
  try       
    LQry.Options := [sqoAutoCommit];    
    LQry.SQLConnection := FSQLConnection;    
    LQry.SQL.Text := ACommand;    
    try
      for LMiniRESTSQLParam in AParams do
      begin
        LParam := LQry.ParamByName(LMiniRESTSQLParam.GetParamName);
        SetMiniRESTSQLParamToSQLParam(LMiniRESTSQLParam, LParam);
      end;
      LQry.ExecSQL;            
    except
      on E: Exception do
      begin
        FTransaction.Rollback;
        raise;
      end;
    end;    
    Result := LQry.RowsAffected;          
  finally
    LQry.Free;
  end;    
end;

function TMiniRESTSQLConnectionSQLDb.GetDatabaseInfo: IMiniRESTSQLDatabaseInfo;
begin
  CheckConnectionIsValid;
  Result := nil;
  case FConnectionParams.GetDatabaseType of
    dbtFirebird: Result := TMiniRESTSQLDatabaseInfoFirebird.Create(Self);
    else
      raise Exception.Create('TMiniRESTSQLConnectionSQLDb.GetDatabaseInfo: ' +
      'DatabaseType not implemented');
  end;
end;

function TMiniRESTSQLConnectionSQLDb.GetObject: TObject;
begin
  Result := Self;
end;

function TMiniRESTSQLConnectionSQLDb.GetDriverName(const ADatabaseType: TMiniRESTSQLDatabaseType): string;
begin
  case ADatabaseType of
    dbtFirebird: Result := 'Firebird';
  end;
end;

procedure TMiniRESTSQLConnectionSQLDb.Log(Sender: TSQLConnection;
  EventType: TDBEventType; const Msg: String);
begin
  if Assigned(FLogEvent) then
    FLogEvent(Sender, EventType, Msg);
end;

procedure TMiniRESTSQLQuerySQLDb.Open;
begin
  FQry.Open;
end;

procedure TMiniRESTSQLQuerySQLDb.Close;
begin
  FQry.Close;
end;

function TMiniRESTSQLQuerySQLDb.GetSQL: string;
begin
  Result := FSQL;
end;

procedure TMiniRESTSQLQuerySQLDb.SetSQL(const ASQL: string);
begin
  FParams.Clear;
  FSQL := ASQL;
  FQry.SQL.Text := ASQL;
end;

function TMiniRESTSQLQuerySQLDb.ParamByName(const AParamName: string): IMiniRESTSQLParam;
var
  LParam: IMiniRESTSQLParam;
  LParamName: string;
begin  
  LParamName := UpperCase(AParamName);
  for LParam in FParams do
  begin
    if SameText(LParamName, LParam.GetParamName) then
      Exit(LParam);
  end;
  LParam := TMiniRESTSQLParam.Create;
  LParam.SetParamName(LParamName);
  FParams.Add(LParam);
  Result := LParam;
end;

function TMiniRESTSQLQuerySQLDb.AddParam(AParam: IMiniRESTSQLParam): IMiniRESTSQLQuery;
var
  LParam: IMiniRESTSQLParam;
begin
  //FParams.AddOrSetData(AParam.GetParamName, AParam);  
  for LParam in FParams do
  begin
    if SameText(LParam.GetParamName, AParam.GetParamName) then
    begin
      FParams.Extract(LParam);
      Break;
    end;
  end;
  FParams.Add(AParam);
  Result := Self;
end;

function TMiniRESTSQLQuerySQLDb.ApplyUpdates(const AMaxErrors: Integer): Integer;
var
  LConnectionInExplicitTransaction: Boolean;
begin    
  LConnectionInExplicitTransaction := TMiniRESTSQLConnectionSQLDb(FConnection.GetObject).FInExplicitTransaction;
  FQry.ApplyUpdates;
  if not LConnectionInExplicitTransaction then    
    FConnection.Commit;  
  Result := 0;
  {TODO: Tratar o retorno. Transformar em procedure?}
end;

function TMiniRESTSQLQuerySQLDb.GetDataSet: TDataSet;
begin
  Result := FQry;
end;

procedure TMiniRESTSQLQuerySQLDb.BeforeOpenMiniRESTDataSet(DataSet: TDataSet);
var
  LMiniRESTSQLParam: IMiniRESTSQLParam;
  LParam: TParam;
  I: Integer;
begin
  Assert(Assigned(FConnection), 'Não está definida a conexão');
  if not Assigned(FConnection) then
    raise Exception.Create('Não está definida a conexão');
  FConnection.Connect;
  for I := 0 to FParams.Count - 1 do
  begin
    //LMiniRESTSQLParam := FParams.Data[I];
    LMiniRESTSQLParam := FParams.Items[I];
    LParam := FQry.ParamByName(LMiniRESTSQLParam.GetParamName);
    TMiniRESTSQLConnectionSQLDb(FConnection.GetObject).SetMiniRESTSQLParamToSQLParam(LMiniRESTSQLParam, LParam);
  end;
end;

constructor TMiniRESTSQLQuerySQLDb.Create(AConnection: IMiniRESTSQLConnection);
begin
  FConnection := AConnection;
  FQry := TSQLQueryMiniREST.Create(nil);
  FQry.Options := [sqoKeepOpenOnCommit];
  TSQLQueryMiniREST(FQry).FInternalMiniRESTSQLBeforeOpenDataSet := BeforeOpenMiniRESTDataSet;
  //FTransaction := TSQLTransaction.Create(nil);
  //FTransaction.Action := caNone;
  //FTransaction.Options := [stoUseImplicit];
  //FTransaction.Database := TMiniRESTSQLConnectionSQLDb(AConnection.GetObject).FSQLConnection;
  //FQry.Transaction := FTransaction;
  FQry.SQLConnection := TMiniRESTSQLConnectionSQLDb(AConnection.GetObject).FSQLConnection;
  //FParams := TFPGMapInterfacedObjectData<string, IMiniRESTSQLParam>.Create();  
  FParams := TFPGInterfacedObjectList<IMiniRESTSQLParam>.Create();
end;

destructor TMiniRESTSQLQuerySQLDb.Destroy;  
begin
  FQry.Free;  
  FParams.Free;  
  //FTransaction.Free;
  inherited Destroy;
end;

function TMiniRESTSQLConnectionSQLDb.GetConnectorType(const ADatabaseType: TMiniRESTSQLDatabaseType): String;
begin
  case ADatabaseType of
    dbtUnknown: raise Exception.Create('Database Type not supported');
    dbtFirebird: Result := 'Firebird';
  end;  
end;

procedure TMiniRESTSQLConnectionSQLDb.OnBeforeConnect(Sender: TObject);
begin
  SetConnectionParams;
end;

function TMiniRESTSQLConnectionParamsSQLDb.GetDatabaseName: string;
begin
  Result := FDatabaseName;
end;

procedure TMiniRESTSQLConnectionParamsSQLDb.SetDatabaseName(const ADatabaseName: string);
begin
  FDatabaseName := ADatabaseName;
end;

procedure TMiniRESTSQLConnectionFactorySQLDb.ReleaseConnection(AConnection: IMiniRESTSQLConnection);
begin
  RTLeventWaitFor(FConnectionGetEvent);
  try    
    FQueue.Add(AConnection);       
    TMiniRESTSQLConnectionBaseCrack(AConnection.GetObject).FEstaNoPool := True;
    Inc(FAvailableConnections);
    LogConnectionPoolEvent(Format('RELEASE CONNECTION %d - %s', [AConnection.GetConnectionID,
      AConnection.GetName]));
  finally
    RTLeventSetEvent(FConnectionReleaseEvent);
    RTLeventSetEvent(FConnectionGetEvent);
  end; 
end;

function TMiniRESTSQLConnectionParamsSQLDb.GetLogEvent: TLogEvent;
begin
  Result := FLogEvent;
end;

procedure TMiniRESTSQLConnectionParamsSQLDb.SetLogEvent(const ALogEvent: TLogEvent);
begin
  FLogEvent := ALogEvent;
end;

procedure TMiniRESTSQLConnectionSQLDb.SetMiniRESTSQLParamToSQLParam(AMiniRESTSQLParam: IMiniRESTSQLParam; ASQLParam: TParam);
begin
  case AMiniRESTSQLParam.GetParamType of
    stString: ASQLParam.AsString := AMiniRESTSQLParam.AsString;
    stFloat: ASQLParam.AsFloat := AMiniRESTSQLParam.AsFloat;
    stInteger: ASQLParam.AsInteger := AMiniRESTSQLParam.AsInteger;
    stDate: ASQLParam.AsDate := AMiniRESTSQLParam.AsDate;
    stDateTime: ASQLParam.AsDateTime := AMiniRESTSQLParam.AsDateTime;
    stBoolean: ASQLParam.AsBoolean := AMiniRESTSQLParam.AsBoolean;
    stVariant, stUndefined: ASQLParam.Value := AMiniRESTSQLParam.GetAsVariant;
  end;
end;

function TMiniRESTSQLConnectionSQLDb.InTransaction: Boolean;
begin
  CheckConnectionIsValid;
  Result := FSQLConnection.Transaction.Active;
end;

function TMiniRESTSQLConnectionFactorySQLDb.GetConnectionsCount: Integer;
begin
  Result := FConnectionsCount;  
end;

function TMiniRESTSQLConnectionFactorySQLDb.GetQueueCount: Integer;
begin
  Result := FAvailableConnections;
  if Result < 0 then
    Result := 0;
end;

procedure TMiniRESTSQLConnectionSQLDb.Invalidate;
begin
  SetValid(False);
  FreeAndNil(FSQLConnection);
  FreeAndNil(FTransaction);  
end;

procedure TMiniRESTSQLConnectionSQLDb.CheckConnectionIsValid;
begin
  if not IsValid then
    raise Exception.Create('A conexão foi invalidada.');  
end;

procedure TMiniRESTSQLConnectionSQLDb.SetConnectionParams;
var
  LStringList: TStringList;
  LName: string;
  I: Integer;
begin
  LStringList := TStringList.Create;
  try
    FSQLConnection.ConnectorType := GetConnectorType(FConnectionParams.GetDatabaseType);
    FSQLConnection.LoginPrompt := False;
    FSQLConnection.UserName := FConnectionParams.GetUserName;
    FSQLConnection.Password := FConnectionParams.GetPassword;
    FSQLConnection.DatabaseName := FConnectionParams.GetDatabaseName;
    FSQLConnection.HostName := FConnectionParams.GetServerHostName;
    LStringList.Text := FConnectionParams.GetConnectionString;
    for I := 0 to LStringList.Count - 1 do
    begin
      LName := LStringList.Names[I];
      FSQLConnection.Params.Values[LName] := LStringList.Values[LName];
    end;
  finally
    LStringList.Free;
  end;
end;

function TMiniRESTSQLConnectionParamsSQLDb.GetServerHostName: string;
begin
  Result := FServerHostName;  
end;

procedure TMiniRESTSQLConnectionParamsSQLDb.SetServerHostName(const AServerHostName: string);
begin
  FServerHostName := AServerHostName;
end;

end.
