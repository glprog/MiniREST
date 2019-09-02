{$mode DELPHI}
unit MiniREST.SQL.SQLDb;

interface

uses Classes, SysUtils, MiniREST.SQL.Intf, MiniREST.SQL.Base, MiniREST.SQL.Common, DB,
  sqldb, IBConnection, fpjsondataset, fgl;

type
  TLogEvent = procedure (Sender : TSQLConnection; EventType : TDBEventType; Const Msg : String);
  IMiniRESTSQLConnectionParamsSQLDb = interface
  ['{F2FB358A-6369-4FCE-AA9B-75FFECA18E88}']
    function GetConnectionsCount: Integer;
    function SetConnectionsCount(const AConnectionsCount: Integer): IMiniRESTSQLConnectionParamsSQLDb;
    function GetConnectionString: string;
    function SetConnectionString(const AConnectionString: string): IMiniRESTSQLConnectionParamsSQLDb;
    function GetUserName: string;
    function SetUserName(const AUserName: string): IMiniRESTSQLConnectionParamsSQLDb;
    function GetPassword: string;
    function SetPassword(const APassword: string): IMiniRESTSQLConnectionParamsSQLDb;
    function GetDatabaseType: TMiniRESTSQLDatabaseType;
    function SetDatabseType(const ADatabaseType: TMiniRESTSQLDatabaseType): IMiniRESTSQLConnectionParamsSQLDb;
    function GetDatabaseName: string;
    function SetDatabaseName(const ADatabaseName: string): IMiniRESTSQLConnectionParamsSQLDb;
    function GetLogEvent: TLogEvent;
    function SetLogEvent(const ALogEvent: TLogEvent): IMiniRESTSQLConnectionParamsSQLDb;
  end;

  TMiniRESTSQLConnectionParamsSQLDb = class(TInterfacedObject, IMiniRESTSQLConnectionParamsSQLDb)
  private
    FConnectionsCount: Integer;
    FConnectionString: string;
    FUserName: string;
    FPassword: string;
    FDatabaseType: TMiniRESTSQLDatabaseType;
    FDatabaseName: string;
    FLogEvent: TLogEvent;
  public
    class function New: IMiniRESTSQLConnectionParamsSQLDb; 
    function GetConnectionsCount: Integer;
    function SetConnectionsCount(const AConnectionsCount: Integer): IMiniRESTSQLConnectionParamsSQLDb;
    function GetConnectionString: string;
    function SetConnectionString(const AConnectionString: string): IMiniRESTSQLConnectionParamsSQLDb;
    function GetUserName: string;
    function SetUserName(const AUserName: string): IMiniRESTSQLConnectionParamsSQLDb;
    function GetPassword: string;
    function SetPassword(const APassword: string): IMiniRESTSQLConnectionParamsSQLDb;
    function GetDatabaseType: TMiniRESTSQLDatabaseType;
    function SetDatabseType(const ADatabaseType: TMiniRESTSQLDatabaseType): IMiniRESTSQLConnectionParamsSQLDb;
    function GetDatabaseName: string;
    function SetDatabaseName(const ADatabaseName: string): IMiniRESTSQLConnectionParamsSQLDb;
    function GetLogEvent: TLogEvent;
    function SetLogEvent(const ALogEvent: TLogEvent): IMiniRESTSQLConnectionParamsSQLDb;
  end;

  TMiniRESTSQLConnectionFactorySQLDb = class(TMiniRESTSQLConnectionFactoryBase)
  protected
    FConnectionString: string;
    FConnectionParams: IMiniRESTSQLConnectionParamsSQLDb;
    procedure ReleaseConnection(AConnection: IMiniRESTSQLConnection); override;
    function InternalGetconnection: IMiniRESTSQLConnection; override;
  public
    constructor Create(AParams: IMiniRESTSQLConnectionParamsSQLDb); overload;
  end;

  { TMiniRESTSQLConnectionSQLDb }

  TMiniRESTSQLConnectionSQLDb = class(TMiniRESTSQLConnectionBase)
  private
    function GetConnectorType(const ADatabaseType: TMiniRESTSQLDatabaseType): String;
  protected
    FSQLConnection: TSQLConnector;
    //FTransaction: TDBXTransaction;
    FConnectionParams: IMiniRESTSQLConnectionParamsSQLDb;
    FTransaction: TSQLTransaction;
    FInExplicitTransaction: Boolean;
    FLogEvent: TLogEvent;
    function GetObject: TObject; override;
    function GetDriverName(const ADatabaseType: TMiniRESTSQLDatabaseType): string;
    procedure Log(Sender : TSQLConnection; EventType : TDBEventType; Const Msg : String);
    procedure SetMiniRESTSQLParamToSQLParam(AMiniRESTSQLParam: IMiniRESTSQLParam; ASQLParam: TParam);
  public
    constructor Create(AOwner: IMiniRESTSQLConnectionFactory; AParams: IMiniRESTSQLConnectionParamsSQLDb);
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

class function TMiniRESTSQLConnectionParamsSQLDb.New: IMiniRESTSQLConnectionParamsSQLDb;
begin
  Result := Create;
end;

function TMiniRESTSQLConnectionParamsSQLDb.GetConnectionsCount: Integer;
begin
  Result := FConnectionsCount;
end;

function TMiniRESTSQLConnectionParamsSQLDb.SetConnectionsCount(const AConnectionsCount: Integer): IMiniRESTSQLConnectionParamsSQLDb;
begin
  Result := Self;
  FConnectionsCount := AConnectionsCount;
end;

function TMiniRESTSQLConnectionParamsSQLDb.GetConnectionString: string;
begin
  Result := FConnectionString;
end;

function TMiniRESTSQLConnectionParamsSQLDb.SetConnectionString(const AConnectionString: string): IMiniRESTSQLConnectionParamsSQLDb;
begin
  Result := Self;
  FConnectionString := AConnectionString;
end;

function TMiniRESTSQLConnectionParamsSQLDb.GetUserName: string;
begin
  Result := FUserName;
end;

function TMiniRESTSQLConnectionParamsSQLDb.SetUserName(const AUserName: string): IMiniRESTSQLConnectionParamsSQLDb;
begin
  Result := Self;
  FUserName := AUserName;
end;

function TMiniRESTSQLConnectionParamsSQLDb.GetPassword: string;
begin
  Result := FPassword;
end;

function TMiniRESTSQLConnectionParamsSQLDb.SetPassword(const APassword: string): IMiniRESTSQLConnectionParamsSQLDb;
begin
  Result := Self;
  FPassword := APassword;
end;

function TMiniRESTSQLConnectionParamsSQLDb.GetDatabaseType: TMiniRESTSQLDatabaseType;
begin
  Result := FDatabaseType;
end;

function TMiniRESTSQLConnectionParamsSQLDb.SetDatabseType(const ADatabaseType: TMiniRESTSQLDatabaseType): IMiniRESTSQLConnectionParamsSQLDb;
begin
  Result := Self;
  FDatabaseType := ADatabaseType;
end;

function TMiniRESTSQLConnectionFactorySQLDb.InternalGetconnection: IMiniRESTSQLConnection;
begin
  Result := TMiniRESTSQLConnectionSQLDb.Create(Self, FConnectionParams);
end;

constructor TMiniRESTSQLConnectionFactorySQLDb.Create(AParams: IMiniRESTSQLConnectionParamsSQLDb);
begin
  inherited Create(AParams.GetConnectionsCount);
  FConnectionParams := AParams;
  FConnectionsCount := AParams.GetConnectionsCount;  
end;

constructor TMiniRESTSQLConnectionSQLDb.Create(AOwner: IMiniRESTSQLConnectionFactory; AParams: IMiniRESTSQLConnectionParamsSQLDb);
begin
  FSQLConnection := TSQLConnector.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);    
  //FTransaction.Options := [stoUseImplicit];
  FSQLConnection.Transaction := FTransaction;
  FSQLConnection.OnLog := Log;
  FTransaction.Action := caCommit;
  FConnectionParams := AParams;
  FInExplicitTransaction := False;
  FLogEvent := AParams.GetLogEvent;  
  inherited Create(AOwner);
end;

destructor TMiniRESTSQLConnectionSQLDb.Destroy;
begin  
  FSQLConnection.Free;
  FTransaction.Free;
  inherited Destroy;
end;

procedure TMiniRESTSQLConnectionSQLDb.Connect;
var
  LStringList: TStringList;
  LName: string;
  I: Integer;
  LValue: string;
begin
  LStringList := TStringList.Create;
  try
    if FSQLConnection.Connected then
      Exit;  
    FSQLConnection.ConnectorType := GetConnectorType(FConnectionParams.GetDatabaseType);
    FSQLConnection.LoginPrompt := False;
    FSQLConnection.UserName := FConnectionParams.GetUserName;
    FSQLConnection.Password := FConnectionParams.GetPassword;
    FSQLConnection.DatabaseName := FConnectionParams.GetDatabaseName;  
    LStringList.Text := FConnectionParams.GetConnectionString;
    for I := 0 to LStringList.Count - 1 do      
    begin
      LName := LStringList.Names[I];      
      FSQLConnection.Params.Values[LName] := LStringList.Values[LName];
    end;
    FSQLConnection.Connected := True;
  finally
    LStringList.Free;
  end;
end;

procedure TMiniRESTSQLConnectionSQLDb.StartTransaction;
begin
  if FTransaction.Active then
    FTransaction.Rollback;  
  FTransaction.StartTransaction;
  FInExplicitTransaction := True;
end;

procedure TMiniRESTSQLConnectionSQLDb.Commit;
begin  
  FTransaction.Commit;
  FInExplicitTransaction := False;
end;

procedure TMiniRESTSQLConnectionSQLDb.Rollback;
begin
  FTransaction.Rollback;
  FInExplicitTransaction := False;
end;

function TMiniRESTSQLConnectionSQLDb.GetQuery: IMiniRESTSQLQuery;
begin
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
var
  LMiniRESTSQLParam: IMiniRESTSQLParam;  
  LParam: TParam;
  I: Integer;
begin
  FConnection.Connect;
  for I := 0 to FParams.Count - 1 do
  begin
    //LMiniRESTSQLParam := FParams.Data[I];
    LMiniRESTSQLParam := FParams.Items[I];
    LParam := FQry.ParamByName(LMiniRESTSQLParam.GetParamName);
    TMiniRESTSQLConnectionSQLDb(FConnection.GetObject).SetMiniRESTSQLParamToSQLParam(LMiniRESTSQLParam, LParam);
  end;  
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
  LAchou: Boolean;
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

constructor TMiniRESTSQLQuerySQLDb.Create(AConnection: IMiniRESTSQLConnection);
begin
  FConnection := AConnection;
  FQry := TSQLQuery.Create(nil);
  FQry.Options := [sqoKeepOpenOnCommit];
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

function TMiniRESTSQLConnectionParamsSQLDb.GetDatabaseName: string;
begin
  Result := FDatabaseName;
end;

function TMiniRESTSQLConnectionParamsSQLDb.SetDatabaseName(const ADatabaseName: string): IMiniRESTSQLConnectionParamsSQLDb;
begin
  Result := Self;
  FDatabaseName := ADatabaseName;
end;

procedure TMiniRESTSQLConnectionFactorySQLDb.ReleaseConnection(AConnection: IMiniRESTSQLConnection);
begin
  RTLeventWaitFor(FConnectionGetEvent);
  try    
    FQueue.Add(AConnection);       
    TMiniRESTSQLConnectionBaseCrack(AConnection.GetObject).FEstaNoPool := True;
    RemoveConnectionToNotifyFree(AConnection);
    Inc(FAvailableConnections);
  finally
    RTLeventSetEvent(FConnectionReleaseEvent);
    RTLeventSetEvent(FConnectionGetEvent);
  end; 
end;

function TMiniRESTSQLConnectionParamsSQLDb.GetLogEvent: TLogEvent;
begin
  Result := FLogEvent;
end;

function TMiniRESTSQLConnectionParamsSQLDb.SetLogEvent(const ALogEvent: TLogEvent): IMiniRESTSQLConnectionParamsSQLDb;
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

end.
