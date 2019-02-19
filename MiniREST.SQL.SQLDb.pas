unit MiniREST.SQL.SQLDb;

interface

uses Classes, SysUtils, MiniREST.SQL.Intf, MiniREST.SQL.Base, MiniREST.SQL.Common, DB,
  sqldb, IBConnection;

type
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
  end;

  TMiniRESTSQLConnectionParamsSQLDb = class(TInterfacedObject, IMiniRESTSQLConnectionParamsSQLDb)
  private
    FConnectionsCount: Integer;
    FConnectionString: string;
    FUserName: string;
    FPassword: string;
    FDatabaseType: TMiniRESTSQLDatabaseType;
    FDatabaseName: string;
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
  end;

  TMiniRESTSQLConnectionFactorySQLDb = class(TMiniRESTSQLConnectionFactoryBase)
  protected
    FConnectionString: string;
    FConnectionParams: IMiniRESTSQLConnectionParamsSQLDb;
    function InternalGetconnection: IMiniRESTSQLConnection; override;
  public
    constructor Create(AParams: IMiniRESTSQLConnectionParamsSQLDb); overload;
  end;

  TMiniRESTSQLConnectionSQLDb = class(TMiniRESTSQLConnectionBase)
  private
    function GetConnectorType(const ADatabaseType: TMiniRESTSQLDatabaseType): String;
  protected
    FSQLConnection: TSQLConnector;
    //FTransaction: TDBXTransaction;
    FConnectionParams: IMiniRESTSQLConnectionParamsSQLDb;
    FTransaction: TSQLTransaction;
    function GetObject: TObject; override;
    function GetDriverName(const ADatabaseType: TMiniRESTSQLDatabaseType): string;
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
    function ToJSON: string;
  end;

implementation

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
  FSQLConnection.Transaction := FTransaction;
  FConnectionParams := AParams;
  inherited Create(AOwner);
end;

destructor TMiniRESTSQLConnectionSQLDb.Destroy;
begin
  FSQLConnection.Free;
  FTransaction.Free;
end;

procedure TMiniRESTSQLConnectionSQLDb.Connect;
var
  LStringList: TStringList;
  LName: string;
  I: Integer;
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
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTSQLConnectionSQLDb.Commit;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTSQLConnectionSQLDb.Rollback;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLConnectionSQLDb.GetQuery: IMiniRESTSQLQuery;
begin
  Result := TMiniRESTSQLQuerySQLDb.Create(Self);
end;

function TMiniRESTSQLConnectionSQLDb.GetQuery(const ASQL: string; AParams: array of IMiniRESTSQLParam): IMiniRESTSQLQuery;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLConnectionSQLDb.GetQuery(const ASQL: string): IMiniRESTSQLQuery;
begin
  Result := TMiniRESTSQLQuerySQLDb.Create(Self);
  Result.SQL := ASQL;
end;

function TMiniRESTSQLConnectionSQLDb.Execute(const ACommand: string; AParams: array of IMiniRESTSQLParam): Integer;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLConnectionSQLDb.GetDatabaseInfo: IMiniRESTSQLDatabaseInfo;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLConnectionSQLDb.GetObject: TObject;
begin
  Result := Self;
end;

function TMiniRESTSQLConnectionSQLDb.GetDriverName(const ADatabaseType: TMiniRESTSQLDatabaseType): string;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTSQLQuerySQLDb.Open;
begin
  FConnection.Connect;
  FQry.Open;
end;

procedure TMiniRESTSQLQuerySQLDb.Close;
begin
  FQry.Close;
end;

function TMiniRESTSQLQuerySQLDb.GetSQL: string;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTSQLQuerySQLDb.SetSQL(const ASQL: string);
begin
  FSQL := ASQL;
  FQry.SQL.Text := ASQL;
end;

function TMiniRESTSQLQuerySQLDb.ParamByName(const AParamName: string): IMiniRESTSQLParam;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLQuerySQLDb.AddParam(AParam: IMiniRESTSQLParam): IMiniRESTSQLQuery;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLQuerySQLDb.ApplyUpdates(const AMaxErrors: Integer): Integer;
begin  
  FQry.ApplyUpdates(AMaxErrors);
  TSQLTransaction(FQry.Transaction).Commit;  
  Result := 0;
  {TODO: Tratar o retorno. Transformar em procedure?}
end;

function TMiniRESTSQLQuerySQLDb.GetDataSet: TDataSet;
begin
  Result := FQry;
end;

function TMiniRESTSQLQuerySQLDb.ToJSON: string;
begin
  raise Exception.Create('Not implemented');
end;

constructor TMiniRESTSQLQuerySQLDb.Create(AConnection: IMiniRESTSQLConnection);
begin
  FConnection := AConnection;
  FQry := TSQLQuery.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FQry.Transaction := FTransaction;
  FQry.SQLConnection := TMiniRESTSQLConnectionSQLDb(AConnection.GetObject).FSQLConnection;
end;

destructor TMiniRESTSQLQuerySQLDb.Destroy;
begin
  FQry.Free;
  FTransaction.Free;
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

end.
