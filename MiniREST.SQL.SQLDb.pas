unit MiniREST.SQL.SQLDb;

interface

uses SysUtils, MiniREST.SQL.Intf, MiniREST.SQL.Base, MiniREST.SQL.Common, DB;

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
  end;

  TMiniRESTSQLConnectionParamsSQLDb = class(TInterfacedObject, IMiniRESTSQLConnectionParamsSQLDb)
  private
    FConnectionsCount: Integer;
    FConnectionString: string;
    FUserName: string;
    FPassword: string;
    FDatabaseType: TMiniRESTSQLDatabaseType;
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
  protected
    FSQLConnection: TSQLConnection;
    FTransaction: TDBXTransaction;
    FConnectionParams: IMiniRESTSQLConnectionParamsSQLDb;
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
  FSQLConnection := TSQLConnection.Create(nil);
  FConnectionParams := AParams;
  inherited Create(AOwner);
end;

destructor TMiniRESTSQLConnectionSQLDb.Destroy;
begin
  
end;

procedure TMiniRESTSQLConnectionSQLDb.Connect;
begin
  
end;

procedure TMiniRESTSQLConnectionSQLDb.StartTransaction;
begin
  
end;

procedure TMiniRESTSQLConnectionSQLDb.Commit;
begin
  
end;

procedure TMiniRESTSQLConnectionSQLDb.Rollback;
begin
  
end;

function TMiniRESTSQLConnectionSQLDb.GetQuery: IMiniRESTSQLQuery;
begin
  
end;

function TMiniRESTSQLConnectionSQLDb.GetQuery(const ASQL: string; AParams: array of IMiniRESTSQLParam): IMiniRESTSQLQuery;
begin
  
end;

function TMiniRESTSQLConnectionSQLDb.GetQuery(const ASQL: string): IMiniRESTSQLQuery;
begin
  
end;

function TMiniRESTSQLConnectionSQLDb.Execute(const ACommand: string; AParams: array of IMiniRESTSQLParam): Integer;
begin
  
end;

function TMiniRESTSQLConnectionSQLDb.GetDatabaseInfo: IMiniRESTSQLDatabaseInfo;
begin
  
end;

function TMiniRESTSQLConnectionSQLDb.GetObject: TObject;
begin
  
end;

function TMiniRESTSQLConnectionSQLDb.GetDriverName(const ADatabaseType: TMiniRESTSQLDatabaseType): string;
begin
  
end;

end.
