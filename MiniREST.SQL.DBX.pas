unit MiniREST.SQL.DBX;

interface

uses MiniREST.SQL.Intf, MiniREST.SQL.Base, SqlExpr, DBXCommon;

type
  TMiniRESTSQLConnectionFactoryDBX = class(TMiniRESTSQLConnectionFactoryBase)
  protected
    FConnectionString: string;
    function InternalGetconnection: IMiniRESTSQLConnection; override;
  public
    constructor Create(const AConnectionString: string; const AConnections: Integer); overload;
    procedure GenerateConnections; override;
  end;

  TMiniRESTSQLConnectionDBX = class(TMiniRESTSQLConnectionBase)
  protected
    FSQLConnection: TSQLConnection;
    FTransaction: TDBXTransaction;
    function GetObject: TObject; override;
  public
    constructor Create(const AConnectionString: string);
    destructor Destroy; override;
    procedure Connect; override;
    function GetQuery(ASQL: string): IMiniRESTSQLQuery; overload; override;
    function GetQuery(ASQL: string; AParams : array of Variant): IMiniRESTSQLQuery; overload; override;
    function Execute(ACommand: string) : IMiniRESTSQLQuery; overload; override;
    function Execute(ACommand: string; AParams : array of Variant) : IMiniRESTSQLQuery; overload; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
  end;

  TMiniRESTSQLQueryDBX = class(TInterfacedObject, IMiniRESTSQLQuery)
  private
    FQry : TSQLQuery;
    FIConnection : IMiniRESTSQLConnection;
    FSQL: string;
  public
    constructor Create(AConnection: IMiniRESTSQLConnection);
    destructor Destroy; override;
    function Eof: Boolean;
    function GetValue(AField: string): Variant; overload;
    function GetValue(AField: string; ADefault : Variant): Variant; overload;
    procedure Next;
    function IsEmpty : Boolean;
    function GetSQL: string;
    procedure SetSQL(const ASQL: string);
    procedure Open;
    procedure Close;
  end;

implementation

uses Variants;

{ TMiniRESTSQLConnectionFactoryDBX }

constructor TMiniRESTSQLConnectionFactoryDBX.Create(const AConnectionString: string;
  const AConnections: Integer);
begin
  FConnectionString := AConnectionString;
  FConnectionsCount := AConnections;
end;

procedure TMiniRESTSQLConnectionFactoryDBX.GenerateConnections;
begin
  inherited;
end;

function TMiniRESTSQLConnectionFactoryDBX.InternalGetconnection: IMiniRESTSQLConnection;
begin
  Result := TMiniRESTSQLConnectionDBX.Create(FConnectionString);
end;

{ TMiniRESTSQLQueryDBX }

procedure TMiniRESTSQLQueryDBX.Close;
begin
  FQry.Close;
end;

constructor TMiniRESTSQLQueryDBX.Create(AConnection: IMiniRESTSQLConnection);
begin
  FQry := TSQLQuery.Create(nil);
  FQry.SQLConnection := TSQLConnection(AConnection.GetObject);
end;

destructor TMiniRESTSQLQueryDBX.Destroy;
begin
  FQry.Free;
  inherited;
end;

function TMiniRESTSQLQueryDBX.Eof: Boolean;
begin
  Result := FQry.Eof;
end;

function TMiniRESTSQLQueryDBX.GetSQL: string;
begin
  Result := FSQL;
end;

function TMiniRESTSQLQueryDBX.GetValue(AField: string; ADefault: Variant): Variant;
var
  LValue: Variant;
begin
  LValue := FQry.FieldByName(AField).Value;
  if LValue = Null then
    Result := ADefault
  else
    Result := LValue;
end;

function TMiniRESTSQLQueryDBX.GetValue(AField: string): Variant;
begin
  Result := FQry.FieldByName(AField).Value;
end;

function TMiniRESTSQLQueryDBX.IsEmpty: Boolean;
begin
  Result := FQry.IsEmpty;
end;

procedure TMiniRESTSQLQueryDBX.Next;
begin
  FQry.Next;
end;

procedure TMiniRESTSQLQueryDBX.Open;
begin
  FQry.Open;
end;

procedure TMiniRESTSQLQueryDBX.SetSQL(const ASQL: string);
begin
  FSQL := ASQL;
end;

{ TMiniRESTSQLConnectionDBX }

procedure TMiniRESTSQLConnectionDBX.Commit;
begin
  FSQLConnection.CommitFreeAndNil(FTransaction);
end;

procedure TMiniRESTSQLConnectionDBX.Connect;
begin
  FSQLConnection.Connected := True;
end;

constructor TMiniRESTSQLConnectionDBX.Create(const AConnectionString: string);
begin
  FSQLConnection := TSQLConnection.Create(nil);
  FSQLConnection.Params.Text := AConnectionString;
end;

destructor TMiniRESTSQLConnectionDBX.Destroy;
begin
  FSQLConnection.Free;
  inherited;
end;

function TMiniRESTSQLConnectionDBX.Execute(ACommand: string): IMiniRESTSQLQuery;
begin
end;

function TMiniRESTSQLConnectionDBX.Execute(ACommand: string;
  AParams: array of Variant): IMiniRESTSQLQuery;
begin

end;

function TMiniRESTSQLConnectionDBX.GetObject: TObject;
begin
  Result := FSQLConnection;
end;

function TMiniRESTSQLConnectionDBX.GetQuery(ASQL: string): IMiniRESTSQLQuery;
begin

end;

function TMiniRESTSQLConnectionDBX.GetQuery(ASQL: string;
  AParams: array of Variant): IMiniRESTSQLQuery;
begin

end;

procedure TMiniRESTSQLConnectionDBX.Rollback;
begin
  FSQLConnection.RollbackFreeAndNil(FTransaction);
end;

procedure TMiniRESTSQLConnectionDBX.StartTransaction;
begin
  FTransaction := FSQLConnection.BeginTransaction
end;

end.
