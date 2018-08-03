unit MiniREST.SQL.DBX;

interface

uses MiniREST.SQL.Intf, MiniREST.SQL.Base, MiniREST.SQL.Common, SqlExpr,
  DBXCommon, SimpleDS, DB;

type
  IMiniRESTSQLConnectionParamsDBX = interface
  ['{329B2932-7EA8-4C2B-82C2-42580EA69416}']
    function GetConnectionsCount: Integer;
    function SetConnectionsCount(const AConnectionsCount: Integer): IMiniRESTSQLConnectionParamsDBX;
    function GetConnectionString: string;
    function SetConnectionString(const AConnectionString: string): IMiniRESTSQLConnectionParamsDBX;
    function GetDriverName: string;
    function SetDriverName(const ADriverName: string): IMiniRESTSQLConnectionParamsDBX;
    function GetUserName: string;
    function SetUserName(const AUserName: string): IMiniRESTSQLConnectionParamsDBX;
    function GetPassword: string;
    function SetPassword(const APassword: string): IMiniRESTSQLConnectionParamsDBX;
  end;

  TMiniRESTSQLConnectionParamsDBX = class(TInterfacedObject, IMiniRESTSQLConnectionParamsDBX)
  private
    FConnectionString: string;
    FConnectionsCount: Integer;
    FDriverName: string;
    FUserName: string;
    FPassword: string;
  public
    class function New: IMiniRESTSQLConnectionParamsDBX;
    function GetConnectionString: string;
    function GetConnectionsCount: Integer;
    function GetDriverName: string;
    function SetConnectionString(const AConnectionString: string): IMiniRESTSQLConnectionParamsDBX;
    function SetConnectionsCount(const AConnectionsCount: Integer): IMiniRESTSQLConnectionParamsDBX;
    function SetDriverName(const ADriverName: string): IMiniRESTSQLConnectionParamsDBX;
    function GetUserName: string;
    function SetUserName(const AUserName: string): IMiniRESTSQLConnectionParamsDBX;
    function GetPassword: string;
    function SetPassword(const APassword: string): IMiniRESTSQLConnectionParamsDBX;
  end;

  TMiniRESTSQLConnectionFactoryDBX = class(TMiniRESTSQLConnectionFactoryBase)
  protected
    FConnectionString: string;
    FConnectionParams: IMiniRESTSQLConnectionParamsDBX;
    function InternalGetconnection: IMiniRESTSQLConnection; override;
  public
    constructor Create(AParams: IMiniRESTSQLConnectionParamsDBX); overload;
    procedure GenerateConnections; override;
  end;

  TMiniRESTSQLConnectionDBX = class(TMiniRESTSQLConnectionBase, IMiniRESTSQLConnectionExecute)
  protected
    FSQLConnection: TSQLConnection;
    FTransaction: TDBXTransaction;
    FConnectionParams: IMiniRESTSQLConnectionParamsDBX;
    function GetObject: TObject; override;
  public
    constructor Create(AOwner: IMiniRESTSQLConnectionFactory; AParams: IMiniRESTSQLConnectionParamsDBX);
    destructor Destroy; override;
    procedure Connect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function GetQuery: IMiniRESTSQLQuery; override;
    function GetQuery(ASQL: string;
    AParams: array of IMiniRESTSQLParam): IMiniRESTSQLQuery; override;
    function GetQuery(ASQL: string): IMiniRESTSQLQuery; override;
    function Execute(ACommand: string): Integer;
  end;

  TMiniRESTSQLQueryDBX = class(TInterfacedObject, IMiniRESTSQLQuery)
  private
    FQry : TSimpleDataSet;
    {$HINTS OFF}
    FIConnection : IMiniRESTSQLConnection;
    {$HINTS ON}
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
    function AddParam(AParam: IMiniRESTSQLParam): IMiniRESTSQLQuery;
    procedure Append;
    procedure Insert;
    procedure Post;
    procedure Cancel;
    function ApplyUpdates(const AMaxErrors: Integer = 0): Integer;
    function GetDataSet: TDataSet;
    function ToJSON: string;
  end;

implementation

uses Variants, MiniREST.JSON;

{ TMiniRESTSQLConnectionFactoryDBX }

constructor TMiniRESTSQLConnectionFactoryDBX.Create(AParams: IMiniRESTSQLConnectionParamsDBX);
begin
  inherited Create(AParams.GetConnectionsCount);
  FConnectionParams := AParams;
  FConnectionsCount := AParams.GetConnectionsCount;
end;

procedure TMiniRESTSQLConnectionFactoryDBX.GenerateConnections;
begin
  inherited;
end;

function TMiniRESTSQLConnectionFactoryDBX.InternalGetconnection: IMiniRESTSQLConnection;
begin
  Result := TMiniRESTSQLConnectionDBX.Create(Self, FConnectionParams);
end;

{ TMiniRESTSQLQueryDBX }

function TMiniRESTSQLQueryDBX.AddParam(
  AParam: IMiniRESTSQLParam): IMiniRESTSQLQuery;
var
  LParamType: TMiniRESTSQLParamType;
  LParamName: string;
begin
  LParamType := AParam.GetParamType;
  LParamName := AParam.GetParamName;
  case LParamType of
    stString: FQry.Params.ParamByName(LParamName).AsString := AParam.GetAsString;
    stFloat: FQry.Params.ParamByName(LParamName).AsFloat := AParam.GetAsFloat;
    stInteger: FQry.Params.ParamByName(LParamName).AsInteger := AParam.GetAsInteger;
    stDate: FQry.Params.ParamByName(LParamName).AsDate := AParam.GetAsDate;
    stDateTime: FQry.Params.ParamByName(LParamName).AsDateTime := AParam.GetAsDateTime;
    stBoolean: FQry.Params.ParamByName(LParamName).AsBoolean := AParam.GetAsBoolean;
    stVariant: FQry.Params.ParamByName(LParamName).Value := AParam.GetAsVariant;
    stUndefined: FQry.Params.ParamByName(LParamName).Value := Null;
  end;
end;

procedure TMiniRESTSQLQueryDBX.Append;
begin
  FQry.Append;
end;

function TMiniRESTSQLQueryDBX.ApplyUpdates(const AMaxErrors: Integer): Integer;
begin
  Result := FQry.ApplyUpdates(AMaxErrors);
end;

procedure TMiniRESTSQLQueryDBX.Cancel;
begin
  FQry.Cancel;
end;

procedure TMiniRESTSQLQueryDBX.Close;
begin
  FQry.Close;
end;

constructor TMiniRESTSQLQueryDBX.Create(AConnection: IMiniRESTSQLConnection);
begin
  FQry := TSimpleDataSet.Create(nil);
  FQry.DataSet.SQLConnection := TSQLConnection(AConnection.GetObject);
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

function TMiniRESTSQLQueryDBX.GetDataSet: TDataSet;
begin
  Result := FQry;
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

procedure TMiniRESTSQLQueryDBX.Insert;
begin
  FQry.Insert;
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

procedure TMiniRESTSQLQueryDBX.Post;
begin
  FQry.Post;
end;

procedure TMiniRESTSQLQueryDBX.SetSQL(const ASQL: string);
begin
  FSQL := ASQL;
  FQry.DataSet.CommandText := FSQL;
end;

function TMiniRESTSQLQueryDBX.ToJSON: string;
begin
  Result := TMiniRESTJSON.DatasetToJson2(FQry);
end;

{ TMiniRESTSQLConnectionDBX }

procedure TMiniRESTSQLConnectionDBX.Commit;
begin
  FSQLConnection.CommitFreeAndNil(FTransaction);
end;

procedure TMiniRESTSQLConnectionDBX.Connect;
begin
  FSQLConnection.DriverName := FConnectionParams.GetDriverName;
  FSQLConnection.LoginPrompt := False;
  FSQLConnection.Params.Text := FConnectionParams.GetConnectionString;
  FSQLConnection.Connected := True;
end;

constructor TMiniRESTSQLConnectionDBX.Create(AOwner: IMiniRESTSQLConnectionFactory;
  AParams: IMiniRESTSQLConnectionParamsDBX);
begin
  FSQLConnection := TSQLConnection.Create(nil);
  FConnectionParams := AParams;
  inherited Create(AOwner);
end;

destructor TMiniRESTSQLConnectionDBX.Destroy;
begin
  FSQLConnection.Free;
  inherited;
end;

function TMiniRESTSQLConnectionDBX.Execute(ACommand: string): Integer;
begin
//  Result := FSQLConnection.Execute(ACommand, );
end;

function TMiniRESTSQLConnectionDBX.GetObject: TObject;
begin
  Result := FSQLConnection;
end;

function TMiniRESTSQLConnectionDBX.GetQuery(ASQL: string;
  AParams: array of IMiniRESTSQLParam): IMiniRESTSQLQuery;
var
  LParam: IMiniRESTSQLParam;
begin
  Result := TMiniRESTSQLQueryDBX.Create(Self);
  Result.SQL := ASQL;
  for LParam in AParams do
  begin
    Result.AddParam(LParam);
  end;
end;

function TMiniRESTSQLConnectionDBX.GetQuery(ASQL: string): IMiniRESTSQLQuery;
begin
  Result := TMiniRESTSQLQueryDBX.Create(Self);
  Result.SQL := ASQL;
end;

function TMiniRESTSQLConnectionDBX.GetQuery: IMiniRESTSQLQuery;
begin
  Result := TMiniRESTSQLQueryDBX.Create(Self);
end;

procedure TMiniRESTSQLConnectionDBX.Rollback;
begin
  FSQLConnection.RollbackFreeAndNil(FTransaction);
end;

procedure TMiniRESTSQLConnectionDBX.StartTransaction;
begin
  FTransaction := FSQLConnection.BeginTransaction
end;

{ TMiniRESTSQLConnectionParamsDBX }

function TMiniRESTSQLConnectionParamsDBX.GetConnectionsCount: Integer;
begin
  Result := FConnectionsCount;
end;

function TMiniRESTSQLConnectionParamsDBX.GetConnectionString: string;
begin
  Result := FConnectionString;
end;

function TMiniRESTSQLConnectionParamsDBX.GetDriverName: string;
begin
  Result := FDriverName;
end;

function TMiniRESTSQLConnectionParamsDBX.GetPassword: string;
begin
  Result := FPassword;
end;

function TMiniRESTSQLConnectionParamsDBX.GetUserName: string;
begin
  Result := FUserName;
end;

class function TMiniRESTSQLConnectionParamsDBX.New: IMiniRESTSQLConnectionParamsDBX;
begin
  Result := Create;
end;

function TMiniRESTSQLConnectionParamsDBX.SetConnectionsCount(
  const AConnectionsCount: Integer): IMiniRESTSQLConnectionParamsDBX;
begin
  FConnectionsCount := AConnectionsCount;
  Result := Self;
end;

function TMiniRESTSQLConnectionParamsDBX.SetConnectionString(
  const AConnectionString: string): IMiniRESTSQLConnectionParamsDBX;
begin
  FConnectionString := AConnectionString;
  Result := Self;
end;

function TMiniRESTSQLConnectionParamsDBX.SetDriverName(
  const ADriverName: string): IMiniRESTSQLConnectionParamsDBX;
begin
  FDriverName := ADriverName;
  Result := Self;
end;

function TMiniRESTSQLConnectionParamsDBX.SetPassword(
  const APassword: string): IMiniRESTSQLConnectionParamsDBX;
begin
  FPassword := APassword;
  Result := Self;
end;

function TMiniRESTSQLConnectionParamsDBX.SetUserName(
  const AUserName: string): IMiniRESTSQLConnectionParamsDBX;
begin
  FUserName := AUserName;
  Result := Self;
end;

end.
