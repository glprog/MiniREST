unit MiniREST.SQL.DBX;

interface

uses MiniREST.SQL.Intf, MiniREST.SQL.Base, MiniREST.SQL.Common, SqlExpr,
  DBXCommon, SimpleDS, DB, Generics.Collections, SysUtils, Classes;

type
  IMiniRESTSQLConnectionParamsDBX = interface
  ['{329B2932-7EA8-4C2B-82C2-42580EA69416}']
    function GetConnectionsCount: Integer;
    function SetConnectionsCount(const AConnectionsCount: Integer): IMiniRESTSQLConnectionParamsDBX;
    function GetConnectionString: string;
    function SetConnectionString(const AConnectionString: string): IMiniRESTSQLConnectionParamsDBX;
    function GetUserName: string;
    function SetUserName(const AUserName: string): IMiniRESTSQLConnectionParamsDBX;
    function GetPassword: string;
    function SetPassword(const APassword: string): IMiniRESTSQLConnectionParamsDBX;
    function GetDatabaseType: TMiniRESTSQLDatabaseType;
    function SetDatabseType(const ADatabaseType: TMiniRESTSQLDatabaseType): IMiniRESTSQLConnectionParamsDBX;
  end;

  TMiniRESTSQLConnectionParamsDBX = class(TInterfacedObject, IMiniRESTSQLConnectionParamsDBX)
  private
    FConnectionString: string;
    FConnectionsCount: Integer;
    FDriverName: string;
    FUserName: string;
    FPassword: string;
    FDatabaseType: TMiniRESTSQLDatabaseType;
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
    function GetDatabaseType: TMiniRESTSQLDatabaseType;
    function SetDatabseType(const ADatabaseType: TMiniRESTSQLDatabaseType): IMiniRESTSQLConnectionParamsDBX;
  end;

  TMiniRESTSQLConnectionFactoryDBX = class(TMiniRESTSQLConnectionFactoryBase)
  protected
    FConnectionString: string;
    FConnectionParams: IMiniRESTSQLConnectionParamsDBX;
    function InternalGetconnection: IMiniRESTSQLConnection; override;
  public
    constructor Create(AParams: IMiniRESTSQLConnectionParamsDBX); overload;
  end;

  TMiniRESTSQLConnectionDBX = class(TMiniRESTSQLConnectionBase)
  protected
    FSQLConnection: TSQLConnection;
    FTransaction: TDBXTransaction;
    FConnectionParams: IMiniRESTSQLConnectionParamsDBX;
    function GetObject: TObject; override;
    function GetDriverName(const ADatabaseType: TMiniRESTSQLDatabaseType): string;
  public
    constructor Create(AOwner: IMiniRESTSQLConnectionFactory; AParams: IMiniRESTSQLConnectionParamsDBX);
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
  end;

  TMiniRESTSQLQueryDBX = class(TInterfacedObject, IMiniRESTSQLQuery)
  protected
    FQry : TSimpleDataSet;
    FConnection : IMiniRESTSQLConnection;
    FSQL: string;
    FParams: TObjectDictionary<string, IMiniRESTSQLParam>;
    procedure InternalAddParam(AParam: IMiniRESTSQLParam);
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
    function ParamByName(const AParamName: string): IMiniRESTSQLParam;
    function FieldByName(const AFieldName: string): TField;
    procedure Append;
    procedure Insert;
    procedure Post;
    procedure Cancel;
    function ApplyUpdates(const AMaxErrors: Integer = 0): Integer;
    function GetDataSet: TDataSet;
    function ToJSON: string;
  end;

implementation

uses Variants, MiniREST.JSON, MiniREST.SQL.Firebird;

{ TMiniRESTSQLConnectionFactoryDBX }

constructor TMiniRESTSQLConnectionFactoryDBX.Create(AParams: IMiniRESTSQLConnectionParamsDBX);
begin
  inherited Create(AParams.GetConnectionsCount);
  FConnectionParams := AParams;
  FConnectionsCount := AParams.GetConnectionsCount;
end;

function TMiniRESTSQLConnectionFactoryDBX.InternalGetconnection: IMiniRESTSQLConnection;
begin
  Result := TMiniRESTSQLConnectionDBX.Create(Self, FConnectionParams);
end;

{ TMiniRESTSQLQueryDBX }

function TMiniRESTSQLQueryDBX.AddParam(
  AParam: IMiniRESTSQLParam): IMiniRESTSQLQuery;
begin
  FParams.AddOrSetValue(AParam.GetParamName, AParam);
  Result := Self;
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
  FParams.Clear;
  FQry.Close;
end;

constructor TMiniRESTSQLQueryDBX.Create(AConnection: IMiniRESTSQLConnection);
begin
  FConnection := AConnection;
  FQry := TSimpleDataSet.Create(nil);
  FQry.DataSet.SQLConnection := TSQLConnection(AConnection.GetObject);
  FParams := TObjectDictionary<string, IMiniRESTSQLParam>.Create([]);
end;

destructor TMiniRESTSQLQueryDBX.Destroy;
begin
  FQry.Free;
  FParams.Free;
  inherited;
end;

function TMiniRESTSQLQueryDBX.Eof: Boolean;
begin
  Result := FQry.Eof;
end;

function TMiniRESTSQLQueryDBX.FieldByName(const AFieldName: string): TField;
begin
  Result := FQry.FieldByName(AFieldName);
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

procedure TMiniRESTSQLQueryDBX.InternalAddParam(AParam: IMiniRESTSQLParam);
var
  LParamType: TMiniRESTSQLParamType;
  LParam: TParam;
begin
  LParamType := AParam.GetParamType;
  LParam := FQry.Params.ParamByName(AParam.GetParamName);
  case LParamType of
    stString: LParam.AsString := AParam.GetAsString;
    stFloat: LParam.AsFloat := AParam.GetAsFloat;
    stInteger: LParam.AsInteger := AParam.GetAsInteger;
    stDate: LParam.AsDate := AParam.GetAsDate;
    stDateTime: LParam.AsDateTime := AParam.GetAsDateTime;
    stBoolean: LParam.AsBoolean := AParam.GetAsBoolean;
    stVariant: LParam.Value := AParam.GetAsVariant;
    stUndefined: LParam.Value := Null;
  end;
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
var
  LParam: IMiniRESTSQLParam;
begin
  FQry.Params.Clear;
  FQry.FetchParams;
  for LParam in FParams.Values do
  begin
    InternalAddParam(LParam);
  end;
  FConnection.Connect;
  FQry.Open;
end;

function TMiniRESTSQLQueryDBX.ParamByName(
  const AParamName: string): IMiniRESTSQLParam;
var
  LParam: IMiniRESTSQLParam;
  LParamName: string;
begin
  LParamName := UpperCase(AParamName);
  if not FParams.TryGetValue(LParamName, LParam) then
  begin
    LParam := TMiniRESTSQLParam.Create;
    LParam.SetParamName(LParamName);
    FParams.Add(LParamName, LParam);
  end;
  Result := LParam;
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
var
  LStringList: TStringList;
  LName: string;
  I: Integer;
begin
  LStringList := TStringList.Create;
  try
    if FSQLConnection.Connected then
      Exit;
    FSQLConnection.DriverName := GetDriverName(FConnectionParams.GetDatabaseType);
    FSQLConnection.LoginPrompt := False;
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

function TMiniRESTSQLConnectionDBX.Execute(const ACommand: string; AParams: array of IMiniRESTSQLParam): Integer;
var
  LParams: TParams;
  LParam: TParam;
  LMiniRESTSQLParam: IMiniRESTSQLParam;
begin
  LParams := TParams.Create;
  try
    for LMiniRESTSQLParam in AParams do
    begin
      LParam := LParams.AddParameter;
      case LMiniRESTSQLParam.GetParamType of
        stString: LParam.AsString := LMiniRESTSQLParam.AsString;
        stFloat: LParam.AsFloat := LMiniRESTSQLParam.AsFloat;
        stInteger: LParam.AsInteger := LMiniRESTSQLParam.AsInteger;
        stDate: LParam.AsDate := LMiniRESTSQLParam.AsDate;
        stDateTime: LParam.AsDateTime := LMiniRESTSQLParam.AsDateTime;
        stBoolean: LParam.AsBoolean := LMiniRESTSQLParam.AsBoolean;
        stVariant, stUndefined: LParam.Value := LMiniRESTSQLParam.GetAsVariant;
      end;
    end;
    Self.Connect;
    Result := FSQLConnection.Execute(ACommand, LParams);    
  finally
    LParams.Free;  
  end;
end;

function TMiniRESTSQLConnectionDBX.GetDatabaseInfo: IMiniRESTSQLDatabaseInfo;
begin
  Result := nil;
  case FConnectionParams.GetDatabaseType of
    dbtFirebird: Result := TMiniRESTSQLDatabaseInfoFirebird.Create(Self);
    else
      raise Exception.Create('TMiniRESTSQLConnectionDBX.GetDatabaseInfo: ' +
      'DatabaseType not implemented');
  end;
end;

function TMiniRESTSQLConnectionDBX.GetDriverName(
  const ADatabaseType: TMiniRESTSQLDatabaseType): string;
begin
  case ADatabaseType of
    dbtFirebird: Result := 'Firebird';
  end;
end;

function TMiniRESTSQLConnectionDBX.GetObject: TObject;
begin
  Result := FSQLConnection;
end;

function TMiniRESTSQLConnectionDBX.GetQuery(const ASQL: string;
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

function TMiniRESTSQLConnectionDBX.GetQuery(const ASQL: string): IMiniRESTSQLQuery;
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
  if not FSQLConnection.Connected then
    FSQLConnection.Connected := True;
  FTransaction := FSQLConnection.BeginTransaction;
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

function TMiniRESTSQLConnectionParamsDBX.GetDatabaseType: TMiniRESTSQLDatabaseType;
begin
  Result := FDatabaseType;
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

function TMiniRESTSQLConnectionParamsDBX.SetDatabseType(
  const ADatabaseType: TMiniRESTSQLDatabaseType): IMiniRESTSQLConnectionParamsDBX;
begin
  FDatabaseType := ADatabaseType;
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

function TMiniRESTSQLConnectionDBX.InTransaction: Boolean;
begin
  Result := FSQLConnection.InTransaction;
end;

end.
