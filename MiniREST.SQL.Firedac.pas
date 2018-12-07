unit MiniREST.SQL.Firedac;

interface

uses SysUtils, Variants, Classes, MiniREST.SQL.Intf, MiniREST.SQL.Base, MiniREST.SQL.Common, DB,
  Firedac.Comp.Client, Firedac.Stan.Def, Firedac.Stan.Param, Generics.Collections, FireDAC.Phys.IBBase,
  FireDAC.Phys.IB, FireDAC.Phys.FB, FireDAC.Stan.Async, FireDAC.Dapt;

type
  IMiniRESTSQLConnectionParamsFiredac = interface
  ['{09A0CEDE-A6F1-4B3B-BF93-7024DE4CB743}']
    function GetConnectionsCount: Integer;
    function SetConnectionsCount(const AConnectionsCount: Integer): IMiniRESTSQLConnectionParamsFiredac;
    function GetConnectionString: string;
    function SetConnectionString(const AConnectionString: string): IMiniRESTSQLConnectionParamsFiredac;
    function GetUserName: string;
    function SetUserName(const AUserName: string): IMiniRESTSQLConnectionParamsFiredac;
    function GetPassword: string;
    function SetPassword(const APassword: string): IMiniRESTSQLConnectionParamsFiredac;
    function GetDatabaseType: TMiniRESTSQLDatabaseType;
    function SetDatabseType(const ADatabaseType: TMiniRESTSQLDatabaseType): IMiniRESTSQLConnectionParamsFiredac;
  end;

  TMiniRESTSQLConnectionParamsFiredac = class(TInterfacedObject, IMiniRESTSQLConnectionParamsFiredac)
  private
    FConnectionsCount: Integer;
    FConnectionString: string;
    FUserName: string;
    FPassword: string;
    FDatabaseType: TMiniRESTSQLDatabaseType;
  public
    class function New: IMiniRESTSQLConnectionParamsFiredac; 
    function GetConnectionsCount: Integer;
    function SetConnectionsCount(const AConnectionsCount: Integer): IMiniRESTSQLConnectionParamsFiredac;
    function GetConnectionString: string;
    function SetConnectionString(const AConnectionString: string): IMiniRESTSQLConnectionParamsFiredac;
    function GetUserName: string;
    function SetUserName(const AUserName: string): IMiniRESTSQLConnectionParamsFiredac;
    function GetPassword: string;
    function SetPassword(const APassword: string): IMiniRESTSQLConnectionParamsFiredac;
    function GetDatabaseType: TMiniRESTSQLDatabaseType;
    function SetDatabseType(const ADatabaseType: TMiniRESTSQLDatabaseType): IMiniRESTSQLConnectionParamsFiredac;
  end;

  TMiniRESTSQLConnectionFactoryFiredac = class(TMiniRESTSQLConnectionFactoryBase)
  protected
    FConnectionString: string;
    FConnectionParams: IMiniRESTSQLConnectionParamsFiredac;
    function InternalGetconnection: IMiniRESTSQLConnection; override;
  public
    constructor Create(AParams: IMiniRESTSQLConnectionParamsFiredac); overload;
  end;

  TMiniRESTSQLConnectionFiredac = class(TMiniRESTSQLConnectionBase)
  protected
    FFDConnection: TFDConnection;
    FConnectionParams: IMiniRESTSQLConnectionParamsFiredac;
    function GetObject: TObject; override;
    function GetDriverName(const ADatabaseType: TMiniRESTSQLDatabaseType): string;
  public
    constructor Create(AOwner: IMiniRESTSQLConnectionFactory; AParams: IMiniRESTSQLConnectionParamsFiredac);
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

  TMiniRESTSQLQueryFiredac = class(TInterfacedObject, IMiniRESTSQLQuery)
  protected
    FConnection: IMiniRESTSQLConnection;
    FQry: TFDQuery;
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

uses MiniREST.JSON, MiniREST.SQL.Firebird;

{ TMiniRESTSQLConnectionFactoryFiredac }

constructor TMiniRESTSQLConnectionFactoryFiredac.Create(
  AParams: IMiniRESTSQLConnectionParamsFiredac);
begin
  inherited Create(AParams.GetConnectionsCount);
  FConnectionParams := AParams;
  FConnectionsCount := AParams.GetConnectionsCount;
end;

function TMiniRESTSQLConnectionFactoryFiredac.InternalGetconnection: IMiniRESTSQLConnection;
begin
  Result := TMiniRESTSQLConnectionFiredac.Create(Self, FConnectionParams);  
end;

{ TMiniRESTSQLConnectionFiredac }

procedure TMiniRESTSQLConnectionFiredac.Commit;
begin
  FFDConnection.Commit;
end;

procedure TMiniRESTSQLConnectionFiredac.Connect;
var
  LStringList: TStringList;
  LName: string;
  I: Integer;
begin
  LStringList := TStringList.Create;
  try
    if FFDConnection.Connected then
      Exit;
    FFDConnection.DriverName := GetDriverName(FConnectionParams.GetDatabaseType);
    FFDConnection.LoginPrompt := False;
    FFDConnection.Params.UserName := FConnectionParams.GetUserName;
    FFDConnection.Params.Password := FConnectionParams.GetPassword;
    LStringList.Text := FConnectionParams.GetConnectionString;
    for I := 0 to LStringList.Count - 1 do
    begin
      LName := LStringList.Names[I];
      FFDConnection.Params.Values[LName] := LStringList.Values[LName];
    end;
    FFDConnection.Connected := True;
  finally
    LStringList.Free;
  end;
end;

constructor TMiniRESTSQLConnectionFiredac.Create(AOwner: IMiniRESTSQLConnectionFactory;
  AParams: IMiniRESTSQLConnectionParamsFiredac);
begin
  FFDConnection := TFDConnection.Create(nil);
  FConnectionParams := AParams;
  inherited Create(AOwner);
end;

destructor TMiniRESTSQLConnectionFiredac.Destroy;
begin
  FFDConnection.Free;
  inherited;
end;

function TMiniRESTSQLConnectionFiredac.Execute(const ACommand: string;
  AParams: array of IMiniRESTSQLParam): Integer;
var
  LParams: TFDParams;
  LParam: TFDParam;
  LMiniRESTSQLParam: IMiniRESTSQLParam;
begin
  LParams := TFDParams.Create;
  try
    for LMiniRESTSQLParam in AParams do
    begin
      LParam := LParams.Add;
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
    Result := FFDConnection.ExecSQL(ACommand, LParams);    
  finally
    //LParams.Free;
  end;
end;

function TMiniRESTSQLConnectionFiredac.GetDatabaseInfo: IMiniRESTSQLDatabaseInfo;
begin
  Result := nil;
  case FConnectionParams.GetDatabaseType of
    dbtFirebird: Result := TMiniRESTSQLDatabaseInfoFirebird.Create(Self);
    else
      raise Exception.Create('TMiniRESTSQLConnectionFiredac.GetDatabaseInfo: ' +
      'DatabaseType not implemented');
  end;
end;

function TMiniRESTSQLConnectionFiredac.GetObject: TObject;
begin
  Result := FFDConnection;
end;

function TMiniRESTSQLConnectionFiredac.GetQuery(const ASQL: string): IMiniRESTSQLQuery;
begin
  Result := TMiniRESTSQLQueryFiredac.Create(Self);
  Result.SQL := ASQL;
end;

function TMiniRESTSQLConnectionFiredac.GetQuery(const ASQL: string;
  AParams: array of IMiniRESTSQLParam): IMiniRESTSQLQuery;
var
  LParam: IMiniRESTSQLParam;
begin
  Result := TMiniRESTSQLQueryFiredac.Create(Self);
  Result.SQL := ASQL;
  for LParam in AParams do
  begin
    Result.AddParam(LParam);
  end;
end;

function TMiniRESTSQLConnectionFiredac.GetQuery: IMiniRESTSQLQuery;
begin
  Result := TMiniRESTSQLQueryFiredac.Create(Self);
end;

procedure TMiniRESTSQLConnectionFiredac.Rollback;
begin
  FFDConnection.Rollback;
end;

procedure TMiniRESTSQLConnectionFiredac.StartTransaction;
begin
  FFDConnection.StartTransaction;
end;

{ TMiniRESTSQLQueryFiredac }

function TMiniRESTSQLQueryFiredac.AddParam(AParam: IMiniRESTSQLParam): IMiniRESTSQLQuery;
begin
  FParams.AddOrSetValue(AParam.GetParamName, AParam);
  Result := Self;
end;

procedure TMiniRESTSQLQueryFiredac.Append;
begin
  FQry.Append;
end;

function TMiniRESTSQLQueryFiredac.ApplyUpdates(const AMaxErrors: Integer): Integer;
begin
  Result := FQry.ApplyUpdates(AMaxErrors);
end;

procedure TMiniRESTSQLQueryFiredac.Cancel;
begin
  FQry.Cancel;
end;

procedure TMiniRESTSQLQueryFiredac.Close;
begin
  FQry.Close;
end;

constructor TMiniRESTSQLQueryFiredac.Create(AConnection: IMiniRESTSQLConnection);
begin
  FConnection := AConnection;
  FQry := TFDQuery.Create(nil);
  FQry.CachedUpdates := True;
  FQry.Connection := TFDConnection(AConnection.GetObject);
  FParams := TObjectDictionary<string, IMiniRESTSQLParam>.Create([]);
end;

destructor TMiniRESTSQLQueryFiredac.Destroy;
begin
  FQry.Free;
  FParams.Free;
  inherited;
end;

function TMiniRESTSQLQueryFiredac.Eof: Boolean;
begin
  Result := FQry.Eof;
end;

function TMiniRESTSQLQueryFiredac.FieldByName(const AFieldName: string): TField;
begin
  Result := FQry.FieldByName(AFieldName);
end;

function TMiniRESTSQLQueryFiredac.GetDataSet: TDataSet;
begin
  Result := FQry;
end;

function TMiniRESTSQLQueryFiredac.GetSQL: string;
begin
  Result := FSQL;
end;

function TMiniRESTSQLQueryFiredac.GetValue(AField: string): Variant;
begin
  Result := FQry.FieldByName(AField).Value;
end;

function TMiniRESTSQLQueryFiredac.GetValue(AField: string; ADefault: Variant): Variant;
var
  LValue: Variant;
begin
  LValue := FQry.FieldByName(AField).Value;
  if LValue = Null then
    Result := ADefault
  else
    Result := LValue;
end;

procedure TMiniRESTSQLQueryFiredac.Insert;
begin
  FQry.Insert;
end;

function TMiniRESTSQLQueryFiredac.IsEmpty: Boolean;
begin
  Result := FQry.IsEmpty;
end;

procedure TMiniRESTSQLQueryFiredac.Next;
begin
  FQry.Next;
end;

procedure TMiniRESTSQLQueryFiredac.Open;
var
  LParam: IMiniRESTSQLParam;
begin
  FQry.Params.Clear;
  for LParam in FParams.Values do
  begin
    InternalAddParam(LParam);
  end;
  FConnection.Connect;
  FQry.Open;
end;

function TMiniRESTSQLQueryFiredac.ParamByName(const AParamName: string): IMiniRESTSQLParam;
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

procedure TMiniRESTSQLQueryFiredac.Post;
begin
  FQry.Post;
end;

procedure TMiniRESTSQLQueryFiredac.SetSQL(const ASQL: string);
begin
  FSQL := ASQL;
  FQry.SQL.Text := ASQL;
end;

function TMiniRESTSQLQueryFiredac.ToJSON: string;
begin
  Result := TMiniRESTJSON.DatasetToJson2(FQry);
end;

function TMiniRESTSQLConnectionParamsFiredac.GetConnectionsCount: Integer;
begin
  Result := FConnectionsCount;  
end;

function TMiniRESTSQLConnectionParamsFiredac.SetConnectionsCount(const AConnectionsCount: Integer): IMiniRESTSQLConnectionParamsFiredac;
begin
  Result := Self;
  FConnectionsCount := AConnectionsCount;
end;

function TMiniRESTSQLConnectionParamsFiredac.GetConnectionString: string;
begin
  Result := FConnectionString;
end;

function TMiniRESTSQLConnectionParamsFiredac.SetConnectionString(const AConnectionString: string): IMiniRESTSQLConnectionParamsFiredac;
begin
  Result := Self;
  FConnectionString := AConnectionString;
end;

function TMiniRESTSQLConnectionParamsFiredac.GetUserName: string;
begin
  Result := FUserName;
end;

function TMiniRESTSQLConnectionParamsFiredac.SetUserName(const AUserName: string): IMiniRESTSQLConnectionParamsFiredac;
begin
  Result := Self;
  FUserName := AUserName;
end;

function TMiniRESTSQLConnectionParamsFiredac.GetPassword: string;
begin
  Result := FPassword;
end;

function TMiniRESTSQLConnectionParamsFiredac.SetPassword(const APassword: string): IMiniRESTSQLConnectionParamsFiredac;
begin
  Result := Self;
  FPassword := APassword;
end;

function TMiniRESTSQLConnectionParamsFiredac.GetDatabaseType: TMiniRESTSQLDatabaseType;
begin
  Result := FDatabaseType;
end;

function TMiniRESTSQLConnectionParamsFiredac.SetDatabseType(const ADatabaseType: TMiniRESTSQLDatabaseType): IMiniRESTSQLConnectionParamsFiredac;
begin
  Result := Self;
  FDatabaseType := ADatabaseType;
end;

class function TMiniRESTSQLConnectionParamsFiredac.New: IMiniRESTSQLConnectionParamsFiredac;
begin
  Result := Create;  
end;

function TMiniRESTSQLConnectionFiredac.GetDriverName(const ADatabaseType: TMiniRESTSQLDatabaseType): string;
begin
  case ADatabaseType of
    dbtFirebird: Result := 'FB';
  end;
end;

procedure TMiniRESTSQLQueryFiredac.InternalAddParam(AParam: IMiniRESTSQLParam);
var
  LParamType: TMiniRESTSQLParamType;
  LParam: TFDParam;
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

end.
