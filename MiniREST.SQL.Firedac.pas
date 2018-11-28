unit MiniREST.SQL.Firedac;

interface

uses SysUtils, MiniREST.SQL.Intf, MiniREST.SQL.Base, MiniREST.SQL.Common, DB;

type
  IMiniRESTSQLConnectionParamsFiredac = interface
  ['{09A0CEDE-A6F1-4B3B-BF93-7024DE4CB743}']
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
    function GetObject: TObject; override;
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

{ TMiniRESTSQLConnectionFactoryFiredac }

constructor TMiniRESTSQLConnectionFactoryFiredac.Create(
  AParams: IMiniRESTSQLConnectionParamsFiredac);
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLConnectionFactoryFiredac.InternalGetconnection: IMiniRESTSQLConnection;
begin
  raise Exception.Create('Not implemented');
end;

{ TMiniRESTSQLConnectionFiredac }

procedure TMiniRESTSQLConnectionFiredac.Commit;
begin
  inherited;
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTSQLConnectionFiredac.Connect;
begin
  inherited;
  raise Exception.Create('Not implemented');
end;

constructor TMiniRESTSQLConnectionFiredac.Create(AOwner: IMiniRESTSQLConnectionFactory;
  AParams: IMiniRESTSQLConnectionParamsFiredac);
begin
  raise Exception.Create('Not implemented');
end;

destructor TMiniRESTSQLConnectionFiredac.Destroy;
begin
  raise Exception.Create('Not implemented');
  inherited;
end;

function TMiniRESTSQLConnectionFiredac.Execute(const ACommand: string;
  AParams: array of IMiniRESTSQLParam): Integer;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLConnectionFiredac.GetDatabaseInfo: IMiniRESTSQLDatabaseInfo;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLConnectionFiredac.GetObject: TObject;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLConnectionFiredac.GetQuery(const ASQL: string): IMiniRESTSQLQuery;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLConnectionFiredac.GetQuery(const ASQL: string;
  AParams: array of IMiniRESTSQLParam): IMiniRESTSQLQuery;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLConnectionFiredac.GetQuery: IMiniRESTSQLQuery;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTSQLConnectionFiredac.Rollback;
begin
  inherited;
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTSQLConnectionFiredac.StartTransaction;
begin
  inherited;
  raise Exception.Create('Not implemented');
end;

{ TMiniRESTSQLQueryFiredac }

function TMiniRESTSQLQueryFiredac.AddParam(AParam: IMiniRESTSQLParam): IMiniRESTSQLQuery;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTSQLQueryFiredac.Append;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLQueryFiredac.ApplyUpdates(const AMaxErrors: Integer): Integer;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTSQLQueryFiredac.Cancel;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTSQLQueryFiredac.Close;
begin
  raise Exception.Create('Not implemented');
end;

constructor TMiniRESTSQLQueryFiredac.Create(AConnection: IMiniRESTSQLConnection);
begin
  raise Exception.Create('Not implemented');
end;

destructor TMiniRESTSQLQueryFiredac.Destroy;
begin
  raise Exception.Create('Not implemented');
  inherited;
end;

function TMiniRESTSQLQueryFiredac.Eof: Boolean;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLQueryFiredac.FieldByName(const AFieldName: string): TField;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLQueryFiredac.GetDataSet: TDataSet;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLQueryFiredac.GetSQL: string;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLQueryFiredac.GetValue(AField: string): Variant;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLQueryFiredac.GetValue(AField: string; ADefault: Variant): Variant;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTSQLQueryFiredac.Insert;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLQueryFiredac.IsEmpty: Boolean;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTSQLQueryFiredac.Next;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTSQLQueryFiredac.Open;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLQueryFiredac.ParamByName(const AParamName: string): IMiniRESTSQLParam;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTSQLQueryFiredac.Post;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTSQLQueryFiredac.SetSQL(const ASQL: string);
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTSQLQueryFiredac.ToJSON: string;
begin
  raise Exception.Create('Not implemented');
end;

end.
