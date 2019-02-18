{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
unit MiniREST.SQL.Intf;

interface

uses SysUtils, MiniREST.SQL.Common, DB;

type
  IMiniRESTSQLDatabaseInfo = interface;

  IMiniRESTSQLQuery = interface
  ['{A6624CAD-F305-48BD-B1D4-736DEE685A83}']
    procedure Open;
    procedure Close;
    function GetSQL: string;
    procedure SetSQL(const ASQL: string);
    function ParamByName(const AParamName: string): IMiniRESTSQLParam;
    function AddParam(AParam: IMiniRESTSQLParam): IMiniRESTSQLQuery;
    {function GetValue(AField: string) : Variant; overload;
    function GetValue(AField: string; ADefault: Variant): Variant; overload;
    function FieldByName(const AFieldName: string): TField;
    function Eof: Boolean;
    procedure Next;
    function IsEmpty: Boolean;
    procedure Post;
    procedure Cancel;
    procedure Insert;
    procedure Append;}
    function ApplyUpdates(const AMaxErrors: Integer = 0): Integer;
    function GetDataSet: TDataSet;
    function ToJSON: string;
    property SQL: string read GetSQL write SetSQL;
    property DataSet: TDataset read GetDataSet;
  end;

  IMiniRESTSQLConnection = interface
  ['{AED7B927-D53B-4E92-B4CA-3CC12182E757}']
    function GetQuery: IMiniRESTSQLQuery; overload;
    function GetQuery(const ASQL: string): IMiniRESTSQLQuery; overload;
    function GetQuery(const ASQL: string; AParams : array of IMiniRESTSQLParam): IMiniRESTSQLQuery; overload;
    //function Execute(ACommand: string): IMiniRESTSQLQuery; overload;
    //function Execute(ACommand: string; AParams: array of Variant): IMiniRESTSQLQuery; overload;    
    function Execute(const ACommand: string; AParams: array of IMiniRESTSQLParam): Integer;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function GetObject: TObject;
    procedure Connect;
    function GetName: string;
    function SetName(const AName: string): IMiniRESTSQLConnection;
    function GetDatabaseInfo: IMiniRESTSQLDatabaseInfo;
  end;

  IMiniRESTSQLConnectionFactory = interface
  ['{6E405916-A78D-4C75-BCE7-07378517AB2D}']
    function GetConnection : IMiniRESTSQLConnection;
    procedure ReleaseConnection(AConnection : IMiniRESTSQLConnection);
    function GetObject: TObject;
  end;

  IMiniRESTSQLConnectionExecute = interface
  ['{3F313146-9CBE-4037-AA79-EB7D9924D449}']
    function Execute(ACommand: string): Integer;
  end;

  IMiniRESTSQLPrimaryKeyInfo = interface
  ['{5FBC1287-FB0E-4457-8E05-F009D0F79AB8}']
    function GetName: string;
    procedure SetName(const AName: string);
    function GetFields: TArray<string>;
    procedure SetFields(const AFields: TArray<string>);
    property Name: string read GetName write SetName;
    property Fields: TArray<string> read GetFields write SetFields;
  end;

  IMiniRESTSQLForeignKeyInfo = interface
  ['{06E949D0-3D42-4888-949A-1B08BB987048}']
    function GetName: string;
    procedure SetName(const AName: string);
    function GetFields: TArray<string>;
    procedure SetFields(const AFields: TArray<string>);
    function GetFKFields: TArray<string>;
    procedure SetFKFields(const AFields: TArray<string>);
    function GetFKTableName: string;
    procedure SetFKTableName(const AName: string);
    property Name: string read GetName write SetName;
    property Fields: TArray<string> read GetFields write SetFields;
    property FKFields: TArray<string> read GetFKFields write SetFKFields;
    property FKTableName: string read GetFKTableName write SetFKTableName;
  end;

  IMiniRESTSQLColumnInfo = interface
  ['{8959F76F-5F67-400E-A52E-3AB11B265BE5}']
    function GetName: string;
    property Name: string read GetName;
  end;

  IMiniRESTSQLDatabaseInfo = interface
  ['{7DD8F064-52CC-4D1C-B2A2-E3591D918B05}']
    function DatabaseType: TMiniRESTSQLDatabaseType;
    function TableExists(const ATableName: string): Boolean;
    function FieldExists(const ATableName, AFieldName: string): Boolean;
    function GetPrimaryKey(const ATableName: string): IMiniRESTSQLPrimaryKeyInfo;
    function GetForeignKeys(const ATableName: string): TArray<IMiniRESTSQLForeignKeyInfo>;
    function GetColumns(const ATableName: string): TArray<IMiniRESTSQLColumnInfo>;
  end;

implementation

end.
