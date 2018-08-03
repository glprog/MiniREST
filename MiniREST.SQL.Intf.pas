unit MiniREST.SQL.Intf;

interface

uses MiniREST.SQL.Common, DB;

type
  IMiniRESTSQLQuery = interface
  ['{A6624CAD-F305-48BD-B1D4-736DEE685A83}']
    function GetSQL: string;
    procedure SetSQL(const ASQL: string);
    function AddParam(AParam: IMiniRESTSQLParam): IMiniRESTSQLQuery;
    function GetValue(AField: string) : Variant; overload;
    function GetValue(AField: string; ADefault: Variant): Variant; overload;
    function Eof: Boolean;
    procedure Next;
    function IsEmpty: Boolean;
    procedure Open;
    procedure Close;
    procedure Post;
    procedure Cancel;
    procedure Insert;
    procedure Append;
    function ApplyUpdates(const AMaxErrors: Integer = 0): Integer;
    function GetDataSet: TDataSet;
    function ToJSON: string;
    property SQL: string read GetSQL write SetSQL;
  end;

  IMiniRESTSQLConnection = interface
  ['{AED7B927-D53B-4E92-B4CA-3CC12182E757}']
    function GetQuery(ASQL: string): IMiniRESTSQLQuery; overload;
    function GetQuery(ASQL: string; AParams : array of IMiniRESTSQLParam): IMiniRESTSQLQuery; overload;
    //function Execute(ACommand: string): IMiniRESTSQLQuery; overload;
    //function Execute(ACommand: string; AParams: array of Variant): IMiniRESTSQLQuery; overload;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function GetObject: TObject;
    procedure Connect;
    function GetName: string;
    function SetName(const AName: string): IMiniRESTSQLConnection;
  end;

  IMiniRESTSQLConnectionFactory = interface
  ['{6E405916-A78D-4C75-BCE7-07378517AB2D}']
    function GetConnection : IMiniRESTSQLConnection;
    procedure ReleaseConnection(AConnection : IMiniRESTSQLConnection);
    procedure GenerateConnections;
  end;

  IMiniRESTSQLConnectionExecute = interface
  ['{3F313146-9CBE-4037-AA79-EB7D9924D449}']
    function Execute(ACommand: string): Integer;
  end;

implementation

end.
