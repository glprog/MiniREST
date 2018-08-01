unit MiniREST.SQL.Intf;

interface

type
  IMiniRESTSQLQuery = interface
  ['{A6624CAD-F305-48BD-B1D4-736DEE685A83}']
    function GetValue(AField : string) : Variant; overload;
    function GetValue(AField : string; ADefault : Variant) : Variant; overload;
    function Eof : Boolean;
    procedure Next;
    function IsEmpty : Boolean;
  end;

  IMiniRESTSQLConnection = interface
  ['{AED7B927-D53B-4E92-B4CA-3CC12182E757}']
    function GetQuery(ASQL : string) : IMiniRESTSQLQuery; overload;
    function GetQuery(ASQL : string; AParams : array of Variant) : IMiniRESTSQLQuery; overload;
    function Execute(ACommand : string) : IMiniRESTSQLQuery; overload;
    function Execute(ACommand : string; AParams : array of Variant) : IMiniRESTSQLQuery; overload;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
  end;

  IMiniRESTSQLConnectionFactory = interface
  ['{6E405916-A78D-4C75-BCE7-07378517AB2D}']
    function GetConnection : IMiniRESTSQLConnection;
    procedure ReleaseConnection(AConnection : IMiniRESTSQLConnection);
  end;

implementation

end.
