unit Test.SQL.Sqlite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, MiniREST.SQL.SQLDb,
  MiniREST.SQL.Intf, MiniREST.SQL.Common, sqldb, Test.SQL.SQLDb;

type
  TMiniRESTSQLTestSQLSqlite = class(TMiniRESTSQLTestSQLDbFPC)
  protected
    function GetConnectionFactoryParams: IMiniRESTSQLConnectionFactoryParams; override;
    function GetDatabaseType: TMiniRESTSQLDatabaseType; override;
  end;

implementation

function TMiniRESTSQLTestSQLSqlite.GetConnectionFactoryParams: IMiniRESTSQLConnectionFactoryParams;
var
  LConnectionInfo: TStringList;
  LDBFilePath: string;
  LPathAux: string;
  LParams: IMiniRESTSQLConnectionFactoryParamsSQLDb;
begin  
  LPathAux := ExtractFilePath(ParamStr(0)) + '..\TEST.SQLITE';
  LDBFilePath := ExpandFileName(LPathAux);    
  LParams := TMiniRESTSQLConnectionParamsSQLDb.Create;
  LParams.SetConnectionsCount(FConnectionCount);
  LParams.SetDatabseType(dbtSqlite);
  LParams.SetDatabaseName(LDBFilePath);
  LParams.SetLogEvent(@LogEvent);  
  Result := LParams;
end;

function TMiniRESTSQLTestSQLSqlite.GetDatabaseType: TMiniRESTSQLDatabaseType;
begin
  Result := dbtSqlite;
end;

initialization
  RegisterTest(TMiniRESTSQLTestSQLSqlite.Suite);

end.

