unit Test.SQL.PostgreSQL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, MiniREST.SQL.SQLDb,
  MiniREST.SQL.Intf, MiniREST.SQL.Common, sqldb, Test.SQL.SQLDb;

type
  TMiniRESTSQLTestSQLPostgreSQL = class(TMiniRESTSQLTestSQLDbFPC)
  protected
    function GetConnectionFactoryParams: IMiniRESTSQLConnectionFactoryParams; override;
    function GetDatabaseType: TMiniRESTSQLDatabaseType; override;
  end;

implementation

function TMiniRESTSQLTestSQLPostgreSQL.GetConnectionFactoryParams: IMiniRESTSQLConnectionFactoryParams;
var
  //LConnectionInfo: TStringList;
  //LDBFilePath: string;
  //LPathAux: string;
  LParams: IMiniRESTSQLConnectionFactoryParamsSQLDb;
begin
  //LConnectionInfo := TStringList.Create;
  try
    //LConnectionInfo.LoadFromFile('..\..\dbxcon.txt');
    //LPathAux := ExtractFilePath(ParamStr(0)) + '..\TEST.FDB';
    //LDBFilePath := ExpandFileName(LPathAux);
    //LConnectionInfo.Values['DatabaseName'] := LDBFilePath;
    //LConnectionInfo.Values['Server'] := GetServerHostName;
    LParams := TMiniRESTSQLConnectionParamsSQLDb.Create;
    LParams.SetConnectionsCount(FConnectionCount);
    //LParams.SetConnectionString('');
    LParams.SetDatabseType(dbtPostgreSQL);
    LParams.SetDatabaseName('minirestsql');
    LParams.SetUserName('postgres');
    LParams.SetPassword('postgres');
    LParams.SetLogEvent(@LogEvent);
    LParams.SetServerHostName(GetServerHostName);
    LParams.SetServerPort(GetServerPort);
    Result := LParams;
  finally
    //LConnectionInfo.Free;
  end;
end;

function TMiniRESTSQLTestSQLPostgreSQL.GetDatabaseType: TMiniRESTSQLDatabaseType;
begin
  Result := dbtPostgreSQL;
end;

initialization
  RegisterTest(TMiniRESTSQLTestSQLPostgreSQL.Suite);

end.

