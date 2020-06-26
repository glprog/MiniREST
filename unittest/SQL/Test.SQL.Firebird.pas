unit Test.SQL.Firebird;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, MiniREST.SQL.SQLDb,
  MiniREST.SQL.Intf, MiniREST.SQL.Common, sqldb, Test.SQL.SQLDb;

type
  TMiniRESTSQLTestSQLFirebird = class(TMiniRESTSQLTestSQLDbFPC)
  protected
    function GetConnectionFactoryParams: IMiniRESTSQLConnectionFactoryParams; override;
    function GetDatabaseType: TMiniRESTSQLDatabaseType; override;
  end;

implementation

function TMiniRESTSQLTestSQLFirebird.GetConnectionFactoryParams: IMiniRESTSQLConnectionFactoryParams;
var
  LConnectionInfo: TStringList;
  LDBFilePath: string;
  LPathAux: string;
  LParams: IMiniRESTSQLConnectionFactoryParamsSQLDb;
begin
  LConnectionInfo := TStringList.Create;
  try
    //LConnectionInfo.LoadFromFile('..\..\dbxcon.txt');
    LPathAux := ExtractFilePath(ParamStr(0)) + '..\TEST.FDB';
    LDBFilePath := ExpandFileName(LPathAux);
    LConnectionInfo.Values['DatabaseName'] := LDBFilePath;
    LConnectionInfo.Values['Server'] := GetServerHostName;
    LParams := TMiniRESTSQLConnectionParamsSQLDb.Create;
    LParams.SetConnectionsCount(FConnectionCount);
    LParams.SetConnectionString(LConnectionInfo.Text);
    LParams.SetDatabseType(dbtFirebird);
    LParams.SetDatabaseName(LDBFilePath);
    LParams.SetUserName('SYSDBA');
    LParams.SetPassword('masterkey');
    LParams.SetLogEvent(@LogEvent);
    LParams.SetServerHostName(GetServerHostName);
    LParams.SetServerPort(GetServerPort);
    Result := LParams;
  finally
    LConnectionInfo.Free;
  end;
end;

function TMiniRESTSQLTestSQLFirebird.GetDatabaseType: TMiniRESTSQLDatabaseType;
begin
  Result := dbtFirebird;
end;

initialization
  RegisterTest(TMiniRESTSQLTestSQLFirebird.Suite);

end.

