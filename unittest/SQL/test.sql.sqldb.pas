unit Test.SQL.SQLDb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, Test.SQL.Default, MiniREST.SQL.SQLDb,
  MiniREST.SQL.Intf, MiniREST.SQL.Common;

type
  { TMiniRESTSQLTestSQLDbFPC }

  TMiniRESTSQLTestSQLDbFPC= class(TMiniRESTSQLTest)
  protected
    procedure SetUpOnce; override;
    function GetConnectionFactory: IMiniRESTSQLConnectionFactory; override;
  published
    procedure TestSQLDB1;
  end;

implementation

procedure TMiniRESTSQLTestSQLDbFPC.SetUpOnce;
begin
  SetupFixture;
end;

function TMiniRESTSQLTestSQLDbFPC.GetConnectionFactory: IMiniRESTSQLConnectionFactory;
var
  LConnectionInfo: TStringList;
  LDBFilePath: string;
  LPathAux: string;
begin
  LConnectionInfo := TStringList.Create;
  try
    //LConnectionInfo.LoadFromFile('..\..\dbxcon.txt');
    LPathAux := ExtractFilePath(ParamStr(0)) + '..\TEST.FDB';
    LDBFilePath := ExpandFileName(LPathAux);
    LConnectionInfo.Values['DatabaseName'] := LDBFilePath;
    LConnectionInfo.Values['Server'] := 'localhost';
    Result := TMiniRESTSQLConnectionFactorySQLDb.Create(
      TMiniRESTSQLConnectionParamsSQLDb.New
      .SetConnectionsCount(5)
      .SetConnectionString(LConnectionInfo.Text)
      .SetDatabseType(dbtFirebird)
      .SetDatabaseName(LDBFilePath)
      .SetUserName('SYSDBA')
      .SetPassword('masterkey')
    );
  finally
    LConnectionInfo.Free;
  end;
end;

procedure TMiniRESTSQLTestSQLDbFPC.TestSQLDB1;
var
  LConn1: IMiniRESTSQLConnection;
begin  
  LConn1 := FConnectionFactory.GetConnection;
  Check(Assigned(LConn1));
end;

initialization
  RegisterTest(TMiniRESTSQLTestSQLDbFPC.Suite);
end.

