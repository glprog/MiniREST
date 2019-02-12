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
    procedure SetUp; override;
    procedure TearDown; override;
    procedure SetUpOnce; override;
    function GetConnectionFactory: IMiniRESTSQLConnectionFactory; override;
  published
    procedure TestSQLDB1;
  end;

implementation

procedure TMiniRESTSQLTestSQLDbFPC.SetUp;
begin

end;

procedure TMiniRESTSQLTestSQLDbFPC.TearDown;
begin

end;

procedure TMiniRESTSQLTestSQLDbFPC.SetUpOnce;
begin
  SetupFixture;
end;

function TMiniRESTSQLTestSQLDbFPC.GetConnectionFactory: IMiniRESTSQLConnectionFactory;
var
  LConnectionInfo: TStringList;
  LDBFilePath: string;
begin
  LConnectionInfo := TStringList.Create;
  try
    //LConnectionInfo.LoadFromFile('..\..\dbxcon.txt');
    //LDBFilePath := ExpandFileName('..\..\..\TEST.FDB');
    LConnectionInfo.Values['Database'] := LDBFilePath;
    LConnectionInfo.Values['Server'] := 'localhost';
    Result := TMiniRESTSQLConnectionFactorySQLDb.Create(
      TMiniRESTSQLConnectionParamsSQLDb.New
      .SetConnectionsCount(5)
      .SetConnectionString(LConnectionInfo.Text)
      .SetDatabseType(dbtFirebird)
    );
  finally
    LConnectionInfo.Free;
  end;
end;

procedure TMiniRESTSQLTestSQLDbFPC.TestSQLDB1;
var
  LConn1: IMiniRESTSQLConnection;
begin
  LConn1 := GetConnectionFactory.GetConnection;
end;

initialization
  RegisterTest(TMiniRESTSQLTestSQLDbFPC.Suite);
end.

