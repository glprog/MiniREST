unit Test.SQL.DBX;

interface

uses DUnitX.TestFramework, Test.SQL.Default, MiniREST.SQL.Intf, Classes, SysUtils,
  Data.DBXFirebird;

type
  [TestFixture]
  TMiniRESTSQLTestDBX = class(TMiniRESTSQLTest)
  protected
    function GetConnectionFactory: IMiniRESTSQLConnectionFactory; override;
  end;

implementation

uses MiniREST.SQL.DBX, MiniREST.SQL.Common;

{ TMiniRESTSQLTestDBX }

function TMiniRESTSQLTestDBX.GetConnectionFactory: IMiniRESTSQLConnectionFactory;
var
  LConnectionInfo: TStringList;
  LDBFilePath: string;
begin
  LConnectionInfo := TStringList.Create;
  try
    LConnectionInfo.LoadFromFile('..\..\dbxcon.txt');
    LDBFilePath := ExpandFileName('..\..\..\TEST.FDB');
    LConnectionInfo.Values['Database'] := LDBFilePath;
    LConnectionInfo.Values['Server'] := 'localhost';
    Result := TMiniRESTSQLConnectionFactoryDBX.Create(
      TMiniRESTSQLConnectionParamsDBX.New
      .SetConnectionsCount(5)
      .SetConnectionString(LConnectionInfo.Text)
      .SetDatabseType(dbtFirebird)
    );
  finally
    LConnectionInfo.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMiniRESTSQLTestDBX);

end.
