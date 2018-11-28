unit Test.SQL.DBX;

interface

uses DUnitX.TestFramework, Test.SQL.Default, MiniREST.SQL.Intf, Classes, SysUtils, Data.DBXFirebird;

type
  [TestFixture]
  TMiniRESTSQLTestDBX = class(TMiniRESTSQLTest)
  protected
    function GetConnectionFactory: IMiniRESTSQLConnectionFactory; override;
  end;

implementation

uses MiniREST.SQL.DBX;

{ TMiniRESTSQLTestDBX }

function TMiniRESTSQLTestDBX.GetConnectionFactory: IMiniRESTSQLConnectionFactory;
var
  LConnectionInfo: TStringList;
begin
  LConnectionInfo := TStringList.Create;
  try
    LConnectionInfo.LoadFromFile('..\..\dbxcon.txt');
    Result := TMiniRESTSQLConnectionFactoryDBX.Create(
      TMiniRESTSQLConnectionParamsDBX.New
      .SetConnectionsCount(5)
      .SetConnectionString(LConnectionInfo.Text)
      .SetDriverName('Firebird')
    );
  finally
    LConnectionInfo.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMiniRESTSQLTestDBX);

end.
