unit Test.SQL.Firedac;

interface

uses DUnitX.TestFramework, Test.SQL.Default, MiniREST.SQL.Intf, Classes, SysUtils;

type
  [TestFixture]
  TMiniRESTSQLTestFiredac = class(TMiniRESTSQLTest)
  protected
    function GetConnectionFactory: IMiniRESTSQLConnectionFactory; override;
  end;

implementation

uses MiniREST.SQL.Firedac, MiniREST.SQL.Common;

{ TMiniRESTSQLTestFiredac }

function TMiniRESTSQLTestFiredac.GetConnectionFactory: IMiniRESTSQLConnectionFactory;
var
  LConnectionInfo: TStringList;
  LDBFilePath: string;
begin
  LConnectionInfo := TStringList.Create;
  try
    LDBFilePath := ExpandFileName('..\..\..\TEST.FDB');
    LConnectionInfo.Values['Database'] := LDBFilePath;
    LConnectionInfo.Values['Server'] := 'localhost';
    Result := TMiniRESTSQLConnectionFactoryFiredac.Create(
      TMiniRESTSQLConnectionParamsFiredac.New
      .SetConnectionsCount(5)
      .SetConnectionString(LConnectionInfo.Text)
      .SetDatabseType(dbtFirebird)
    );
  finally
    LConnectionInfo.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMiniRESTSQLTestFiredac);

end.
