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

uses MiniREST.SQL.Firedac;

{ TMiniRESTSQLTestFiredac }

function TMiniRESTSQLTestFiredac.GetConnectionFactory: IMiniRESTSQLConnectionFactory;
begin

end;

initialization
  TDUnitX.RegisterTestFixture(TMiniRESTSQLTestFiredac);

end.
