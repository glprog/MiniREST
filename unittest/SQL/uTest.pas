unit uTest;

interface
uses
  DUnitX.TestFramework, Classes, SysUtils, MiniREST.SQL.Intf, Data.DBXFirebird;

type

  [TestFixture]
  TMiniRESTSQLTest = class(TObject)
  private
    FConnectionFactory: IMiniRESTSQLConnectionFactory;
  public
    [SetupFixture]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestSelect;
  end;

implementation

uses MiniREST.SQL.DBX;

procedure TMiniRESTSQLTest.Setup;
var
  LConnectionInfo: TStringList;
begin
  LConnectionInfo := TStringList.Create;
  try
    LConnectionInfo.LoadFromFile('..\..\dbxcon.txt');
    FConnectionFactory := TMiniRESTSQLConnectionFactoryDBX.Create(
      TMiniRESTSQLConnectionParamsDBX.New
      .SetConnectionsCount(5)
      .SetConnectionString(LConnectionInfo.Text)
      .SetDriverName('Firebird')
    );    
  finally
    LConnectionInfo.Free;
  end;
end;

procedure TMiniRESTSQLTest.TearDown;
begin
end;

procedure TMiniRESTSQLTest.TestSelect;
var
  LConnection: IMiniRESTSQLConnection;
  LQry: IMiniRESTSQLQuery;
  LId: Integer;
begin
  LConnection := FConnectionFactory.GetConnection;
  LQry := LConnection.GetQuery;
  LQry.SQL := 'select gen_id(gen_customer_id, 1) from rdb$database';
  LQry.Open;
  LId := LQry.DataSet.FieldByName('GEN_ID').AsInteger;
  LQry.Close;
  LQry.SQL := 'SELECT * FROM CUSTOMER WHERE 1=0';
  LQry.Open;
  LQry.DataSet.Insert;
  LQry.DataSet.FieldByName('ID').AsInteger := LId;
  LQry.DataSet.FieldByName('NAME').AsString := 'HUE';
  LQry.DataSet.Post;
  LQry.ApplyUpdates(0);
end;

initialization
  TDUnitX.RegisterTestFixture(TMiniRESTSQLTest);
end.
