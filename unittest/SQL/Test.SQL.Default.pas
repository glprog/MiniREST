unit Test.SQL.Default;

interface
uses
  DUnitX.TestFramework, Classes, SysUtils, MiniREST.SQL.Intf;

type

  TMiniRESTSQLTest = class(TObject)
  private
    FConnectionFactory: IMiniRESTSQLConnectionFactory;
  protected
    function GetConnectionFactory: IMiniRESTSQLConnectionFactory; virtual; abstract;
  public        
    [SetupFixture]
    procedure SetupFixture;
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestInsert;
    [Test]
    procedure TestExecute;
    [Test]
    procedure TestJSON;    
    [Test]
    procedure TestJSON2;
    [Test]
    procedure TestTransaction;
    [Test]
    procedure TestTransaction2;    
  end;

implementation

procedure TMiniRESTSQLTest.SetupFixture;
begin
  FConnectionFactory := GetConnectionFactory;
end;

procedure TMiniRESTSQLTest.TearDown;
var
  LConnection: IMiniRESTSQLConnection;  
begin
  LConnection := FConnectionFactory.GetConnection;  
  LConnection.Execute('DELETE FROM CUSTOMER', []);
end;

procedure TMiniRESTSQLTest.TestInsert;
var
  LConn1, LConn2: IMiniRESTSQLConnection;
  LQry, LQryID, LQryCheck: IMiniRESTSQLQuery;
  LId: Integer;
  I: Integer;
begin
  LConn1 := FConnectionFactory.GetConnection;
  LConn2 := FConnectionFactory.GetConnection;
  LQry := LConn1.GetQuery;
  LQryID := LConn1.GetQuery;
  LQry.SQL := 'SELECT * FROM CUSTOMER WHERE 1=0';
  LQry.Open;
  for I := 0 to 99 do
  begin
    LQryID.Close;
    LQryID.SQL := 'select gen_id(gen_customer_id, 1) from rdb$database';
    LQryID.Open;
    LId := LQryID.DataSet.FieldByName('GEN_ID').AsInteger;      
    LQry.DataSet.Append;
    LQry.DataSet.FieldByName('ID').AsInteger := LId;
    LQry.DataSet.FieldByName('NAME').AsString := 'HUE';
    LQry.DataSet.Post;    
  end;
  LQry.ApplyUpdates(0);
  LQryCheck := LConn2.GetQuery('SELECT COUNT(*) FROM CUSTOMER');
  LQryCheck.Open;
  Assert.AreEqual(100, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
end;

procedure TMiniRESTSQLTest.Setup;
var
  LConnection: IMiniRESTSQLConnection;  
begin
  LConnection := FConnectionFactory.GetConnection;  
  LConnection.Execute('DELETE FROM CUSTOMER', []);
end;

procedure TMiniRESTSQLTest.TestExecute;
var
  LConn1, LConn2: IMiniRESTSQLConnection;
  LQryCheck: IMiniRESTSQLQuery;  
  I: Integer;
begin
  LConn1 := FConnectionFactory.GetConnection;
  LConn2 := FConnectionFactory.GetConnection;
  for I := 0 to 49 do
  begin
    Assert.IsTrue(LConn1.Execute('INSERT INTO CUSTOMER (NAME) VALUES (''HUE EXECUTE'')', []) > 0, 'Should be greater than 0');    
  end;
  LQryCheck := LConn2.GetQuery('SELECT COUNT(*) FROM CUSTOMER');
  LQryCheck.Open;
  Assert.AreEqual(50, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
end;

procedure TMiniRESTSQLTest.TestJSON;
var
  LConn1: IMiniRESTSQLConnection;
  LQry: IMiniRESTSQLQuery;
begin
  LConn1 := FConnectionFactory.GetConnection;
  LQry := LConn1.GetQuery('select ''BOB'' as NAME, 17 as AGE from rdb$database');
  LQry.Open;
  Assert.AreEqual('{"NAME":"BOB","AGE":17}', LQry.ToJSON);
end;

procedure TMiniRESTSQLTest.TestJSON2;
var
  LConn1: IMiniRESTSQLConnection;
  LQry: IMiniRESTSQLQuery;
begin
  LConn1 := FConnectionFactory.GetConnection;
  LQry := LConn1.GetQuery;
  LQry.SQL := 'SELECT * FROM CUSTOMER WHERE 1=0';
  LQry.Open;
  LQry.DataSet.Append;
  LQry.DataSet.FieldByName('ID').AsInteger := 1;
  LQry.DataSet.FieldByName('NAME').AsString := 'BOB';
  LQry.DataSet.Post;

  LQry.DataSet.Append;
  LQry.DataSet.FieldByName('ID').AsInteger := 2;
  LQry.DataSet.FieldByName('NAME').AsString := 'MARIA';
  LQry.DataSet.Post;
  
  Assert.AreEqual('[{"ID":1,"NAME":"BOB"},{"ID":2,"NAME":"MARIA"}]', LQry.ToJSON);
end;

procedure TMiniRESTSQLTest.TestTransaction;
var
  LConn1, LConn2: IMiniRESTSQLConnection;
  LQry, LQryID, LQryCheck: IMiniRESTSQLQuery;
  LId: Integer;
  I: Integer;
begin
  LConn1 := FConnectionFactory.GetConnection;
  LConn2 := FConnectionFactory.GetConnection;
  LQry := LConn1.GetQuery;
  LQryID := LConn1.GetQuery;
  LQry.SQL := 'SELECT * FROM CUSTOMER WHERE 1=0';
  LQry.Open;  
  for I := 0 to 99 do
  begin
    LQryID.Close;
    LQryID.SQL := 'select gen_id(gen_customer_id, 1) from rdb$database';
    LQryID.Open;
    LId := LQryID.DataSet.FieldByName('GEN_ID').AsInteger;      
    LQry.DataSet.Append;
    LQry.DataSet.FieldByName('ID').AsInteger := LId;
    LQry.DataSet.FieldByName('NAME').AsString := 'HUE';
    LQry.DataSet.Post;    
  end;
  LConn1.StartTransaction;
  LQry.ApplyUpdates(0);
  LConn1.Commit;
  LQryCheck := LConn2.GetQuery('SELECT COUNT(*) FROM CUSTOMER');
  LQryCheck.Open;
  Assert.AreEqual(100, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);  
end;

procedure TMiniRESTSQLTest.TestTransaction2;
var
  LConn1, LConn2: IMiniRESTSQLConnection;
  LQry, LQryID, LQryCheck: IMiniRESTSQLQuery;
  LId: Integer;
  I: Integer;
begin
  LConn1 := FConnectionFactory.GetConnection;
  LConn2 := FConnectionFactory.GetConnection;
  LQry := LConn1.GetQuery;
  LQryID := LConn1.GetQuery;
  LQry.SQL := 'SELECT * FROM CUSTOMER WHERE 1=0';
  LQry.Open;  
  for I := 0 to 99 do
  begin
    LQryID.Close;
    LQryID.SQL := 'select gen_id(gen_customer_id, 1) from rdb$database';
    LQryID.Open;
    LId := LQryID.DataSet.FieldByName('GEN_ID').AsInteger;      
    LQry.DataSet.Append;
    LQry.DataSet.FieldByName('ID').AsInteger := LId;
    LQry.DataSet.FieldByName('NAME').AsString := 'HUE';
    LQry.DataSet.Post;    
  end;
  LConn1.StartTransaction;
  LQry.ApplyUpdates(0);
  LConn1.Rollback;
  LQryCheck := LConn2.GetQuery('SELECT COUNT(*) FROM CUSTOMER');
  LQryCheck.Open;
  Assert.AreEqual(0, LQryCheck.DataSet.FieldByName('COUNT').AsInteger); 
end;

end.
