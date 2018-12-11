unit Test.SQL.Default;

interface
uses
  {$IFNDEF FPC}DUnitX.TestFramework {$ELSE} TestFramework{$ENDIF}, Classes,
    SysUtils, MiniREST.SQL.Intf, MiniREST.SQL.Common;

type

  TMiniRESTSQLTest = class({$IFNDEF FPC}TObject{$ELSE}TTestCase{$IFEND})
  private
    FConnectionFactory: IMiniRESTSQLConnectionFactory;
  protected
    function GetConnectionFactory: IMiniRESTSQLConnectionFactory; virtual; abstract;
  public
    {$IFNDEF FPC}
    [SetupFixture]
    {$IFEND}
    procedure SetupFixture;
    {$IFNDEF FPC}
    [TearDownFixture]
    {$IFEND}
    procedure TearDownFixture;
    {$IFNDEF FPC}
    [Setup]
    {$IFEND}
    procedure Setup; {$IFDEF FPC}override;{$IFEND}
    {$IFNDEF FPC}
    [TearDown]
    {$IFEND}
    procedure TearDown; {$IFDEF FPC}override;{$IFEND}
  published
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestInsert;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestExecute;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
	procedure TestExecute2;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestJSON;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestJSON2;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestTransaction;
    {$IFNDEF FPC}
    [Test]
	{$IFEND}
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
  {$IFNDEF FPC}
  Assert.AreEqual(100, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
  {$ELSE}
  Fail('Implement');
  {$IFEND}
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
    {$IFNDEF FPC}
    Assert.IsTrue(LConn1.Execute('INSERT INTO CUSTOMER (NAME) VALUES (''HUE EXECUTE'')', []) > 0, 'Should be greater than 0');
    {$ELSE}
    Fail('Implement');
    {$IFEND}
  end;
  LQryCheck := LConn2.GetQuery('SELECT COUNT(*) FROM CUSTOMER');
  LQryCheck.Open;
  {$IFNDEF FPC}
  Assert.AreEqual(50, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
  {$ELSE}
  Fail('Implement');
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestExecute2;
var
  LConn1, LConn2: IMiniRESTSQLConnection;
  LQryCheck: IMiniRESTSQLQuery;
  I: Integer;
  LParamName: IMiniRESTSQLParam;
begin
  LConn1 := FConnectionFactory.GetConnection;
  LConn2 := FConnectionFactory.GetConnection;
  for I := 0 to 49 do
  begin
    LParamName := TMiniRESTSQLParam.Create;
    LParamName.SetParamName('NAME');
    LParamName.AsString := 'NAME ' + IntToStr(I);
    Assert.IsTrue(LConn1.Execute('INSERT INTO CUSTOMER (NAME) VALUES (:NAME)', [LParamName]) > 0, 'Should be greater than 0');
  end;
  LQryCheck := LConn2.GetQuery('SELECT COUNT(*) FROM CUSTOMER');
  LQryCheck.Open;
  Assert.AreEqual(50, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
  LQryCheck := LConn2.GetQuery('SELECT * FROM CUSTOMER');
  LQryCheck.Open;
  Assert.IsTrue(LQryCheck.DataSet.FieldByName('ID').AsInteger > 0, 'Should be greater than 0');
  Assert.IsTrue(Trim(LQryCheck.DataSet.FieldByName('NAME').AsString) <> '', 'Should be not empty');
end;

procedure TMiniRESTSQLTest.TestJSON;
var
  LConn1: IMiniRESTSQLConnection;
  LQry: IMiniRESTSQLQuery;
begin
  LConn1 := FConnectionFactory.GetConnection;
  LQry := LConn1.GetQuery('select ''BOB'' as NAME, 17 as AGE from rdb$database');
  LQry.Open;
  {$IFNDEF FPC}
  Assert.AreEqual('{"NAME":"BOB","AGE":17}', LQry.ToJSON);
  {$ELSE}
  Fail('Implement');
  {$IFEND}
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
  {$ELSE}
  Fail('Implement');
  {$IFEND}
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
  {$IFNDEF FPC}
  Assert.AreEqual(100, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
  {$ELSE}
  Fail('Implement');
  {$IFEND}
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
  {$IFNDEF FPC}
  Assert.AreEqual(0, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
  {$ELSE}
  Fail('Implement');
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TearDownFixture;
begin
  FConnectionFactory := nil;
end;

end.
