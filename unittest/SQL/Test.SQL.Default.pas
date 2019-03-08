unit Test.SQL.Default;

interface
uses
  {$IFNDEF FPC}Windows, DUnitX.TestFramework {$ELSE} TestFramework{$ENDIF}, Classes,
    SysUtils, MiniREST.SQL.Intf, MiniREST.SQL.Common;

type
  TLogMessageProc = procedure (const AMessage: string) of object;
  TMiniRESTSQLTest = class({$IFNDEF FPC}TObject{$ELSE}TTestCase{$IFEND})
  protected
    FConnectionFactory: IMiniRESTSQLConnectionFactory;
    function GetConnectionFactory: IMiniRESTSQLConnectionFactory; virtual; abstract;
    procedure LogMessage(const AMessage: string); virtual;
    function GetSequenceValue(const ASequenceName: string): Integer;
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
    procedure TestInsert2;  
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestInsert3;
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
    procedure TestTransaction;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestTransaction2;
(*     {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestJSON;    
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestJSON2;     *)  
  end;

  TThreadTesteInsert2 = class(TThread)
  private
    FID: Integer;
    FFactory: IMiniRESTSQLConnectionFactory;
    FLogMessageProc: TLogMessageProc;
    procedure LogMessage(const AMessage: string);
  protected
    procedure Execute; override;
  public
    constructor Create(const AID: Integer; ACreateSuspended: Boolean; AFactory: IMiniRESTSQLConnectionFactory; ALogMessageProc: TLogMessageProc);
  end;

var
  gLogHabilitado: Boolean;
implementation

var
  gContatorTesteInsert2: Integer;

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
  CheckEquals(100, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
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
  LRowsAffected, I: Integer;
begin  
  LConn1 := FConnectionFactory.GetConnection;
  LConn2 := FConnectionFactory.GetConnection;
  for I := 0 to 49 do
  begin
    LRowsAffected := LConn1.Execute('INSERT INTO CUSTOMER (NAME) VALUES (''HUE EXECUTE'')', []);
    {$IFNDEF FPC}
    Assert.IsTrue(LRowsAffected > 0, 'Should be greater than 0');
    {$ELSE}
    CheckTrue(LRowsAffected > 0, 'Should be greater than 0');
    {$IFEND}
  end;
  LQryCheck := LConn2.GetQuery('SELECT COUNT(*) FROM CUSTOMER');
  LQryCheck.Open;
  {$IFNDEF FPC}
  Assert.AreEqual(50, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
  {$ELSE}
  CheckEquals(50, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestExecute2;
var
  LConn1, LConn2: IMiniRESTSQLConnection;
  LQryCheck: IMiniRESTSQLQuery;
  LRowsAffected, I: Integer;
  LParamName: IMiniRESTSQLParam;
begin
  LConn1 := FConnectionFactory.GetConnection;
  LConn2 := FConnectionFactory.GetConnection;
  for I := 0 to 49 do
  begin
    LParamName := TMiniRESTSQLParam.Create;
    LParamName.SetParamName('NAME');
    LParamName.AsString := 'NAME ' + IntToStr(I);
    LRowsAffected := LConn1.Execute('INSERT INTO CUSTOMER (NAME) VALUES (:NAME)', [LParamName]);
    {$IFNDEF FPC}
    Assert.IsTrue(LRowsAffected > 0, 'Should be greater than 0');
    {$ELSE}
    CheckTrue(LRowsAffected > 0, 'Should be greater than 0');
    {$IFEND}
  end;
  LQryCheck := LConn2.GetQuery('SELECT COUNT(*) FROM CUSTOMER');
  LQryCheck.Open;
  {$IFNDEF FPC}
  Assert.AreEqual(50, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
  {$ELSE}
  CheckEquals(50, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
  {$IFEND}
  LQryCheck := LConn2.GetQuery('SELECT * FROM CUSTOMER');
  LQryCheck.Open;
  {$IFNDEF FPC}
  Assert.IsTrue(LQryCheck.DataSet.FieldByName('ID').AsInteger > 0, 'Should be greater than 0');
  Assert.IsTrue(Trim(LQryCheck.DataSet.FieldByName('NAME').AsString) <> '', 'Should be not empty');
  {$ELSE}
  CheckTrue(LQryCheck.DataSet.FieldByName('ID').AsInteger > 0, 'Should be greater than 0');
  CheckTrue(Trim(LQryCheck.DataSet.FieldByName('NAME').AsString) <> '', 'Should be not empty');
  {$IFEND}
end;

(* procedure TMiniRESTSQLTest.TestJSON;
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
end; *)

(* procedure TMiniRESTSQLTest.TestJSON2;
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
  
  {$IFNDEF FPC}
  Assert.AreEqual('[{"ID":1,"NAME":"BOB"},{"ID":2,"NAME":"MARIA"}]', LQry.ToJSON);
  {$ELSE}
  Fail('Implement');
  {$IFEND}
end; *)

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
  CheckEquals(100, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
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
  CheckEquals(0, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TearDownFixture;
begin
  //FConnectionFactory := nil;
end;

procedure TMiniRESTSQLTest.TestInsert2;
var
  LConn: IMiniRESTSQLConnection;
  LQryCheck: IMiniRESTSQLQuery;  
  LCount, I: Integer;
  LThread: TThreadTesteInsert2;
begin
  gLogHabilitado := True;
  LCount := 100;
  gContatorTesteInsert2 := 0;
  LConn := FConnectionFactory.GetConnection;  
  for I := 1 to LCount do
  begin
    TThreadTesteInsert2.Create(I, False, FConnectionFactory, {$IFDEF FPC}@LogMessage{$ELSE}nil{$IFEND});
  end;
  while not (gContatorTesteInsert2 = LCount) do
    Sleep(1000);  
  LQryCheck := LConn.GetQuery('SELECT COUNT(*) FROM CUSTOMER');
  LQryCheck.Open;
  {$IFNDEF FPC}
  Assert.AreEqual(LCount, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
  {$ELSE}
  CheckEquals(LCount, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
  {$IFEND}
  gLogHabilitado := False;
end;

procedure TThreadTesteInsert2.Execute;
var
  LConn: IMiniRESTSQLConnection;
  LQry, LQryID: IMiniRESTSQLQuery;
  LId: Integer;
begin
  try    
    LConn := FFactory.GetConnection;    
    LQry := LConn.GetQuery;
    LQryID := LConn.GetQuery;
    
    LQryID.Close;
    LQryID.SQL := 'select gen_id(gen_customer_id, 1) from rdb$database';
    LQryID.Open;    
    LId := LQryID.DataSet.FieldByName('GEN_ID').AsInteger;
    LogMessage('CUSTOMER ID: ' + IntToStr(LId));
    LQryID.Close;
    LQry.SQL := 'SELECT * FROM CUSTOMER WHERE 1=0';
    LQry.Open;          
    LQry.DataSet.Insert;
    LQry.DataSet.FieldByName('ID').AsInteger := LId;
    LQry.DataSet.FieldByName('NAME').AsString := 'HUE';
    LQry.DataSet.Post;    
        
    LQry.ApplyUpdates(0);    
  finally
    InterLockedIncrement(gContatorTesteInsert2);    
  end;    
end;

constructor TThreadTesteInsert2.Create(const AID: Integer; ACreateSuspended: Boolean; AFactory: IMiniRESTSQLConnectionFactory; ALogMessageProc: TLogMessageProc);
begin
  inherited Create(ACreateSuspended);
  FFactory := AFactory;
  FreeOnTerminate := True;
  FLogMessageProc := ALogMessageProc;
  FID := AID;  
end;

procedure TMiniRESTSQLTest.LogMessage(const AMessage: string);
begin
  
end;

procedure TThreadTesteInsert2.LogMessage(const AMessage: string);
begin
  if Assigned(FLogMessageProc) then
    FLogMessageProc('THREAD ' + IntToStr(FID) + ' ' + AMessage);
end;

procedure TMiniRESTSQLTest.TestInsert3;
var
  LConn1, LConn2: IMiniRESTSQLConnection;
  LQry, LQryCheck: IMiniRESTSQLQuery;  
  I: Integer;
  LParamName: IMiniRESTSQLParam;
begin  
  LConn1 := FConnectionFactory.GetConnection;
  LConn2 := FConnectionFactory.GetConnection;
  LQry := LConn1.GetQuery('SELECT * FROM CUSTOMER WHERE 1=0');    
  LQry.Open;  
  for I := 1 to 5 do
  begin    
    LQry.DataSet.Append;
    LQry.DataSet.FieldByName('ID').AsInteger := GetSequenceValue('gen_customer_id');
    LQry.DataSet.FieldByName('NAME').AsString := 'HUE';
    LQry.DataSet.Post;    
  end;
  LQry.ApplyUpdates(0);
  LQryCheck := LConn2.GetQuery('SELECT COUNT(*) FROM CUSTOMER WHERE NAME = :NAME');
  LParamName := TMiniRESTSQLParam.Create;
  LParamName.SetParamName('NAME');
  LParamName.AsString := 'HUE';
  LQryCheck.AddParam(LParamName);
  LQryCheck.Open;
  {$IFNDEF FPC}
  Assert.AreEqual(5, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
  {$ELSE}
  CheckEquals(5, LQryCheck.DataSet.FieldByName('COUNT').AsInteger);
  {$IFEND}
end;

function TMiniRESTSQLTest.GetSequenceValue(const ASequenceName: string): Integer;
var
  LConn: IMiniRESTSQLConnection;
  LQryID: IMiniRESTSQLQuery;
begin
  LConn := FConnectionFactory.GetConnection;
  LQryID := LConn.GetQuery;
  LQryID.Close;
  LQryID.SQL := 'select gen_id(' + ASequenceName + ', 1) from rdb$database';
  LQryID.Open;
  Result := LQryID.DataSet.FieldByName('GEN_ID').AsInteger;
end;

end.
