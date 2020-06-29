unit Test.SQL.Default;

interface
uses
  {$IFNDEF FPC}Windows, DUnitX.TestFramework {$ELSE} TestFramework{$ENDIF}, Classes,
    SysUtils, MiniREST.SQL.Intf, MiniREST.SQL.Common;

type
  TLogMessageProc = procedure (const AMessage: string) of object;
  TMiniRESTSQLTest = class({$IFNDEF FPC}TObject{$ELSE}TTestCase{$IFEND})
  private
    FServerHostName: string;
    FServerPort: Integer;
    procedure MethodThatRaiseException;
    procedure MethodThatRaiseExceptionOnOpenQuery;
    procedure OnOpenQueryExceptionRaiseException(AConnection: IMiniRESTSQLConnection; AQuery: IMiniRESTSQLQuery; AException: Exception; var ARaiseException: Boolean);
    procedure OnOpenQueryExceptionNoRaiseException(AConnection: IMiniRESTSQLConnection; AQuery: IMiniRESTSQLQuery; AException: Exception; var ARaiseException: Boolean);
  protected
    FConnectionCount: Integer;
    FConnectionFactory: IMiniRESTSQLConnectionFactory;
    FConnectionPoolEvents: TStringList;
    function GetConnectionFactory: IMiniRESTSQLConnectionFactory; virtual; abstract; overload;
    function GetConnectionFactory(AParams: IMiniRESTSQLConnectionFactoryParams): IMiniRESTSQLConnectionFactory; virtual; abstract; overload;
    function GetConnectionFactoryParams: IMiniRESTSQLConnectionFactoryParams; virtual; abstract;
    procedure LogMessage(const AMessage: string); virtual;
    function GetSequenceValue(const ASequenceName: string): Integer;
    procedure LogConnectionPoolEvent(const AMessage: string);
    function GetServerHostName: string;
    function GetServerPort: Integer;
    function GetDatabaseType: TMiniRESTSQLDatabaseType; virtual; abstract;
    procedure TestCharSetFirebird;
    procedure TestCharSetPostgreSQL;
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
    procedure TestInsert4;    
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
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestInsert5;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestCheckInTransaction;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestClearParamsOnSetSQL;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestConnectionCount;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestQueueCount;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestQueueCount2;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestConnectionPoolEventLogger;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestGetSingletonConnection;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestConnectionIsValid;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestConnectionIsNotValid;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestFailWithInvalidServerHostName;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestFailWithInvalidServerPort;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestSuccessWithDefaultServerPort;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestParamSize;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestCharSet;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestGetDatabaseTypeFromConnection;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestGetDatabaseTypeFromConnectionFactory;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestOnOpenQueryException1;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestOnOpenQueryException2;
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
    FTestCase: TMiniRESTSQLTest;
    procedure LogMessage(const AMessage: string);
  protected
    procedure Execute; override;
  public
    constructor Create(const AID: Integer; ACreateSuspended: Boolean; AFactory: IMiniRESTSQLConnectionFactory; ALogMessageProc: TLogMessageProc;
      ATestCase: TMiniRESTSQLTest);
  end;

  { TConnectionFactoryEventLogger }

  TConnectionFactoryEventLogger = class(TInterfacedObject, IMiniRESTSQLConnectionFactoryEventLogger)
  private
    FList: TStringList;
  public
    constructor Create(AList: TStringList);
    destructor Destroy; override;
    procedure LogPoolEvent(const AMessage: string);
  end;

var
  gLogHabilitado: Boolean;
implementation

var
  gContatorTesteInsert2: Integer;

procedure TMiniRESTSQLTest.SetupFixture;
begin
  FConnectionCount := 3;
  FConnectionFactory := GetConnectionFactory;
end;

procedure TMiniRESTSQLTest.TearDown;
var
  LConnection: IMiniRESTSQLConnection;  
begin
(*   LConnection := FConnectionFactory.GetConnection;  
  LConnection.Execute('DELETE FROM CUSTOMER', []);  
  *)
  FConnectionFactory := nil;
  FConnectionPoolEvents.Free;
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
  if GetDatabaseType = dbtFirebird then
    LQryID := LConn1.GetQuery;
  LQry.SQL := 'SELECT * FROM CUSTOMER WHERE 1=0';
  LQry.Open;
  for I := 0 to 99 do
  begin
    LQry.DataSet.Append;
    if GetDatabaseType = dbtFirebird then
    begin
      LQryID.Close;
      LQryID.SQL := 'select gen_id(gen_customer_id, 1) from rdb$database';
      LQryID.Open;
      LId := LQryID.DataSet.FieldByName('GEN_ID').AsInteger;            
      LQry.DataSet.FieldByName('ID').AsInteger := LId;
    end;
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
  FServerHostName := 'LOCALHOST';
  if GetDatabaseType = dbtFirebird then
    FServerPort := 3050
  else
  if GetDatabaseType = dbtPostgreSQL then
    FServerPort := 5432;
  {$IFDEF FPC}
  FConnectionFactory := GetConnectionFactory;
  {$IFEND}
  FConnectionPoolEvents := TStringList.Create;
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
    LQry.DataSet.Append;
    if GetDatabaseType = dbtFirebird then
    begin
      LQryID.Close;
      LQryID.SQL := 'select gen_id(gen_customer_id, 1) from rdb$database';
      LQryID.Open;
      LId := LQryID.DataSet.FieldByName('GEN_ID').AsInteger;      
      LQry.DataSet.FieldByName('ID').AsInteger := LId;      
    end;
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
    LQry.DataSet.Append;
    if GetDatabaseType = dbtFirebird then
    begin
      LQryID.Close;
      LQryID.SQL := 'select gen_id(gen_customer_id, 1) from rdb$database';
      LQryID.Open;
      LId := LQryID.DataSet.FieldByName('GEN_ID').AsInteger;      
      LQry.DataSet.FieldByName('ID').AsInteger := LId;      
    end;
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
begin
  gLogHabilitado := True;
  LCount := 100;
  gContatorTesteInsert2 := 0;
  LConn := FConnectionFactory.GetConnection;  
  for I := 1 to LCount do
  begin
    TThreadTesteInsert2.Create(I, False, FConnectionFactory, {$IFDEF FPC}@LogMessage{$ELSE}nil{$IFEND}, Self);
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
    Sleep(Random(10) * 100);
    LConn := FFactory.GetConnection;    
    LQry := LConn.GetQuery;
    if FTestCase.GetDatabaseType = dbtFirebird then
    begin
      LQryID := LConn.GetQuery;    
      LQryID.Close;
      LQryID.SQL := 'select gen_id(gen_customer_id, 1) from rdb$database';
      LQryID.Open;    
      LId := LQryID.DataSet.FieldByName('GEN_ID').AsInteger;
      LogMessage('CUSTOMER ID: ' + IntToStr(LId));
      LQryID.Close;      
    end;
    LQry.SQL := 'SELECT * FROM CUSTOMER WHERE 1=0';
    LQry.Open;          
    LQry.DataSet.Insert;
    if FTestCase.GetDatabaseType = dbtFirebird then
      LQry.DataSet.FieldByName('ID').AsInteger := LId;
    LQry.DataSet.FieldByName('NAME').AsString := 'HUE';
    LQry.DataSet.Post;    
        
    LQry.ApplyUpdates(0);    
  finally
    InterLockedIncrement(gContatorTesteInsert2);    
  end;    
end;

constructor TThreadTesteInsert2.Create(const AID: Integer; ACreateSuspended: Boolean; AFactory: IMiniRESTSQLConnectionFactory; ALogMessageProc: TLogMessageProc;
  ATestCase: TMiniRESTSQLTest);
begin
  inherited Create(ACreateSuspended);
  FFactory := AFactory;
  FreeOnTerminate := True;
  FLogMessageProc := ALogMessageProc;
  FID := AID;
  FTestCase := ATestCase;
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
    if GetDatabaseType = dbtFirebird then
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

procedure TMiniRESTSQLTest.TestInsert4;
var
  LConn1, LConn2: IMiniRESTSQLConnection;
  LQry, LQryCheck: IMiniRESTSQLQuery;
begin  
  LConn1 := FConnectionFactory.GetConnection;
  LConn2 := FConnectionFactory.GetConnection;
  LQry := LConn1.GetQuery;
  LQry.SQL := 'SELECT * FROM CUSTOMER WHERE 1=0';
  LQry.Open;

  LQry.DataSet.Append;
  LQry.DataSet.FieldByName('ID').AsInteger := 456;
  LQry.DataSet.FieldByName('NAME').AsString := 'HUE';
  LQry.DataSet.Post;    
  LQry.ApplyUpdates(0);

  LQryCheck := LConn2.GetQuery('SELECT * FROM CUSTOMER WHERE ID = :ID');
  LQryCheck.ParamByName('ID').AsInteger := 456;
  LQryCheck.Open;
  {$IFNDEF FPC}
  Assert.AreEqual(1, LQryCheck.DataSet.RecordCount);
  {$ELSE}
  CheckEquals(1, LQryCheck.DataSet.RecordCount);
  {$IFEND}  
end;

procedure TMiniRESTSQLTest.TestInsert5;
var
  LConn1: IMiniRESTSQLConnection;
  LConn2: IMiniRESTSQLConnection;
  LQry, LQryCheck: IMiniRESTSQLQuery;
  LTotal: Integer;
begin  
  LConn1 := FConnectionFactory.GetConnection;
  LConn2 := FConnectionFactory.GetConnection;
  LQryCheck := LConn2.GetQuery('SELECT COUNT(*) AS TOTAL FROM CUSTOMER');
  LQryCheck.Open;
  LTotal := LQryCheck.DataSet.FieldByName('TOTAL').AsInteger;
  LQry := LConn1.GetQuery;
  LQry.SQL := 'SELECT * FROM CUSTOMER WHERE 1=0';
  LQry.Open;

  LQry.DataSet.Append;
  LQry.DataSet.FieldByName('ID').AsInteger := 456;
  LQry.DataSet.FieldByName('NAME').AsString := 'HUE';
  LQry.DataSet.Post;    
  LQry.ApplyUpdates(0);

  LQryCheck.Close;  
  LQryCheck.Open;
  {$IFNDEF FPC}
  Assert.AreEqual(LTotal + 1, LQryCheck.DataSet.FieldByName('TOTAL').AsInteger);
  {$ELSE}
  CheckEquals(LTotal + 1, LQryCheck.DataSet.FieldByName('TOTAL').AsInteger);
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestCheckInTransaction;
var
  LConn1: IMiniRESTSQLConnection;
begin
  LConn1 := FConnectionFactory.GetConnection;  
  {$IFNDEF FPC}
  Assert.IsFalse(LConn1.InTransaction, 'Est� em transa��o');
  {$ELSE}
  CheckFalse(LConn1.InTransaction, 'Est� em transa��o');
  {$IFEND}
  LConn1.StartTransaction;

  {$IFNDEF FPC}
  Assert.IsTrue(LConn1.InTransaction, 'N�o est� em transa��o');
  {$ELSE}
  CheckTrue(LConn1.InTransaction, 'N�o est� em transa��o');
  {$IFEND}
  LConn1.Commit;

  {$IFNDEF FPC}
  Assert.IsFalse(LConn1.InTransaction, 'Est� em transa��o');
  {$ELSE}
  CheckFalse(LConn1.InTransaction, 'Est� em transa��o');
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestClearParamsOnSetSQL;
var
  LConn1: IMiniRESTSQLConnection;
  LQry: IMiniRESTSQLQuery;
  LTotal: Integer;
begin  
  LConn1 := FConnectionFactory.GetConnection;
  LQry := LConn1.GetQuery;
  LQry.SQL := 'SELECT * FROM CUSTOMER WHERE NAME = :NAME';
  LQry.ParamByName('NAME').AsString := 'HUE';
  LQry.Open;

  // O teste � assim para verificar se n�o vai dar erro quando n�o existir o par�metro,
  // passado anteriormente
  LQry.Close;
  LQry.SQL := 'SELECT * FROM CUSTOMER';
  LQry.Open;
  {$IFNDEF FPC}
  Assert.AreTrue(LQry.DataSet.Active);
  {$ELSE}
  CheckTrue(LQry.DataSet.Active);
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestConnectionCount;
begin
  {$IFNDEF FPC}
  Assert.AreEqual(FConnectionCount, FConnectionFactory.ConnectionsCount);
  {$ELSE}
  CheckEquals(FConnectionCount, FConnectionFactory.ConnectionsCount);
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestQueueCount;
var
  LConn1: IMiniRESTSQLConnection;
  LConn2: IMiniRESTSQLConnection;
begin
  LConn1 := FConnectionFactory.GetConnection;
  LConn2 := FConnectionFactory.GetConnection;
  {$IFNDEF FPC}
  Assert.AreEqual(FConnectionCount - 2, FConnectionFactory.QueueCount);
  {$ELSE}
  CheckEquals(FConnectionCount - 2, FConnectionFactory.QueueCount);
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestQueueCount2;
var
  LConn1: IMiniRESTSQLConnection;
  LConn2: IMiniRESTSQLConnection;
begin
  LConn1 := FConnectionFactory.GetConnection;
  LConn2 := FConnectionFactory.GetConnection;
  LConn2 := nil;
  {$IFNDEF FPC}
  Assert.AreEqual(FConnectionCount - 1, FConnectionFactory.QueueCount);
  {$ELSE}
  CheckEquals(FConnectionCount - 1, FConnectionFactory.QueueCount);
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestConnectionPoolEventLogger;
var
  LConnectionFactory: IMiniRESTSQLConnectionFactory;
  LParams: IMiniRESTSQLConnectionFactoryParams;
  LConn1: IMiniRESTSQLConnection;
  LConn2: IMiniRESTSQLConnection;
  LLogger: IMiniRESTSQLConnectionFactoryEventLogger;
begin
  LParams := GetConnectionFactoryParams;
  LLogger := TConnectionFactoryEventLogger.Create(FConnectionPoolEvents);
  LParams.SetConnectionFactoryEventLogger(LLogger);
  LConnectionFactory := GetConnectionFactory(LParams);
  LConn1 := LConnectionFactory.GetConnection('teste1');
  LConn2 := LConnectionFactory.GetConnection('teste2');
  LConn1 := nil;
  LConn2 := nil;
  {$IFNDEF FPC}
  Assert.AreEqual(4, FConnectionPoolEvents.Count);
  {$ELSE}
  CheckEquals(4, FConnectionPoolEvents.Count);
  {$IFEND}
  FConnectionPoolEvents.SaveToFile(ExtractFilePath(ParamStr(0)) + 'logPool.txt');
end;

procedure TMiniRESTSQLTest.LogConnectionPoolEvent(const AMessage: string);
begin
  FConnectionPoolEvents.Add(AMessage);  
end;

procedure TConnectionFactoryEventLogger.LogPoolEvent(const AMessage: string);
begin
  FList.Add(AMessage);
end;

constructor TConnectionFactoryEventLogger.Create(AList: TStringList);
begin
  inherited Create;
  FList := AList;  
end;

destructor TConnectionFactoryEventLogger.Destroy;
begin
  FList := nil;
  inherited Destroy;
end;

procedure TMiniRESTSQLTest.TestGetSingletonConnection;
var
  LConn1: IMiniRESTSQLConnection;
  LConn2: IMiniRESTSQLConnection;
begin
  LConn1 := FConnectionFactory.GetSingletonConnection;
  LConn2 := FConnectionFactory.GetSingletonConnection;
  {$IFNDEF FPC}
  Assert.IsTrue(LConn1 <> nil, 'LCon1 est� nil');
  Assert.IsTrue(LConn2 <> nil, 'LCon2 est� nil');
  Assert.IsTrue(LConn1 = LConn2, 'LCon1 est� diferente de LCon2');
  {$ELSE}
  CheckTrue(LConn1 <> nil, 'LCon1 est� nil');
  CheckTrue(LConn2 <> nil, 'LCon2 est� nil');
  CheckTrue(LConn1 = LConn2, 'LCon1 est� diferente de LCon2');
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestConnectionIsValid;
var
  LConn1: IMiniRESTSQLConnection;
begin
  LConn1 := FConnectionFactory.GetConnection;
  {$IFNDEF FPC}
  Assert.IsTrue(LConn1.IsValid, 'LCon1 n�o est� v�lida.');
  {$ELSE}
  CheckTrue(LConn1.IsValid, 'LCon1 n�o est� v�lida.');
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestConnectionIsNotValid;
var
  LConn1: IMiniRESTSQLConnection;
begin
  LConn1 := FConnectionFactory.GetConnection;
  {$IFNDEF FPC}
  Assert.IsTrue(LConn1.IsValid, 'LCon1 n�o est� v�lida.');
  {$ELSE}
  CheckTrue(LConn1.IsValid, 'LCon1 n�o est� v�lida.');
  {$IFEND}
  FConnectionFactory.InvalidateConnections;
  {$IFNDEF FPC}
  Assert.IsFalse(LConn1.IsValid, 'LCon1 est� v�lida.');
  {$ELSE}
  CheckFalse(LConn1.IsValid, 'LCon1 est� v�lida.');
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestFailWithInvalidServerHostName;
begin
  FServerHostName := 'HUE90';
  FConnectionFactory := GetConnectionFactory(GetConnectionFactoryParams);
  CheckException(@MethodThatRaiseException, Exception)
end;

function TMiniRESTSQLTest.GetServerHostName: string;
begin
  Result := FServerHostName;  
end;

procedure TMiniRESTSQLTest.MethodThatRaiseException;
var
  LConn: IMiniRESTSQLConnection;
  LQry: IMiniRESTSQLQuery;
begin
  LConn := FConnectionFactory.GetConnection;
  LQry := LConn.GetQuery('SELECT COUNT(*) AS TOTAL FROM CUSTOMER');
  LQry.Open;  
end;

procedure TMiniRESTSQLTest.TestFailWithInvalidServerPort;
begin
  FServerPort := 3099;
  FConnectionFactory := GetConnectionFactory(GetConnectionFactoryParams);
  CheckException(@MethodThatRaiseException, Exception);
end;

function TMiniRESTSQLTest.GetServerPort: Integer;
begin
  Result := FServerPort;  
end;

procedure TMiniRESTSQLTest.TestSuccessWithDefaultServerPort;
var
  LConn1: IMiniRESTSQLConnection;
  LQry: IMiniRESTSQLQuery;
  LTotal: Integer;
begin
  FServerPort := 0;
  FConnectionFactory := GetConnectionFactory(GetConnectionFactoryParams);
  LConn1 := FConnectionFactory.GetConnection;
  LQry := LConn1.GetQuery;
  LQry.SQL := 'SELECT * FROM CUSTOMER WHERE NAME = :NAME';
  LQry.ParamByName('NAME').AsString := 'HUE';
  LQry.Open;

  {$IFNDEF FPC}
  Assert.AreTrue(LQry.DataSet.Active);
  {$ELSE}
  CheckTrue(LQry.DataSet.Active);
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestParamSize;
var
  LConn1: IMiniRESTSQLConnection;
  LQry, LQryID: IMiniRESTSQLQuery;
  LId: Integer;
  I: Integer;
begin
  {
    LENGTH OF PHONE IS 15, THEN THE PARAM SIZE SHOULD BE 15,
    IF THE VALUE SIZE OF PARAM ARE BIGGER THAN SIZE,
    THE VALUE WILL TRUNCATED
  }  
  LConn1 := FConnectionFactory.GetConnection;
  LQry := LConn1.GetQuery;
  LQryID := LConn1.GetQuery;
  LQry.SQL := 'SELECT * FROM CUSTOMER WHERE 1=0';
  LQry.Open;
  for I := 0 to 5 do
  begin
    LQry.DataSet.Append;
    if GetDatabaseType = dbtFirebird then
    begin
      LQryID.Close;
      LQryID.SQL := 'select gen_id(gen_customer_id, 1) from rdb$database';
      LQryID.Open;
      LId := LQryID.DataSet.FieldByName('GEN_ID').AsInteger;      
      LQry.DataSet.FieldByName('ID').AsInteger := LId;
    end;
    LQry.DataSet.FieldByName('NAME').AsString := 'HUE';
    LQry.DataSet.FieldByName('PHONE').AsString := '999999999999999';
    LQry.DataSet.Post;    
  end;
  LQry.ApplyUpdates(0);
  LQry := LConn1.GetQuery;
  LQry.SQL := 'SELECT * FROM CUSTOMER WHERE PHONE = :PHONE';
  LQry.ParamByName('PHONE').SetParamSize(15);
  LQry.ParamByName('PHONE').AsString := 'BIG BIG BIG BIG 999999999999999 BIG BIG';
  LQry.Open;
  {$IFNDEF FPC}
  Assert.AreTrue(LQry.DataSet.Active);
  {$ELSE}
  CheckTrue(LQry.DataSet.Active);
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestCharSetFirebird;
var
  LConn1: IMiniRESTSQLConnection;
  LQry: IMiniRESTSQLQuery;  
  LConnectionFactory: IMiniRESTSQLConnectionFactory;
  LConnectionFactoryParams: IMiniRESTSQLConnectionFactoryParams;
  LExpectedFilePath: string;
begin
  LConnectionFactoryParams := GetConnectionFactoryParams;  
  LConnectionFactoryParams.SetCharSet('UTF8');
  LConnectionFactory := GetConnectionFactory(LConnectionFactoryParams);
  LExpectedFilePath := ParamStr(0);
  LConn1 := LConnectionFactory.GetConnection;
  LQry := LConn1.GetQuery;
  LQry.SQL := 'SELECT * FROM MON$ATTACHMENTS WHERE MON$CHARACTER_SET_ID = :CHARSET_ID ' +
  ' AND MON$REMOTE_PROCESS = :REMOTE_PROCESS';
  LQry.ParamByName('CHARSET_ID').AsInteger := 4; // UTF8 FIREBIRD 2.5;
  LQry.ParamByName('REMOTE_PROCESS').AsString := LExpectedFilePath;  
  LQry.Open;    
  {$IFNDEF FPC}
  Assert.AreTrue(LQry.DataSet.RecordCount = 1);
  {$ELSE}
  CheckTrue(LQry.DataSet.RecordCount = 1);
  {$IFEND}

  LConnectionFactoryParams.SetCharSet('WIN1252');
  LConnectionFactory := GetConnectionFactory(LConnectionFactoryParams);  
  LConn1 := LConnectionFactory.GetConnection;
  LQry := LConn1.GetQuery;
  LQry.SQL := 'SELECT * FROM MON$ATTACHMENTS WHERE MON$CHARACTER_SET_ID = :CHARSET_ID ' +
  ' AND MON$REMOTE_PROCESS = :REMOTE_PROCESS';
  LQry.ParamByName('CHARSET_ID').AsInteger := 53; // WIN1252 FIREBIRD 2.5;
  LQry.ParamByName('REMOTE_PROCESS').AsString := LExpectedFilePath;  
  LQry.Open;    
  {$IFNDEF FPC}
  Assert.AreTrue(LQry.DataSet.RecordCount = 1);
  {$ELSE}
  CheckTrue(LQry.DataSet.RecordCount = 1);
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestCharSet;
begin
  if GetDatabaseType = dbtFirebird then
    TestCharSetFirebird
  else
  if GetDatabaseType = dbtPostgreSQL then
    CheckTrue(True); {TODO: Verificar charset postgresql}
    //TestCharSetPostgreSQL;
end;

procedure TMiniRESTSQLTest.TestCharSetPostgreSQL;
var
  LConn1: IMiniRESTSQLConnection;
  LQry: IMiniRESTSQLQuery;  
  LConnectionFactory: IMiniRESTSQLConnectionFactory;
  LConnectionFactoryParams: IMiniRESTSQLConnectionFactoryParams;
  LExpectedFilePath: string;
begin
  LConnectionFactoryParams := GetConnectionFactoryParams;  
  LConnectionFactoryParams.SetCharSet('UTF8');
  LConnectionFactory := GetConnectionFactory(LConnectionFactoryParams);
  LExpectedFilePath := ParamStr(0);
  LConn1 := LConnectionFactory.GetConnection;
  LQry := LConn1.GetQuery;
  LQry.SQL := 'SHOW CLIENT_ENCODING';  
  LQry.Open;    
  {$IFNDEF FPC}
  Assert.AreTrue(LQry.DataSet.FieldByName('CLIENT_ENCODING').AsString = 'UTF8');
  {$ELSE}
  CheckTrue(LQry.DataSet.FieldByName('CLIENT_ENCODING').AsString = 'UTF8');
  {$IFEND}

  LConnectionFactoryParams.SetCharSet('WIN1252');
  LConnectionFactory := GetConnectionFactory(LConnectionFactoryParams);  
  LConn1 := LConnectionFactory.GetConnection;
  LQry := LConn1.GetQuery;
  LQry.SQL := 'SHOW CLIENT_ENCODING';
  LQry.Open;    
  {$IFNDEF FPC}
  Assert.AreTrue(LQry.DataSet.FieldByName('CLIENT_ENCODING').AsString = 'WIN1252');
  {$ELSE}
  CheckEquals('WIN1252', LQry.DataSet.FieldByName('CLIENT_ENCODING').AsString);
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestGetDatabaseTypeFromConnection;
var
  LConn: IMiniRESTSQLConnection;
begin
  LConn := FConnectionFactory.GetConnection;
  {$IFNDEF FPC}
  Assert.AreTrue(LConn.GetDatabaseType = GetDatabaseType);
  {$ELSE}
  CheckTrue(LConn.GetDatabaseType = GetDatabaseType);
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestGetDatabaseTypeFromConnectionFactory;
begin
  {$IFNDEF FPC}
  Assert.AreTrue(FConnectionFactory.GetDatabaseType = GetDatabaseType);
  {$ELSE}
  CheckTrue(FConnectionFactory.GetDatabaseType = GetDatabaseType);
  {$IFEND}
end;

procedure TMiniRESTSQLTest.TestOnOpenQueryException1;
begin
  // Deve lançar exceção
  CheckException(@MethodThatRaiseExceptionOnOpenQuery, Exception);
end;

procedure TMiniRESTSQLTest.TestOnOpenQueryException2;
var
  LConnectionFactory: IMiniRESTSQLConnectionFactory;
  LConnectionFactoryParams: IMiniRESTSQLConnectionFactoryParams;
  LQry: IMiniRESTSQLQuery;
  LConn: IMiniRESTSQLConnection;
begin
  // Não deve lançar exceção
  LConnectionFactoryParams := GetConnectionFactoryParams;
  LConnectionFactoryParams.SetOnOpenQueryException(@OnOpenQueryExceptionNoRaiseException);
  LConnectionFactory := GetConnectionFactory(LConnectionFactoryParams);
  LConn := LConnectionFactory.GetConnection;
  LQry := LConn.GetQuery('HUE');
  LQry.Open;
  CheckTrue(True); // é assim mesmo, só pra ver se chegou aqui
end;

procedure TMiniRESTSQLTest.OnOpenQueryExceptionRaiseException(AConnection: IMiniRESTSQLConnection;
  AQuery: IMiniRESTSQLQuery; AException: Exception; var ARaiseException: Boolean);
begin
  CheckTrue(ARaiseException);
  CheckTrue(AConnection <> nil);
  CheckTrue(AQuery <> nil);
  CheckTrue(AException <> nil);
  ARaiseException := True;
end;

procedure TMiniRESTSQLTest.OnOpenQueryExceptionNoRaiseException(AConnection: IMiniRESTSQLConnection; AQuery: IMiniRESTSQLQuery; AException: Exception; var ARaiseException: Boolean);
begin
  CheckTrue(ARaiseException);
  CheckTrue(AConnection <> nil);
  CheckTrue(AQuery <> nil);
  CheckTrue(AException <> nil);
  ARaiseException := False;
end;

procedure TMiniRESTSQLTest.MethodThatRaiseExceptionOnOpenQuery;
var
  LConnectionFactory: IMiniRESTSQLConnectionFactory;
  LConnectionFactoryParams: IMiniRESTSQLConnectionFactoryParams;
  LQry: IMiniRESTSQLQuery;
  LConn: IMiniRESTSQLConnection;
begin
  LConnectionFactoryParams := GetConnectionFactoryParams;
  LConnectionFactoryParams.SetOnOpenQueryException(@OnOpenQueryExceptionRaiseException);
  LConnectionFactory := GetConnectionFactory(LConnectionFactoryParams);
  LConn := LConnectionFactory.GetConnection;
  LQry := LConn.GetQuery('HUE');
  LQry.Open;
end;

end.
