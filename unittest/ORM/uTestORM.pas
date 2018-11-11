unit uTestORM;

interface
uses
  Classes, SysUtils, DUnitX.TestFramework, MiniREST.SQL.Intf, Data.DBXFirebird;

type

  [TestFixture]
  TMiniRESTORMTest = class(TObject)
  private
    FConnectionFactory: IMiniRESTSQLConnectionFactory;
    FConnection: IMiniRESTSQLConnection;
    procedure CreateConnection;
    procedure DropTable(const ATable: string);
    function TableExists(const ATable: string): Boolean;
  public
    [SetupFixture]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TesteVerifyStructure;
    [Test]
    procedure TesteInsert;
    [Test]
    procedure TestePrimaryKey;
    [Test]
    procedure TesteMerge;
    [Test]
    procedure TesteGet;
    [Test]
    procedure TesteDelete;
    [Test]
    procedure TesteAddColumn;
    [Test]
    procedure TesteInsert2;
    [Test]
    procedure TesteGet2;
    [Test]
    procedure TesteMerge2;
    [Test]
    procedure TesteInsertManyToMany;
    [Test]
    procedure TesteGetManyToMany;
    [Test]
    procedure TesteMergeManyToMany;
  end;

implementation

uses MiniREST.SQL.DBX, MiniREST.ORM.EntityManager, MiniREST.SQL.Common,
  MiniREST.ORM.Mapper, MiniREST.ORM.Intf, Pessoa.Model;

procedure TMiniRESTORMTest.Setup;
begin
   CreateConnection;
   DropTable('PESSOA');
  (*DropTable('PESSOA_DOCUMENTOS');
  DropTable('DOCUMENTO');
  DropTable('ITEM');
  DropTable('MOVIMENTO');
  DropTable('PRODUTO');
  DropTable('PESSOA');
  DropTable('FILIAL');
  DropTable('EMPRESA');
  RegisterModels; *)
end;

procedure TMiniRESTORMTest.TearDown;
begin
  //raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTest.TesteAddColumn;
begin
  Assert.Fail('Not implemented');
end;

procedure TMiniRESTORMTest.TesteDelete;
begin
  Assert.Fail('Not implemented');
end;

procedure TMiniRESTORMTest.TesteGet;
begin
  Assert.Fail('Not implemented');
end;

procedure TMiniRESTORMTest.TesteGet2;
begin
  Assert.Fail('Not implemented');
end;

procedure TMiniRESTORMTest.TesteGetManyToMany;
begin
  Assert.Fail('Not implemented');
end;

procedure TMiniRESTORMTest.TesteInsert;
var
  LPessoa: TPessoa;
  LEntityManager: TMiniRESTORMEntityManager;
  LQry: IMiniRESTSQLQuery;
begin
  LEntityManager := TMiniRESTORMEntityManager.Create(FConnectionFactory);
  LPessoa := TPessoa.Create;
  try
    LPessoa.ID := 1;
    LPessoa.Nome := 'Hue';
    LEntityManager.Persist(LPessoa);
  finally
    LPessoa.Free;
    LEntityManager.Free;
  end;
//  CreateConnection;
//  LQry := FConnection.NewQuery.Open('SELECT * FROM PESSOA WHERE ID = 1');
//  Assert.IsTrue(not LQry.IsEmpty, 'Query Pessoa no retornou resultados');
//  if not LQry.IsEmpty then
//  begin
//    Assert.IsTrue(LQry.GetField('ENDERECO').IsNull,
//        'Campo endere no est null. / Campo Nullable<string>');
//    Assert.IsTrue(LQry.GetField('ENDERECO2').AsString = '',
//        'Campo endere no est vazio.');
//  end;
//  LQry := FConnection.NewQuery.Open('SELECT * FROM DOCUMENTO WHERE ID = 2');
//  Assert.IsTrue(not LQry.IsEmpty, 'Query Documento no retornou resultados');
//  if not LQry.IsEmpty then
//  begin
//    Assert.AreEqual(2, LQry.GetField('ID').AsInteger, 'Documento: ID');
//    Assert.AreEqual('Teste Doc', LQry.GetField('NOME').AsString, 'Documento: Nome');
//  end;
end;

procedure TMiniRESTORMTest.TesteInsert2;
begin
  Assert.Fail('Not implemented');
end;

procedure TMiniRESTORMTest.TesteInsertManyToMany;
begin
  Assert.Fail('Not implemented');
end;

procedure TMiniRESTORMTest.TesteMerge;
begin
  Assert.Fail('Not implemented');
end;

procedure TMiniRESTORMTest.TesteMerge2;
begin
  Assert.Fail('Not implemented');
end;

procedure TMiniRESTORMTest.TesteMergeManyToMany;
begin
  Assert.Fail('Not implemented');
end;

procedure TMiniRESTORMTest.TestePrimaryKey;
begin
  Assert.Fail('Not implemented');
end;

procedure TMiniRESTORMTest.TesteVerifyStructure;
var
  LEntityManager: TMiniRESTORMEntityManager;
  LDatabaseInfo: IMiniRESTSQLDatabaseInfo;
  LTable: IMiniRESTORMTable;
  LColumn: IMiniRESTORMColumn;
  LColumnSQL: IMiniRESTSQLColumnInfo;
  LColumnsSQL:  TArray<IMiniRESTSQLColumnInfo>;
  LFoundColumn: Boolean;
begin
  LEntityManager := TMiniRESTORMEntityManager.Create(FConnectionFactory);
  LDatabaseInfo := FConnectionFactory.GetConnection.GetDatabaseInfo;
  try
    LEntityManager.VerifyStructure;
    LEntityManager.VerifyStructure;
    for LTable in TMiniRESTORMMapper.GetTables do
    begin
      Assert.IsTrue(LDatabaseInfo.TableExists(LTable.GetName + '1'), 'Table ' +
        LTable.GetName + ' not found in database.');
      LColumnsSQL := LDatabaseInfo.GetColumns(LTable.GetName);
      for LColumn in LTable.GetColumns do
      begin
        LFoundColumn := False;
        for LColumnSQL in LColumnsSQL do
        begin
          if SameText(LColumn.GetName, LColumnSQL.Name) then
          begin
            LFoundColumn := True;
            Break;
          end;
        end;
        Assert.IsTrue(LFoundColumn, 'Column ' + LTable.GetName + '.' +
          LColumn.GetName + ' not found in database.');
      end;
    end;
  finally
    LEntityManager.Free;
  end;
end;

procedure TMiniRESTORMTest.CreateConnection;
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
      .SetDatabseType(dbtFirebird)
    );
    FConnection := FConnectionFactory.GetConnection;    
  finally
    LConnectionInfo.Free;
  end;  
end;

procedure TMiniRESTORMTest.DropTable(const ATable: string);
var
  LQry: IMiniRESTSQLQuery;
begin
  LQry := FConnection.GetQuery('select 1 from rdb$relations where rdb$relation_name = :TABLE');
  LQry.ParamByName('TABLE').AsString := ATable;
  LQry.Open;
  if not LQry.Dataset.IsEmpty then
    FConnection.Execute('DROP TABLE ' + ATable, []);    
end;

function TMiniRESTORMTest.TableExists(const ATable: string): Boolean;
var
  LQry: IMiniRESTSQLQuery;
begin
  LQry := FConnection.GetQuery('select 1 from rdb$relations where rdb$relation_name = :TABLE');
  LQry.ParamByName('TABLE').AsString := ATable;
  LQry.Open;
  Result := not LQry.Dataset.IsEmpty;
end;

initialization
  TDUnitX.RegisterTestFixture(TMiniRESTORMTest);
end.
