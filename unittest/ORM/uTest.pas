unit uTest;

interface
uses
  DUnitX.TestFramework, SysUtils;

type

  [TestFixture]
  TMiniRESTORMTest = class(TObject)
  private
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

procedure TMiniRESTORMTest.Setup;
begin
  CreateConnection;
  DropTable('PESSOA_DOCUMENTOS');
  DropTable('DOCUMENTO');
  DropTable('ITEM');
  DropTable('MOVIMENTO');
  DropTable('PRODUTO');
  DropTable('PESSOA');
  DropTable('FILIAL');
  DropTable('EMPRESA');
  RegisterModels;
end;

procedure TMiniRESTORMTest.TearDown;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTest.TesteAddColumn;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTest.TesteDelete;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTest.TesteGet;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTest.TesteGet2;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTest.TesteGetManyToMany;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTest.TesteInsert;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTest.TesteInsert2;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTest.TesteInsertManyToMany;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTest.TesteMerge;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTest.TesteMerge2;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTest.TesteMergeManyToMany;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTest.TestePrimaryKey;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTest.TesteVerifyStructure;
begin
  raise Exception.Create('Not implemented');
end;

initialization
  TDUnitX.RegisterTestFixture(TMiniRESTORMTest);
end.
