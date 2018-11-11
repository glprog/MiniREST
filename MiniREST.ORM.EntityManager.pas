unit MiniREST.ORM.EntityManager;

interface

uses SysUtils, MiniREST.SQL.Intf, MiniREST.ORM.Intf;

type
  TMiniRESTORMEntityManager = class
  private
    FConnectionFactory: IMiniRESTSQLConnectionFactory;
  public
    constructor Create(AConnectionFactory: IMiniRESTSQLConnectionFactory);
    destructor Destroy;override;
    function Get<Chave;T :class, constructor>(AId: Chave): T;
    function Persist(AObject: TObject): Boolean;
    function Delete(AObject: TObject): Boolean;
    function Merge(AObject: TObject): Boolean;
    procedure Manage(AObject: TObject);
    procedure VerifyStructure;
  end;

implementation

uses MiniREST.ORM.Mapper, MiniREST.ORM.Schema;

{ TMiniRESTORMEntityManager }

constructor TMiniRESTORMEntityManager.Create(
  AConnectionFactory: IMiniRESTSQLConnectionFactory);
begin
  FConnectionFactory := AConnectionFactory;
end;

function TMiniRESTORMEntityManager.Delete(AObject: TObject): Boolean;
begin
  raise Exception.Create('Not implemented');
end;

destructor TMiniRESTORMEntityManager.Destroy;
begin

  inherited;
end;

function TMiniRESTORMEntityManager.Get<Chave, T>(AId: Chave): T;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMEntityManager.Manage(AObject: TObject);
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTORMEntityManager.Merge(AObject: TObject): Boolean;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTORMEntityManager.Persist(AObject: TObject): Boolean;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMEntityManager.VerifyStructure;
var
  LSchema: IMiniRESTORMSchema;
begin
  LSchema := TMiniRESTORMSchema.Create(FConnectionFactory.GetConnection);
  LSchema.VerifyStructure;
end;

end.
