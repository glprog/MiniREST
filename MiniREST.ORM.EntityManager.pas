unit MiniREST.ORM.EntityManager;

interface

uses SysUtils, MiniREST.SQL.Intf;

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
  end;

implementation

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

end.
