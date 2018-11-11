unit Pessoa.Model;

interface

uses  Pessoa.Model.Intf, MiniREST.ORM.Attribute, Spring;

type
  [Table('PESSOA')]
  TPessoa = class(TInterfacedObject, IPessoa)
  private
    FID: Integer;
    FNome: string;
    FNomeDoCachorro: Nullable<string>;
    function GetID: Integer;
    function GetNome: string;
    procedure SetID(const Value: Integer);
    procedure SetNome(const Value: string);
    function GetNomeDoCachorro: Nullable<string>;
    procedure SetNomeDoCachorro(const ANome: Nullable<string>);
  published
    [ID]
    property ID: Integer read GetID write SetID;
    property Nome: string read GetNome write SetNome;
    property NomeDoCachorro: Nullable<string> read GetNomeDoCachorro write SetNomeDoCachorro;
  end;

implementation

uses MiniREST.ORM.Mapper;

{ TPessoa }

function TPessoa.GetID: Integer;
begin
  Result := FID;
end;

function TPessoa.GetNome: string;
begin
  Result := FNome;
end;

function TPessoa.GetNomeDoCachorro: Nullable<string>;
begin

end;

procedure TPessoa.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TPessoa.SetNome(const Value: string);
begin
  FNome := Value;
end;

procedure TPessoa.SetNomeDoCachorro(const ANome: Nullable<string>);
begin

end;

initialization
  TMiniRESTORMMapper.RegisterClass(TPessoa);

end.
