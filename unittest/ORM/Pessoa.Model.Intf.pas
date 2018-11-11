unit Pessoa.Model.Intf;

interface

uses Spring;

type
  IPessoa = interface
  ['{FA95CDA4-E351-40C8-84FF-979B6595EB43}']
    function GetID: Integer;
    procedure SetID(const AID: Integer);
    function GetNome: string;
    procedure SetNome(const ANome: string);
    function GetNomeDoCachorro: Nullable<string>;
    procedure SetNomeDoCachorro(const ANome: Nullable<string>);
    property ID: Integer read GetID write SetID;
    property Nome: string read GetNome write SetNome;
    property NomeDoCachorro: Nullable<string> read GetNomeDoCachorro write SetNomeDoCachorro;
  end;

implementation

end.
