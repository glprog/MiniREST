unit Documento.Model.Intf;

interface

type
  IDocumento = interface
  ['{4E63778F-82AD-435D-8712-D1E9B8DED33E}']
    function GetID: Integer;
    procedure SetID(const AID: Integer);
    function GetNome: string;
    procedure SetNome(const ANome: string);
  end;

implementation

end.
