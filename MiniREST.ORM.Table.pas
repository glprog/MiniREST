unit MiniREST.ORM.Table;

interface

uses MiniREST.ORM.Intf;

type
  TMiniRESTORMTable = class(TInterfacedObject, IMiniRESTORMTable)
  public
    function AddTableJoin(ATableJoin: IMiniRESTORMTableJoin): IMiniRESTORMTable;
    function GetColumn(const AColumn: string): IMiniRESTORMColumn;
    function GetColumns: System.TArray<MiniREST.ORM.Intf.IMiniRESTORMColumn>;
    function GetTableJoins: System.TArray<MiniREST.ORM.Intf.IMiniRESTORMTableJoin>;
    function GetIsManyToManyControlTable: Boolean;
    procedure SetManyToManyControlTable(const AIsControlTable: Boolean);
    function GetManyToManyClass: TClass;
    procedure SetManyToManyClass(AClass: TClass);
    function GetTablePrefix: string;
    procedure SetTablePrefix(const APrefix: string);
    function GetName: string;
    procedure SetName(const AName: string);
    procedure AddColumn(Acolumn: IMiniRESTORMColumn);
  end;

implementation

{ TMiniRESTORMTable }

procedure TMiniRESTORMTable.AddColumn(Acolumn: IMiniRESTORMColumn);
begin

end;

function TMiniRESTORMTable.AddTableJoin(
  ATableJoin: IMiniRESTORMTableJoin): IMiniRESTORMTable;
begin

end;

function TMiniRESTORMTable.GetColumn(const AColumn: string): IMiniRESTORMColumn;
begin

end;

function TMiniRESTORMTable.GetColumns: System.TArray<MiniREST.ORM.Intf.IMiniRESTORMColumn>;
begin

end;

function TMiniRESTORMTable.GetIsManyToManyControlTable: Boolean;
begin

end;

function TMiniRESTORMTable.GetManyToManyClass: TClass;
begin

end;

function TMiniRESTORMTable.GetName: string;
begin

end;

function TMiniRESTORMTable.GetTableJoins: System.TArray<MiniREST.ORM.Intf.IMiniRESTORMTableJoin>;
begin

end;

function TMiniRESTORMTable.GetTablePrefix: string;
begin

end;

procedure TMiniRESTORMTable.SetManyToManyClass(AClass: TClass);
begin

end;

procedure TMiniRESTORMTable.SetManyToManyControlTable(
  const AIsControlTable: Boolean);
begin

end;

procedure TMiniRESTORMTable.SetName(const AName: string);
begin

end;

procedure TMiniRESTORMTable.SetTablePrefix(const APrefix: string);
begin

end;

end.
