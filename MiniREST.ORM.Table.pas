unit MiniREST.ORM.Table;

interface

uses SysUtils, MiniREST.ORM.Intf;

type
  TMiniRESTORMTable = class(TInterfacedObject, IMiniRESTORMTable)
  private
    FName: string;
    FTablePrefix: string;
    FColumns: TArray<IMiniRESTORMColumn>;
    FTableJoins: TArray<IMiniRESTORMTableJoin>;
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
    function GetPrimaryKeyColumns: System.TArray<MiniREST.ORM.Intf.IMiniRESTORMColumn>;
  end;

implementation

uses MiniREST.Common.Utils;

{ TMiniRESTORMTable }

procedure TMiniRESTORMTable.AddColumn(Acolumn: IMiniRESTORMColumn);
begin
  SetLength(FColumns, Length(FColumns) + 1);
  FColumns[Length(FColumns) - 1] := Acolumn;
end;

function TMiniRESTORMTable.AddTableJoin(
  ATableJoin: IMiniRESTORMTableJoin): IMiniRESTORMTable;
begin
  TMiniRESTCommonUtils.AddToArray<IMiniRESTORMTableJoin>(ATableJoin, FTableJoins);
end;

function TMiniRESTORMTable.GetColumn(const AColumn: string): IMiniRESTORMColumn;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTORMTable.GetColumns: System.TArray<MiniREST.ORM.Intf.IMiniRESTORMColumn>;
begin
  Result := FColumns;
end;

function TMiniRESTORMTable.GetIsManyToManyControlTable: Boolean;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTORMTable.GetManyToManyClass: TClass;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTORMTable.GetName: string;
begin
  Result := FName;
end;

function TMiniRESTORMTable.GetPrimaryKeyColumns: System.TArray<MiniREST.ORM.Intf.IMiniRESTORMColumn>;
var
  LColumn: IMiniRESTORMColumn;
begin
  SetLength(Result, 0);
  for LColumn in FColumns do
  begin
    if LColumn.GetIsPrimaryKey then
      TMiniRESTCommonUtils.AddToArray<IMiniRESTORMColumn>(LColumn, Result);
  end;
end;

function TMiniRESTORMTable.GetTableJoins: System.TArray<MiniREST.ORM.Intf.IMiniRESTORMTableJoin>;
begin
  Result := FTableJoins;
end;

function TMiniRESTORMTable.GetTablePrefix: string;
begin
  Result := FTablePrefix;
end;

procedure TMiniRESTORMTable.SetManyToManyClass(AClass: TClass);
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTable.SetManyToManyControlTable(
  const AIsControlTable: Boolean);
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMTable.SetName(const AName: string);
begin
  FName := AName;
end;

procedure TMiniRESTORMTable.SetTablePrefix(const APrefix: string);
begin
  FTablePrefix := APrefix;
end;

end.
