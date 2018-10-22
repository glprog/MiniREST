unit MiniREST.ORM.Column;

interface

uses SysUtils, Rtti, MiniREST.ORM.Intf, MiniREST.ORM.Common;

type
  TMiniRESTORMColumn = class(TInterfacedObject, IMiniRESTORMColumn)

  public
    function Clone: IMiniRESTORMColumn;
    function GetCaption: string;
    function GetFieldAlias: string;
    function GetIsLazy: Boolean;
    function GetIsManyToMany: Boolean;
    function GetIsManyToOne: Boolean;
    function GetIsNullable: Boolean;
    function GetIsOneToMany: Boolean;
    function GetIsPkPrimaryKey: Boolean;
    function GetName: string;
    function GetOnGetValue: TFunc<MiniREST.ORM.Intf.IMiniRESTORMColumn, System.TObject, System.Rtti.TValue>;
    function GetOnSetValue: TProc<MiniREST.ORM.Intf.IMiniRESTORMColumn, System.TObject ,System.Rtti.TValue>;
    function GetParentColumn: IMiniRESTORMColumn;
    function GetParentPKProperty: TRttiProperty;
    function GetPath: string;
    function GetPrecision: SmallInt;
    function GetRttiProperty: TRttiProperty;
    function GetScale: SmallInt;
    function GetSize: SmallInt;
    function GetTablePrefix: string;
    function GetType: TMiniRESTORMColumnType;
    function GetValue(AObj: TObject): TValue;
    procedure SetCaption(const ACaption: string);
    procedure SetFieldAlias(const AAlias: string);
    procedure SetIsLazy(const ALazy: Boolean);
    procedure SetIsManyToMany(const AIsManyToMany: Boolean);
    procedure SetIsManyToOne(const AIsManyToOne: Boolean);
    procedure SetIsNullable(const ANullable: Boolean);
    procedure SetIsOneToMany(const AIsOneToMany: Boolean);
    procedure SetIsPrimeryKey(const AIsPrimaryKey: Boolean);
    procedure SetName(const AName: string);
    procedure SetOnGetValue(const AFunc: TFunc<MiniREST.ORM.Intf.IMiniRESTORMColumn,System.TObject,System.Rtti.TValue>);
    procedure SetOnSetValue(const AProc: TProc<MiniREST.ORM.Intf.IMiniRESTORMColumn,System.TObject,System.Rtti.TValue>);
    procedure SetParentColumn(AColumn: IMiniRESTORMColumn);
    procedure SetParentPKProperty(AProp: TRttiProperty);
    procedure SetPath(const APath: string);
    procedure SetPrecision(const APrecision: SmallInt);
    function SetRttiProperty(const AProp: TRttiProperty): IMiniRESTORMColumn;
    procedure SetScale(const AScale: SmallInt);
    function SetSize(const ASize: SmallInt): IMiniRESTORMColumn;
    procedure SetTablePrefix(const APrefix: string);
    procedure SetType(const AType: TMiniRESTORMColumnType);
    procedure SetValue(AObj: TObject; AValue: TValue);
  end;

implementation

{ TMiniRESTORMColumn }

function TMiniRESTORMColumn.Clone: IMiniRESTORMColumn;
begin

end;

function TMiniRESTORMColumn.GetCaption: string;
begin

end;

function TMiniRESTORMColumn.GetFieldAlias: string;
begin

end;

function TMiniRESTORMColumn.GetIsLazy: Boolean;
begin

end;

function TMiniRESTORMColumn.GetIsManyToMany: Boolean;
begin

end;

function TMiniRESTORMColumn.GetIsManyToOne: Boolean;
begin

end;

function TMiniRESTORMColumn.GetIsNullable: Boolean;
begin

end;

function TMiniRESTORMColumn.GetIsOneToMany: Boolean;
begin

end;

function TMiniRESTORMColumn.GetIsPkPrimaryKey: Boolean;
begin

end;

function TMiniRESTORMColumn.GetName: string;
begin

end;

function TMiniRESTORMColumn.GetOnGetValue: TFunc<MiniREST.ORM.Intf.IMiniRESTORMColumn, System.TObject, System.Rtti.TValue>;
begin

end;

function TMiniRESTORMColumn.GetOnSetValue: TProc<MiniREST.ORM.Intf.IMiniRESTORMColumn, System.TObject, System.Rtti.TValue>;
begin

end;

function TMiniRESTORMColumn.GetParentColumn: IMiniRESTORMColumn;
begin

end;

function TMiniRESTORMColumn.GetParentPKProperty: TRttiProperty;
begin

end;

function TMiniRESTORMColumn.GetPath: string;
begin

end;

function TMiniRESTORMColumn.GetPrecision: SmallInt;
begin

end;

function TMiniRESTORMColumn.GetRttiProperty: TRttiProperty;
begin

end;

function TMiniRESTORMColumn.GetScale: SmallInt;
begin

end;

function TMiniRESTORMColumn.GetSize: SmallInt;
begin

end;

function TMiniRESTORMColumn.GetTablePrefix: string;
begin

end;

function TMiniRESTORMColumn.GetType: TMiniRESTORMColumnType;
begin

end;

function TMiniRESTORMColumn.GetValue(AObj: TObject): TValue;
begin

end;

procedure TMiniRESTORMColumn.SetCaption(const ACaption: string);
begin

end;

procedure TMiniRESTORMColumn.SetFieldAlias(const AAlias: string);
begin

end;

procedure TMiniRESTORMColumn.SetIsLazy(const ALazy: Boolean);
begin

end;

procedure TMiniRESTORMColumn.SetIsManyToMany(const AIsManyToMany: Boolean);
begin

end;

procedure TMiniRESTORMColumn.SetIsManyToOne(const AIsManyToOne: Boolean);
begin

end;

procedure TMiniRESTORMColumn.SetIsNullable(const ANullable: Boolean);
begin

end;

procedure TMiniRESTORMColumn.SetIsOneToMany(const AIsOneToMany: Boolean);
begin

end;

procedure TMiniRESTORMColumn.SetIsPrimeryKey(const AIsPrimaryKey: Boolean);
begin

end;

procedure TMiniRESTORMColumn.SetName(const AName: string);
begin

end;

procedure TMiniRESTORMColumn.SetOnGetValue(
  const AFunc: TFunc<MiniREST.ORM.Intf.IMiniRESTORMColumn, System.TObject, System.Rtti.TValue>);
begin

end;

procedure TMiniRESTORMColumn.SetOnSetValue(
  const AProc: TProc<MiniREST.ORM.Intf.IMiniRESTORMColumn, System.TObject, System.Rtti.TValue>);
begin

end;

procedure TMiniRESTORMColumn.SetParentColumn(AColumn: IMiniRESTORMColumn);
begin

end;

procedure TMiniRESTORMColumn.SetParentPKProperty(AProp: TRttiProperty);
begin

end;

procedure TMiniRESTORMColumn.SetPath(const APath: string);
begin

end;

procedure TMiniRESTORMColumn.SetPrecision(const APrecision: SmallInt);
begin

end;

function TMiniRESTORMColumn.SetRttiProperty(
  const AProp: TRttiProperty): IMiniRESTORMColumn;
begin

end;

procedure TMiniRESTORMColumn.SetScale(const AScale: SmallInt);
begin

end;

function TMiniRESTORMColumn.SetSize(const ASize: SmallInt): IMiniRESTORMColumn;
begin

end;

procedure TMiniRESTORMColumn.SetTablePrefix(const APrefix: string);
begin

end;

procedure TMiniRESTORMColumn.SetType(const AType: TMiniRESTORMColumnType);
begin

end;

procedure TMiniRESTORMColumn.SetValue(AObj: TObject; AValue: TValue);
begin

end;

end.
