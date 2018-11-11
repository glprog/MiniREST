unit MiniREST.ORM.Column;

interface

uses SysUtils, Rtti, MiniREST.ORM.Intf, MiniREST.ORM.Common;

type
  TMiniRESTORMColumn = class(TInterfacedObject, IMiniRESTORMColumn)
  private
    FCaption: string;
    FColumnAlias: string;
    FIsLazy: Boolean;
    FIsManyToOne: Boolean;
    FIsManyToMany: Boolean;
    FIsOneToMany: Boolean;
    FIsNullable: Boolean;
    FIsPrimaryKey: Boolean;
    FName: string;
    FTablePrefix: string;
    FPrecision: SmallInt;
    FScale: SmallInt;
    FSize: SmallInt;
    FType: TMiniRESTORMColumnType;
    FRttiProperty: TRttiProperty;
    FParentPKProperty: TRttiProperty;
    FOnGetValue : TFunc<IMiniRESTORMColumn, TObject, TValue>;
    FOnSetValue : TProc<IMiniRESTORMColumn, TObject, TValue>;
    FPath: string;
  public
    function Clone: IMiniRESTORMColumn;
    function GetCaption: string;
    function GetIsLazy: Boolean;
    function GetIsManyToMany: Boolean;
    function GetIsManyToOne: Boolean;
    function GetIsNullable: Boolean;
    function GetIsOneToMany: Boolean;
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
    procedure SetIsLazy(const ALazy: Boolean);
    procedure SetIsManyToMany(const AIsManyToMany: Boolean);
    procedure SetIsManyToOne(const AIsManyToOne: Boolean);
    procedure SetIsNullable(const ANullable: Boolean);
    procedure SetIsOneToMany(const AIsOneToMany: Boolean);
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
    function GetColumnAlias: string;
    procedure SetColumnAlias(const AAlias: string);
    function GetIsPrimaryKey: Boolean;
    procedure SetIsPrimaryKey(const AIsPrimaryKey: Boolean);
  end;

implementation

{ TMiniRESTORMColumn }

function TMiniRESTORMColumn.Clone: IMiniRESTORMColumn;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTORMColumn.GetCaption: string;
begin
  Result := FCaption;
end;

function TMiniRESTORMColumn.GetColumnAlias: string;
begin
  Result := FColumnAlias;
end;

function TMiniRESTORMColumn.GetIsLazy: Boolean;
begin
  Result := FIsLazy;
end;

function TMiniRESTORMColumn.GetIsManyToMany: Boolean;
begin
  Result := FIsManyToMany;
end;

function TMiniRESTORMColumn.GetIsManyToOne: Boolean;
begin
  Result := FIsManyToOne;
end;

function TMiniRESTORMColumn.GetIsNullable: Boolean;
begin
  Result := FIsNullable;
end;

function TMiniRESTORMColumn.GetIsOneToMany: Boolean;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTORMColumn.GetIsPrimaryKey: Boolean;
begin
  Result := FIsPrimaryKey;
end;

function TMiniRESTORMColumn.GetName: string;
begin
  Result := FName;
end;

function TMiniRESTORMColumn.GetOnGetValue: TFunc<MiniREST.ORM.Intf.IMiniRESTORMColumn, System.TObject, System.Rtti.TValue>;
begin
  Result := FOnGetValue;
end;

function TMiniRESTORMColumn.GetOnSetValue: TProc<MiniREST.ORM.Intf.IMiniRESTORMColumn, System.TObject, System.Rtti.TValue>;
begin
  Result := FOnSetValue;
end;

function TMiniRESTORMColumn.GetParentColumn: IMiniRESTORMColumn;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTORMColumn.GetParentPKProperty: TRttiProperty;
begin
  Result := FParentPKProperty;
end;

function TMiniRESTORMColumn.GetPath: string;
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTORMColumn.GetPrecision: SmallInt;
begin
  Result := FPrecision;
end;

function TMiniRESTORMColumn.GetRttiProperty: TRttiProperty;
begin
  Result := FRttiProperty;
end;

function TMiniRESTORMColumn.GetScale: SmallInt;
begin
  Result := FScale;
end;

function TMiniRESTORMColumn.GetSize: SmallInt;
begin
  Result := FSize;
end;

function TMiniRESTORMColumn.GetTablePrefix: string;
begin
  Result := FTablePrefix;
end;

function TMiniRESTORMColumn.GetType: TMiniRESTORMColumnType;
begin
  Result := FType;
end;

function TMiniRESTORMColumn.GetValue(AObj: TObject): TValue;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMColumn.SetCaption(const ACaption: string);
begin
  FCaption := ACaption;
end;

procedure TMiniRESTORMColumn.SetColumnAlias(const AAlias: string);
begin
  FColumnAlias := AAlias;
end;

procedure TMiniRESTORMColumn.SetIsLazy(const ALazy: Boolean);
begin
  FIsLazy := ALazy;
end;

procedure TMiniRESTORMColumn.SetIsManyToMany(const AIsManyToMany: Boolean);
begin
  FIsManyToMany := AIsManyToMany;
end;

procedure TMiniRESTORMColumn.SetIsManyToOne(const AIsManyToOne: Boolean);
begin
  FIsManyToOne := AIsManyToOne;
end;

procedure TMiniRESTORMColumn.SetIsNullable(const ANullable: Boolean);
begin
  FIsNullable := ANullable;
end;

procedure TMiniRESTORMColumn.SetIsOneToMany(const AIsOneToMany: Boolean);
begin
  FIsOneToMany := AIsOneToMany;
end;

procedure TMiniRESTORMColumn.SetIsPrimaryKey(const AIsPrimaryKey: Boolean);
begin
  FIsPrimaryKey := AIsPrimaryKey;
end;

procedure TMiniRESTORMColumn.SetName(const AName: string);
begin
  FName := AName;
end;

procedure TMiniRESTORMColumn.SetOnGetValue(
  const AFunc: TFunc<MiniREST.ORM.Intf.IMiniRESTORMColumn, System.TObject, System.Rtti.TValue>);
begin
  FOnGetValue := AFunc;
end;

procedure TMiniRESTORMColumn.SetOnSetValue(
  const AProc: TProc<MiniREST.ORM.Intf.IMiniRESTORMColumn, System.TObject, System.Rtti.TValue>);
begin
  FOnSetValue := AProc;
end;

procedure TMiniRESTORMColumn.SetParentColumn(AColumn: IMiniRESTORMColumn);
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMColumn.SetParentPKProperty(AProp: TRttiProperty);
begin
  FParentPKProperty := AProp;
end;

procedure TMiniRESTORMColumn.SetPath(const APath: string);
begin
  FPath := APath;
end;

procedure TMiniRESTORMColumn.SetPrecision(const APrecision: SmallInt);
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTORMColumn.SetRttiProperty(
  const AProp: TRttiProperty): IMiniRESTORMColumn;
begin
  Result := Self;
  FRttiProperty := AProp;
end;

procedure TMiniRESTORMColumn.SetScale(const AScale: SmallInt);
begin
  raise Exception.Create('Not implemented');
end;

function TMiniRESTORMColumn.SetSize(const ASize: SmallInt): IMiniRESTORMColumn;
begin
  raise Exception.Create('Not implemented');
end;

procedure TMiniRESTORMColumn.SetTablePrefix(const APrefix: string);
begin
  FTablePrefix := APrefix;
end;

procedure TMiniRESTORMColumn.SetType(const AType: TMiniRESTORMColumnType);
begin
  FType := AType;
end;

procedure TMiniRESTORMColumn.SetValue(AObj: TObject; AValue: TValue);
begin
  raise Exception.Create('Not implemented');
end;

end.
