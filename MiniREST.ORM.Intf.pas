unit MiniREST.ORM.Intf;

interface

uses SysUtils, Rtti, MiniREST.ORM.Common;

type

  IMiniRESTORMTableJoin = interface;

  IMiniRESTORMColumn = interface
    ['{4F43605B-305E-49C0-8B0C-72A60A73890E}']
    function GetName: string;
    procedure SetName(const AName: string);
    function GetCaption: string;
    procedure SetCaption(const ACaption: string);
    function GetType: TMiniRESTORMColumnType;
    procedure SetType(const AType: TMiniRESTORMColumnType);
    function GetSize: SmallInt;
    function SetSize(const ASize: SmallInt): IMiniRESTORMColumn;
    function GetPrecision: SmallInt;
    procedure SetPrecision(const APrecision: SmallInt);
    function GetScale: SmallInt;
    procedure SetScale(const AScale: SmallInt);
    function GetColumnAlias: string;
    procedure SetColumnAlias(const AAlias : string);
    function GetIsLazy: Boolean;
    procedure SetIsLazy(const ALazy: Boolean);
    function GetIsNullable: Boolean;
    procedure SetIsNullable(const ANullable: Boolean);
    function GetIsPrimaryKey: Boolean;
    procedure SetIsPrimaryKey(const AIsPrimaryKey: Boolean);
    function GetRttiProperty: TRttiProperty;
    function SetRttiProperty(const AProp: TRttiProperty): IMiniRESTORMColumn;
    function GetOnGetValue: TFunc<IMiniRESTORMColumn, TObject, TValue>;
    procedure SetOnGetValue(const AFunc: TFunc<IMiniRESTORMColumn, TObject, TValue>);
    function GetOnSetValue: TProc<IMiniRESTORMColumn, TObject, TValue>;
    procedure SetOnSetValue(const AProc : TProc<IMiniRESTORMColumn, TObject, TValue>);
    function GetValue(AObj: TObject): TValue;
    procedure SetValue(AObj: TObject; AValue: TValue);
    function GetParentPKProperty: TRttiProperty;
    procedure SetParentPKProperty(AProp: TRttiProperty);
    function GetParentColumn: IMiniRESTORMColumn;
    procedure SetParentColumn(AColumn : IMiniRESTORMColumn);
    function GetTablePrefix : string;
    procedure SetTablePrefix(const APrefix: string);
    function GetPath: string;
    procedure SetPath(const APath: string);
    function GetIsOneToMany: Boolean;
    procedure SetIsOneToMany(const AIsOneToMany: Boolean);
    function GetIsManyToOne: Boolean;
    procedure SetIsManyToOne(const AIsManyToOne: Boolean);
    function GetIsManyToMany: Boolean;
    procedure SetIsManyToMany(const AIsManyToMany : Boolean);
    function Clone: IMiniRESTORMColumn;
  end;

  IMiniRESTORMTable = interface
    ['{C50D8C81-89AD-4DB1-B927-3B9543EFD3BD}']
    function GetName: string;
    procedure SetName(const AName : string);
    procedure AddColumn(Acolumn: IMiniRESTORMColumn);
    function GetColumn(const AColumn: string): IMiniRESTORMColumn;
    function GetColumns: TArray<IMiniRESTORMColumn>;
    function GetPrimaryKeyColumns: TArray<IMiniRESTORMColumn>;
    function GetTablePrefix: string;
    procedure SetTablePrefix(const APrefix : string);
    function GetTableJoins: TArray<IMiniRESTORMTableJoin>;
    function AddTableJoin(ATableJoin : IMiniRESTORMTableJoin): IMiniRESTORMTable;
    function GetIsManyToManyControlTable: Boolean;
    procedure SetManyToManyControlTable(const AIsControlTable: Boolean);
    function GetManyToManyClass: TClass;
    procedure SetManyToManyClass(AClass : TClass);
  end;

  IMiniRESTORMColumnJoin = interface
    ['{14998351-8DBC-4744-A3BC-AEE12B758212}']
    function GetColumnL: IMiniRESTORMColumn;
    procedure SetColumnL(AColumn: IMiniRESTORMColumn);
    function GetColumnR: IMiniRESTORMColumn;
    procedure SetColumnR(AColumn: IMiniRESTORMColumn);
  end;

  IMiniRESTORMTableJoin = interface
  ['{84FD0FC0-E37D-4ECE-B52C-C56BE97E43F5}']
    function GetTable: IMiniRESTORMTable;
    procedure SetTable(ATable: IMiniRESTORMTable);
    procedure AddColumnJoin(AColumnJoin: IMiniRESTORMColumnJoin);
    function GetColumnsJoin: TArray<IMiniRESTORMColumnJoin>;
    procedure AddInverseJoinColumn(AColumnJoin: IMiniRESTORMColumnJoin);
    function GetInverseJoinColumns: TArray<IMiniRESTORMColumnJoin>;
    function GetPrefix: string;
    function GetRttiProperty: TRttiProperty;
    procedure SetRttiProperty(AProp: TRttiProperty);
    function GetIsManyToOne: Boolean;
    procedure SetIsManyToOne(const AIsManyToOne: Boolean);
    function GetIsManyToMany: Boolean;
    procedure SetIsManyToMany(const AIsManyToMany: Boolean);
    function GetControlTable: string;
    procedure SetControlTable(const AControlTable: string);
  end;

  IMiniRESTORMManyToManyTableJoin = interface
  ['{2C29D63A-4304-4F9D-A5FF-0C62F8BC24E1}']
    function GetControlTable: string;
    procedure SetControlTable(AControlTable: string);
    function GetRttiProperty: TRttiProperty;
    procedure SetRttiProperty(ARttiProperty: TRttiProperty);
  end;

  IMiniRESTORMField = interface
  ['{0E74ACF6-717C-4B7E-8057-841F2B3B0C40}']
    function GetColumn : IMiniRESTORMColumn;
    procedure SetColumn(AColumn : IMiniRESTORMColumn);
    function GetValue: TValue;
    procedure SetValue(AValue: TValue);
  end;

  IMiniRESTORMSchema = interface
  ['{688CCCFF-B637-4935-A62C-73FC4A609E30}']
    procedure VerifyStructure;
  end;

  IMiniRESTORMDDL = interface
  ['{9B6677D0-82ED-4E56-B141-93CE2BFCA136}']
    procedure CreateTable(ATable: IMiniRESTORMTable);
  end;

implementation

end.
