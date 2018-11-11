unit MiniREST.ORM.Attribute;

interface

uses MiniREST.ORM.Common, MiniREST.ORM.Intf;

type
  IDAttribute = class(TCustomAttribute)
  end;

  ColumnAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  SizeAttribute = class(TCustomAttribute)
  private
    FSize: SmallInt;
  public
    constructor Create(const ASize: SmallInt);
    property Value: SmallInt read FSize;
  end;

  PrecisionAttribute = class(TCustomAttribute)
  private
    FPrecision: SmallInt;
  public
    constructor Create(const APrecision: SmallInt);
    property Value: SmallInt read FPrecision;
  end;

  ScaleAttribute = class(TCustomAttribute)
  private
    FScale: SmallInt;
  public
    constructor Create(const AScale: SmallInt);
    property Value: SmallInt read FScale;
  end;

  TableAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  ColumnTypeAttribute = class(TCustomAttribute)
  private
    FColumnType: TMiniRESTORMColumnType;
  public
    constructor Create(const AColumnType: TMiniRESTORMColumnType);
    property Value: TMiniRESTORMColumnType read FColumnType;
  end;

  TransientAttribute = class(TCustomAttribute);

  JoinColumnsAttribute = class(TCustomAttribute)
  private
    FColumnL: string;
    FColumnR: string;
    FNullable: Boolean;
  public
    constructor Create(const AColumnL, AColumnR: string; const ANullable: Boolean = True);
    property ColumnL: string read FColumnL;
    property ColumnR: string read FColumnR;
    property IsNullable: Boolean read FNullable;
  end;

  InverseJoinColumns = class(TCustomAttribute)
  private
    FColumnL: string;
    FColumnR: string;
  public
    constructor Create(const AColumnL, AColumnR: string);
    property ColumnL: string read FColumnL;
    property ColumnR: string read FColumnR;
  end;

  OneToManyAttribute = class(TCustomAttribute)
  private
    FLazy: Boolean;
    FCascadeTypes: TMiniRESTORMCascadeTypes;
  public
    constructor Create(const ALazy: Boolean; const ACascadeType: TMiniRESTORMCascadeTypes);
    property IsLazy: Boolean read FLazy;
    property CascadeType: TMiniRESTORMCascadeTypes read FCascadeTypes;
  end;

  ManyToOneAttribute = class(TCustomAttribute)
  end;

  ManyToManyAttribute = class(TCustomAttribute)
  private
    FTable: IMiniRESTORMTable;
    FClass: TClass;
    procedure SetTable(ATable: IMiniRESTORMTable);
  public
    constructor Create(AControlClass: TClass);
    property &Class: TClass read FClass;
  end;

implementation

{ PrecisionAttribute }

constructor PrecisionAttribute.Create(const APrecision: SmallInt);
begin
  FPrecision := APrecision;
end;

{ ColumnAttribute }

constructor ColumnAttribute.Create(const AName: string);
begin
  FName := AName;
end;

{ SizeAttribute }

constructor SizeAttribute.Create(const ASize: SmallInt);
begin
  FSize := ASize;
end;

{ TableAttribute }

constructor TableAttribute.Create(const AName: string);
begin
  FName := AName;
end;

{ ColumnTypeAttribute }

constructor ColumnTypeAttribute.Create(
  const AColumnType: TMiniRESTORMColumnType);
begin
  FColumnType := AColumnType;
end;

constructor ScaleAttribute.Create(const AScale: SmallInt);
begin
  FScale := AScale;  
end;

{ InverseJoinColumns }

constructor InverseJoinColumns.Create(const AColumnL, AColumnR: string);
begin
  FColumnL := AColumnL;
  FColumnR := AColumnR;
end;

{ JoinColumnsAttribute }

constructor JoinColumnsAttribute.Create(const AColumnL, AColumnR: string;
  const ANullable: Boolean);
begin
  FColumnL := AColumnL;
  FColumnR := AColumnR;
  FNullable := ANullable;
end;

{ OneToManyAttribute }

constructor OneToManyAttribute.Create(const ALazy: Boolean;
  const ACascadeType: TMiniRESTORMCascadeTypes);
begin

end;

{ ManyToManyAttribute }

constructor ManyToManyAttribute.Create(AControlClass: TClass);
begin

end;

procedure ManyToManyAttribute.SetTable(ATable: IMiniRESTORMTable);
begin

end;

end.
