unit MiniREST.ORM.Attribute;

interface

uses MiniREST.ORM.Common;

type
  IDAttribute = class(TCustomAttribute)
  end;

  ColumnAttribute = class(TCustomAttribute)
  private
    FName : string;
  public
    constructor Create(const AName : string);
    property Name : string read FName;
  end;

  SizeAttribute = class(TCustomAttribute)
  private
    FSize : SmallInt;
  public
    constructor Create(const ASize : SmallInt);
    property Value : SmallInt read FSize;
  end;

  PrecisionAttribute = class(TCustomAttribute)
  private
    FPrecision : SmallInt;
  public
    constructor Create(const APrecision : SmallInt);
    property Value : SmallInt read FPrecision;
  end;

  ScaleAttribute = class(TCustomAttribute)
  private
    FScale : SmallInt;
  public
    constructor Create(const AScale : SmallInt);
    property Value : SmallInt read FScale;
  end;

  TableAttribute = class(TCustomAttribute)
  private
    FName : string;
  public
    constructor Create(const AName : string);
    property Name : string read FName;
  end;

  ColumnTypeAttribute = class(TCustomAttribute)
  private
    FColumnType : TMiniRESTORMColumnType;
  public
    constructor Create(const AColumnType : TMiniRESTORMColumnType);
    property Value : TMiniRESTORMColumnType read FColumnType;
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

end.
