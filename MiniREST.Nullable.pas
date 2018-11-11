unit MiniREST.Nullable;

interface

uses Rtti, TypInfo, StrUtils;

type
  PTypeInfo = TypInfo.PTypeInfo;

  TValueHelper = record helper for TValue
  public
    procedure SetNullableValue(const value: TValue);
  end;

  TNullableHelper = record
  strict private
    fValueType: PTypeInfo;
    fHasValueOffset: NativeInt;
  public
    constructor Create(typeInfo: PTypeInfo);
    function GetValue(instance: Pointer): TValue; inline;
    function HasValue(instance: Pointer): Boolean; inline;
    procedure SetValue(instance: Pointer; const value: TValue); inline;
    property ValueType: PTypeInfo read fValueType;
  end;

  Nullable = record
  private
    const HasValue = 'True';
    class function GetNull: Nullable; static; inline;
  public
    class property Null: Nullable read GetNull;
  end;



implementation

function IsNullable(typeInfo: PTypeInfo): Boolean;
  const
  PrefixString = 'Nullable<';    // DO NOT LOCALIZE
begin
  Result := Assigned(typeInfo) and (typeInfo.Kind = tkRecord)
    and StartsText(PrefixString, typeInfo.Name);
end;

{ TValueHelper }

procedure TValueHelper.SetNullableValue(const value: TValue);
var
  typeInfo: PTypeInfo;
  nullable: TNullableHelper;
  instance: Pointer;
begin
  typeInfo := TValueData(Self).FTypeInfo;
  if IsNullable(typeInfo) then
  begin
    instance := GetReferenceToRawData;
    nullable := TNullableHelper.Create(typeInfo);
    nullable.SetValue(instance, value);
  end;
end;

{ TNullableHelper }

constructor TNullableHelper.Create(typeInfo: PTypeInfo);
begin

end;

function TNullableHelper.GetValue(instance: Pointer): TValue;
begin

end;

function TNullableHelper.HasValue(instance: Pointer): Boolean;
begin

end;

procedure TNullableHelper.SetValue(instance: Pointer; const value: TValue);
begin

end;

class function Nullable.GetNull: Nullable;
begin
  
end;

end.
