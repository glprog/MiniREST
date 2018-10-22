unit MiniREST.ORM.Utils;

interface

uses SysUtils, Rtti;

type
  TMiniRESTORMUtils = class
  private
    class var FCx : TRttiContext;
  public
    class constructor Create;
    class destructor Destroy;
    class function GetAttribute<T: TCustomAttribute>(AClass: TClass): T; overload;
    class function GetAttribute<T: TCustomAttribute>(ARttiProperty: TRttiProperty): T; overload;
//    class function ContainsAttribute<T : TCustomAttribute>(AProp : TRttiProperty) : T;overload;
//    class function ContainsAttribute<T : TCustomAttribute>(AClass : TClass) : T;overload;
//    class function GetAttributes(AClass : TClass)
//    class function ContainsAttribute<T : TCustomAttribute>(AProp : TRttiPropertyEx) : T;overload;
//    class function GetPrimaryKey(AObj : TObject) : TObject;
    class function GetProperties(AClass: TClass; APredicate: TFunc<TRttiProperty, Boolean>): TArray<TRttiProperty>;overload;
    class function GetProperties(AClass: TClass): TArray<TRttiProperty>;overload;
//    class function GetPropertyByColumn(AColumnName : string; AClass : TClass) : TRttiProperty;
//    class function GetProperty(AClass : TClass; AProperty : string) : TRttiProperty;
//    class function GetColumns(ATable : IMiniORMTable; APredicate : TPredicate<IMiniORMColumn> = nil; AWithPrefix : Boolean = True) : string;overload;
//    class function GetColumns(ATableJoin : IMiniORMTableJoin; APredicate : TPredicate<IMiniORMColumn> = nil; AWithPrefix : Boolean = True) : string;overload;
//    class function IIF<T>(ACondition : Boolean; R1, R2 : T) : T;
//    class function Invoke(AObj : TObject; AMethod : string; const Args : TArray<TValue>) : TValue;
//    class function GetRecordFieldValue(AFieldName : string; AObj : TValue; AProp : TRttiProperty) : TValue;
//    class function SetRecordFieldValue(AFieldName: string; AObj: TObject; AProp: TRttiProperty; AValue : TValue) : TValue;
//    class function GetObjectState(AObj : TObject) : TMiniORMObjectState;
//    class procedure SetObjectState(AObj : TObject; AState : TMiniORMObjectState);
//    class function MD5(AValue : string) : string;
//    class function GetValue(AObj : TObject; AProp : TRttiProperty) : TValue;
//    class function GetValues(AObj : TObject; AProp : TRttiProperty) : TArray<TValue>;
//    class procedure SetValue(AObj : TObject; AProp : TRttiProperty; AValue : TValue);
//    class function GetPKObj(AObj : TObject) : TObject;
    class function GetRttiContext : TRttiContext;
//    class function IsObjectOrInterface(AValue : TValue) : Boolean;
//    class function ValueToObject(AValue : TValue) : TObject;
//    class function IsPKObject(AObj : TObject; AColumn : IMiniORMColumn) : Boolean;
//    class procedure SetOwner(AObj, AOwner : TObject);
//    class function GetParentClass(AProp : TRttiProperty) : TClass;
//    class function GetClass(AProp : TRttiProperty) : TClass;
    //class function GetValueByPath(AObj : TObject; APath : string) : TValue;
    //class procedure SetValueByPath(AObj : TObject; AValue : TValue; APath : string);
  end;

implementation

uses MiniREST.ORM.Attribute;

{ TMiniRESTORMUtils }

class constructor TMiniRESTORMUtils.Create;
begin
  FCx := TRttiContext.Create;
  FCx.KeepContext;
end;

class destructor TMiniRESTORMUtils.Destroy;
begin
  FCx.DropContext;
end;

class function TMiniRESTORMUtils.GetAttribute<T>(AClass: TClass): T;
var
  LType : TRttiType;
  LAttr : TCustomAttribute;
begin
  Result := nil;
  LType := FCx.GetType(AClass);
  for LAttr in LType.GetAttributes do
    if LAttr is T then
      Exit(T(LAttr));
end;

class function TMiniRESTORMUtils.GetProperties(AClass: TClass;
  APredicate: TFunc<TRttiProperty, Boolean>): TArray<TRttiProperty>;
var
  LProp : TRttiProperty;
begin
  SetLength(Result, 0);
  for LProp in FCx.GetType(AClass).GetProperties do
  begin
    if LProp.Name = 'RefCount' then
      Continue;
    if TMiniRESTORMUtils.GetAttribute<TransientAttribute>(LProp) <> nil then
      Continue;
    if (Assigned(APredicate)) then
    begin
      if (APredicate(LProp)) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := LProp;
      end;
    end
    else
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := LProp;
    end;
  end;
end;

class function TMiniRESTORMUtils.GetAttribute<T>(
  ARttiProperty: TRttiProperty): T;
var
  LAttr : TCustomAttribute;
begin
  Result := nil;
  for LAttr in ARttiProperty.GetAttributes do
    if LAttr is T then
      Exit(T(LAttr));
end;

class function TMiniRESTORMUtils.GetProperties(
  AClass: TClass): TArray<TRttiProperty>;
begin

end;

class function TMiniRESTORMUtils.GetRttiContext: TRttiContext;
begin
  Result := FCx;
end;

end.
