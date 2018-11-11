unit MiniREST.ORM.Utils;

interface

uses Classes, SysUtils, Rtti, MiniREST.ORM.Intf, IdHashMessageDigest, idHash;

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
//    class function GetColumns(ATable : IMiniRESTORMTable; APredicate : TPredicate<IMiniRESTORMColumn> = nil; AWithPrefix : Boolean = True) : string;overload;
//    class function GetColumns(ATableJoin : IMiniRESTORMTableJoin; APredicate : TPredicate<IMiniRESTORMColumn> = nil; AWithPrefix : Boolean = True) : string;overload;
//    class function IIF<T>(ACondition : Boolean; R1, R2 : T) : T;
//    class function Invoke(AObj : TObject; AMethod : string; const Args : TArray<TValue>) : TValue;
    class function GetRecordFieldValue(const AFieldName: string; AObj: TValue; AProp: TRttiProperty) : TValue;
//    class function SetRecordFieldValue(AFieldName: string; AObj: TObject; AProp: TRttiProperty; AValue : TValue) : TValue;
//    class function GetObjectState(AObj : TObject) : TMiniRESTORMObjectState;
//    class procedure SetObjectState(AObj : TObject; AState : TMiniRESTORMObjectState);
    class function MD5(const AValue : string): string;
    class function GetValue(AObj : TObject; AProp : TRttiProperty) : TValue;
//    class function GetValues(AObj : TObject; AProp : TRttiProperty) : TArray<TValue>;
    class procedure SetValue(AObj : TObject; AProp : TRttiProperty; AValue : TValue);
    class function GetPKObj(AObj : TObject) : TObject;
    class function GetRttiContext : TRttiContext;
//    class function IsObjectOrInterface(AValue : TValue) : Boolean;
    class function ValueToObject(AValue: TValue): TObject;
    class function IsPKObject(AObj : TObject; AColumn : IMiniRESTORMColumn) : Boolean;
//    class procedure SetOwner(AObj, AOwner : TObject);
//    class function GetParentClass(AProp : TRttiProperty) : TClass;
//    class function GetClass(AProp : TRttiProperty) : TClass;
    //class function GetValueByPath(AObj : TObject; APath : string) : TValue;
    //class procedure SetValueByPath(AObj : TObject; AValue : TValue; APath : string);
  end;

implementation

uses MiniREST.ORM.Attribute, MiniREST.Nullable;

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

class function TMiniRESTORMUtils.GetPKObj(AObj: TObject): TObject;
var
  LProp : TRttiProperty;
begin
  Result := nil;
  for LProp in TMiniRESTORMUtils.GetProperties(AObj.ClassType) do
    if (TMiniRESTORMUtils.GetAttribute<IDAttribute>(LProp) <> nil) then
    begin
      if LProp.PropertyType.IsInstance then
        Exit(LProp.GetValue(AObj).AsObject)
      else
      if LProp.PropertyType.TypeKind = tkInterface then
        Exit(TObject(LProp.GetValue(AObj).AsInterface));
    end;
end;

class function TMiniRESTORMUtils.GetProperties(
  AClass: TClass): TArray<TRttiProperty>;
begin
  Result := GetProperties(AClass, nil);
end;

class function TMiniRESTORMUtils.GetRecordFieldValue(const AFieldName: string;
  AObj: TValue; AProp: TRttiProperty): TValue;
var
  LValue, LValueAux : TValue;
  LType : TRttiType;
begin
  try
    if AObj.IsObject then
    begin
      LValue := AProp.GetValue(AObj.AsObject);
      LValueAux := AProp.PropertyType.AsRecord.GetField(AFieldName).GetValue(LValue.GetReferenceToRawData);
      TValue.Make(LValueAux.GetReferenceToRawData,LValueAux.TypeInfo,Result);
    end
    else
    if AObj.Kind = tkRecord then
    begin
      LType := GetRttiContext.GetType(AObj.TypeInfo);
      LValue := LType.AsRecord.GetField(AFieldName).GetValue(AObj.GetReferenceToRawData);
      TValue.Make(LValue.GetReferenceToRawData,LType.AsRecord.GetField(AFieldName).FieldType.Handle,Result);
    end;
  except
    Result := nil;
  end;
end;

class function TMiniRESTORMUtils.GetRttiContext: TRttiContext;
begin
  Result := FCx;
end;

class function TMiniRESTORMUtils.GetValue(AObj: TObject;
  AProp: TRttiProperty): TValue;
begin
  { TODO : Revisar Nullable }
  if Pos('MiniRESTORMLazy<',AProp.PropertyType.Name) > 0 then
  begin
    if TMiniRESTORMUtils.GetRecordFieldValue('FLoaded',AObj,AProp).AsBoolean then
    begin
      if (Pos('Nullable<',AProp.PropertyType.Name) > 0) then
      begin
         if Length(TMiniRESTORMUtils.GetRecordFieldValue('fHasValue',TMiniRESTORMUtils.GetRecordFieldValue('fValue',AObj,AProp),AProp).AsString) > 0 then
          Exit(TMiniRESTORMUtils.GetRecordFieldValue('fValue',TMiniRESTORMUtils.GetRecordFieldValue('fValue',AObj,AProp),AProp))
        else
          Exit(nil);
      end;
      Exit(TMiniRESTORMUtils.GetRecordFieldValue('fValue',AObj,AProp))
    end
    else
      Exit(nil);
  end;
  if Pos('Nullable<',AProp.PropertyType.Name) > 0 then
  begin
    if Length(TMiniRESTORMUtils.GetRecordFieldValue('fHasValue',AObj,AProp).AsString) > 0 then
      Exit(TMiniRESTORMUtils.GetRecordFieldValue('fValue',AObj,AProp))
    else
      Exit(nil);
  end;
  Result := AProp.GetValue(AObj);
end;

class function TMiniRESTORMUtils.IsPKObject(AObj: TObject;
  AColumn: IMiniRESTORMColumn): Boolean;
begin
  Result := False;
  if AColumn.GetParentPKProperty <> nil then
  begin
    if AColumn.GetParentPKProperty.PropertyType.IsInstance then
      Result := AObj.ClassType = AColumn.GetParentPKProperty.PropertyType.AsInstance.MetaclassType
    else
    if AColumn.GetParentPKProperty.PropertyType.TypeKind = tkInterface then
      Result := Supports(AObj.ClassType, TRttiInterfaceType(AColumn.GetParentPKProperty.PropertyType).GUID);
  end;
end;

class function TMiniRESTORMUtils.MD5(const AValue: string): string;
var
  idmd5 : TIdHashMessageDigest5;
  hash : T4x4LongWordRecord;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  try
    result := idmd5.HashStringAsHex(AValue) ;
  finally
    idmd5.Free;
  end;
end;

class procedure TMiniRESTORMUtils.SetValue(AObj: TObject; AProp: TRttiProperty;
  AValue: TValue);
var
  LValue : TValue;
begin
  { TODO : Revisar Nullable }
  if Pos('MiniRESTORMLazy<',AProp.PropertyType.Name) > 0 then
  begin
    raise Exception.Create('TMiniRESTORMUtils.SetValue: Set MiniRESTORMLazy não implementado.');
    (*if TMiniRESTORMUtils.GetRecordFieldValue('FLoaded',AObj,AProp).AsBoolean then
    begin
      if (Pos('Nullable<',AProp.PropertyType.Name) > 0) then
      begin
        AProp.GetValue(AObj).SetNullableValue(AValue);
        {if Length(TMiniRESTORMUtils.GetRecordFieldValue('fHasValue',TMiniRESTORMUtils.GetRecordFieldValue('fValue',AObj,AProp),AProp).AsString) > 0 then
          Exit(TMiniRESTORMUtils.GetRecordFieldValue('fValue',TMiniRESTORMUtils.GetRecordFieldValue('fValue',AObj,AProp),AProp))
        else
          Exit(nil);}
      end
      else
      Exit(TMiniRESTORMUtils.GetRecordFieldValue('fValue',AObj,AProp))
    end
    else
      Exit(nil);*)
  end;
  if Pos('Nullable<',AProp.PropertyType.Name) > 0 then
  begin
    LValue := AProp.GetValue(AObj);
    LValue.SetNullableValue(AValue);
    AProp.SetValue(AObj, LValue);
    {if Length(TMiniRESTORMUtils.GetRecordFieldValue('fHasValue',AObj,AProp).AsString) > 0 then
      Exit(TMiniRESTORMUtils.GetRecordFieldValue('fValue',AObj,AProp))
    else
      Exit(nil);}
  end
  else
    AProp.SetValue(AObj, AValue);
end;

class function TMiniRESTORMUtils.ValueToObject(AValue: TValue): TObject;
begin
  if AValue.IsObject then
    Result := AValue.AsObject
  else
  if AValue.Kind = tkInterface then
    Result := TObject(AValue.AsInterface);
end;

end.
