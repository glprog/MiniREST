unit MiniREST.ORM.Mapper;

interface

uses SysUtils, Generics.Collections, Rtti, MiniREST.ORM.Intf,
  MiniREST.ORM.Attribute, MiniREST.ORM.Common, MiniREST.ORM.Table,
  MiniREST.ORM.Column;

type
  TMiniRESTORMMapper = class
  strict private
    class var FCountAux: Integer;
    class var FClassDictionary: TDictionary<string, IMiniRESTORMTable>;
//    class var FClassDictionary : IDictionary<string, IMiniORMTable>;
//    //class var FManyToManyDictionary : IDictionary<string, IMiniORMTable>;
    class var FIntfDictionary: TDictionary<string, TClass>;
//    class var FCountAux : integer;
    class function FillTable(AProps : TArray<TRttiProperty>; ATable : IMiniRESTORMTable; AIsPrimaryKey : Boolean = false; AParentPkProperty : TRttiProperty = nil): IMiniRESTORMTable;
    class function FillColumn(AColumn : IMiniRESTORMColumn{; AProp : TRttiProperty}): IMiniRESTORMColumn;
//    class function ContainsObjectState(AClass : TClass) : Boolean;
    class procedure FillManyToOneRelation(AOwnerTable: IMiniRESTORMTable; ARttiProp,
    AParentPKProperty : TRttiProperty; const AIsPrimaryKey : Boolean);
    class procedure FillManyToManyRelation(AOwnerTable: IMiniRESTORMTable; ARttiProp,
    AParentPKProperty : TRttiProperty; const AIsPrimaryKey : Boolean);
    class function RegisterInterface(AClass : TClass) : Boolean;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterClass(AClass: TClass);
    class function GetTables: TArray<IMiniRESTORMTable>;
    class function GetTable(AClass : TClass): IMiniRESTORMTable;
    //class function CheckInterface(AClass : TClass; AGUID : string) : Boolean;
    //class function GetClassByInterface(AGUID : string) : TClass;
  end;

implementation

uses MiniREST.ORM.Utils;

{ TMiniRESTORMMapper }

class constructor TMiniRESTORMMapper.Create;
begin
  FClassDictionary := TDictionary<string, IMiniRESTORMTable>.Create;
  FIntfDictionary := TDictionary<string, TClass>.Create;
end;

class destructor TMiniRESTORMMapper.Destroy;
begin
  FClassDictionary.Free;
  FIntfDictionary.Free;
end;

class function TMiniRESTORMMapper.FillColumn(
  AColumn: IMiniRESTORMColumn): IMiniRESTORMColumn;
var LColumnAttr : ColumnAttribute;
    LSizeAttr : SizeAttribute;
    LPrecisionAttr : PrecisionAttribute;
    LScaleAttr : ScaleAttribute;
    LColumnTypeAttr : ColumnTypeAttribute;
    LProp, LParentPKProperty : TRttiProperty;
    LPath : string;
    function PropertyTypeToColumnType(AProp2 : TRttiType) : TMiniRESTORMColumnType;
    begin
      if (Pos('Nullable<',AProp2.Name) > 0) or (Pos('MiniORMLazy<',AProp2.Name) > 0) then
        Exit(PropertyTypeToColumnType(AProp2.AsRecord.GetField('FValue').FieldType));
      case AProp2.TypeKind of
        tkString,tkUString : Result := ctString;
        tkInteger : Result := ctInteger;
        tkInt64 : Result := ctBigInteger;
        tkFloat :
        begin
          if AProp2.Name = 'TDate' then
            Result := ctDate
          else
          if AProp2.Name = 'TDateTime' then
            Result := ctDateTime
          else
            Result := ctDecimal;
        end;
      end;
    end;
begin
  if AColumn.GetRttiProperty.PropertyType.TypeKind in [tkClass, tkInterface] then
    raise Exception.Create('TMiniRESTORMMapper.FillColumn: Tipo não suportado ' + AColumn.GetRttiProperty.PropertyType.QualifiedName);
  LProp := AColumn.GetRttiProperty;
  LColumnAttr := TMiniRESTORMUtils.GetAttribute<ColumnAttribute>(LProp);
  LSizeAttr := TMiniRESTORMUtils.GetAttribute<SizeAttribute>(LProp);
  LPrecisionAttr := TMiniRESTORMUtils.GetAttribute<PrecisionAttribute>(LProp);
  LScaleAttr := TMiniRESTORMUtils.GetAttribute<ScaleAttribute>(LProp);
  LColumnTypeAttr := TMiniRESTORMUtils.GetAttribute<ColumnTypeAttribute>(LProp);
  if LColumnAttr <> nil then
    AColumn.SetName(LColumnAttr.Name)
  else
    AColumn.SetName(LProp.Name);
  if LSizeAttr <> nil then
    AColumn.SetSize(LSizeAttr.Value);
  if LPrecisionAttr <> nil then
    AColumn.SetPrecision(LPrecisionAttr.Value);
  if LScaleAttr <> nil then
    AColumn.SetScale(LScaleAttr.Value);
  if LColumnTypeAttr <> nil then
    AColumn.SetType(LColumnTypeAttr.Value)
  else
    AColumn.SetType(PropertyTypeToColumnType(LProp.PropertyType));
  AColumn.SetOnGetValue(
  function (AColumn : IMiniRESTORMColumn; AObj : TObject) : TValue
  var LObj : TObject;
  begin
    LObj := AObj;
    if (AColumn.GetIsPrimaryKey) then
    begin
      LObj := TMiniRESTORMUtils.GetPKObj(AObj);
      if LObj = nil then
        LObj := AObj;
    end;
    Result := TMiniRESTORMUtils.GetValue(LObj, AColumn.GetRttiProperty);
    //Result := AColumn.GetRttiProperty.GetValue(AObj);
  end);
  AColumn.SetOnSetValue(
  procedure (AColumn : IMiniRESTORMColumn; AObj : TObject; AValue : TValue)
  var LObj : TObject;
      LParentColumn : IMiniRESTORMColumn;
      LAux : TValue;
      LColumn : IMiniRESTORMColumn;
      //LProp : TRttiProperty;
  begin
    LObj := AObj;
    if (AColumn.GetParentPKProperty <> nil) and (not TMiniRESTORMUtils.IsPKObject(LObj, AColumn)) then
    begin
      LAux := AColumn.GetParentPKProperty.GetValue(LObj);
      if LAux.IsObject then
        LObj := LAux.AsObject
      else
      if LAux.Kind = tkInterface then
        LObj := TObject(LAux.AsInterface)
      else
      begin
        TMiniRESTORMUtils.SetValue(LObj, AColumn.GetRttiProperty, AValue);
        Exit;
      end;
    end;

    LAux := TMiniRESTORMUtils.GetValue(LObj, AColumn.GetRttiProperty);
    LParentColumn := AColumn.GetParentColumn;
    LColumn := AColumn;
    if (LParentColumn <> nil) and (LAux.IsObject or (LAux.Kind = tkInterface)) and (not LAux.IsEmpty) then
    begin
      repeat
        //if LAux.IsObject or (LAux.Kind = tkInterface) then
        LObj := TMiniRESTORMUtils.ValueToObject(LAux);
        LColumn := LParentColumn;
        {if LAux.Kind = tkInterface then
          LObj := TObject(LAux.AsInterface)
        else
          LObj := LAux.AsObject;}
        if LParentColumn.GetParentPKProperty <> nil then
        begin
          LAux := TMiniRESTORMUtils.GetValue(LObj, LParentColumn.GetParentPKProperty);
          if LAux.IsObject or (LAux.Kind = tkInterface)  then
          begin
            if LAux.IsObject then
              LObj := LAux.AsObject
            else
              LObj := TObject(LAux.AsInterface);
          end
          else
          begin
            TMiniRESTORMUtils.SetValue(LObj, LParentColumn.GetParentPKProperty, AValue);
            Exit;
          end;
        end;
        LAux := TMiniRESTORMUtils.GetValue(LObj, LParentColumn.GetRttiProperty);
        {if (LAux.IsObject) or (LAux.Kind = tkInterface) then
          LObj := TMiniRESTORMUtils.ValueToObject(LAux);}
        LParentColumn := LParentColumn.GetParentColumn;
      until (LParentColumn = nil) or (LAux.IsEmpty) or (not (LAux.IsObject or (LAux.Kind = tkInterface)));
    end;
    //else
    //if LParentColumn <> nil then
    //  AColumn := LParentColumn;
//    if  then

    TMiniRESTORMUtils.SetValue(LObj, LColumn.GetRttiProperty, AValue);
  end);
  AColumn.SetIsLazy(Pos('MiniRESTORMLazy<',LProp.PropertyType.Name) > 0);
  AColumn.SetIsNullable(Pos('Nullable<', LProp.PropertyType.Name) > 0);
  LParentPKProperty := AColumn.GetParentPKProperty;
  if LParentPKProperty <> nil then
    LPath := LParentPKProperty.Name + '.' + AColumn.GetRttiProperty.Name
  else
    LPath := AColumn.GetRttiProperty.Name;
//  LPath := TMiniRESTORMUtils.IIF<string>(LParentPKProperty <> nil, LParentPKProperty.Name + '.', '') +
  //AColumn.GetRttiProperty.Name;
  AColumn.SetIsOneToMany(TMiniRESTORMUtils.GetAttribute<OneToManyAttribute>(AColumn.GetRttiProperty) <> nil);
  AColumn.SetIsManyToOne(TMiniRESTORMUtils.GetAttribute<ManyToOneAttribute>(AColumn.GetRttiProperty) <> nil);
  AColumn.SetIsManyToMany(TMiniRESTORMUtils.GetAttribute<ManyToManyAttribute>(AColumn.GetRttiProperty) <> nil);
  AColumn.SetPath(LPath);
  Result := AColumn;
end;

class procedure TMiniRESTORMMapper.FillManyToManyRelation(
  AOwnerTable: IMiniRESTORMTable; ARttiProp, AParentPKProperty: TRttiProperty;
  const AIsPrimaryKey: Boolean);
begin
  raise Exception.Create('Not implemented');
end;

class procedure TMiniRESTORMMapper.FillManyToOneRelation(
  AOwnerTable: IMiniRESTORMTable; ARttiProp, AParentPKProperty: TRttiProperty;
  const AIsPrimaryKey: Boolean);
begin
  raise Exception.Create('Not implemented');
end;

class function TMiniRESTORMMapper.FillTable(AProps: TArray<TRttiProperty>;
  ATable: IMiniRESTORMTable; AIsPrimaryKey: Boolean;
  AParentPkProperty: TRttiProperty): IMiniRESTORMTable;
var
  LRttiProp: TRttiProperty;
  LName: string;
  LClassAux: TClass;
  LTableJoin: IMiniREsTORMTableJoin;
  LColumnJoin: IMiniRESTORMColumnJoin;
  LJoinAttr: JoinColumnsAttribute;
  LColumnTemp: IMiniRESTORMColumn;
  LTable, LControlTable: IMiniRESTORMTable;
  LClass: TClass;
  LRttiProps: TArray<TRttiProperty>;
  LRttiContext: TRttiContext;
  LColumn: IMiniRESTORMColumn;
begin
  LRttiContext := TMiniRESTORMUtils.GetRttiContext;
  ATable.SetTablePrefix('T' + IntToStr(FCountAux));
  Inc(FCountAux);
  for LRttiProp in AProps do
  begin
    SetLength(LRttiProps, 0);
    if (LRttiProp.Name = 'RefCount') then
      Continue;
    if (TMiniRESTORMUtils.GetAttribute<TransientAttribute>(LRttiProp) <> nil) then
      Continue;
    if (TMiniRESTORMUtils.GetAttribute<IDAttribute>(LRttiProp) <> nil) then
    begin
      if (LRttiProp.PropertyType.IsInstance) then
      begin
        LRttiProps := LRttiContext.GetType(LRttiProp.PropertyType.Handle).GetProperties;
        FillTable(LRttiProps, ATable, True, LRttiProp);
      end
      {else
      if LProp.PropertyType.TypeKind = tkInterface then
      begin
        if not FIntfDictionary.TryGetValue(TRttiInterfaceType(LProp.PropertyType).GUID.ToString, LClass) then
          raise Exception.Create('TMiniRESTORMMapper.FillTable: Interface ' + LProp.PropertyType.QualifiedName + ' não registrada.');
        FillTable(TMiniRESTORMUtils.GetRttiContext.GetType(LClass).GetProperties,ATable,True, LProp);
      end }
      else
      begin
        LColumn := TMiniRESTORMColumn.Create;
        LColumn.SetRttiProperty(LRttiProp);
        LColumn.SetParentPKProperty(AParentPkProperty);
        LColumn.SetIsPrimaryKey((TMiniRESTORMUtils.GetAttribute<IDAttribute>(LRttiProp) <> nil) or AIsPrimaryKey);
        LColumn.SetTablePrefix(ATable.GetTablePrefix);
        FillColumn(LColumn);
        ATable.AddColumn(LColumn);
        //ATable.GetColumns.Add(FillColumn(TMiniRESTORMColumn.New((LProp.ContainsAttribute<IDAttribute> <> nil) or (IsPrimaryKey), LProp, AParentPkProperty).SetTablePrefix(ATable.GetTablePrefix)));
      end;
    end
    else
    if (TMiniRESTORMUtils.GetAttribute<OneToManyAttribute>(LRttiProp) = nil) and
      (TMiniRESTORMUtils.GetAttribute<ManyToOneAttribute>(LRttiProp) = nil) and
      (TMiniRESTORMUtils.GetAttribute<ManyToManyAttribute>(LRttiProp) = nil) then
    begin
      LColumn := TMiniRESTORMColumn.Create;
      LColumn.SetRttiProperty(LRttiProp);
      LColumn.SetParentPKProperty(AParentPkProperty);
      LColumn.SetIsPrimaryKey((TMiniRESTORMUtils.GetAttribute<IDAttribute>(LRttiProp) <> nil) or AIsPrimaryKey);
      LColumn.SetTablePrefix(ATable.GetTablePrefix);
      FillColumn(LColumn);
      ATable.AddColumn(LColumn);
      //ATable.GetColumns.Add(FillColumn(TMiniRESTORMColumn.New((LProp.ContainsAttribute<IDAttribute> <> nil) or (IsPrimaryKey), LProp, AParentPkProperty).SetTablePrefix(ATable.GetTablePrefix)));
    end;

    if (TMiniRESTORMUtils.GetAttribute<ManyToOneAttribute>(LRttiProp) <> nil)
      or (TMiniRESTORMUtils.GetAttribute<ManyToManyAttribute>(LRttiProp) <> nil) then
    begin
      if (TMiniRESTORMUtils.GetAttribute<ManyToOneAttribute>(LRttiProp) <> nil) then
      begin
        FillManyToOneRelation(ATable, LRttiProp, AParentPkProperty, AIsPrimaryKey);
      end
      else
      if (TMiniRESTORMUtils.GetAttribute<ManyToManyAttribute>(LRttiProp) <> nil) then
      begin
        FillManyToManyRelation(ATable, LRttiProp, AParentPkProperty, AIsPrimaryKey);
      end;
    end;
  end;
  Result := ATable;
end;

class function TMiniRESTORMMapper.GetTable(AClass: TClass): IMiniRESTORMTable;
begin
  raise Exception.Create('Not implemented');
end;

class function TMiniRESTORMMapper.GetTables: TArray<IMiniRESTORMTable>;
begin
  Result := FClassDictionary.Values.ToArray;
end;

class procedure TMiniRESTORMMapper.RegisterClass(AClass: TClass);
var
  LTableAttr : TableAttribute;
  LRttiProperty : TRttiProperty;
  LTable : IMiniRESTORMTable;
begin
  if FClassDictionary.ContainsKey(AClass.QualifiedClassName) then
    Exit;
  LTableAttr := nil;
  LTableAttr := TMiniRESTORMUtils.GetAttribute<TableAttribute>(AClass);
  if not Assigned(LTableAttr) then
    raise MiniRESTORMException.CreateFmt('The class %s does not contain table attribute',[AClass.ClassName]);
  { TODO : Verificar workaround fObjectState }
  //if not ContainsObjectState(AClass) then
  //  raise MiniRESTORMException.CreateFmt('A classe %s não contém o field fObjectState',[AClass.ClassName]);
  { TODO : Ver isso aqui }
  //if TDPA2Utils.ContainsAttribute<ID>(AClass) = nil then
  //  raise DPA2Exception.CreateFmt('A classe %s não contém o atributo ID',[AClass.ClassName]);
  { TODO : Criar verificação --> verificar se a classe tem somente 1 PK }
  LTable := TMiniRESTORMTable.Create;
  LTable.SetName(LTableAttr.Name);
  FClassDictionary.Add(AClass.QualifiedClassName, LTable);
  FillTable(TMiniRESTORMUtils.GetProperties(AClass), LTable);
  RegisterInterface(AClass);
  //  FClassDictionary.Add(AClass.QualifiedClassName, FillTable(TMiniRESTORMUtils.GetProperties(AClass),LDPATable));
end;

class function TMiniRESTORMMapper.RegisterInterface(AClass: TClass): Boolean;
var LIntfTable : PInterfaceTable;
    LIntfEntry : TInterfaceEntry;
    LCount, I : Integer;
begin
  LIntfTable := AClass.GetInterfaceTable;
  if LIntfTable <> nil then
  begin
    LCount := LIntfTable.EntryCount;
    for I := 0 to Pred(LCount) do
    begin
      LIntfEntry := LIntfTable.Entries[I];
      FIntfDictionary.Add(LIntfEntry.IID.ToString, AClass);
    end;
  end;
end;

end.
