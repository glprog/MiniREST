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
//    class var FIntfDictionary : IDictionary<string, TClass>;
//    class var FCountAux : integer;
    class function FillTable(AProps : TArray<TRttiProperty>; ATable : IMiniRESTORMTable; AIsPrimaryKey : Boolean = false; AParentPkProperty : TRttiProperty = nil): IMiniRESTORMTable;
//    class function FillColumn(AColumn : IMiniORMColumn{; AProp : TRttiProperty}) : IMiniORMColumn;
//    class function ContainsObjectState(AClass : TClass) : Boolean;
    class procedure FillManyToOneRelation(AOwnerTable: IMiniRESTORMTable; ARttiProp,
    AParentPKProperty : TRttiProperty; const AIsPrimaryKey : Boolean);
    class procedure FillManyToManyRelation(AOwnerTable: IMiniRESTORMTable; ARttiProp,
    AParentPKProperty : TRttiProperty; const AIsPrimaryKey : Boolean);
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterClass(AClass: TClass);
    class function GetTables: TArray<IMiniRESTORMTable>;
    class function GetTable(AClass : TClass): IMiniRESTORMTable;
    //class function RegisterInterface(AClass : TClass) : Boolean;
    //class function CheckInterface(AClass : TClass; AGUID : string) : Boolean;
    //class function GetClassByInterface(AGUID : string) : TClass;
  end;

implementation

uses MiniREST.ORM.Utils;

{ TMiniRESTORMMapper }

class constructor TMiniRESTORMMapper.Create;
begin
  FClassDictionary := TDictionary<string, IMiniRESTORMTable>.Create;
end;

class destructor TMiniRESTORMMapper.Destroy;
begin
  FClassDictionary.Free;
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
          raise Exception.Create('TMiniORMMapper.FillTable: Interface ' + LProp.PropertyType.QualifiedName + ' não registrada.');
        FillTable(TMiniORMUtils.GetRttiContext.GetType(LClass).GetProperties,ATable,True, LProp);
      end }
      else
      begin
        LColumn := TMiniRESTORMColumn.Create;
        LColumn.SetRttiProperty(LRttiProp);
        LColumn.SetParentPKProperty(AParentPkProperty);
        LColumn.SetIsPrimeryKey((TMiniRESTORMUtils.GetAttribute<IDAttribute>(LRttiProp) <> nil) or AIsPrimaryKey);
        LColumn.SetTablePrefix(ATable.GetTablePrefix);
        ATable.AddColumn(LColumn);
        //ATable.GetColumns.Add(FillColumn(TMiniORMColumn.New((LProp.ContainsAttribute<IDAttribute> <> nil) or (IsPrimaryKey), LProp, AParentPkProperty).SetTablePrefix(ATable.GetTablePrefix)));
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
      LColumn.SetIsPrimeryKey((TMiniRESTORMUtils.GetAttribute<IDAttribute>(LRttiProp) <> nil) or AIsPrimaryKey);
      LColumn.SetTablePrefix(ATable.GetTablePrefix);
      ATable.AddColumn(LColumn);
      //ATable.GetColumns.Add(FillColumn(TMiniORMColumn.New((LProp.ContainsAttribute<IDAttribute> <> nil) or (IsPrimaryKey), LProp, AParentPkProperty).SetTablePrefix(ATable.GetTablePrefix)));
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
  raise Exception.Create('Not implemented');
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
    raise MiniRESTORMException.CreateFmt('A classe %s não contém o atributo Table',[AClass.ClassName]);
  { TODO : Verificar workaround fObjectState }
  //if not ContainsObjectState(AClass) then
  //  raise MiniORMException.CreateFmt('A classe %s não contém o field fObjectState',[AClass.ClassName]);
  { TODO : Ver isso aqui }
  //if TDPA2Utils.ContainsAttribute<ID>(AClass) = nil then
  //  raise DPA2Exception.CreateFmt('A classe %s não contém o atributo ID',[AClass.ClassName]);
  { TODO : Criar verificação --> verificar se a classe tem somente 1 PK }
  LTable := TMiniRESTORMTable.Create;
  LTable.SetName(LTableAttr.Name);
  FClassDictionary.Add(AClass.QualifiedClassName, LTable);
  FillTable(TMiniRESTORMUtils.GetProperties(AClass), LTable);
  //RegisterInterface(AClass);
  //  FClassDictionary.Add(AClass.QualifiedClassName, FillTable(TMiniORMUtils.GetProperties(AClass),LDPATable));
end;

end.
