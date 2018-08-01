unit MiniREST.JSON;

interface

uses SysUtils, Rtti, Generics.Collections, Contnrs, JsonDataObjects,
  Variants, VarUtils{$IF CompilerVersion > 22} ,
  REST.JsonReflect, System.JSON {$ELSE} ,DBXJson{$IFEND}, DB, DateUtils;

type
  {$IF DEFINED(VER310) OR DEFINED(VER290)}
  TMiniRESTJsonInterceptor = class(TJSONInterceptor)
  public
    function ObjectsConverter(Data: TObject; Field: string): TListOfObjects; override;
    procedure ObjectsReverter(Data: TObject; Field: string; Args: TListOfObjects); override;
  end;

  TMiniRESTJsonInterceptorClass = class of TMiniRESTJsonInterceptor;
  MiniRESTJsonAttribute = class(JsonReflectAttribute)
  private
    FObjClass : TClass;
  public
    constructor Create(AObjClass : TClass; ConverterType: TConverterType; ReverterType: TReverterType); overload;
    property ObjClass : TClass read FObjClass;
  end;
  {$IFEND}

  TMiniRESTJSON = class
    class function TratarJsonString(AJson : string) : string; deprecated;
    class function DatasetToJson(ADataset : TDataset) : string; deprecated 'Use DatasetToJson2 instead';
    class function DatasetToJson2(ADataset : TDataset) : string;
    class function TratarJSONArray(AJSON : string) : string;
  end;

  MiniRESTJsonNullAttribute = class(TCustomAttribute)
  end;

  MiniRESTJsonDateAttribute = class(TCustomAttribute)
  end;

  MiniRESTJsonTimeAttribute = class(TCustomAttribute)
  end;

implementation

uses MiniREST.Util;

{ TMiniRESTJsonInterceptor }
{$IF DEFINED(VER310) OR DEFINED(VER290)}
function TMiniRESTJsonInterceptor.ObjectsConverter(Data: TObject;
  Field: string): TListOfObjects;
var LContext : TRttiContext;
    LType : TRttiType;
    LField : TRttiField;
    LMethod : TRttiMethod;
    LObjectList : TObjectList;
    I : Integer;
    LValue : TValue;
begin
  LContext := TRttiContext.Create;
  LType := LContext.GetType(Data.ClassType);
  LField := LType.GetField(Field);
  if LField.FieldType.Name.StartsWith('TObjectList<') then
  begin
    LMethod := LContext.GetType(LField.GetValue(Data).AsObject.ClassType).GetMethod('ToArray');
    LValue := LMethod.Invoke(LField.GetValue(Data),[]);
    for I := 0 to LValue.GetArrayLength - 1 do
      Result := Result + [LValue.GetArrayElement(I).AsObject];
  end
  else
    raise Exception.Create('Tipo não suportado'); { TODO : Melhorar msg }
end;

procedure TMiniRESTJsonInterceptor.ObjectsReverter(Data: TObject; Field: string;
  Args: TListOfObjects);
var LContext : TRttiContext;
    LType : TRttiType;
    LField : TRttiField;
    LMethod : TRttiMethod;
    I : Integer;
begin
  LContext := TRttiContext.Create;
  LType := LContext.GetType(Data.ClassType);
  LField := LType.GetField(TMiniRESTJsonUtil.ConvertFieldNameFromJson(Data,Field));
  if LField.FieldType.Name.StartsWith('TObjectList<') then
  begin
    LMethod := LField.FieldType.GetMethod('Add');
    for I := 0 to Length(Args) - 1 do
      LMethod.Invoke(LField.GetValue(Data).AsObject,[Args[I]]);
  end
  else
    raise Exception.Create('Tipo não suportado'); { TODO : Melhorar msg }
end;


{ MiniRESTJsonAttribute }

constructor MiniRESTJsonAttribute.Create(AObjClass: TClass;
  ConverterType: TConverterType; ReverterType: TReverterType);
begin
  inherited Create(ConverterType, ReverterType, TMiniRESTJsonInterceptor);
  FObjClass := AObjClass;
end;
{$IFEND}
{ TMiniRESTJSON }

class function TMiniRESTJSON.DatasetToJson(ADataset: TDataset): string;
var LField : TField;
    LJSONValue : TJSONValue;
    LJSONObject : TJSONObject;
    LJSONArray : TJSONArray;
begin
  ADataset.First;
  LJSONArray := TJSONArray.Create;
  try
    while not ADataset.Eof do
    begin
      LJSONObject := TJSONObject.Create;
      for LField in ADataset.Fields do
      begin
        case LField.DataType of
          ftSmallint, ftSingle, ftInteger, ftWord, ftShortint : LJSONValue := TJSONNumber.Create(LField.AsInteger);
          ftLargeint : LJSONValue := TJSONNumber.Create(LField.AsLargeInt);
          ftFMTBcd, ftExtended, ftFloat, ftCurrency, ftBCD : LJSONValue := TJSONNumber.Create(LField.AsFloat);
          ftDate, ftTime, ftDateTime, ftTimeStamp : LJSONValue := TJSONString.Create(FormatDateTime('yyyy-mm-dd hh:nn:ss', LField.AsDateTime));
          ftString, ftWideString, ftMemo :
          {$IF DEFINED(VER220)}
            LJSONValue := TJSONString.Create(TratarJsonString(LField.AsString));
          {$ELSE}
            LJSONValue := TJSONString.Create(LField.AsString);
          {$IFEND}
          else
            raise Exception.Create('Tipo não suportado: Campo ' + LField.FieldName);
        end;
        LJSONObject.AddPair(LField.FieldName, LJSONValue);
      end;
      LJSONArray.AddElement(LJSONObject);
      ADataset.Next;
    end;
    if LJSONArray.Size = 1 then
      Result := LJSONArray.Get(0).ToString
    else
      Result := LJSONArray.ToString
  finally
    LJSONArray.Free;
  end;
end;

class function TMiniRESTJSON.DatasetToJson2(ADataset: TDataset): string;
var LField : TField;
    LJSONObject : JsonDataObjects.TJSONObject;
    LJSONArray : JsonDataObjects.TJSONArray;
begin
  ADataset.First;
  LJSONArray := JsonDataObjects.TJSONArray.Create;
  try
    while not ADataset.Eof do
    begin
      LJSONObject := JsonDataObjects.TJSONObject.Create;
      for LField in ADataset.Fields do
      begin
        if LField.IsNull then
          LJSONObject.Values[LField.FieldName].VariantValue := Null
        else
        case LField.DataType of
          ftSmallint, ftSingle, ftInteger, ftWord, ftShortint : LJSONObject.Values[LField.FieldName].IntValue := LField.AsInteger;
          ftLargeint : LJSONObject.Values[LField.FieldName].LongValue := LField.AsLargeInt;
          ftFMTBcd, ftExtended, ftFloat, ftCurrency, ftBCD : LJSONObject.Values[LField.FieldName].FloatValue := LField.AsFloat;
          ftDate, ftTime, ftDateTime, ftTimeStamp : LJSONObject.Values[LField.FieldName].DateTimeValue := LField.AsDateTime;
          ftString, ftWideString, ftMemo : LJSONObject.Values[LField.FieldName].Value := LField.AsString;
          else
            raise Exception.Create('Tipo não suportado: Campo ' + LField.FieldName);
        end;
      end;
      LJSONArray.Add(LJSONObject);
      ADataset.Next;
    end;
    if LJSONArray.Count = 1 then
      Result := LJSONArray.Values[0].ObjectValue.ToJSON
    else
      Result := LJSONArray.ToJSON()
  finally
    LJSONArray.Free;
  end;
end;

class function TMiniRESTJSON.TratarJSONArray(AJSON: string): string;
begin
  if Pos('[', AJSON) <> 1 then
    Result := '[' + AJSON + ']'
  else
    Result := AJSON;
end;

class function TMiniRESTJSON.TratarJsonString(AJson: string): string;
begin
  Result := StringReplace(Trim(AJson), '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;


end.

