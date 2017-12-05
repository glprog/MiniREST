unit MiniRest.Util;

interface

uses Classes, SysUtils, MiniRest.Intf, Generics.Collections, DateUtils,
  {$IF DEFINED(VER310) OR DEFINED(VER290)} JSON, REST.Json, REST.Json.Types,
  REST.JsonReflect {$ELSE} DBXJSON {$IFEND}, Rtti;

type
  TMiniRestUtil = class
  private
    class var FRttiContext : TRttiContext;
  public
    class constructor Create;
    class destructor Destroy;
    class function GetRttiContext : TRttiContext;
    class function ParseMapping(AMapping : string) : string;
    class function ParseMappingtoPathParams(AMapping : string) : TArray<string>;
    class function GetPathVariable(AVariable, APath : string; AActionContext : IMiniRestActionContext) : string;
  end;

  {$IF DEFINED(VER310) OR DEFINED(VER290)}
  TMiniRestJsonUtil = class
    class function ObjectToJsonString(AObject: TObject; AOptions: TJsonOptions = [joDateIsUTC, joDateFormatISO8601]): string;
    class function JsonToObject<T: class, constructor>(AJsonObject: TJSOnObject; AOptions: TJsonOptions = [joDateIsUTC, joDateFormatISO8601]): T; overload;
    class function JsonToObject<T: class, constructor>(AJson: string; AOptions: TJsonOptions = [joDateIsUTC, joDateFormatISO8601]): T; overload;
    class procedure ProcessOptions(AJsonObject: TJSOnObject; AOptions: TJsonOptions);
    class function ConvertFieldNameFromJson(AObject: TObject; const AFieldName: string): string;
    class function ConvertFieldNameToJson(const AField: TRttiField): string;
    class function TratarJsonObject(AClass : TClass; AJsonObject : TJSONObject): TJSONObject;
  end;
  {$IFEND}

implementation

uses MiniRest.JSON;

{ TMiniRestUtil }

class function TMiniRestUtil.GetRttiContext : TRttiContext;
begin
  Result := FRttiContext;
end;

class constructor TMiniRestUtil.Create;
begin
  FRttiContext := TRttiContext.Create;
  {$IF DEFINED(VER310) OR DEFINED(VER290)}
  FRttiContext.KeepContext;
  {$IFEND}
end;

class destructor TMiniRestUtil.Destroy;
begin
  {$IF DEFINED(VER310) OR DEFINED(VER290)}
  FRttiContext.DropContext;
  {$IFEND}
end;

class function TMiniRestUtil.GetPathVariable(AVariable, APath: string;
  AActionContext: IMiniRestActionContext): string;
var LPathParamsRequest, LPathParamsController : TArray<string>;
  I : Integer;
begin
  LPathParamsRequest := TMiniRestUtil.ParseMappingtoPathParams(APath);
  //Refatorar para pegar do obj TMiniRestActionInfo
  LPathParamsController := TMiniRestUtil.ParseMappingtoPathParams(AActionContext.ActionInfo.Mapping);
  for I := 0 to Length(LPathParamsController) - 1 do
  begin
    if ('{' + AVariable + '}' = LPathParamsController[I]) then
      Exit(LPathParamsRequest[I]);
  end;
end;

class function TMiniRestUtil.ParseMapping(AMapping: string): string;
var LPath : TStringList;
    LPathVariableCount : Integer;
    S : string;
begin
  LPath := TStringList.Create;
  LPath.StrictDelimiter := True;
  LPath.Delimiter := '/';
  LPathVariableCount := 1;
  try
    LPath.DelimitedText := AMapping;
    for S in LPath do
    begin
      if Trim(S) = '' then
        Continue;
      if Pos('{', S) > 0 then
      begin
        Result := Result + '/' + '{p' + IntToStr(LPathVariableCount) + '}';
        Inc(LPathVariableCount);
      end
      else
        Result := Result + '/' + S;
    end;
  finally
    LPath.Free;
  end;
end;

class function TMiniRestUtil.ParseMappingtoPathParams(
  AMapping: string): TArray<string>;
var LPath : TStringList;
    I : Integer;
    S : string;
begin
  //Result := [];
  SetLength(Result, 0);
  LPath := TStringList.Create; { TODO : Refatorar: Mandar para classe utilitária }
  LPath.StrictDelimiter := True;
  LPath.Delimiter := '/';
  try
    LPath.DelimitedText := AMapping;
    for I := 0 to LPath.Count - 1 do
      if Trim(LPath[I]) <> '' then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := LPath[I];
        //Result := Result + [LPath[I]];
      end;
  finally
    LPath.Free;
  end;
end;

{ TMiniRestJsonUtil }
{$IF DEFINED(VER310) OR DEFINED(VER290)}
class function TMiniRestJsonUtil.TratarJsonObject(AClass : TClass; AJsonObject : TJSONObject): TJSONObject;
var LType : TRttiType;
    LField : TRttiField;
    LCustomAttribute : TCustomAttribute;
    LJsonObject : TJSONObject;
    LValue : string;
    LFieldName : string;
    LJsonPair : TJSONPair;
    LJsonValue : TJSONValue;
    LJsonArray : TJSONArray;
    LClass : TClass;
    I : Integer;
begin
  { TODO : Revisar performance }
  LJsonObject := AJsonObject;
  LType := TMiniRestUtil.GetRttiContext.GetType(AClass);
  for LField in LType.GetFields do
  begin
    LJsonPair := nil;
    LFieldName := ConvertFieldNameToJson(LField);
    try
      LJsonPair := LJsonObject.Get(LFieldName);
    except
    end;
    if LJsonPair = nil then
      Continue;
    {if (LJsonPair.JsonValue.ClassType = TJSONArray) and (LField.FieldType.IsInstance) then
    begin
      if LField.FieldType.QualifiedName.Contains('<') then
        LClass := TMiniRestUtil.GetRttiContext.FindType(LField.FieldType.QualifiedName.Split(['<','>'])[1]).AsInstance.MetaclassType
      else
        LClass := LField.FieldType.AsInstance.MetaclassType;
      LJsonArray := TJSONArray(LJsonPair.JsonValue);
      for I := 0 to Pred(LJsonArray.Count) do
      begin
        LJsonValue := LJsonArray.Items[I];
        if LJsonValue.ClassType = TJSONObject then
          TratarJsonObject(LClass, TJSONObject(LJsonValue));
      end;
    end
    else
    if (LJsonPair.JsonValue.ClassType = TJSONObject) then
    begin
      LJsonValue := TratarJsonObject(LField.FieldType.AsInstance.MetaclassType, TJSONObject(TJSONObject.ParseJSONValue(LJsonPair.JsonValue.ToJSON)));
      LJsonObject.RemovePair(LFieldName).Free;
      LJsonObject.AddPair(LFieldName, LJsonValue);
    end
    else}
    begin
      for LCustomAttribute in LField.GetAttributes do
      begin
      if LCustomAttribute is MiniRestJsonDateAttribute then
      begin
        if (LJsonPair.JsonValue.Value <> '0') or (LJsonPair.JsonValue.Value <> '') then
        begin
          LJsonValue := TJSONString.Create(DateToISO8601(TDateTime(LJsonPair.JsonValue.Value.ToDouble)));
          LJsonObject.RemovePair(LFieldName).Free;
          LJsonPair := TJSONPair.Create(LFieldName, LJsonValue);
          LJsonObject.AddPair(LJsonPair);
        end;
      end
      else
      if LCustomAttribute is MiniRestJsonNullAttribute then
      begin
        LFieldName := ConvertFieldNameToJson(LField);
        LValue := LJsonObject.Get(LFieldName).JsonValue.Value;
        if (LValue = '') or (LValue = '0') then
          LJsonObject.RemovePair(LFieldName).Free;
      end;
    end;
    end;
  end;
  Result := LJsonObject;
end;

class function TMiniRestJsonUtil.JsonToObject<T>(AJsonObject: TJSOnObject;
  AOptions: TJsonOptions): T;
var
  LUnMarshaler: TJSONUnMarshal;
  LContext : TRttiContext;
  LType : TRttiType;
  LField : TRttiField;
  LAttribute : TCustomAttribute;
  LMiniRestJsonAttribute : MiniRestJsonAttribute;
  LObjs : TObjectList<TJSONInterceptor>;
  LInterceptor : TJSONInterceptor;
  LReverter : TReverterEvent;
  LFieldName : string;
begin
  LUnMarshaler := TJSONUnMarshal.Create;
  LObjs := TObjectList<TJSONInterceptor>.Create(True);
  LContext := TRttiContext.Create;
  LType := LContext.GetType(T);
  for LField in LType.GetFields do
  begin
    for LAttribute in LField.GetAttributes do
      if LAttribute is MiniRestJsonAttribute then
      begin
        LMiniRestJsonAttribute := MiniRestJsonAttribute(LAttribute);
        LFieldName := TJSONConverter.ConvertFieldNameToJson(LField);
        LInterceptor := LMiniRestJsonAttribute.JSONInterceptor;
        LReverter := TReverterEvent.Create(LMiniRestJsonAttribute.ObjClass, LFieldName);
        LReverter.ObjectsReverter := LInterceptor.ObjectsReverter;
        LObjs.Add(LInterceptor);
        LUnMarshaler.RegisterReverter(LType.AsInstance.MetaclassType, LFieldName, LReverter);
        LFieldName := '';
        //LUnMarshaler.RegisterReverter(LType.AsInstance.MetaclassType, TJSONConverter.ConvertFieldNameToJson(LField), LInterceptor.ObjectsReverter);
      end;
  end;

  try
    LUnMarshaler.DateTimeIsUTC  := joDateIsUTC in AOptions;
    if joDateFormatUnix in AOptions then
      LUnMarshaler.DateFormat :=jdfUnix
    else if joDateFormatISO8601 in AOptions then
     LUnMarshaler.DateFormat := jdfISO8601
    else if joDateFormatMongo in AOptions then
     LUnMarshaler.DateFormat := jdfMongo;
    ProcessOptions(AJSONObject, AOptions);

    Result := LUnMarshaler.CreateObject(T, AJsonObject) as T;
  finally
    LObjs.Free;
    FreeAndNil(LUnMarshaler);
  end;
end;

class function TMiniRestJsonUtil.JsonToObject<T>(AJson: string;
  AOptions: TJsonOptions): T;
var
  LJSONValue: TJsonValue;
  LJSONObject: TJSOnObject;
begin
  LJSONValue := TJSOnObject.ParseJSONValue(AJson);
  try
    if assigned(LJSONValue) and (LJSONValue is TJSOnObject) then
      LJSONObject := LJSONValue as TJSOnObject
    else
      raise EConversionError.Create('The input value is not a valid Object');
    Result := JsonToObject<T>(LJSONObject, AOptions);
  finally
    FreeAndNil(LJSONObject);
  end;
end;

class function TMiniRestJsonUtil.ObjectToJsonString(AObject: TObject;
  AOptions: TJsonOptions): string;
var LType : TRttiType;
    LField : TRttiField;
    LCustomAttribute : TCustomAttribute;
    LJsonObject : TJSONObject;
    LValue : string;
    LFieldName : string;
    LJsonPair : TJSONPair;
    LJsonValue : TJSONValue;
begin
  LJsonObject := TJSONObject(TJSONObject.ParseJSONValue(TJson.ObjectToJsonString(AObject, AOptions)));
  try
    TratarJsonObject(AObject.ClassType, LJsonObject);
    Result := LJsonObject.ToJSON;
  finally
    LJsonObject.Free;
  end;
end;

class procedure TMiniRestJsonUtil.ProcessOptions(AJsonObject: TJSOnObject; AOptions: TJsonOptions);
var
  LPair: TJSONPair;
  LItem: TObject;
  i: Integer;

  function IsEmpty(ASet: TJsonOptions):boolean;
  var
    LElement: TJsonOption;
  begin
    Result := true;
    for LElement in ASet do
    begin
      Result := false;
      break;
    end;
  end;

begin
  if assigned(AJsonObject) and not isEmpty(AOptions) then

   for i := AJsonObject.Count -1 downto 0  do
    begin
      LPair := TJSONPair(AJsonObject.Pairs[i]);
      if LPair.JsonValue is TJSOnObject then
        TMiniRestJsonUtil.ProcessOptions(TJSOnObject(LPair.JsonValue), AOptions)
      else if LPair.JsonValue is TJSONArray then
      begin
        if (joIgnoreEmptyArrays in AOptions) and (TJSONArray(LPair.JsonValue).Count = 0) then
        begin
          AJsonObject.RemovePair(LPair.JsonString.Value);
        end;
        for LItem in TJSONArray(LPair.JsonValue) do
        begin
          if LItem is TJSOnObject then
            TMiniRestJsonUtil.ProcessOptions(TJSOnObject(LItem), AOptions)
        end;
      end
      else
      begin
        if (joIgnoreEmptyStrings in AOptions) and (LPair.JsonValue.value = '') then
        begin
          AJsonObject.RemovePair(LPair.JsonString.Value).Free;
        end;
      end;
    end;
end;

class function TMiniRestJsonUtil.ConvertFieldNameFromJson(AObject: TObject; const AFieldName: string): string;
var
  LFieldName: string;
  LRTTICtx: TRttiContext;
  LRTTIType: TRttiType;
  LRTTIField: TRttiField;
  LAttribute: TCustomAttribute;
begin
  result := '';
  //First check if any of the fields in AObject has a JSONName field name mapping
  LRTTICtx := TRttiContext.Create;

  LRTTIType := LRTTICtx.GetType(AObject.ClassType);
  for LRTTIField in LRTTIType.GetFields do
  begin
    for LAttribute in LRTTIField.GetAttributes do
    begin
       if LAttribute is JSONNameAttribute then
       begin
         if AFieldName = JSONNameAttribute(LAttribute).Name then
          result := LRTTIField.Name;
          Break;
       end;
    end;
  end;


  if result = '' then begin
    // Delphi Fieldname usually start with an "F", which we don't have in JSON:
    // FName = 'Elmo'  => {"Name":"Elmo"}

    LFieldName := 'F' + AFieldName;
    Result := LFieldName;
  end;
end;

class function TMiniRestJsonUtil.ConvertFieldNameToJson(const AField: TRttiField): string;
var
  LFieldName: string;
  LAttribute: TCustomAttribute;
begin
  //First check if JsonNameAttribute is applied. Take without conversion
  Result := '';
  LFieldName := '';
  for LAttribute in AField.GetAttributes do
  begin
    if LAttribute is JsonNameAttribute then
    begin
      result := JsonNameAttribute(LAttribute).Name;
      break;
    end;
  end;
  //No Name Attribute found, regular rules apply
  if result = '' then
  begin
    // Delphi Fieldname usually start with an "F", which we don't want in JSON.
    // Also Javascript (i.e. JSON) defaults to lower Camel case, i.e. first letter lower case
    // FFullName = 'Elmo'  => {"fullName":"Elmo"}
    LFieldName := AField.Name;
    if LFieldName.StartsWith('F', true) then
      LFieldName := LFieldName.Remove(0, 1);
    LFieldName := LowerCase(LFieldName.Chars[0]) + LFieldName.Substring(1);
    Result := LFieldName;
  end;
end;
{$IFEND}

end.
