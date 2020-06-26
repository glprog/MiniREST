unit MiniREST.SQL.Common;

interface

uses SysUtils, Variants{$IFNDEF FPC},DB, Rtti{$ENDIF};

type
  TMiniRESTSQLDatabaseType = (dbtUnknown, dbtFirebird, dbtPostgreSQL);

  TMiniRESTSQLParamType = (stString, stFloat, stInteger, stDate, stDateTime,
  stBoolean, stVariant, stUndefined);

  TMiniRESTSQLColumnType = (ctString, ctInteger, ctBigInteger, ctSmallInt, ctDecimal,
    ctDate, ctDateTime, ctBlob, ctMemo, ctDoublePrecision);

  IMiniRESTSQLParam = interface
  ['{3C3154D3-6D52-463B-8899-40976ED7B036}']
    function GetParamName: string;
    procedure SetParamName(const AName: string);
    function GetAsString: string;
    procedure SetAsString(const AValue: string);
    function GetAsFloat: Double;
    procedure SetAsFloat(const AValue: Double);
    function GetAsInteger: Integer;
    procedure SetAsInteger(const AValue: Integer);
    function GetAsDate: TDate;
    procedure SetAsDate(const AValue: TDate);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(const AValue: TDateTime);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const AValue: Boolean);
    function GetAsVariant: Variant;
    procedure SetAsVariant(const AValue: Variant);
    function GetParamType: TMiniRESTSQLParamType;
    function GetParamSize: Integer;
    function SetParamSize(const AParamSize: Integer): IMiniRESTSQLParam;
    property AsString: string read GetAsString write SetAsString;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsDate: TDate read GetAsDate write SetAsDate;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
  end;

  TMiniRESTSQLParam = class(TInterfacedObject, IMiniRESTSQLParam)
  private
    FParamName: string;
    FParamType: TMiniRESTSQLParamType;
    FParamSize: Integer;
    {$IFNDEF FPC}
    FValue: TValue;
    {$ELSE}
    FValueDate: TDate;
    FValueDateTime: TDateTime;
    FValueBoolean: Boolean;
    FValueDouble: Double;
    FValueInteger: Integer;
    FValueString: string;
    FValueVariant: Variant;
    {$ENDIF}
  public
    constructor Create;
    class function New: IMiniRESTSQLParam;
    function GetParamName: string;
    procedure SetParamName(const AName: string);
    function GetAsBoolean: Boolean;
    function GetAsDate: TDate;
    function GetAsDateTime: TDateTime;
    function GetAsFloat: Double;
    function GetAsInteger: Integer;
    function GetAsString: string;
    procedure SetAsBoolean(const AValue: Boolean);
    procedure SetAsDate(const AValue: TDate);
    procedure SetAsDateTime(const AValue: TDateTime);
    procedure SetAsFloat(const AValue: Double);
    procedure SetAsInteger(const AValue: Integer);
    procedure SetAsString(const AValue: string);
    function GetAsVariant: Variant;
    procedure SetAsVariant(const AValue: Variant);
    function GetParamType: TMiniRESTSQLParamType;
    function GetParamSize: Integer;
    function SetParamSize(const AParamSize: Integer): IMiniRESTSQLParam;
  end;

implementation

{ TMiniRESTSQLParam }

constructor TMiniRESTSQLParam.Create;
begin
  FParamType := stUndefined;
end;

function TMiniRESTSQLParam.GetAsBoolean: Boolean;
begin
  {$IFNDEF FPC}
  Result := FValue.AsBoolean;
  {$ELSE}
  Result := FValueBoolean;
  {$IFEND}
end;

function TMiniRESTSQLParam.GetAsDate: TDate;
begin
  {$IFNDEF FPC}
  Result := FValue.AsExtended;
  {$ELSE}
  Result := FValueDate;
  {$IFEND}
end;

function TMiniRESTSQLParam.GetAsDateTime: TDateTime;
begin
  {$IFNDEF FPC}
  Result := FValue.AsExtended;
  {$ELSE}
  Result := FValueDateTime;
  {$IFEND}
end;

function TMiniRESTSQLParam.GetAsFloat: Double;
begin
  {$IFNDEF FPC}
  Result := FValue.AsExtended;
  {$ELSE}
  Result := FValueDouble;
  {$IFEND}
end;

function TMiniRESTSQLParam.GetAsInteger: Integer;
begin
  {$IFNDEF FPC}
  Result := FValue.AsInteger;
  {$ELSE}
  Result := FValueInteger;
  {$IFEND}
end;

function TMiniRESTSQLParam.GetAsString: string;
begin
  {$IFNDEF FPC}
  Result := FValue.AsString;
  {$ELSE}
  Result := FValueString;
  {$IFEND}
end;

function TMiniRESTSQLParam.GetAsVariant: Variant;
begin
  {$IFNDEF FPC}
  Result := FValue.AsVariant;
  {$ELSE}
  Result := FValueVariant;
  {$IFEND}
end;

function TMiniRESTSQLParam.GetParamName: string;
begin
  Result := FParamName;
end;

function TMiniRESTSQLParam.GetParamType: TMiniRESTSQLParamType;
begin
  Result := FParamType;
end;

class function TMiniRESTSQLParam.New: IMiniRESTSQLParam;
begin
  Result := Create;
end;

procedure TMiniRESTSQLParam.SetAsBoolean(const AValue: Boolean);
begin
  {$IFNDEF FPC}
  FValue := AValue;
  {$ELSE}
  FValueBoolean := AValue;
  {$IFEND}
  FParamType := stBoolean;
end;

procedure TMiniRESTSQLParam.SetAsDate(const AValue: TDate);
begin
  {$IFNDEF FPC}
  FValue := AValue;
  {$ELSE}
  FValueDate := AValue;
  {$IFEND}
  FParamType := stDate;
end;

procedure TMiniRESTSQLParam.SetAsDateTime(const AValue: TDateTime);
begin
  {$IFNDEF FPC}
  FValue := AValue;
  {$ELSE}
  FValueDateTime := AValue;
  {$IFEND}
  FParamType := stDateTime;
end;

procedure TMiniRESTSQLParam.SetAsFloat(const AValue: Double);
begin
  {$IFNDEF FPC}
  FValue := AValue;
  {$ELSE}
  FValueDouble := AValue;
  {$IFEND}
  FParamType := stFloat;
end;

procedure TMiniRESTSQLParam.SetAsInteger(const AValue: Integer);
begin
  {$IFNDEF FPC}
  FValue := AValue;
  {$ELSE}
  FValueInteger := AValue;
  {$IFEND}
  FParamType := stInteger;
end;

procedure TMiniRESTSQLParam.SetAsString(const AValue: string);
begin
  {$IFNDEF FPC}
  FValue := AValue;
  {$ELSE}
  FValueString := AValue;
  {$IFEND}
  FParamType := stString;
end;

procedure TMiniRESTSQLParam.SetAsVariant(const AValue: Variant);
begin
  {$IFNDEF FPC}
  FValue := TValue.FromVariant(AValue);
  {$ELSE}
  FValueVariant := AValue;
  {$IFEND}
  FParamType := stVariant;
end;

procedure TMiniRESTSQLParam.SetParamName(const AName: string);
begin
  FParamName := UpperCase(AName);
end;

function TMiniRESTSQLParam.GetParamSize: Integer;
begin
  Result := FParamSize;
end;

function TMiniRESTSQLParam.SetParamSize(const AParamSize: Integer): IMiniRESTSQLParam;
begin
  FParamSize := AParamSize;
  Result := Self;
end;

end.
