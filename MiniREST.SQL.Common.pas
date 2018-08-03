unit MiniREST.SQL.Common;

interface

uses DB, Rtti;

type
  TMiniRESTSQLParamType = (stString, stFloat, stInteger, stDate, stDateTime,
  stBoolean, stVariant, stUndefined);

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
//    property AsString: string read GetAsString write SetAsString;
//    property AsFloat: Double read GetAsFloat write SetAsFloat;
//    property AsInteger: Integer read GetAsInteger write SetAsInteger;
//    property AsDate: TDate read GetAsDate write SetAsDate;
//    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
//    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
  end;

  TMiniRESTSQLParam = class(TInterfacedObject, IMiniRESTSQLParam)
  private
    FParamName: string;
    FParamType: TMiniRESTSQLParamType;
    FValue: TValue;
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
  end;

implementation

{ TMiniRESTSQLParam }

constructor TMiniRESTSQLParam.Create;
begin
  FParamType := stUndefined;
end;

function TMiniRESTSQLParam.GetAsBoolean: Boolean;
begin
  Result := FValue.AsBoolean;
end;

function TMiniRESTSQLParam.GetAsDate: TDate;
begin
  Result := FValue.AsExtended;
end;

function TMiniRESTSQLParam.GetAsDateTime: TDateTime;
begin
  Result := FValue.AsExtended;
end;

function TMiniRESTSQLParam.GetAsFloat: Double;
begin
  Result := FValue.AsExtended;
end;

function TMiniRESTSQLParam.GetAsInteger: Integer;
begin
  Result := FValue.AsInteger;
end;

function TMiniRESTSQLParam.GetAsString: string;
begin
  Result := FValue.AsString;
end;

function TMiniRESTSQLParam.GetAsVariant: Variant;
begin
  Result := FValue.AsVariant;
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
  FValue := AValue;
  FParamType := stBoolean;
end;

procedure TMiniRESTSQLParam.SetAsDate(const AValue: TDate);
begin
  FValue := AValue;
  FParamType := stDate;
end;

procedure TMiniRESTSQLParam.SetAsDateTime(const AValue: TDateTime);
begin
  FValue := AValue;
  FParamType := stDateTime;
end;

procedure TMiniRESTSQLParam.SetAsFloat(const AValue: Double);
begin
  FValue := AValue;
  FParamType := stFloat;
end;

procedure TMiniRESTSQLParam.SetAsInteger(const AValue: Integer);
begin
  FValue := AValue;
  FParamType := stInteger;
end;

procedure TMiniRESTSQLParam.SetAsString(const AValue: string);
begin
  FValue := AValue;
  FParamType := stString;
end;

procedure TMiniRESTSQLParam.SetAsVariant(const AValue: Variant);
begin
  FValue := TValue.FromVariant(AValue);
  FParamType := stVariant;
end;

procedure TMiniRESTSQLParam.SetParamName(const AName: string);
begin
  FParamName := AName;
end;

end.
