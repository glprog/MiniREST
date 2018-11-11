unit MiniREST.ORM.Firebird;

interface

uses SysUtils, StrUtils, MiniREST.ORM.Intf, MiniREST.ORM.SQL, MiniREST.ORM.Common;

type
  TMiniRESTORMDDLFirebird = class(TMiniRESTORMDDLBase)
  protected
    function ColumnToSQL(AColumn: IMiniRESTORMColumn): string; override;
  end;

implementation

{ TMiniRESTORMDDLFirebird }

function TMiniRESTORMDDLFirebird.ColumnToSQL(
  AColumn: IMiniRESTORMColumn): string;
begin
  Result := AColumn.GetName;
  case AColumn.GetType of
    ctString: Result := Result + ' VARCHAR(' + IfThen(AColumn.GetSize > 0,
      IntToStr(AColumn.GetSize), '100') + ')';
    ctInteger: Result := Result + ' INTEGER';
    ctBigInteger: Result := Result + ' BIGINT';
    ctSmallInt: Result := Result + ' SMALLINT';
    ctDecimal: Result := Result + ' DECIMAL(' + IfThen(AColumn.GetPrecision > 0,
      IntToStr(AColumn.GetPrecision) ,'18') +
      ',' + IfThen(AColumn.GetScale > 0, IntToStr(AColumn.GetScale), '10') + ')';
    ctDate : Result := Result + ' DATE';
    ctDateTime: Result := Result + ' TIMESTAMP';
    ctBlob: Result := Result + ' BLOB SUB_TYPE 0 SEGMENT SIZE 4096';
    ctMemo: Result := Result + ' BLOB SUB_TYPE 1 SEGMENT SIZE 100';
  end;
end;

end.
