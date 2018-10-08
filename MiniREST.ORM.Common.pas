unit MiniREST.ORM.Common;

interface

uses SysUtils, Rtti;

type
  TMiniRESTORMColumnType = (ctString, ctInteger, ctBigInteger, ctSmallInt, ctDecimal,
    ctDate, ctDateTime, ctBlob, ctMemo, ctDoublePrecision);
  TMiniRESTORMObjectState = (osNew, osManaged, osDetached, osRemoved);
  TMiniRESTORMCascadeType = (caPersist, caMerge, caDelete, caAll, caNone);
  TMiniRESTORMCascadeTypes = set of TMiniRESTORMCascadeType;
  TMiniRESTORMDatabaseDriver = (ddFirebird, ddSQLite, ddMySQL);
  TMiniRESTORMVariantDynArray = array of Variant;
  TMiniRESTORMLazyProc = procedure (const AValue : TValue) of object;
  TMiniRESTORMLazyProcAnon = reference to procedure (const AValue : TValue);
  TMiniRESTORMListFunc = function : TArray<TValue> of object;

  MiniRESTORMException = class(Exception)
  end;

  TMiniRESTORMCommandType = (cmInsert, cmSelect, cmUpdate, cmDelete);

implementation

end.
