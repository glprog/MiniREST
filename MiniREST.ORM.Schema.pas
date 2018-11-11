unit MiniREST.ORM.Schema;

interface

uses SysUtils, MiniREST.ORM.Intf, MiniREST.SQL.Intf, MiniREST.SQL.Common;

type
  TMiniRESTORMSchema = class(TInterfacedObject, IMiniRESTORMSchema)
  private
    FConnection: IMiniRESTSQLConnection;
  public
    constructor Create(AConnection: IMiniRESTSQLConnection);
    procedure VerifyStructure;
  end;

implementation

uses MiniREST.ORM.Mapper, MiniREST.ORM.SQL, MiniREST.ORM.Firebird;

{ TMiniRESTORMSchema }

constructor TMiniRESTORMSchema.Create(AConnection: IMiniRESTSQLConnection);
begin
  FConnection := AConnection;
end;

procedure TMiniRESTORMSchema.VerifyStructure;
var
  LTable: IMiniRESTORMTable;
  LDDL: IMiniRESTORMDDL;
begin
  case FConnection.GetDatabaseInfo.DatabaseType of
    dbtFirebird: LDDL := TMiniRESTORMDDLFirebird.Create(FConnection);
    else
      raise Exception.Create('DatabaseType not implemented');
  end;
  for LTable in TMiniRESTORMMapper.GetTables do
  begin
    LDDL.CreateTable(LTable);
  end;
end;

end.
