unit MiniREST.ORM.SQL;

interface

uses SysUtils, StrUtils, MiniREST.SQL.Intf, MiniREST.ORM.Intf;

type
  TMiniRESTORMDDLBase = class(TInterfacedObject, IMiniRESTORMDDL)
  protected
    FConnection: IMiniRESTSQLConnection;
    function ColumnToSQL(AColumn: IMiniRESTORMColumn): string; virtual; abstract;
    function TableExists(ATableName: string): Boolean; virtual; abstract;
    function ForeignKeyExists(ATable: IMiniRESTORMTable;
      ATableJoin: IMiniRESTORMTableJoin): Boolean; virtual; abstract;
    function PrimaryKeyExists(ATable: IMiniRESTORMTable): Boolean; virtual;
    procedure CreatePrimaryKey(ATable: IMiniRESTORMTable); virtual;
    procedure CreateForeignKeys(ATable: IMiniRESTORMTable); virtual;
  public
    constructor Create(AConnection: IMiniRESTSQLConnection);
    procedure CreateTable(ATable: IMiniRESTORMTable);
  end;

implementation

uses MiniREST.ORM.Utils;

{ TMiniRESTORMDDLBase }

constructor TMiniRESTORMDDLBase.Create(AConnection: IMiniRESTSQLConnection);
begin
  FConnection := AConnection;
end;

procedure TMiniRESTORMDDLBase.CreateForeignKeys(ATable: IMiniRESTORMTable);
var
  LTableJoin: IMiniRESTORMTableJoin;
  LColumnJoin: IMiniRESTORMColumnJoin;
  LSql, LSqlFieldsL, LSqlFieldsR: string;
begin
  for LTableJoin in ATable.GetTableJoins do
  begin
    {if LTableJoin.IsManyToMany then
      LTableName := LTableJoin.GetControlTable
    else
      LTableName := LTableJoin.GetTable.GetName;}
    if not TableExists(LTableJoin.GetTable.GetName) then
      CreateTable(LTableJoin.GetTable);
    if not ForeignKeyExists(ATable, LTableJoin) then
    begin
      LSqlFieldsL := '';
      LSqlFieldsR := '';
      LSql := 'ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s (%s)';
      for LColumnJoin in LTableJoin.GetColumnsJoin do
      begin
        LSqlFieldsL := LSqlFieldsL + ',' + LColumnJoin.GetColumnL.GetName;
        LSqlFieldsR := LSqlFieldsR + ',' + LColumnJoin.GetColumnR.GetName;
      end;
      System.Delete(LSqlFieldsL, 1, 1);
      System.Delete(LSqlFieldsR, 1, 1);
      LSql := Format(LSql, [ATable.GetName, 'FK_' +
        TMiniRESTORMUtils.MD5(LTableJoin.GetTable.GetName + ATable.GetName + LSqlFieldsL).Substring(1,
        27), LSqlFieldsL, LTableJoin.GetTable.GetName, LSqlFieldsR]);
      FConnection.Execute(LSql, []);
    end;
  end;
end;

procedure TMiniRESTORMDDLBase.CreatePrimaryKey(ATable: IMiniRESTORMTable);
var
  LSQL: string;
  LColumn: IMiniRESTORMColumn;
begin
  if PrimaryKeyExists(ATable) then
    Exit;

  for LColumn in ATable.GetColumns do
  begin
    if LColumn.GetIsPrimaryKey then
      LSQL := LSQL + ',' + LColumn.GetName;
  end;
  System.Delete(LSQL, 1, 1);
  FConnection.Execute(
    Format('ALTER TABLE %s ADD PRIMARY KEY (%s)', [ATable.GetName, LSQL]), []
  );
  { TODO : DELETAR }
//  ATable.GetColumns.ForEach(
//    procedure(const AColumn: IMiniRESTORMColumn)
//    begin
//      if AColumn.IsPkPrimaryKey then
//        LSql := LSql + ',' + AColumn.GetName;
//    end);
//  System.Delete(LSql, 1, 1);
//  FConnection.NewQuery.ExecuteCommand
//    (Format('ALTER TABLE %s ADD PRIMARY KEY (%s)', [ATable.GetName, LSql]));
end;

procedure TMiniRESTORMDDLBase.CreateTable(ATable: IMiniRESTORMTable);
var
  LTable: IMiniRESTORMTable;
  LDatabaseInfo: IMiniRESTSQLDatabaseInfo;
  LSql, LFields, LAux: string;
  LColumn: IMiniRESTORMColumn;
begin
  LDatabaseInfo := FConnection.GetDatabaseInfo;
  LAux := '';
  if not LDatabaseInfo.TableExists(ATable.GetName) then
    LSql := 'CREATE TABLE ' + ATable.GetName + ' ('
  else
  begin
    LSql := 'ALTER TABLE ' + ATable.GetName;
    LAux := ' ADD ';
  end;
  for LColumn in ATable.GetColumns do
  begin
    if not LDatabaseInfo.FieldExists(ATable.GetName, LColumn.GetName) then
    begin
      LFields := LFields + ' , ' + LAux + ColumnToSQL(LColumn) +
        IfThen(LColumn.GetIsPrimaryKey or not LColumn.GetIsNullable, ' NOT NULL','');
    end;
  end;
  System.Delete(LFields, 1, 2);
  LSql := LSql + LFields;
  if LAux.IsEmpty then
    LSql := LSql + ')';
  FConnection.StartTransaction;
  try
    if not LFields.IsEmpty then
      FConnection.Execute(LSql, []);
    CreatePrimaryKey(ATable);
    CreateForeignKeys(ATable);
    FConnection.Commit;
  except
    on E : Exception do
    begin
      FConnection.Rollback;
      raise Exception.Create('TMiniRESTORMDDLBase.CreateTable: ' + E.ToString);
    end;
  end;
end;

function TMiniRESTORMDDLBase.PrimaryKeyExists(
  ATable: IMiniRESTORMTable): Boolean;
var
  LColumn: IMiniRESTORMColumn;
  LPrimaryKey: IMiniRESTSQLPrimaryKeyInfo;
  LPrimaryKeyColumns: TArray<string>;
  LPrimaryKeyColumnsOnTable: TArray<IMiniRESTORMColumn>;
  I: Integer;
begin
  Result := True;
  LPrimaryKey := FConnection.GetDatabaseInfo.GetPrimaryKey(ATable.GetName);
  if not Assigned(LPrimaryKey) then
    Exit(False);
  LPrimaryKeyColumns := LPrimaryKey.Fields;
  LPrimaryKeyColumnsOnTable := ATable.GetPrimaryKeyColumns;
  if Length(LPrimaryKeyColumns) <> Length(LPrimaryKeyColumnsOnTable) then
    Exit(False);
  for I := 0 to Length(LPrimaryKey.Fields) - 1 do
  begin
    if not SameText(LPrimaryKeyColumns[I], LPrimaryKeyColumnsOnTable[I].GetName) then
      Exit(False);
  end;
end;

end.
