unit MiniREST.SQL.Firebird;

interface

uses SysUtils, MiniREST.SQL.Intf, MiniREST.SQL.Common;

type
  TMiniRESTSQLDatabaseInfoFirebird = class(TInterfacedObject, IMiniRESTSQLDatabaseInfo)
  private
    //Declared as pointer to avoid circular reference that causes memory leak
    FConnection: Pointer;
    function CreateQuery: IMiniRESTSQLQuery;
  public
    constructor Create(AConnection: IMiniRESTSQLConnection);
    function FieldExists(const ATableName: string;
      const AFieldName: string): Boolean;
    function GetForeignKeys(const ATableName: string): System.TArray<MiniREST.SQL.Intf.IMiniRESTSQLForeignKeyInfo>;
    function GetPrimaryKey(const ATableName: string): IMiniRESTSQLPrimaryKeyInfo;
    function TableExists(const ATableName: string): Boolean;
    function DatabaseType: TMiniRESTSQLDatabaseType;
    function GetColumns(const ATableName: string): System.TArray<MiniREST.SQL.Intf.IMiniRESTSQLColumnInfo>;
  end;

implementation

uses MiniREST.SQL.Base, MiniREST.Common.Utils;

{ TMiniRESTSQLDatabaseInfoFirebird }

constructor TMiniRESTSQLDatabaseInfoFirebird.Create(
  AConnection: IMiniRESTSQLConnection);
begin
  FConnection := AConnection;
end;

function TMiniRESTSQLDatabaseInfoFirebird.CreateQuery: IMiniRESTSQLQuery;
begin
  Result := IMiniRESTSQLConnection(FConnection).GetQuery;
end;

function TMiniRESTSQLDatabaseInfoFirebird.DatabaseType: TMiniRESTSQLDatabaseType;
begin
  Result := dbtFirebird;
end;

function TMiniRESTSQLDatabaseInfoFirebird.FieldExists(const ATableName,
  AFieldName: string): Boolean;
var
  LConnection: IMiniRESTSQLConnection;
  LQry: IMiniRESTSQLQuery;
begin
  LConnection := IMiniRESTSQLConnection(FConnection);
  LQry := LConnection.GetQuery('SELECT FIRST 1 0 FROM RDB$RELATION_FIELDS R WHERE (UPPER(R.RDB$RELATION_NAME) = UPPER(:TABLE)) ' +
  ' AND (UPPER(R.RDB$FIELD_NAME) = UPPER(:FIELD))');
  LQry.ParamByName('TABLE').AsString := ATableName;
  LQry.ParamByName('FIELD').AsString := AFieldName;
  LQry.Open;
  Result := not LQry.DataSet.IsEmpty;
end;

function TMiniRESTSQLDatabaseInfoFirebird.GetColumns(
  const ATableName: string): System.TArray<MiniREST.SQL.Intf.IMiniRESTSQLColumnInfo>;
var
  LQry: IMiniRESTSQLQuery;
  LColumnInfo: IMiniRESTSQLColumnInfo;
begin
  SetLength(Result, 0);
  LQry := CreateQuery;
  LQry.SQL := 'select f.rdb$relation_name, f.rdb$field_name' + #13#10 +
              'from rdb$relation_fields f' + #13#10 +
              'join rdb$relations r on f.rdb$relation_name = r.rdb$relation_name' + #13#10 +
              'and r.rdb$view_blr is null' + #13#10 +
              'and (r.rdb$system_flag is null or r.rdb$system_flag = 0)' + #13#10 +
              'where f.rdb$relation_name = :TABLE' + #13#10 +
              'order by 1, f.rdb$field_position';
  LQry.ParamByName('TABLE').AsString := ATableName;
  LQry.Open;
  while not LQry.DataSet.Eof do
  begin
    LColumnInfo := TMiniRESTSQLColumnInfo.Create(
      Trim(LQry.DataSet.FieldByName('RDB$FIELD_NAME').AsString)
      );
    TMiniRESTCommonUtils.AddToArray<IMiniRESTSQLColumnInfo>(LColumnInfo, Result);
    LQry.DataSet.Next;
  end;
end;

function TMiniRESTSQLDatabaseInfoFirebird.GetForeignKeys(
  const ATableName: string): System.TArray<MiniREST.SQL.Intf.IMiniRESTSQLForeignKeyInfo>;
var
  LConnection: IMiniRESTSQLConnection;
  LQry: IMiniRESTSQLQuery;
  LSQL: string;
  LForeignKey: IMiniRESTSQLForeignKeyInfo;
  LFields, LFKFields: TArray<string>;
begin
  LForeignKey := nil;
  SetLength(LFields, 0);
  SetLength(LFKFields, 0);
  SetLength(Result, 0);
  LConnection := IMiniRESTSQLConnection(FConnection);
  LSQL := 'SELECT IDX.RDB$FIELD_POSITION AS ORDEM, RL.RDB$CONSTRAINT_NAME AS NAME, RL.RDB$RELATION_NAME AS TBL, IDX.RDB$FIELD_NAME AS FIELD, RL2.RDB$RELATION_NAME AS TBL_FK,' + #13#10 +
          'IDX2.RDB$FIELD_NAME AS FIELD_FK' + #13#10 +
          'FROM RDB$RELATION_CONSTRAINTS RL' + #13#10 +
          'JOIN RDB$INDEX_SEGMENTS IDX ON RL.RDB$INDEX_NAME = IDX.RDB$INDEX_NAME' + #13#10 +
          'JOIN RDB$REF_CONSTRAINTS REF ON REF.RDB$CONSTRAINT_NAME = RL.RDB$CONSTRAINT_NAME' + #13#10 +
          'JOIN RDB$RELATION_CONSTRAINTS RL2 ON RL2.RDB$CONSTRAINT_NAME = REF.RDB$CONST_NAME_UQ' + #13#10 +
          'JOIN RDB$INDEX_SEGMENTS IDX2 ON IDX2.RDB$INDEX_NAME = RL2.RDB$INDEX_NAME AND' + #13#10 +
          'IDX2.RDB$FIELD_POSITION = IDX.RDB$FIELD_POSITION' + #13#10 +
          'WHERE RL.RDB$RELATION_NAME = :TABLE AND' + #13#10 +
          'RL.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY''' + #13#10 +
          'ORDER BY 1 ASC';
  LQry := LConnection.GetQuery(LSQL);
  LQry.ParamByName('TABLE').AsString := ATableName;
  LQry.Open;
  while not LQry.DataSet.Eof do
  begin
    if (not Assigned(LForeignKey)) or
      (LForeignKey.Name <> LQry.DataSet.FieldByName('NAME').AsString) then
    begin
      if (not Assigned(LForeignKey)) then
      begin
        LForeignKey := TMiniRESTSQLForeignKeyInfo.Create;
      end
      else
      begin
        LForeignKey.Fields := LFields;
        LForeignKey.FKFields := LFKFields;
        LForeignKey := TMiniRESTSQLForeignKeyInfo.Create;
      end;
      LForeignKey.Name := LQry.DataSet.FieldByName('NAME').AsString;
      SetLength(LFields, 1);
      SetLength(LFKFields, 1);
      LFields[0] := LQry.DataSet.FieldByName('FIELD').AsString;
      LFKFields[0] := LQry.DataSet.FieldByName('FIELD_FK').AsString;
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := LForeignKey;
    end
    else
    begin
      SetLength(LFields, Length(LFields) + 1);
      SetLength(LFKFields, Length(LFKFields) + 1);
      LFields[Length(LFields) - 1] := LQry.DataSet.FieldByName('FIELD').AsString;
      LFKFields[Length(LFKFields) - 1] := LQry.DataSet.FieldByName('FIELD_FK').AsString;
    end;
    LQry.DataSet.Next;
    if LQry.DataSet.Eof then
    begin
      LForeignKey.Fields := LFields;
      LForeignKey.FKFields := LFKFields;
    end;
  end;
end;

function TMiniRESTSQLDatabaseInfoFirebird.GetPrimaryKey(
  const ATableName: string): IMiniRESTSQLPrimaryKeyInfo;
var
  LConnection: IMiniRESTSQLConnection;
  LQry: IMiniRESTSQLQuery;
  LSQL: string;
  LFields: TArray<string>;
begin
  Result := nil;
  SetLength(LFields, 0);
  LConnection := IMiniRESTSQLConnection(FConnection);
  LSQL := 'SELECT IDX.RDB$FIELD_POSITION AS ORDEM, IDX.RDB$FIELD_NAME AS FIELD,' + #13#10 +
          'RL.RDB$CONSTRAINT_NAME AS NAME' + #13#10 +
          'FROM RDB$RELATION_CONSTRAINTS RL' + #13#10 +
          'JOIN RDB$INDEX_SEGMENTS IDX ON RL.RDB$INDEX_NAME = IDX.RDB$INDEX_NAME' + #13#10 +
          'WHERE RL.RDB$RELATION_NAME = :TABLE AND' + #13#10 +
          'RL.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY''' + #13#10 +
          'ORDER BY 1';
  LQry := LConnection.GetQuery(LSQL);
  LQry.ParamByName('TABLE').AsString := ATableName;
  LQry.Open;
  if not LQry.DataSet.IsEmpty then
  begin
    Result := TMiniRESTSQLPrimaryKeyInfo.Create;
    Result.Name := Trim(LQry.DataSet.FieldByName('NAME').AsString);
    while not LQry.DataSet.Eof do
    begin
      SetLength(LFields, Length(LFields) + 1);
      LFields[Length(LFields) - 1] := Trim(LQry.DataSet.FieldByName('FIELD').AsString);
      LQry.DataSet.Next;
    end;
    Result.Fields := LFields;
  end;
end;

function TMiniRESTSQLDatabaseInfoFirebird.TableExists(
  const ATableName: string): Boolean;
var
  LConnection: IMiniRESTSQLConnection;
  LQry: IMiniRESTSQLQuery;
begin
  LConnection := IMiniRESTSQLConnection(FConnection);
  LQry := LConnection.GetQuery('SELECT FIRST 1 0 FROM RDB$RELATIONS R WHERE ' +
  'UPPER(R.RDB$RELATION_NAME) = UPPER(:TABELA)');
  LQry.ParamByName('TABELA').AsString := ATableName;
  LQry.Open;
  Result := not LQry.DataSet.IsEmpty;
end;

end.
