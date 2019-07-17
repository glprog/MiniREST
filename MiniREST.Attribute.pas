unit MiniREST.Attribute;

interface

uses MiniREST.Common;

type
  RequestMappingAttribute = class(TCustomAttribute)
  private
    FMapping : string;
    FRequestMethod : TMiniRESTRequestMethod;
    FPermission : string;
  public
    constructor Create(AMapping : string; APermission : string = ''; ARequestMethod : TMiniRESTRequestMethod = rmGet); overload;
    constructor Create(AMapping : string; ARequestMethod : TMiniRESTRequestMethod); overload;
    property Mapping : string read FMapping;
    property RequestMethod : TMiniRESTRequestMethod read FRequestMethod;
    property Permission : string read FPermission;
  end;

  RequestMappingDesctriptionAttribute = class(TCustomAttribute)
  private
    FDescription: string;
  public
    constructor Create(ADescription: string);
    property Description: string read FDescription;
  end;

implementation

{ RequestMappingAttribute }

constructor RequestMappingAttribute.Create(AMapping: string; APermission : string;
  ARequestMethod: TMiniRESTRequestMethod);
begin
  FMapping := AMapping;
  FRequestMethod := ARequestMethod;
  FPermission := APermission;
end;

constructor RequestMappingAttribute.Create(AMapping: string;
  ARequestMethod: TMiniRESTRequestMethod);
begin
  Create(AMapping, '', ARequestMethod);
end;

{ RequestMappingDesctriptionAttribute }

constructor RequestMappingDesctriptionAttribute.Create(ADescription: string);
begin
  FDescription := ADescription;
end;

end.
