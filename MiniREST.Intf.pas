{$IFDEF FPC}
  {$mode DELPHI}
{$IFEND}
unit MiniREST.Intf;

interface

uses Classes, SysUtils, {$IFNDEF FPC}Rtti,{$IFEND} MiniREST.Common;

type
  IMiniRESTControllerFactory = interface;
  IMiniRESTRequestInfo = interface
  ['{EFAAA153-42E0-455B-BE9B-6EEBDAE3563B}']
    function GetPathLength : Integer;
    function GetRequestMethod : TMiniRESTRequestMethod;
    function IsMatch(AMapping : string; ARequestMethod : TMiniRESTRequestMethod) : Boolean;
  end;

  IMiniRESTActionInfo = interface
  ['{C9E2FE39-C04D-451F-9DC8-B6203E7F9A05}']
    function GetMapping : string;
    function GetMethod : TRttiMethod;
    function GetRequestMethod : TMiniRESTRequestMethod;
    function GetClass : TClass;
    function GetPermission : string;
    function GetIsFactory : Boolean;
    function GetFactory : IMiniRESTControllerFactory;
    property IsFactory : Boolean read GetIsFactory;
    property Mapping : string read GetMapping;
    property Method : TRttiMethod read GetMethod;
    property RequestMethod : TMiniRESTRequestMethod read GetRequestMethod;
    property &Class : TClass read GetClass;
    property Factory : IMiniRESTControllerFactory read GetFactory;
    property Permission : string read GetPermission;
  end;

  IMiniRESTControllerFactory = interface
  ['{65DBD1D5-30BE-43DD-AA18-9117B4C96973}']
    procedure ClearFactory;
    function GetClass : TClass; // para RTTI, registro endpoints
    function GetController : TObject; { TODO: mudar para IMiniRESTController, ver referencia circular }
  end;

  IMiniRESTQueryParam = interface
  ['{8835B189-F4B2-487D-BC1A-4C5D9521DB35}']
    function GetName : string;
    function GetValue : string;
  end;

  { TODO : Remover dependência Indy }
  IMiniRESTActionContext = interface
  ['{1BF3D3A8-014F-4C84-861D-E9C94D3C1439}']
    function GetActionInfo : IMiniRESTActionInfo;
    procedure SetActionInfo(AActionInfo : IMiniRESTActionInfo);
    function GetRequestContentAsString : string;
    function GetPathVariable(AVariable : string) : string;
    function GetQueryParam(AQueryParam : string) : IMiniRESTQueryParam;
    function GetQueryParams : TArray<IMiniRESTQueryParam>;
    function GetAuthToken : string;
    function GetURI : string;
    function GetCommandType : TMiniRESTRequestMethod;
    function GetHeader(AName : string) : string;
    procedure SetHeader(AName, AValue : string);
    procedure AppendHeader(AName, AValue : string);
    //procedure SetResponseContent(AContent : string; AContentType : TMiniRESTResponseType = rtTextHtml; AStatusCode: Integer = 200);
    function GetResponseContent: string;
    procedure SetResponseContent(const AContent: string);
    function GetResponseContentType: TMiniRESTResponseType;
    procedure SetResponseContentType(const AContentType: TMiniRESTResponseType);
    function GetResponseStatusCode: Integer;
    procedure SetResponseStatusCode(const AStatusCode: Integer);
    procedure SetResponseStream(AStream : TStream);
    procedure ServeFile(AFilePath : string);
    procedure SendRedirect(ALocation : string);    
    property ActionInfo : IMiniRESTActionInfo read GetActionInfo write SetActionInfo;
  end;

  IMiniRESTMiddleware = interface
  ['{1FC10AA6-3DB0-4B4B-9F0A-AE96F94B871C}']
    function Process(AActionContext : IMiniRESTActionContext) : Boolean;
  end;

  IMiniRESTLogger = interface
  ['{86B5F708-DA04-4A96-B08F-4337CF3B5A73}']
    procedure Info(AInfo : string);
    procedure Debug(ADebug : string);
    procedure Warn(AWarn : string);
    procedure Exception(AMsg : string; AException : Exception);
  end;

implementation

end.
