unit MiniRest.Intf;

interface

uses Classes, SysUtils, Rtti, MiniRest.Common;

type
  IMiniRestControllerFactory = interface;
  IMiniRestRequestInfo = interface
  ['{EFAAA153-42E0-455B-BE9B-6EEBDAE3563B}']
    function GetPathLength : Integer;
    function GetRequestMethod : TMiniRestRequestMethod;
    function IsMatch(AMapping : string; ARequestMethod : TMiniRestRequestMethod) : Boolean;
  end;

  IMiniRestActionInfo = interface
  ['{C9E2FE39-C04D-451F-9DC8-B6203E7F9A05}']
    function GetMapping : string;
    function GetMethod : TRttiMethod;
    function GetRequestMethod : TMiniRestRequestMethod;
    function GetClass : TClass;
    function GetPermission : string;
    function GetIsFactory : Boolean;
    function GetFactory : IMiniRestControllerFactory;
    property IsFactory : Boolean read GetIsFactory;
    property Mapping : string read GetMapping;
    property Method : TRttiMethod read GetMethod;
    property RequestMethod : TMiniRestRequestMethod read GetRequestMethod;
    property &Class : TClass read GetClass;
    property Factory : IMiniRestControllerFactory read GetFactory;
    property Permission : string read GetPermission;
  end;

  IMiniRestControllerFactory = interface
  ['{65DBD1D5-30BE-43DD-AA18-9117B4C96973}']
    procedure ClearFactory;
    function GetClass : TClass; // para RTTI, registro endpoints
    function GetController : TObject; { TODO: mudar para IMiniRestController, ver referencia circular }
  end;

  IMiniRestQueryParam = interface
  ['{8835B189-F4B2-487D-BC1A-4C5D9521DB35}']
    function GetName : string;
    function GetValue : string;
  end;

  { TODO : Remover dependência Indy }
  IMiniRestActionContext = interface
  ['{1BF3D3A8-014F-4C84-861D-E9C94D3C1439}']
    function GetActionInfo : IMiniRestActionInfo;
    procedure SetActionInfo(AActionInfo : IMiniRestActionInfo);
    function GetRequestContentAsString : string;
    function GetPathVariable(AVariable : string) : string;
    function GetQueryParam(AQueryParam : string) : IMiniRestQueryParam;
    function GetQueryParams : TArray<IMiniRestQueryParam>;
    function GetAuthToken : string;
    function GetURI : string;
    function GetCommandType : TMiniRestRequestMethod;
    function GetHeader(AName : string) : string;
    procedure SetHeader(AName, AValue : string);
    procedure AppendHeader(AName, AValue : string);
    procedure SetResponseContent(AContent : string; AContentType : TMiniRestResponseType = rtTextHtml; AStatusCode: Integer = 200);
    procedure SetResponseStream(AStream : TStream);
    procedure ServeFile(AFilePath : string);
    procedure SendRedirect(ALocation : string);
    property ActionInfo : IMiniRestActionInfo read GetActionInfo write SetActionInfo;
  end;

  IMiniRestMiddleware = interface
  ['{1FC10AA6-3DB0-4B4B-9F0A-AE96F94B871C}']
    function Process(AActionContext : IMiniRestActionContext) : Boolean;
  end;

  IMiniRestLogger = interface
  ['{86B5F708-DA04-4A96-B08F-4337CF3B5A73}']
    procedure Info(AInfo : string);
    procedure Debug(ADebug : string);
    procedure Warn(AWarn : string);
    procedure Exception(AException : Exception);
  end;

implementation

end.
