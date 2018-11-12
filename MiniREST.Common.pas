unit MiniREST.Common;

interface

uses IdContext, IdCustomHttpServer;

type
  TMiniRESTRequestMethod = (rmGet, rmPost, rmPut, rmDelete, rmOptions);
  //TMiniRESTAction = procedure(AContext: TIdContext;
  //ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo) of object;

  //TMiniRESTControllerBaseClass = class of TMiniRESTControllerBase;

  TMiniRESTResponseType = (rtTextHtml, rtApplicationJson);

  TMiniRESTServerType = (stIndy, stmORMot);

const
  MiniRESTResponseTypes : array[rtTextHtml..rtApplicationJson] of string =
  ('text/html',
  'application/json');

implementation

end.
