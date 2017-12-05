unit MiniRest.Common;

interface

uses IdContext, IdCustomHttpServer;

type
  TMiniRestRequestMethod = (rmGet, rmPost, rmPut, rmDelete, rmOptions);
  //TMiniRestAction = procedure(AContext: TIdContext;
  //ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo) of object;

  //TMiniRestControllerBaseClass = class of TMiniRestControllerBase;

  TMiniRestResponseType = (rtTextHtml, rtApplicationJson);

const
  MiniRestResponseTypes : array[rtTextHtml..rtApplicationJson] of string =
  ('text/html',
  'application/json');

implementation

end.
