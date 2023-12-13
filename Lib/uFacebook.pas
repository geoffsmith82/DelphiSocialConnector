unit uFacebook;

interface

uses
  System.SysUtils,
  REST.Client,
  REST.Types;

type
  TFacebookApi = class
  private
    FAccessToken: string;
  public
    constructor Create(const AAccessToken: string);
    function CreateEvent(const AGroupID, AEventName, AStartTime, AEndTime, ADescription: string): Boolean;
    function CreatePost(const AGroupID, AMessage: string): Boolean;
  end;

implementation

constructor TFacebookApi.Create(const AAccessToken: string);
begin
  inherited Create;
  FAccessToken := AAccessToken;
end;

function TFacebookApi.CreateEvent(const AGroupID, AEventName, AStartTime, AEndTime, ADescription: string): Boolean;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  Result := False;

  RESTClient := TRESTClient.Create('https://graph.facebook.com');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;
    RESTRequest.Resource := '{group-id}/events';

    RESTRequest.Params.AddURLSegment('group-id', AGroupID);
    RESTRequest.AddAuthParameter('access_token', FAccessToken, pkHTTPHEADER, [poDoNotEncode]);

    RESTRequest.AddParameter('name', AEventName, pkGETorPOST);
    RESTRequest.AddParameter('start_time', AStartTime, pkGETorPOST);
    RESTRequest.AddParameter('end_time', AEndTime, pkGETorPOST);
    RESTRequest.AddParameter('description', ADescription, pkGETorPOST);

    RESTRequest.Execute;

    if RESTResponse.StatusCode = 200 then
    begin
      // Event created successfully
      Result := True;
    end
    else
    begin
      // Handle error
      // Optionally, log or display RESTResponse.Content to see the error details
    end;
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

function TFacebookApi.CreatePost(const AGroupID, AMessage: string): Boolean;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  Result := False;

  RESTClient := TRESTClient.Create('https://graph.facebook.com');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;
    RESTRequest.Resource := '{group-id}/feed';

    RESTRequest.Params.AddURLSegment('group-id', AGroupID);
    RESTRequest.AddAuthParameter('access_token', FAccessToken, pkHTTPHEADER, [poDoNotEncode]);

    RESTRequest.AddParameter('message', AMessage, pkGETorPOST);

    RESTRequest.Execute;

    if RESTResponse.StatusCode = 200 then
    begin
      // Post created successfully
      Result := True;
    end
    else
    begin
      // Handle error
      // Optionally, log or display RESTResponse.Content to see the error details
    end;
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

end.

