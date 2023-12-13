unit uTwitter;

interface


uses
  System.SysUtils,
  System.JSON,
  REST.Authenticator.OAuth,
  REST.Client,
  REST.Types;

type
  TTwitterApi = class
  private
    FOAuth1Authenticator: TOAuth1Authenticator;
  public
    constructor Create(const ConsumerKey, ConsumerSecret, AccessToken, AccessTokenSecret: string);
    destructor Destroy; override;
    procedure PostTweet(const Message: string);
  end;

implementation

constructor TTwitterApi.Create(const ConsumerKey, ConsumerSecret, AccessToken, AccessTokenSecret: string);
begin
  inherited Create;

  FOAuth1Authenticator := TOAuth1Authenticator.Create(nil);
  FOAuth1Authenticator.ConsumerKey := ConsumerKey;
  FOAuth1Authenticator.ConsumerSecret := ConsumerSecret;
  FOAuth1Authenticator.AccessToken := AccessToken;
  FOAuth1Authenticator.AccessTokenSecret := AccessTokenSecret;
end;

destructor TTwitterApi.Destroy;
begin
  FOAuth1Authenticator.Free;
  inherited Destroy;
end;

procedure TTwitterApi.PostTweet(const Message: string);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONObj: TJSONObject;
begin
  RESTClient := TRESTClient.Create('https://api.twitter.com/2');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    RESTClient.Authenticator := FOAuth1Authenticator;
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Resource := 'tweets';
    RESTRequest.Method := rmPOST;

    JSONObj := TJSONObject.Create;
    try
      JSONObj.AddPair('text', Message);
      RESTRequest.AddBody(JSONObj.ToString, ctAPPLICATION_JSON);
    finally
      JSONObj.Free;
    end;

    RESTRequest.Execute;

    if RESTResponse.StatusCode <> 201 then  // HTTP status code 201 indicates the tweet was created successfully
      raise Exception.Create('Error: ' + RESTResponse.StatusText);

  except
    on E: Exception do
      // Handle exception (e.g., show a message to the user or log the error)
  end;

  RESTResponse.Free;
  RESTRequest.Free;
  RESTClient.Free;
end;

end.
