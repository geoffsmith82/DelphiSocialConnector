unit uWordpress;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  REST.Authenticator.Basic
  ;

type
  TWordPressMedia = class
  public
    ID: Integer;
    Title: string;
    URL: string;
    Description: string;
    MediaType: string;
  end;

  TWordPressPost = class
    ID: Integer;
    Date: TDateTime;
    Slug: string;
    Status: string;
    &Type: string;
    Title: string;
    Content: string;
    Author: Integer;
    Excerpt: string;
    // ... other fields as needed ...
  end;

  TWordPressPage = class
  public
    ID: Integer;
    Date: TDateTime;
    Slug: string;
    Status: string;
    &Type: string;
    Title: string;
    Content: string;
    Author: Integer;
    Excerpt: string;
    // ... other fields as needed ...
  end;

  TWordPressUser = class
    ID: Integer;
    Name: string;
    Slug: string;
    Email: string;
    URL: string;
    Description: string;
    // ... other fields as needed ...
  end;


  TWordPressApi = class
  private
    FEndpoint: string;
    FUsername: string;
    FPassword: string;
  public
    constructor Create(const Endpoint, Username, Password: string);
  public  // Post functions
    function CreatePost(const Title, Content: string): Boolean;
    function ListPosts(status: string = 'publish'): TObjectList<TWordPressPost>;
    function DeletePost(const PostID: Integer): Boolean;
  public // Page functions
    function CreatePage(const Title, Content: string; const Status: string = 'draft'): Boolean;
    function ListPages(status: string = 'publish'): TObjectList<TWordPressPage>;
    function DeletePage(const PageID: Integer): Boolean;
  public  // User functions
    function CreateUser(const Username, Email, Password: string; const Role: string = 'subscriber'): Boolean;
    function ListUsers: TObjectList<TWordPressUser>;
    function RetrieveUser(const Username: string = 'me'): TWordPressUser;
    function DeleteUser(const Username: string): Boolean;
  public
    function GetSiteSettings: TStringList;
  public
    function CreateMedia(const FilePath: string;  const Title: string = ''): TWordPressMedia;
    function ListMedia: TObjectList<TWordPressMedia>;
    function RetrieveMedia(const MediaID: Integer): TWordPressMedia;
    function DeleteMedia(const MediaID: Integer; const ForceDelete: Boolean = False): Boolean;
  end;

implementation

uses
  System.DateUtils,
  System.Net.Mime
  ;

constructor TWordPressApi.Create(const Endpoint, Username, Password: string);
begin
  inherited Create;
  FEndpoint := Endpoint;
  FUsername := Username;
  FPassword := Password;
end;

function TWordPressApi.CreatePost(const Title, Content: string): Boolean;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  JSONBody: TJSONObject;
  Authenticator: THTTPBasicAuthenticator;
begin
  Result := False;

  RestClient := nil;
  RestRequest := nil;
  RestResponse := nil;
  Authenticator := nil;
  try
    RestClient := TRESTClient.Create(FEndpoint);
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    Authenticator := THTTPBasicAuthenticator.Create(FUsername, FPassword);
    RestClient.Authenticator := Authenticator;

    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Method := rmPOST;
    RestRequest.Resource := 'wp/v2/posts';

    JSONBody := TJSONObject.Create;
    try
      JSONBody.AddPair('title', Title);
      JSONBody.AddPair('content', Content);
      JSONBody.AddPair('status', 'draft');

      RestRequest.AddBody(JSONBody);

      RestRequest.Execute;

      Result := (RestResponse.StatusCode = 201);  // HTTP 201 Created
    finally
      JSONBody.Free;
    end;
  finally
    RestRequest.Free;
    RestResponse.Free;
    RestClient.Free;
    Authenticator.Free;
  end;
end;

function TWordPressApi.CreatePage(const Title, Content: string; const Status: string = 'draft'): Boolean;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  Authenticator: THTTPBasicAuthenticator;
  PageJSON: TJSONObject;
  JSONValue: TJSONValue;
begin
  Result := False;

  RestClient := nil;
  RestRequest := nil;
  RestResponse := nil;
  Authenticator := nil;
  try
    RestClient := TRESTClient.Create(FEndpoint);
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    Authenticator := THTTPBasicAuthenticator.Create(FUsername, FPassword);
    RestClient.Authenticator := Authenticator;

    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Method := rmPOST;
    RestRequest.Resource := 'wp/v2/pages';

    // Create JSON object with page details
    PageJSON := TJSONObject.Create;
    try
      PageJSON.AddPair('title', TJSONObject.Create.AddPair('rendered', Title));
      PageJSON.AddPair('content', TJSONObject.Create.AddPair('rendered', Content));
      PageJSON.AddPair('status', Status);

      RestRequest.AddBody(PageJSON.ToString, ctAPPLICATION_JSON);
    finally
      PageJSON.Free;
    end;

    RestRequest.Execute;

    if RestResponse.StatusCode = 201 then  // HTTP 201 Created
    begin
      JSONValue := RestResponse.JSONValue;
      if Assigned(JSONValue) and (JSONValue is TJSONObject) then
        Result := True;  // Page created successfully
    end;
  finally
    RestRequest.Free;
    RestResponse.Free;
    RestClient.Free;
    Authenticator.Free;
  end;
end;


function TWordPressApi.CreateUser(const Username, Email, Password: string; const Role: string = 'subscriber'): Boolean;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  Authenticator: THTTPBasicAuthenticator;
  UserJSON: TJSONObject;
  JSONValue: TJSONValue;
begin
  Result := False;

  RestClient := nil;
  RestRequest := nil;
  RestResponse := nil;
  Authenticator := nil;
  try
    RestClient := TRESTClient.Create(FEndpoint);
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    Authenticator := THTTPBasicAuthenticator.Create(FUsername, FPassword);
    RestClient.Authenticator := Authenticator;

    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Method := rmPOST;
    RestRequest.Resource := 'wp/v2/users';

    // Create JSON object with user details
    UserJSON := TJSONObject.Create;
    try
      UserJSON.AddPair('username', Username);
      UserJSON.AddPair('email', Email);
      UserJSON.AddPair('password', Password);
      UserJSON.AddPair('role', Role);

      RestRequest.AddBody(UserJSON.ToString, ctAPPLICATION_JSON);
    finally
      UserJSON.Free;
    end;

    RestRequest.Execute;

    if RestResponse.StatusCode = 201 then  // HTTP 201 Created
    begin
      JSONValue := RestResponse.JSONValue;
      if Assigned(JSONValue) and (JSONValue is TJSONObject) then
        Result := True;  // User created successfully
    end;
  finally
    RestRequest.Free;
    RestResponse.Free;
    RestClient.Free;
    Authenticator.Free;
  end;
end;

function TWordPressApi.RetrieveUser(const Username: string = 'me'): TWordPressUser;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  Authenticator: THTTPBasicAuthenticator;
  JSONUser: TJSONObject;
  JSONValue: TJSONValue;
begin
  Result := nil;

  RestClient := nil;
  RestRequest := nil;
  RestResponse := nil;
  Authenticator := nil;
  try
    RestClient := TRESTClient.Create(FEndpoint);
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    Authenticator := THTTPBasicAuthenticator.Create(FUsername, FPassword);
    RestClient.Authenticator := Authenticator;

    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Method := rmGET;
    RestRequest.Resource := 'wp/v2/users/{username}';
    RestRequest.AddParameter('username', Username, pkURLSEGMENT);

    RestRequest.Execute;

    if RestResponse.StatusCode = 200 then  // HTTP 200 OK
    begin
      JSONValue := RestResponse.JSONValue;
      if Assigned(JSONValue) and (JSONValue is TJSONObject) then
      begin
        JSONUser := JSONValue as TJSONObject;
        Result := TWordPressUser.Create;
        Result.ID := JSONUser.GetValue<Integer>('id');
        Result.Name := JSONUser.GetValue<string>('name');
        Result.Slug := JSONUser.GetValue<string>('slug');
        Result.Email := JSONUser.GetValue<string>('email', '');  // Email might not be always present
        Result.URL := JSONUser.GetValue<string>('url');
        Result.Description := JSONUser.GetValue<string>('description');
        // ... extract other fields as needed ...
      end;
    end;
  finally
    RestRequest.Free;
    RestResponse.Free;
    RestClient.Free;
    Authenticator.Free;
  end;
end;

function TWordPressApi.DeleteUser(const Username: string): Boolean;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  Authenticator: THTTPBasicAuthenticator;
begin
  Result := False;

  RestClient := nil;
  RestRequest := nil;
  RestResponse := nil;
  Authenticator := nil;
  try
    RestClient := TRESTClient.Create(FEndpoint);
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    Authenticator := THTTPBasicAuthenticator.Create(FUsername, FPassword);
    RestClient.Authenticator := Authenticator;

    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Method := rmDELETE;
    RestRequest.Resource := 'wp/v2/users/{username}';
    RestRequest.AddParameter('username', Username, pkURLSEGMENT);

    // Optional: Specify reassignment of posts if necessary
    // RestRequest.AddParameter('reassign', 'ID_OF_ANOTHER_USER', pkGETorPOST);

    RestRequest.Execute;

    if RestResponse.StatusCode = 200 then  // HTTP 200 OK
    begin
      Result := True;  // User deleted successfully
    end;
  finally
    RestRequest.Free;
    RestResponse.Free;
    RestClient.Free;
    Authenticator.Free;
  end;
end;


function TWordPressApi.ListPosts(status: string = 'publish'): TObjectList<TWordPressPost>;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  Authenticator: THTTPBasicAuthenticator;
  JSONPost: TJSONObject;
  JSONArray: TJSONArray;
  I : Integer;
  Post : TWordPressPost;
begin
  Result := TObjectList<TWordPressPost>.Create;

  RestClient := nil;
  RestRequest := nil;
  RestResponse := nil;
  Authenticator := nil;
  try
    RestClient := TRESTClient.Create(FEndpoint);
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    Authenticator := THTTPBasicAuthenticator.Create(FUsername, FPassword);
    RestClient.Authenticator := Authenticator;

    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Method := rmGET;
    RestRequest.Resource := 'wp/v2/posts';
    RestRequest.Params.AddItem('status', status);

    RestRequest.Execute;

    if RestResponse.StatusCode = 200 then  // HTTP 200 OK
    begin
      JSONArray := RestResponse.JSONValue as TJSONArray;
      for I := 0 to JSONArray.Count - 1 do
      begin
        JSONPost := JSONArray.Items[I] as TJSONObject;
        Post := TWordPressPost.Create;
        Post.ID := JSONPost.GetValue<Integer>('id');
        Post.Date := ISO8601ToDate(JSONPost.GetValue<string>('date'));
        Post.Slug := JSONPost.GetValue<string>('slug');
        Post.Status := JSONPost.GetValue<string>('status');
        Post.&Type := JSONPost.GetValue<string>('type');
        Post.Title := JSONPost.GetValue<string>('title.rendered');
        Post.Content := JSONPost.GetValue<string>('content.rendered');
        Post.Author := JSONPost.GetValue<Integer>('author');
        Post.Excerpt := JSONPost.GetValue<string>('excerpt.rendered');
        // ... extract other fields as needed ...

        Result.Add(Post);
      end;
    end;
  finally
    RestRequest.Free;
    RestResponse.Free;
    RestClient.Free;
    Authenticator.Free;
  end;
end;

function TWordPressApi.DeletePost(const PostID: Integer): Boolean;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  Authenticator: THTTPBasicAuthenticator;
begin
  Result := False;

  RestClient := nil;
  RestRequest := nil;
  RestResponse := nil;
  Authenticator := nil;
  try
    RestClient := TRESTClient.Create(FEndpoint);
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    Authenticator := THTTPBasicAuthenticator.Create(FUsername, FPassword);
    RestClient.Authenticator := Authenticator;

    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Method := rmDELETE;
    RestRequest.Resource := 'wp/v2/posts/{id}';
    RestRequest.AddParameter('id', PostID.ToString, pkURLSEGMENT);

    RestRequest.Execute;

    if RestResponse.StatusCode = 200 then  // HTTP 200 OK
    begin
      Result := True;  // Post deleted successfully
    end;
  finally
    RestRequest.Free;
    RestResponse.Free;
    RestClient.Free;
    Authenticator.Free;
  end;
end;


function TWordPressApi.ListUsers: TObjectList<TWordPressUser>;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  Authenticator: THTTPBasicAuthenticator;
  JSONUser: TJSONObject;
  JSONArray: TJSONArray;
  I : Integer;
  User : TWordPressUser;
begin
  Result := TObjectList<TWordPressUser>.Create;

  RestClient := nil;
  RestRequest := nil;
  RestResponse := nil;
  Authenticator := nil;
  try
    RestClient := TRESTClient.Create(FEndpoint);
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    Authenticator := THTTPBasicAuthenticator.Create(FUsername, FPassword);
    RestClient.Authenticator := Authenticator;

    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Method := rmGET;
    RestRequest.Resource := 'wp/v2/users';

    RestRequest.Execute;

    if RestResponse.StatusCode = 200 then  // HTTP 200 OK
    begin
      JSONArray := RestResponse.JSONValue as TJSONArray;
      for I := 0 to JSONArray.Count - 1 do
      begin
        JSONUser := JSONArray.Items[I] as TJSONObject;
        User := TWordPressUser.Create;
        User.ID := JSONUser.GetValue<Integer>('id');
        User.Name := JSONUser.GetValue<string>('name');
        User.Slug := JSONUser.GetValue<string>('slug');
        User.Email := JSONUser.GetValue<string>('email', '');  // Email might not be always present
        User.URL := JSONUser.GetValue<string>('url');
        User.Description := JSONUser.GetValue<string>('description');
        // ... extract other fields as needed ...

        Result.Add(User);
      end;
    end;
  finally
    RestRequest.Free;
    RestResponse.Free;
    RestClient.Free;
    Authenticator.Free;
  end;
end;

function TWordPressApi.ListPages(status: string): TObjectList<TWordPressPage>;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  Authenticator: THTTPBasicAuthenticator;
  JSONPage: TJSONObject;
  JSONArray: TJSONArray;
  I: Integer;
  Page: TWordPressPage;
begin
  Result := TObjectList<TWordPressPage>.Create;

  RestClient := nil;
  RestRequest := nil;
  RestResponse := nil;
  Authenticator := nil;
  try
    RestClient := TRESTClient.Create(FEndpoint);
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    Authenticator := THTTPBasicAuthenticator.Create(FUsername, FPassword);
    RestClient.Authenticator := Authenticator;

    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Method := rmGET;
    RestRequest.Resource := 'wp/v2/pages';
    RestRequest.Params.AddItem('status', status);

    RestRequest.Execute;

    if RestResponse.StatusCode = 200 then  // HTTP 200 OK
    begin
      JSONArray := RestResponse.JSONValue as TJSONArray;
      for I := 0 to JSONArray.Count - 1 do
      begin
        JSONPage := JSONArray.Items[I] as TJSONObject;
        Page := TWordPressPage.Create;
        Page.ID := JSONPage.GetValue<Integer>('id');
        Page.Date := ISO8601ToDate(JSONPage.GetValue<string>('date'));
        Page.Slug := JSONPage.GetValue<string>('slug');
        Page.Status := JSONPage.GetValue<string>('status');
        Page.&Type := JSONPage.GetValue<string>('type');
        Page.Title := JSONPage.GetValue<string>('title.rendered');
        Page.Content := JSONPage.GetValue<string>('content.rendered');
        Page.Author := JSONPage.GetValue<Integer>('author');
        Page.Excerpt := JSONPage.GetValue<string>('excerpt.rendered');
        // ... extract other fields as needed ...

        Result.Add(Page);
      end;
    end;
  finally
    RestRequest.Free;
    RestResponse.Free;
    RestClient.Free;
    Authenticator.Free;
  end;
end;

function TWordPressApi.DeletePage(const PageID: Integer): Boolean;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  Authenticator: THTTPBasicAuthenticator;
begin
  Result := False;

  RestClient := nil;
  RestRequest := nil;
  RestResponse := nil;
  Authenticator := nil;
  try
    RestClient := TRESTClient.Create(FEndpoint);
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    Authenticator := THTTPBasicAuthenticator.Create(FUsername, FPassword);
    RestClient.Authenticator := Authenticator;

    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Method := rmDELETE;
    RestRequest.Resource := 'wp/v2/pages/{id}';
    RestRequest.AddParameter('id', PageID.ToString, pkURLSEGMENT);

    RestRequest.Execute;

    if RestResponse.StatusCode = 200 then  // HTTP 200 OK
    begin
      Result := True;  // Page deleted successfully
    end;
  finally
    RestRequest.Free;
    RestResponse.Free;
    RestClient.Free;
    Authenticator.Free;
  end;
end;

function TWordPressApi.GetSiteSettings: TStringList;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  Authenticator: THTTPBasicAuthenticator;
  JSONValue: TJSONValue;
  JSONObj: TJSONObject;
  Pair: TJSONPair;
begin
  Result := TStringList.Create;

  RestClient := nil;
  RestRequest := nil;
  RestResponse := nil;
  Authenticator := nil;
  try
    RestClient := TRESTClient.Create(FEndpoint);
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    Authenticator := THTTPBasicAuthenticator.Create(FUsername, FPassword);
    RestClient.Authenticator := Authenticator;

    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Method := rmGET;
    RestRequest.Resource := 'wp/v2/settings';

    RestRequest.Execute;

    if RestResponse.StatusCode = 200 then  // HTTP 200 OK
    begin
      JSONValue := RestResponse.JSONValue;
      if Assigned(JSONValue) and (JSONValue is TJSONObject) then
      begin
        JSONObj := JSONValue as TJSONObject;
        for Pair in JSONObj do
        begin
          Result.AddPair(Pair.JsonString.Value , Pair.JsonValue.Value);
        end;
      end;
    end;
  finally
    RestRequest.Free;
    RestResponse.Free;
    RestClient.Free;
    Authenticator.Free;
  end;
end;

function TWordPressApi.CreateMedia(const FilePath: string; const Title: string = ''): TWordPressMedia;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  Authenticator: THTTPBasicAuthenticator;
  FileStream: TFileStream;
  JSONValue: TJSONValue;
  JSONMedia: TJSONObject;
  mime : TMimeTypes;
  mimeType : string;
  kind : TMimeTypes.TKind;
begin
  Result := nil;

  RestClient := nil;
  RestRequest := nil;
  RestResponse := nil;
  Authenticator := nil;
  FileStream := nil;
  try
    RestClient := TRESTClient.Create(FEndpoint);
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    Authenticator := THTTPBasicAuthenticator.Create(FUsername, FPassword);
    RestClient.Authenticator := Authenticator;


    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Method := rmPOST;
    RestRequest.Resource := 'wp/v2/media';

    // Set header for file upload
    RestRequest.Params.AddItem('Content-Disposition', 'attachment; filename="' + ExtractFileName(FilePath) + '"', pkHTTPHEADER, [poDoNotEncode]);

    mime := TMimeTypes.Create;
    try
      mime.AddDefTypes;
      if mime.GetFileInfo(FilePath, mimeType, kind) then
      begin
        RestRequest.Params.AddHeader('Content-Type', mimeType);//'application/octet-stream');
      end;
    finally
      FreeAndNil(mime);
    end;
    if not Title.IsEmpty then
      RestRequest.Params.AddItem('title', Title, pkQUERY);

    RestRequest.Params.AddHeader('Accept', 'application/json');

    // Load file
    FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
    RestRequest.AddBody(FileStream, ctAPPLICATION_OCTET_STREAM);

    RestRequest.Execute;

    if RestResponse.StatusCode = 201 then  // HTTP 201 Created
    begin
      JSONValue := RestResponse.JSONValue;
      if Assigned(JSONValue) and (JSONValue is TJSONObject) then
      begin
        JSONMedia := JSONValue as TJSONObject;
        Result := TWordPressMedia.Create;
        Result.ID := JSONMedia.GetValue<Integer>('id');
        Result.Title := JSONMedia.GetValue<string>('title.rendered');
        Result.URL := JSONMedia.GetValue<string>('source_url');
        // ... extract other fields as needed ...
      end;
    end;
  finally
    FileStream.Free;
    RestRequest.Free;
    RestResponse.Free;
    RestClient.Free;
    Authenticator.Free;
  end;
end;

function TWordPressApi.ListMedia: TObjectList<TWordPressMedia>;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  Authenticator: THTTPBasicAuthenticator;
  JSONArray: TJSONArray;
  I: Integer;
  MediaItem: TWordPressMedia;
  JSONMedia: TJSONObject;
begin
  Result := TObjectList<TWordPressMedia>.Create;

  RestClient := nil;
  RestRequest := nil;
  RestResponse := nil;
  Authenticator := nil;
  try
    RestClient := TRESTClient.Create(FEndpoint);
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    Authenticator := THTTPBasicAuthenticator.Create(FUsername, FPassword);
    RestClient.Authenticator := Authenticator;

    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Method := rmGET;
    RestRequest.Resource := 'wp/v2/media';

    RestRequest.Execute;

    if RestResponse.StatusCode = 200 then  // HTTP 200 OK
    begin
      JSONArray := RestResponse.JSONValue as TJSONArray;
      for I := 0 to JSONArray.Count - 1 do
      begin
        JSONMedia := JSONArray.Items[I] as TJSONObject;
        MediaItem := TWordPressMedia.Create;
        MediaItem.ID := JSONMedia.GetValue<Integer>('id');
        MediaItem.Title := JSONMedia.GetValue<string>('title.rendered');
        MediaItem.URL := JSONMedia.GetValue<string>('source_url');
        // ... extract other fields as needed ...

        Result.Add(MediaItem);
      end;
    end;
  finally
    RestRequest.Free;
    RestResponse.Free;
    RestClient.Free;
    Authenticator.Free;
  end;
end;

function TWordPressApi.RetrieveMedia(const MediaID: Integer): TWordPressMedia;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  Authenticator: THTTPBasicAuthenticator;
  JSONValue: TJSONValue;
  JSONMedia: TJSONObject;
begin
  Result := nil;

  RestClient := nil;
  RestRequest := nil;
  RestResponse := nil;
  Authenticator := nil;
  try
    RestClient := TRESTClient.Create(FEndpoint);
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    Authenticator := THTTPBasicAuthenticator.Create(FUsername, FPassword);
    RestClient.Authenticator := Authenticator;

    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Method := rmGET;
    RestRequest.Resource := 'wp/v2/media/{id}';
    RestRequest.AddParameter('id', IntToStr(MediaID), pkURLSEGMENT);

    RestRequest.Execute;

    if RestResponse.StatusCode = 200 then  // HTTP 200 OK
    begin
      JSONValue := RestResponse.JSONValue;
      if Assigned(JSONValue) and (JSONValue is TJSONObject) then
      begin
        JSONMedia := JSONValue as TJSONObject;
        Result := TWordPressMedia.Create;
        Result.ID := JSONMedia.GetValue<Integer>('id');
        Result.Title := JSONMedia.GetValue<string>('title.rendered');
        Result.URL := JSONMedia.GetValue<string>('source_url');
        // ... extract other fields as needed ...
      end;
    end;
  finally
    RestRequest.Free;
    RestResponse.Free;
    RestClient.Free;
    Authenticator.Free;
  end;
end;

function TWordPressApi.DeleteMedia(const MediaID: Integer; const ForceDelete: Boolean): Boolean;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  Authenticator: THTTPBasicAuthenticator;
begin
  Result := False;

  RestClient := nil;
  RestRequest := nil;
  RestResponse := nil;
  Authenticator := nil;
  try
    RestClient := TRESTClient.Create(FEndpoint);
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    Authenticator := THTTPBasicAuthenticator.Create(FUsername, FPassword);
    RestClient.Authenticator := Authenticator;

    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Method := rmDELETE;
    RestRequest.Resource := 'wp/v2/media/{id}';
    RestRequest.AddParameter('id', IntToStr(MediaID), pkURLSEGMENT);
    if ForceDelete then
      RestRequest.AddParameter('force', 'true', pkGETorPOST);

    RestRequest.Execute;

    if RestResponse.StatusCode = 200 then  // HTTP 200 OK
    begin
      Result := True;  // Media deleted successfully
    end;
  finally
    RestRequest.Free;
    RestResponse.Free;
    RestClient.Free;
    Authenticator.Free;
  end;
end;



end.
