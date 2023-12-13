unit uDiscourse;

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
  TDiscourseUser = class
  public
    Id: Integer;
    Username: string;
    Name: string;
    Trust_Level: Integer;
    Title: string;
    Active: Boolean;
    Admin: Boolean;
    Moderator: Boolean;
    Staged: Boolean;
  end;

  TDiscourseAPI = class
  private
    FBaseURL: string;
    FAPIKey: string;
    FUsername: string;
    function CreateRESTRequest: TRESTRequest;
  public
    constructor Create(BaseURL, APIKey, Username: string);
    function GetTopics(Category: string): string;
    function GetUsers: TObjectList<TDiscourseUser>;
    // Add more methods for other API endpoints
  end;

implementation

{ TDiscourseAPI }

constructor TDiscourseAPI.Create(BaseURL, APIKey, Username: string);
begin
  inherited Create;
  FBaseURL := BaseURL;
  FAPIKey := APIKey;
  FUsername := Username;
end;

function TDiscourseAPI.CreateRESTRequest: TRESTRequest;
var
  RESTClient: TRESTClient;
  RESTResponse: TRESTResponse;
begin
  RESTClient := TRESTClient.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  Result := TRESTRequest.Create(nil);
  Result.Client := RESTClient;
  Result.Response := RESTResponse;
  RESTClient.BaseURL := FBaseURL;
  Result.Params.AddItem('api_key', FAPIKey, pkGETorPOST);
  Result.Params.AddItem('api_username', FUsername, pkGETorPOST);
end;

function TDiscourseAPI.GetTopics(Category: string): string;
var
  RESTRequest: TRESTRequest;
begin
  RESTRequest := CreateRESTRequest;
  try
    RESTRequest.Resource := 'categories/{category}/topics.json';
    RESTRequest.Params.AddUrlSegment('category', Category);
    RESTRequest.Execute;
    Result := RESTRequest.Response.Content;
  finally
    RESTRequest.Free;
  end;
end;

function TDiscourseAPI.GetUsers: TObjectList<TDiscourseUser>;
var
  RESTRequest: TRESTRequest;
  JSONValue: TJSONValue;
  JSONArray: TJSONArray;
  JSONItem: TJSONValue;
  User: TDiscourseUser;
  I: Integer;
begin
  Result := TObjectList<TDiscourseUser>.Create(True); // 'True' for owning the objects
  RESTRequest := CreateRESTRequest;
  try
    RESTRequest.Resource := 'admin/users/list/active.json';
    RESTRequest.Execute;
    JSONValue := RESTRequest.Response.JSONValue;
    try
      if JSONValue is TJSONArray then
      begin
        JSONArray := JSONValue as TJSONArray;
        for I := 0 to JSONArray.Count - 1 do
        begin
          JSONItem := JSONArray.Items[I];
          User := TDiscourseUser.Create;
          try
            User.Id := JSONItem.GetValue<Integer>('id', 0);
            User.Username := JSONItem.GetValue<string>('username', '');
            User.name := JSONItem.GetValue<string>('name', '');
            User.Title := JSONItem.GetValue<string>('title', '');
            User.Trust_Level := JSONItem.GetValue<Integer>('trust_level', 0);
            User.Active := JSONItem.GetValue<Boolean>('active', False);
            User.Admin := JSONItem.GetValue<Boolean>('admin', False);
            User.Moderator := JSONItem.GetValue<Boolean>('moderator', False);
            User.Staged := JSONItem.GetValue<Boolean>('staged', False);
            // Set other properties similarly
            Result.Add(User);
          except
            User.Free;
            raise;
          end;
        end;
      end;
    finally

    end;
  finally
    RESTRequest.Free;
  end;
end;


end.
