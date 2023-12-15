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
  TDiscoursePost = class
  private
    FId: Integer;
    FContent: string;
    FAuthor: string;
    FTimestamp: TDateTime;
    FTitle: string;
    // Add more fields as per the JSON structure
  public
    property Id: Integer read FId write FId;
    property Content: string read FContent write FContent;
    property Title: string read FTitle write FTitle;
    property Author: string read FAuthor write FAuthor;
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
    // Add more properties as per the JSON structure
  end;


  TDiscourseCategory = class
  private
    FId: Integer;
    FName: string;
    // Add more fields as per the JSON structure
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    // Add more properties as per the JSON structure
  end;

  TDiscourseGroup = class
  private
    FId: Integer;
    FName: string;
    // Add more fields as per the JSON structure
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    // Add more properties as per the JSON structure
  end;

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
    constructor Create(const BaseURL, APIKey, Username: string);
    function GetTopics(const Category: string): string;
    function GetUsers: TObjectList<TDiscourseUser>;
    function GetPosts: TObjectList<TDiscoursePost>;
    function GetGroups: TObjectList<TDiscourseGroup>;
    // Add more methods for other API endpoints
  public
    function GetCategories: TObjectList<TDiscourseCategory>;
  end;

implementation

{ TDiscourseAPI }

constructor TDiscourseAPI.Create(const BaseURL, APIKey, Username: string);
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
  Result.Params.AddHeader('Api-Key', FAPIKey);
  Result.Params.AddHeader('Api-Username', FUsername);
end;

function TDiscourseAPI.GetTopics(const Category: string): string;
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
    FreeAndNil(RESTRequest.Response);
    FreeAndNil(RESTRequest.Client);
    FreeAndNil(RESTRequest);
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
    FreeAndNil(RESTRequest.Response);
    FreeAndNil(RESTRequest.Client);
    FreeAndNil(RESTRequest);
  end;
end;

function TDiscourseAPI.GetCategories: TObjectList<TDiscourseCategory>;
var
  RESTRequest: TRESTRequest;
  JSONValue: TJSONValue;
  JSONArray: TJSONArray;
  JSONItem: TJSONValue;
  Category: TDiscourseCategory;
  I: Integer;
begin
  Result := TObjectList<TDiscourseCategory>.Create(True); // 'True' for owning the objects
  RESTRequest := CreateRESTRequest;
  try
    RESTRequest.Resource := 'categories.json'; // Update this with the correct endpoint
    RESTRequest.Execute;
    JSONValue := RESTRequest.Response.JSONValue.GetValue<TJSONArray>('category_list.categories');
    if JSONValue is TJSONArray then
    begin
      JSONArray := JSONValue as TJSONArray;
      for I := 0 to JSONArray.Count - 1 do
      begin
        JSONItem := JSONArray.Items[I];
        Category := TDiscourseCategory.Create;
        try
          Category.Id := JSONItem.GetValue<Integer>('id', 0);
          Category.Name := JSONItem.GetValue<string>('name', '');
          // Set other properties similarly
          Result.Add(Category);
        except
          Category.Free;
          raise;
        end;
      end;
    end;
  finally
    FreeAndNil(RESTRequest.Response);
    FreeAndNil(RESTRequest.Client);
    FreeAndNil(RESTRequest);
  end;
end;

function TDiscourseAPI.GetPosts: TObjectList<TDiscoursePost>;
var
  RESTRequest: TRESTRequest;
  JSONValue: TJSONValue;
  JSONArray: TJSONArray;
  JSONItem: TJSONValue;
  Post: TDiscoursePost;
  I: Integer;
begin
  Result := TObjectList<TDiscoursePost>.Create; // 'True' for owning the objects
  RESTRequest := CreateRESTRequest;
  try
    RESTRequest.Resource := 'posts.json'; // Update this with the correct endpoint
    RESTRequest.Execute;
    JSONValue := RESTRequest.Response.JSONValue.GetValue<TJSONArray>('latest_posts');
    if JSONValue is TJSONArray then
    begin
      JSONArray := JSONValue as TJSONArray;
      for I := 0 to JSONArray.Count - 1 do
      begin
        JSONItem := JSONArray.Items[I];
        Post := TDiscoursePost.Create;
        try
          Post.Id := JSONItem.GetValue<Integer>('id', 0);
          Post.Content := JSONItem.GetValue<string>('raw', '');
          Post.Author := JSONItem.GetValue<string>('username', '');
          Post.Title := JSONItem.GetValue<string>('topic_title', '');
          Post.Timestamp := JSONItem.GetValue<TDateTime>('timestamp', 0);
          // Set other properties similarly
          Result.Add(Post);
        except
          Post.Free;
          raise;
        end;
      end;
    end;
  finally
    FreeAndNil(RESTRequest.Response);
    FreeAndNil(RESTRequest.Client);
    FreeAndNil(RESTRequest);
  end;
end;

function TDiscourseAPI.GetGroups: TObjectList<TDiscourseGroup>;
var
  RESTRequest: TRESTRequest;
  JSONValue: TJSONValue;
  JSONArray: TJSONArray;
  JSONItem: TJSONValue;
  Group: TDiscourseGroup;
  I: Integer;
begin
  Result := TObjectList<TDiscourseGroup>.Create(True); // 'True' for owning the objects
  RESTRequest := CreateRESTRequest;
  try
    RESTRequest.Resource := 'groups.json'; // Adjust this endpoint as necessary
    RESTRequest.Execute;
    JSONValue := RESTRequest.Response.JSONValue.GetValue<TJSONArray>('groups');
    if JSONValue is TJSONArray then
    begin
      JSONArray := JSONValue as TJSONArray;
      for I := 0 to JSONArray.Count - 1 do
      begin
        JSONItem := JSONArray.Items[I];
        Group := TDiscourseGroup.Create;
        try
          Group.Id := JSONItem.GetValue<Integer>('id', 0);
          Group.Name := JSONItem.GetValue<string>('name', '');
          // Set other properties similarly
          Result.Add(Group);
        except
          Group.Free;
          raise;
        end;
      end;
    end;
  finally
    FreeAndNil(RESTRequest.Response);
    FreeAndNil(RESTRequest.Client);
    FreeAndNil(RESTRequest);
  end;
end;


end.
