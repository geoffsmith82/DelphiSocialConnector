unit uSocialMediaDemo;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  uDiscourse,
  uWordpress,
  uTwitter
  ;

type
  TfrmSocialMainForm = class(TForm)
    Memo1: TMemo;
    btnWordpress: TButton;
    btnTweet: TButton;
    btnWebBrowser: TButton;
    btnDiscourse: TButton;
    procedure btnDiscourseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnWordpressClick(Sender: TObject);
    procedure btnTweetClick(Sender: TObject);
    procedure btnWebBrowserClick(Sender: TObject);
  private
    { Private declarations }
    FSettings : TInifile;
    FWp : TWordPressApi;
    FTwitter: TTwitterApi;
    FDiscourse: TDiscourseAPI;
  public
    { Public declarations }
  end;

var
  frmSocialMainForm: TfrmSocialMainForm;

implementation

{$R *.dfm}

uses uWebBrowswer;

procedure TfrmSocialMainForm.btnDiscourseClick(Sender: TObject);
var
  posts : TObjectList<TDiscoursePost>;
  categories : TObjectList<TDiscourseCategory>;
  users : TObjectList<TDiscourseUser>;
  groups : TObjectList<TDiscourseGroup>;
  I: Integer;
begin
  users := FDiscourse.GetUsers;
  try
    for I := 0 to users.Count - 1 do
    begin
      Memo1.Lines.Add(users[i].Id.ToString + ' ' + users[i].Username + ' ' + users[i].Name);
    end;
  finally
    FreeAndNil(users);
  end;


  Memo1.Lines.Add('==== GROUPS ====');
  groups := FDiscourse.GetGroups;
  try
    for I := 0 to groups.Count - 1 do
    begin
      Memo1.Lines.Add(groups[i].Id.ToString + ' ' + groups[i].Name);
    end;
  finally
    FreeAndNil(groups);
  end;


  Memo1.Lines.Add('==== CATEGORIES ====');
  categories := FDiscourse.GetCategories;
  try
    for I := 0 to categories.Count - 1 do
    begin
      Memo1.Lines.Add(categories[i].Id.ToString + ' ' + categories[i].Name);
    end;
  finally
    FreeAndNil(categories);
  end;

  posts := FDiscourse.GetPosts;
  try
    for I := 0 to posts.Count - 1 do
    begin
      Memo1.Lines.Add(posts[i].Id.ToString + ' ' + posts[i].Author);
      Memo1.Lines.Add(posts[i].Content);
    end;
  finally
    FreeAndNil(posts);
  end;
end;

procedure TfrmSocialMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWp);
  FreeAndNil(FDiscourse);
  FreeAndNil(FTwitter);
  FreeAndNil(FSettings);
end;

procedure TfrmSocialMainForm.FormCreate(Sender: TObject);
var
  WordpressUsername : string;
  WordpressPassword : string;
  DiscourseBaseURL : string;
  DiscourseAPIKey : string;
  DiscourseUsername : string;
begin
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  WordpressUsername := FSettings.ReadString('Wordpress', 'Username', '');
  WordpressPassword := FSettings.ReadString('Wordpress', 'Password', '');

  FWp := TWordPressApi.Create('https://www.readabouttech.info/wp-json/', WordpressUsername, WordpressPassword);

  DiscourseUsername := FSettings.ReadString('Discourse', 'Username', '');
  DiscourseBaseURL := FSettings.ReadString('Discourse', 'BaseURL', '');
  DiscourseAPIKey := FSettings.ReadString('Discourse', 'APIKey', '');

  FDiscourse := TDiscourseAPI.Create(DiscourseBaseURL, DiscourseAPIKey, DiscourseUsername);
end;

procedure TfrmSocialMainForm.btnWordpressClick(Sender: TObject);
var
  posts : TObjectList<TWordPressPost>;
  pages : TObjectList<TWordPressPage>;
  mediaList : TObjectList<TWordPressMedia>;
  categories : TObjectList<TWordPressCategory>;
  i : Integer;
  DeleteID : Integer;
  Settings : TStringList;
  mediaItem: TWordPressMedia;
  filename : string;
begin
//  FWp.CreatePost('Test Title', 'Test Content');
  DeleteID := -1;
  posts := FWp.ListPosts('draft');
  try
    Memo1.Lines.Add('=== POSTS ===');
    for i := 0 to posts.Count - 1 do
    begin
      Memo1.Lines.Add(posts[i].Title);
      if posts[i].Title = 'Test Title' then
      begin
        DeleteID := posts[i].ID;
      end;
    end;

    FWp.DeletePost(DeleteID);
  finally
    FreeAndNil(posts);
  end;

  pages := FWp.ListPages('publish');
  try
    Memo1.Lines.Add('=== PAGES ===');
    for i := 0 to pages.Count - 1 do
    begin
      Memo1.Lines.Add(pages[i].Title);
    end;
  finally
    FreeAndNil(pages);
  end;

  categories := FWp.ListCategories;
  try
    Memo1.Lines.Add('=== CATEGORY ===');
    for i := 0 to categories.Count - 1 do
    begin
      Memo1.Lines.Add('ID=' + categories[i].ID.ToString + ' Name=' + categories[i].Name + ' Slug=' + categories[i].Slug + ' Description=' + categories[i].Description);
    end;
  finally
    FreeAndNil(categories);
  end;


  Memo1.Lines.Add('=== Media ===');
  filename := 'D:\ADUG\Symposium2023\advert.png';
  mediaItem := FWp.CreateMedia(filename, 'Test Image Upload');
  try
    Memo1.Lines.Add('Uploaded MediaID: ' + mediaItem.ID.ToString);
  finally
    FreeAndNil(mediaItem);
  end;


  mediaList := FWp.ListMedia;
  try

    for i := 0 to mediaList.Count - 1 do
    begin
      Memo1.Lines.Add(mediaList[i].Title);
    end;
  finally
    FreeAndNil(mediaList);
  end;

  Settings := FWp.GetSiteSettings;
  try
    Memo1.Lines.Add('=== SITE SETTINGS ===');
    Memo1.Lines.AddStrings(Settings);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfrmSocialMainForm.btnTweetClick(Sender: TObject);
var
  TwitterAPIKey : string;
  TwitterKeySecret : string;
  TwitterAccessToken : string;
  TwitterAccessTokenSecret : string;
begin
  TwitterAPIKey := FSettings.ReadString('Twitter', 'TwitterAPIKey', '');
  TwitterKeySecret := FSettings.ReadString('Twitter', 'TwitterKeySecret', '');
  TwitterAccessToken := FSettings.ReadString('Twitter', 'TwitterAccessToken', '');
  TwitterAccessTokenSecret := FSettings.ReadString('Twitter', 'TwitterAccessTokenSecret', '');

  FTwitter := TTwitterApi.Create(TwitterAPIKey, TwitterKeySecret, TwitterAccessToken, TwitterAccessTokenSecret);
  try
    FTwitter.PostTweet('Check out https://forums.adug.org.au/c/meetings/20 to find out next meetings');
  finally
    FreeAndNil(FTwitter);
  end;
end;

procedure TfrmSocialMainForm.btnWebBrowserClick(Sender: TObject);
begin
  Form1.ShowModal;
end;

end.
