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
  uWordpress,
  uTwitter
  ;

type
  TfrmSocialMainForm = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    btnTweet: TButton;
    Button3: TButton;
    Button2: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnTweetClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FSettings : TInifile;
    FWp : TWordPressApi;
    FTwitter: TTwitterApi;
  public
    { Public declarations }
  end;

var
  frmSocialMainForm: TfrmSocialMainForm;

implementation

{$R *.dfm}

uses uWebBrowswer;

procedure TfrmSocialMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWp);
  FreeAndNil(FSettings);
end;

procedure TfrmSocialMainForm.FormCreate(Sender: TObject);
var
  WordpressUsername : string;
  WordpressPassword : string;
begin
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  WordpressUsername := FSettings.ReadString('Wordpress', 'Username', '');
  WordpressPassword := FSettings.ReadString('Wordpress', 'Password', '');

  FWp := TWordPressApi.Create('https://www.readabouttech.info/wp-json/', WordpressUsername, WordpressPassword);
end;

procedure TfrmSocialMainForm.Button1Click(Sender: TObject);
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

procedure TfrmSocialMainForm.Button2Click(Sender: TObject);
begin
  Form1.ShowModal;
end;

end.
