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
  Vcl.ComCtrls,
  uDiscourse,
  uWordpress,
  uTwitter
  ;

type
  TfrmSocialMainForm = class(TForm)
    btnWordpress: TButton;
    btnTweet: TButton;
    btnWebBrowser: TButton;
    btnDiscourse: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Pages: TTabSheet;
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    Posts: TTabSheet;
    lvPosts: TListView;
    lvPages: TListView;
    TabSheet2: TTabSheet;
    lvBlocks: TListView;
    TsWpUsers: TTabSheet;
    lvUsers: TListView;
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
  WordpressSiteURL : string;
  WordpressUsername : string;
  WordpressPassword : string;
  DiscourseBaseURL : string;
  DiscourseAPIKey : string;
  DiscourseUsername : string;
begin
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  WordpressSiteURL := FSettings.ReadString('Wordpress', 'SiteURL', '');
  WordpressUsername := FSettings.ReadString('Wordpress', 'Username', '');
  WordpressPassword := FSettings.ReadString('Wordpress', 'Password', '');

  FWp := TWordPressApi.Create(WordpressSiteURL, WordpressUsername, WordpressPassword);

  DiscourseUsername := FSettings.ReadString('Discourse', 'Username', '');
  DiscourseBaseURL := FSettings.ReadString('Discourse', 'BaseURL', '');
  DiscourseAPIKey := FSettings.ReadString('Discourse', 'APIKey', '');

  FDiscourse := TDiscourseAPI.Create(DiscourseBaseURL, DiscourseAPIKey, DiscourseUsername);
end;

procedure TfrmSocialMainForm.btnWordpressClick(Sender: TObject);
var
  posts : TObjectList<TWordPressPost>;
  pages : TObjectList<TWordPressPage>;
  blocks : TObjectList<TWordPressBlock>;
  mediaList : TObjectList<TWordPressMedia>;
  categories : TObjectList<TWordPressCategory>;
  users : TObjectList<TWordPressUser>;
  i : Integer;
  DeleteID : Integer;
  Settings : TStringList;
  mediaItem: TWordPressMedia;
  filename : string;
  block : TWordPressBlock;
  lvPostItem : TListItem;
  lvPageItem : TListItem;
  lvBlockItem : TListItem;
  lvUserItem : TListItem;
begin
  FWp.CreatePost('Test Title', '<h1>Test Content</h1>This is some content');
  DeleteID := -1;
  posts := FWp.ListPosts('draft');
  try
    Memo1.Lines.Add('=== POSTS ===');
    for i := 0 to posts.Count - 1 do
    begin
      Memo1.Lines.Add(posts[i].ID.ToString + ' ' + posts[i].Title);
      lvPostItem := lvPosts.Items.Add;
      lvPostItem.Caption := Posts[i].ID.ToString;
      lvPostItem.SubItems.Add(posts[i].Title);
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
      lvPageItem := lvPages.Items.Add;
      lvPageItem.Caption := pages[i].ID.ToString;
      lvPageItem.SubItems.Add(pages[i].Title);
      Memo1.Lines.Add(pages[i].ID.ToString  + ' ' + pages[i].Title);
    end;
  finally
    FreeAndNil(pages);
  end;

  blocks := FWp.ListBlocks;
  try
    Memo1.Lines.Add('=== BLOCKS ===');
    for i := 0 to blocks.Count - 1 do
    begin
      lvBlockItem := lvBlocks.Items.Add;
      lvBlockItem.Caption := blocks[i].ID.ToString;
      lvBlockItem.SubItems.Add(blocks[i].Title);
      Memo1.Lines.Add(blocks[i].ID.ToString  + ' ' + blocks[i].Title);
      Memo1.Lines.Add(blocks[i].Content);
    end;
  finally
    FreeAndNil(blocks);
  end;

  block := FWp.UpdateBlock(12, 'Updated Block', '''
  <!-- wp:paragraph -->
<p>This block was updated at
'''
+ ' ' + DateTimeToStr(now) + '''
</p>
<!-- /wp:paragraph -->
''' ,  'my-test-pattern-block', 'wp_block');
  FreeAndNil(block);

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

  users := FWp.ListUsers;
  try
    Memo1.Lines.Add('=== USER ===');
    for i := 0 to users.Count - 1 do
    begin
      lvUserItem := lvUsers.Items.Add;
      lvUserItem.Caption := users[i].ID.ToString;
      lvUserItem.SubItems.Add(users[i].Username);
      lvUserItem.SubItems.Add(users[i].Name);
      Memo1.Lines.Add('ID=' + users[i].ID.ToString + ' Name=' + users[i].Name + ' Slug=' + users[i].Slug + ' Description=' + users[i].Description);
    end;
  finally
    FreeAndNil(users);
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
