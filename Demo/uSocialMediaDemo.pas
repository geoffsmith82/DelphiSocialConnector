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
  Vcl.ExtCtrls,
  uDiscourse,
  uWordpress
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
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    btnDeletePage: TButton;
    btnDeletePost: TButton;
    btnDeleteBlock: TButton;
    btnDeleteUser: TButton;
    tsWpMedia: TTabSheet;
    lvMedia: TListView;
    Panel5: TPanel;
    btnDeleteMedia: TButton;
    procedure btnDeleteBlockClick(Sender: TObject);
    procedure btnDeleteMediaClick(Sender: TObject);
    procedure btnDeletePageClick(Sender: TObject);
    procedure btnDeletePostClick(Sender: TObject);
    procedure btnDeleteUserClick(Sender: TObject);
    procedure btnDiscourseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnWordpressClick(Sender: TObject);
    procedure btnTweetClick(Sender: TObject);
    procedure btnWebBrowserClick(Sender: TObject);
    procedure lvMediaDblClick(Sender: TObject);
  private
    { Private declarations }
    FSettings : TInifile;
    FWp : TWordPressApi;

    FDiscourse: TDiscourseAPI;
  public
    { Public declarations }
  end;

var
  frmSocialMainForm: TfrmSocialMainForm;

implementation

{$R *.dfm}

uses uWebBrowswer,
  uImageDisplayForm
  , uTwitterForm;

procedure TfrmSocialMainForm.btnDeleteBlockClick(Sender: TObject);
var
  BlockID : Integer;
begin
  if Assigned(lvBlocks.Selected) then
  begin
    BlockID := lvBlocks.Selected.Caption.ToInteger;
    FWp.DeleteBlock(BlockID);
    lvBlocks.Selected.Delete;
  end;
end;

procedure TfrmSocialMainForm.btnDeleteMediaClick(Sender: TObject);
var
  MediaID : Integer;
begin
  if Assigned(lvMedia.Selected) then
  begin
    MediaID := lvMedia.Selected.Caption.ToInteger;
    FWp.DeleteMedia(MediaID);
    lvMedia.Selected.Delete;
  end;
end;

procedure TfrmSocialMainForm.btnDeletePageClick(Sender: TObject);
var
  PageID : Integer;
begin
  if Assigned(lvPages.Selected) then
  begin
    PageID := lvPages.Selected.Caption.ToInteger;
    FWp.DeletePage(PageID);
    lvPages.Selected.Delete;
  end;
end;

procedure TfrmSocialMainForm.btnDeletePostClick(Sender: TObject);
var
  PostID : Integer;
begin
  if Assigned(lvPosts.Selected) then
  begin
    PostID := lvPosts.Selected.Caption.ToInteger;
    FWp.DeletePost(PostID);
    lvPosts.Selected.Delete;
  end;
end;

procedure TfrmSocialMainForm.btnDeleteUserClick(Sender: TObject);
var
  UserID : Integer;
begin
  if Assigned(lvUsers.Selected) then
  begin
    UserID := lvUsers.Selected.Caption.ToInteger;
    FWp.DeleteUser(UserID);
    lvUsers.Selected.Delete;
  end;
end;

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
  Settings : TStringList;
  mediaItem: TWordPressMedia;
  filename : string;
  block : TWordPressBlock;
  lvPostItem : TListItem;
  lvPageItem : TListItem;
  lvBlockItem : TListItem;
  lvUserItem : TListItem;
  lvMediaItem : TListItem;
begin
  FWp.CreatePost('Test Title', '<h1>Test Content</h1>This is some content');
  posts := FWp.ListPosts('');
  try
    Memo1.Lines.Add('=== POSTS ===');
    for i := 0 to posts.Count - 1 do
    begin
      Memo1.Lines.Add(posts[i].ID.ToString + ' ' + posts[i].Title);
      lvPostItem := lvPosts.Items.Add;
      lvPostItem.Caption := Posts[i].ID.ToString;
      lvPostItem.SubItems.Add(posts[i].Title);
      lvPostItem.SubItems.Add(posts[i].Status);
    end;
  finally
    FreeAndNil(posts);
  end;

  pages := FWp.ListPages('');
  try
    Memo1.Lines.Add('=== PAGES ===');
    for i := 0 to pages.Count - 1 do
    begin
      lvPageItem := lvPages.Items.Add;
      lvPageItem.Caption := pages[i].ID.ToString;
      lvPageItem.SubItems.Add(pages[i].Title);
      lvPageItem.SubItems.Add(pages[i].Status);
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
      lvBlockItem.SubItems.Add(blocks[i].Status);
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
      lvMediaItem := lvMedia.Items.Add;
      lvMediaItem.Caption := mediaList[i].ID.ToString;
      lvMediaItem.SubItems.Add(mediaList[i].Title);
      lvMediaItem.SubItems.Add(mediaList[i].URL);
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
  FormTwitter : TFormTwitter;
begin
  FormTwitter := TFormTwitter.Create(nil, FSettings);
  try
    FormTwitter.ShowModal;
  finally
    FreeAndNil(FormTwitter);
  end;
end;

procedure TfrmSocialMainForm.btnWebBrowserClick(Sender: TObject);
begin
  Form1.ShowModal;
end;

procedure TfrmSocialMainForm.lvMediaDblClick(Sender: TObject);
begin
  if Assigned(lvMedia.Selected) then
  begin
    FormImageDisplay.ShowImage(lvMedia.Selected.SubItems[1]);
  end;
end;

end.
