unit uWordpressForm;

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
  TFormWordpress = class(TForm)
    btnWordpress: TButton;
    btnWebBrowser: TButton;
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
    pnlPage: TPanel;
    pnlBlock: TPanel;
    pnlPosts: TPanel;
    pnlUser: TPanel;
    btnDeletePage: TButton;
    btnDeletePost: TButton;
    btnDeleteBlock: TButton;
    btnDeleteUser: TButton;
    tsWpMedia: TTabSheet;
    lvMedia: TListView;
    pnlMedia: TPanel;
    btnDeleteMedia: TButton;
    procedure btnDeleteBlockClick(Sender: TObject);
    procedure btnDeleteMediaClick(Sender: TObject);
    procedure btnDeletePageClick(Sender: TObject);
    procedure btnDeletePostClick(Sender: TObject);
    procedure btnDeleteUserClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnWordpressClick(Sender: TObject);
    procedure btnWebBrowserClick(Sender: TObject);
    procedure lvMediaDblClick(Sender: TObject);
  private
    { Private declarations }
    FSettings : TInifile;
    FWp : TWordPressApi;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; inSettings: TInifile); reintroduce;
  end;

var
  FormWordpress: TFormWordpress;

implementation

{$R *.dfm}

uses uWebBrowser,
  uImageDisplayForm,
  uTwitterForm,
  uDiscourseForm
  ;

procedure TFormWordpress.btnDeleteBlockClick(Sender: TObject);
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

procedure TFormWordpress.btnDeleteMediaClick(Sender: TObject);
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

procedure TFormWordpress.btnDeletePageClick(Sender: TObject);
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

procedure TFormWordpress.btnDeletePostClick(Sender: TObject);
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

procedure TFormWordpress.btnDeleteUserClick(Sender: TObject);
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

procedure TFormWordpress.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWp);
end;

procedure TFormWordpress.FormCreate(Sender: TObject);
var
  WordpressSiteURL : string;
  WordpressUsername : string;
  WordpressPassword : string;
begin
  WordpressSiteURL := FSettings.ReadString('Wordpress', 'SiteURL', '');
  WordpressUsername := FSettings.ReadString('Wordpress', 'Username', '');
  WordpressPassword := FSettings.ReadString('Wordpress', 'Password', '');

  FWp := TWordPressApi.Create(WordpressSiteURL, WordpressUsername, WordpressPassword);
end;

procedure TFormWordpress.btnWordpressClick(Sender: TObject);
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

constructor TFormWordpress.Create(AOwner: TComponent; inSettings: TInifile);
begin
  inherited Create(AOwner);
  FSettings := inSettings;
end;

procedure TFormWordpress.btnWebBrowserClick(Sender: TObject);
begin
  Form1.ShowModal;
end;

procedure TFormWordpress.lvMediaDblClick(Sender: TObject);
var
  FormImageDisplay: TFormImageDisplay;
begin
  if Assigned(lvMedia.Selected) then
  begin
    FormImageDisplay := TFormImageDisplay.Create(nil);
    try
      FormImageDisplay.ShowImage(lvMedia.Selected.SubItems[1]);
    finally
      FreeAndNil(FormImageDisplay);
    end;
  end;
end;

end.