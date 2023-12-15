unit uDiscourseForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IniFiles,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  uDiscourse
  ;

type
  TFormDiscourse = class(TForm)
    PageControl1: TPageControl;
    tsLog: TTabSheet;
    tsUsers: TTabSheet;
    lvUsers: TListView;
    tsGroups: TTabSheet;
    lvGroups: TListView;
    Memo1: TMemo;
    tsCategories: TTabSheet;
    lvCategory: TListView;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvCategoryDblClick(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
    FDiscourse: TDiscourseAPI;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; inSettings: TInifile); reintroduce;
  end;

implementation

{$R *.dfm}

{ TFormDiscourse }

constructor TFormDiscourse.Create(AOwner: TComponent; inSettings: TInifile);
begin
  inherited Create(AOwner);
  FSettings := inSettings;
end;

procedure TFormDiscourse.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDiscourse);
end;

procedure TFormDiscourse.FormCreate(Sender: TObject);
var
  DiscourseBaseURL : string;
  DiscourseAPIKey : string;
  DiscourseUsername : string;
  posts : TObjectList<TDiscoursePost>;
  categories : TObjectList<TDiscourseCategory>;
  users : TObjectList<TDiscourseUser>;
  groups : TObjectList<TDiscourseGroup>;
  lvGroupItem : TListItem;
  lvUserItem : TListItem;
  lvCategoryItem : TListItem;
  I: Integer;
begin
  DiscourseUsername := FSettings.ReadString('Discourse', 'Username', '');
  DiscourseBaseURL := FSettings.ReadString('Discourse', 'BaseURL', '');
  DiscourseAPIKey := FSettings.ReadString('Discourse', 'APIKey', '');

  FDiscourse := TDiscourseAPI.Create(DiscourseBaseURL, DiscourseAPIKey, DiscourseUsername);

  users := FDiscourse.GetUsers;
  try
    for I := 0 to users.Count - 1 do
    begin
      lvUserItem := lvUsers.Items.Add;
      lvUserItem.Caption := users[i].Id.ToString;
      lvUserItem.SubItems.Add(users[i].Username);
      lvUserItem.SubItems.Add(users[i].Name);
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
      lvGroupItem := lvGroups.Items.Add;
      lvGroupItem.Caption := groups[i].Id.ToString;
      lvGroupItem.SubItems.Add(groups[i].Name);
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
      lvCategoryItem := lvCategory.Items.Add;
      lvCategoryItem.Caption := categories[i].Id.ToString;
      lvCategoryItem.SubItems.Add(categories[i].Name);
      lvCategoryItem.SubItems.Add(categories[i].Slug);
      Memo1.Lines.Add(categories[i].Id.ToString + ' ' + categories[i].Name);
    end;
  finally
    FreeAndNil(categories);
  end;


  Memo1.Lines.Add('==== POSTS ====');
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

procedure TFormDiscourse.lvCategoryDblClick(Sender: TObject);
var
  slug : string;
  id : Integer;
begin
  if Assigned(lvCategory.Selected) then
  begin
    id := lvCategory.Selected.Caption.ToInteger;
    slug := lvCategory.Selected.SubItems[1];
    FDiscourse.GetTopics(slug, id);
  end;
end;

end.
