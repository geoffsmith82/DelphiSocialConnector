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
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnTweetClick(Sender: TObject);
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
  i : Integer;
begin
  posts := FWp.ListPosts;
  try
    for i := 0 to posts.Count - 1 do
    begin
      Memo1.Lines.Add(posts[i].Title);
    end;
  finally
    FreeAndNil(posts);
  end;

  pages := FWp.ListPages;
  try
    for i := 0 to pages.Count - 1 do
    begin
      Memo1.Lines.Add(pages[i].Title);
    end;
  finally
    FreeAndNil(pages);
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

end.
