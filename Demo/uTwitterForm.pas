unit uTwitterForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IniFiles,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  uTwitter
  ;

type
  TFormTwitter = class(TForm)
    mmoMessage: TMemo;
    Post: TButton;
    Image1: TImage;
    procedure PostClick(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
    FTwitter: TTwitterApi;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; inSettings: TInifile); reintroduce;
  end;

implementation

{$R *.dfm}

constructor TFormTwitter.Create(AOwner: TComponent; inSettings: TInifile);
begin
  inherited Create(AOwner);
  FSettings := inSettings;
end;

procedure TFormTwitter.PostClick(Sender: TObject);
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
    FTwitter.PostTweet(mmoMessage.Text);
    ShowMessage('Message Sent');
  finally
    FreeAndNil(FTwitter);
  end;
end;

end.
