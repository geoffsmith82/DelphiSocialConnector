unit uWordpressLogin;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.IniFiles,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls
  ;

type
  TfrmWordpressLogin = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edtSiteAddress: TEdit;
    edtUsername: TEdit;
    edtPassword: TEdit;
    btnLogin: TButton;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; settings: TIniFile);
  end;

implementation

{$R *.dfm}

procedure TfrmWordpressLogin.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmWordpressLogin.btnLoginClick(Sender: TObject);
begin
  FSettings.WriteString('Wordpress', 'SiteURL', edtSiteAddress.Text);
  FSettings.WriteString('Wordpress', 'Username', edtUsername.Text);
  FSettings.WriteString('Wordpress', 'Password', edtPassword.Text);
end;

constructor TfrmWordpressLogin.Create(AOwner: TComponent; settings: TIniFile);
begin
  inherited Create(AOwner);
  FSettings := Settings;
  edtSiteAddress.Text := FSettings.ReadString('Wordpress', 'SiteURL', '');
  edtUsername.Text := FSettings.ReadString('Wordpress', 'Username', '');
  edtPassword.Text := FSettings.ReadString('Wordpress', 'Password', '');
end;

end.
