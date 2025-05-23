unit uMainForm;

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
  uTwitterForm,
  uDiscourseForm,
  uWordpressForm,
  uWordpressLogin,
  uFacebookForm
  ;

type
  TFormMain = class(TForm)
    btnTwitterX: TButton;
    btnWordpress: TButton;
    btnDiscourse: TButton;
    btnFacebook: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnDiscourseClick(Sender: TObject);
    procedure btnFacebookClick(Sender: TObject);
    procedure btnTwitterXClick(Sender: TObject);
    procedure btnWordpressClick(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSettings);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
end;

procedure TFormMain.btnDiscourseClick(Sender: TObject);
var
  FormDiscourse: TFormDiscourse;
begin
  FormDiscourse := TFormDiscourse.Create(nil, FSettings);
  try
    FormDiscourse.ShowModal;
  finally
    FreeAndNil(FormDiscourse);
  end;
end;

procedure TFormMain.btnFacebookClick(Sender: TObject);
var
  FormFacebook: TFormFacebook;
begin
  FormFacebook := TFormFacebook.Create(nil, FSettings);
  try
    FormFacebook.ShowModal;
  finally
    FreeAndNil(FormFacebook);
  end;
end;

procedure TFormMain.btnTwitterXClick(Sender: TObject);
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

procedure TFormMain.btnWordpressClick(Sender: TObject);
var
  frmWordpressLogin: TfrmWordpressLogin;
  FormWordpress: TFormWordpress;
begin
  frmWordpressLogin := TfrmWordpressLogin.Create(nil, FSettings);
  try
    if frmWordpressLogin.ShowModal = mrOk then
    begin
      FormWordpress := TFormWordpress.Create(nil, FSettings);
      try
        FormWordpress.ShowModal;
      finally
        FreeAndNil(FormWordpress);
      end;
    end;
  finally
    FreeAndNil(frmWordpressLogin);
  end;
end;

end.
