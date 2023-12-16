unit uFacebookForm;

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
  Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  uFacebook
  ;

type
  TFormFacebook = class(TForm)
    Image1: TImage;
    btnAuthenticate: TButton;
    btnPost: TButton;
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FSettings: TIniFile;
    FFacebook: TFacebookApi;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; inSettings: TInifile); reintroduce;
  end;

implementation

{$R *.dfm}

{ TFormFacebook }

constructor TFormFacebook.Create(AOwner: TComponent; inSettings: TInifile);
begin
  inherited Create(AOwner);
  FSettings := inSettings;
//  FFacebook := TFacebookApi.Create();
end;

procedure TFormFacebook.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFacebook);
end;

end.
