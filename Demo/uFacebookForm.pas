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
  Vcl.Dialogs
  ;

type
  TFormFacebook = class(TForm)
  private
    { Private declarations }
    FSettings: TIniFile;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; inSettings: TInifile); reintroduce;
  end;

var
  FormFacebook: TFormFacebook;

implementation

{$R *.dfm}

{ TFormFacebook }

constructor TFormFacebook.Create(AOwner: TComponent; inSettings: TInifile);
begin
  inherited Create(AOwner);
  FSettings := inSettings;
end;

end.
