unit uWordpressMediaForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls
  ;

type
  TFormWordpressMedia = class(TForm)
    OpenDialog: TOpenDialog;
    btnAddMedia: TButton;
    btnCancel: TButton;
    edtFilename: TEdit;
    edtTitle: TEdit;
    btnBrowse: TButton;
    procedure btnAddMediaClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormWordpressMedia.btnAddMediaClick(Sender: TObject);
begin
  ModalResult := Vcl.Controls.TModalResult(mbOK);
  Close;
end;

procedure TFormWordpressMedia.btnBrowseClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    edtFilename.Text := OpenDialog.FileName;
  end;
end;

procedure TFormWordpressMedia.btnCancelClick(Sender: TObject);
begin
  ModalResult := Vcl.Controls.TModalResult(mbCancel);
  Close;
end;

end.
