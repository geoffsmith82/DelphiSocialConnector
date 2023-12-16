unit uWordpressEditorForm;

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
  TWordpressEditorForm = class(TForm)
    Memo1: TMemo;
    btnMakeWordpressPost: TButton;
    btnCancel: TButton;
    edtTitle: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnMakeWordpressPostClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TWordpressEditorForm.FormCreate(Sender: TObject);
begin
  ModalResult := Vcl.Controls.TModalResult(mbCancel);
end;

procedure TWordpressEditorForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := Vcl.Controls.TModalResult(mbCancel);
  Close;
end;

procedure TWordpressEditorForm.btnMakeWordpressPostClick(Sender: TObject);
begin
  ModalResult := Vcl.Controls.TModalResult(mbOK);
  Close;
end;

end.
