unit uWordpressUserForm;

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
  TFormWordpressUser = class(TForm)
    lblUsername: TLabel;
    lblName: TLabel;
    lblEmail: TLabel;
    edtUsername: TEdit;
    edtName: TEdit;
    edtEmail: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    lblPassword: TLabel;
    edtPassword: TEdit;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormWordpressUser.btnCancelClick(Sender: TObject);
begin
  ModalResult := Vcl.Controls.TModalResult(mbCancel);
  Close;
end;

procedure TFormWordpressUser.btnOKClick(Sender: TObject);
begin
  ModalResult := Vcl.Controls.TModalResult(mbOK);
  Close;
end;

end.
