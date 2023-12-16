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
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TWordpressEditorForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

end.
