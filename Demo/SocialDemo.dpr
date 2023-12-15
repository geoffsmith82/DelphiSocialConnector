program SocialDemo;

uses
  {$IFDEF EurekaLog}
  EMemLeaks,
  EResLeaks,
  EDebugJCL,
  EDebugExports,
  EFixSafeCallException,
  EMapWin32,
  EAppVCL,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  ExceptionLog7,
  {$ENDIF EurekaLog}
  Vcl.Forms,
  uSocialMediaDemo in 'uSocialMediaDemo.pas' {frmSocialMainForm},
  uFacebook in '..\Lib\uFacebook.pas',
  uTwitter in '..\Lib\uTwitter.pas',
  uWordpress in '..\Lib\uWordpress.pas',
  uDiscourse in '..\Lib\uDiscourse.pas',
  uWebBrowser in 'uWebBrowser.pas' {Form1},
  uImageDisplayForm in 'uImageDisplayForm.pas' {FormImageDisplay},
  uTwitterForm in 'uTwitterForm.pas' {FormTwitter};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSocialMainForm, frmSocialMainForm);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormImageDisplay, FormImageDisplay);
  Application.Run;
end.


