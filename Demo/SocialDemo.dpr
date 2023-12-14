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
  uWebBrowswer in 'uWebBrowswer.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSocialMainForm, frmSocialMainForm);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


