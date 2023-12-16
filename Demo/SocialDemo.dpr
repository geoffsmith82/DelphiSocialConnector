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
  uWordpressForm in 'uWordpressForm.pas' {FormWordpress},
  uFacebook in '..\Lib\uFacebook.pas',
  uTwitter in '..\Lib\uTwitter.pas',
  uWordpress in '..\Lib\uWordpress.pas',
  uDiscourse in '..\Lib\uDiscourse.pas',
  uWebBrowser in 'uWebBrowser.pas' {FormWebBrowser},
  uImageDisplayForm in 'uImageDisplayForm.pas' {FormImageDisplay},
  uTwitterForm in 'uTwitterForm.pas' {FormTwitter},
  uDiscourseForm in 'uDiscourseForm.pas' {FormDiscourse},
  uFacebookForm in 'uFacebookForm.pas' {FormFacebook},
  uMainForm in 'uMainForm.pas' {FormMain},
  uWordpressEditorForm in 'uWordpressEditorForm.pas' {WordpressEditorForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.


