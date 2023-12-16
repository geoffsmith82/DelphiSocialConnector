unit uWebBrowser;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Win.ComObj,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Winapi.WebView2,
  Winapi.ActiveX,
  Vcl.Edge
  ;

type
  TFormWebBrowser = class(TForm)
    EdgeBrowser: TEdgeBrowser;
    procedure EdgeBrowserCreateWebViewCompleted(Sender: TCustomEdgeBrowser;
        AResult: HRESULT);
    procedure EdgeBrowserWebResourceRequested(Sender: TCustomEdgeBrowser; Args:
        TWebResourceRequestedEventArgs);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FBlockImages : Boolean;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  Winapi.EdgeUtils
  ;

procedure TFormWebBrowser.EdgeBrowserCreateWebViewCompleted(Sender: TCustomEdgeBrowser;
    AResult: HRESULT);
begin
{$IFDEF DEBUG}
  OutputDebugString('EdgeBrowser OnCreateWebViewCompleted');
{$ENDIF}
  if Succeeded(AResult) then
  begin
    Sender.AddWebResourceRequestedFilter('*', COREWEBVIEW2_WEB_RESOURCE_CONTEXT_IMAGE);
  end
  else
  begin
    if AResult = HResultFromWin32(ERROR_FILE_NOT_FOUND) then
      Application.MessageBox('Could not find Edge installation. ' +
        'Do you have a version installed that''s compatible with this WebView2 SDK version?',
        'Edge initialisation error', MB_OK or MB_ICONERROR)
    else if AResult = E_FAIL then
      Application.MessageBox('Failed to initialise Edge loader', 'Edge initialisation error', MB_OK or MB_ICONERROR)
    else
      try
        OleCheck(AResult)
      except
        on E: Exception do
          Application.MessageBox(PChar(Format('Failed to initialise Edge: %s', [E.Message])),
            'Edge initialisation error', MB_OK or MB_ICONERROR)
      end;
  end;
end;

procedure TFormWebBrowser.EdgeBrowserWebResourceRequested(Sender: TCustomEdgeBrowser;
    Args: TWebResourceRequestedEventArgs);
begin
{$IFDEF DEBUG}
  OutputDebugString('EdgeBrowser OnWebResourceRequested');
{$ENDIF}
  // Go to any site that uses image references, e.g. google.com (but not bing.com)
  if FBlockImages then
  begin
    var ResourceContext: COREWEBVIEW2_WEB_RESOURCE_CONTEXT;
    if Succeeded(Args.ArgsInterface.Get_resourceContext(ResourceContext)) then
    begin
      // Ensure that the type is image
      if ResourceContext = COREWEBVIEW2_WEB_RESOURCE_CONTEXT_IMAGE then
      begin
        // Override the response with an empty one to block the image.
        // If Set_Response is not called, the request will continue as normal.
        var Response: ICoreWebView2WebResourceResponse;
        if Succeeded(EdgeBrowser.EnvironmentInterface.CreateWebResourceResponse(
             nil, 403 { NoContent }, 'Blocked', '', Response)) then
          Args.ArgsInterface.Set_Response(Response)
      end;
    end;
  end;
end;

procedure TFormWebBrowser.FormCreate(Sender: TObject);
begin
  FBlockImages := False;
  EdgeBrowser.OnDownloadStarting := nil;
  EdgeBrowser.UserDataFolder := TPath.Combine(TPath.GetDirectoryName(Application.ExeName), 'CustomCache');
  EdgeBrowser.Navigate('https://www.adug.org.au');

end;

end.
