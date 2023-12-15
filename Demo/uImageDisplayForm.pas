unit uImageDisplayForm;

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
  Vcl.ExtCtrls,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.Net.HttpClientComponent,
  Vcl.Imaging.pngimage,
  jpeg
  ;

type
  TFormImageDisplay = class(TForm)
    Image: TImage;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowImage(url: string);
  end;

implementation

{$R *.dfm}

{ TForm2 }

procedure TFormImageDisplay.ShowImage(url: string);
var
  HTTPClient: TNetHTTPClient;
  ResponseStream: TMemoryStream;
begin
  HTTPClient := TNetHTTPClient.Create(nil);
  ResponseStream := TMemoryStream.Create;
  try
    try
      // Download the image from the URL
      HTTPClient.Get(url, ResponseStream);

      // Assign the TBitmap to the TImage control
      Image.Picture.LoadFromStream(ResponseStream);
      ShowModal;
    except
      // Handle any exceptions (e.g., if the downloaded data is not a valid image)
      ShowMessage('Error loading image');
    end;
  finally
    HTTPClient.Free;
    ResponseStream.Free;
  end;
end;

end.
