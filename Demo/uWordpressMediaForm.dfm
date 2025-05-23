object FormWordpressMedia: TFormWordpressMedia
  Left = 0
  Top = 0
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  BorderStyle = bsDialog
  Caption = 'Add Media'
  ClientHeight = 785
  ClientWidth = 983
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -30
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 240
  TextHeight = 41
  object Label1: TLabel
    Left = 60
    Top = 23
    Width = 210
    Height = 41
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Media Filename'
  end
  object Label2: TLabel
    Left = 60
    Top = 163
    Width = 56
    Height = 41
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Title'
  end
  object btnAddMedia: TButton
    Left = 725
    Top = 680
    Width = 188
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Add Media'
    ModalResult = 1
    TabOrder = 0
    OnClick = btnAddMediaClick
  end
  object btnCancel: TButton
    Left = 500
    Top = 680
    Width = 188
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object edtFilename: TEdit
    Left = 60
    Top = 80
    Width = 650
    Height = 49
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    TabOrder = 2
  end
  object edtTitle: TEdit
    Left = 60
    Top = 220
    Width = 650
    Height = 49
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    TabOrder = 3
  end
  object btnBrowse: TButton
    Left = 725
    Top = 78
    Width = 188
    Height = 62
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Browse...'
    TabOrder = 4
    OnClick = btnBrowseClick
  end
  object OpenDialog: TOpenDialog
    Left = 100
    Top = 620
  end
end
