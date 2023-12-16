object FormWordpressMedia: TFormWordpressMedia
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add Media'
  ClientHeight = 314
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object btnAddMedia: TButton
    Left = 290
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Add Media'
    TabOrder = 0
    OnClick = btnAddMediaClick
  end
  object btnCancel: TButton
    Left = 200
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object edtFilename: TEdit
    Left = 24
    Top = 32
    Width = 260
    Height = 23
    TabOrder = 2
  end
  object edtTitle: TEdit
    Left = 24
    Top = 88
    Width = 260
    Height = 23
    TabOrder = 3
  end
  object btnBrowse: TButton
    Left = 290
    Top = 31
    Width = 75
    Height = 25
    Caption = 'Browse...'
    TabOrder = 4
    OnClick = btnBrowseClick
  end
  object OpenDialog: TOpenDialog
    Left = 120
    Top = 40
  end
end
