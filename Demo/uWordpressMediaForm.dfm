object FormWordpressMedia: TFormWordpressMedia
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add Media'
  ClientHeight = 339
  ClientWidth = 292
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object btnAddMedia: TButton
    Left = 200
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Add Media'
    TabOrder = 0
    OnClick = btnAddMediaClick
  end
  object btnCancel: TButton
    Left = 112
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object OpenDialog: TOpenDialog
    Left = 120
    Top = 40
  end
end
