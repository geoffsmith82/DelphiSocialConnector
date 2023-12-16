object FormWordpressUser: TFormWordpressUser
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Wordpress User'
  ClientHeight = 282
  ClientWidth = 302
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object lblUsername: TLabel
    Left = 16
    Top = 32
    Width = 53
    Height = 15
    Caption = 'Username'
  end
  object lblName: TLabel
    Left = 16
    Top = 64
    Width = 32
    Height = 15
    Caption = 'Name'
  end
  object lblEmail: TLabel
    Left = 16
    Top = 96
    Width = 29
    Height = 15
    Caption = 'Email'
  end
  object lblPassword: TLabel
    Left = 16
    Top = 128
    Width = 50
    Height = 15
    Caption = 'Password'
  end
  object edtUsername: TEdit
    Left = 88
    Top = 29
    Width = 200
    Height = 23
    TabOrder = 0
  end
  object edtName: TEdit
    Left = 88
    Top = 58
    Width = 200
    Height = 23
    TabOrder = 1
  end
  object edtEmail: TEdit
    Left = 88
    Top = 93
    Width = 200
    Height = 23
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 213
    Top = 240
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 120
    Top = 240
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = btnCancelClick
  end
  object edtPassword: TEdit
    Left = 88
    Top = 122
    Width = 200
    Height = 23
    TabOrder = 5
  end
end
