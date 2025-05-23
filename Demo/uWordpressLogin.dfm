object frmWordpressLogin: TfrmWordpressLogin
  Left = 0
  Top = 0
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  BorderStyle = bsDialog
  Caption = 'Wordpress Login Details'
  ClientHeight = 339
  ClientWidth = 1193
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -30
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 240
  TextHeight = 41
  object Label1: TLabel
    Left = 120
    Top = 80
    Width = 164
    Height = 41
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Site Address'
  end
  object Label2: TLabel
    Left = 120
    Top = 137
    Width = 134
    Height = 41
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Username'
  end
  object Label3: TLabel
    Left = 120
    Top = 194
    Width = 125
    Height = 41
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Password'
  end
  object edtSiteAddress: TEdit
    Left = 320
    Top = 77
    Width = 601
    Height = 49
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    TabOrder = 0
    Text = 'edtSiteAddress'
  end
  object edtUsername: TEdit
    Left = 320
    Top = 142
    Width = 601
    Height = 49
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    TabOrder = 1
    Text = 'Edit1'
  end
  object edtPassword: TEdit
    Left = 320
    Top = 207
    Width = 601
    Height = 49
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    TabOrder = 2
    Text = 'Edit1'
  end
  object btnLogin: TButton
    Left = 960
    Top = 70
    Width = 188
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Login'
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = btnLoginClick
  end
  object btnCancel: TButton
    Left = 960
    Top = 184
    Width = 188
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    OnClick = btnCancelClick
  end
end
