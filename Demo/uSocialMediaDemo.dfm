object frmSocialMainForm: TfrmSocialMainForm
  Left = 0
  Top = 0
  Caption = 'Social Media Demo'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Memo1: TMemo
    Left = 24
    Top = 56
    Width = 596
    Height = 369
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 504
    Top = 25
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object btnTweet: TButton
    Left = 392
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Tweet'
    TabOrder = 2
    OnClick = btnTweetClick
  end
  object Button3: TButton
    Left = 72
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Authenticate'
    TabOrder = 3
  end
  object Button2: TButton
    Left = 240
    Top = 16
    Width = 97
    Height = 25
    Caption = 'Web Browser'
    TabOrder = 4
    OnClick = Button2Click
  end
end
