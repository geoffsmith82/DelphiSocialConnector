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
  object btnWordpress: TButton
    Left = 536
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Wordpress'
    TabOrder = 1
    OnClick = btnWordpressClick
  end
  object btnTweet: TButton
    Left = 214
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Tweet'
    TabOrder = 2
    OnClick = btnTweetClick
  end
  object btnWebBrowser: TButton
    Left = 111
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Web Browser'
    TabOrder = 3
    OnClick = btnWebBrowserClick
  end
  object btnDiscourse: TButton
    Left = 424
    Top = 8
    Width = 91
    Height = 25
    Caption = 'Discourse'
    TabOrder = 4
    OnClick = btnDiscourseClick
  end
end
