object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Social Media Demo'
  ClientHeight = 300
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object btnTwitterX: TButton
    Left = 152
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Twitter / X'
    TabOrder = 0
    OnClick = btnTwitterXClick
  end
  object btnWordpress: TButton
    Left = 152
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Wordpress'
    TabOrder = 1
    OnClick = btnWordpressClick
  end
  object btnDiscourse: TButton
    Left = 152
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Discourse'
    TabOrder = 2
    OnClick = btnDiscourseClick
  end
  object btnFacebook: TButton
    Left = 152
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Facebook'
    TabOrder = 3
  end
end
