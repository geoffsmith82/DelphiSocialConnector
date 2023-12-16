object WordpressEditorForm: TWordpressEditorForm
  Left = 0
  Top = 0
  Caption = 'Wordpress Editor'
  ClientHeight = 391
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Memo1: TMemo
    Left = 8
    Top = 48
    Width = 593
    Height = 289
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object btnMakeWordpressPost: TButton
    Left = 472
    Top = 343
    Width = 129
    Height = 25
    Caption = 'Make Wordpress Post'
    Default = True
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 384
    Top = 343
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
end
