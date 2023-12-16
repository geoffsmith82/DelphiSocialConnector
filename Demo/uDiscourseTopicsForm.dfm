object DiscourseTopicsForm: TDiscourseTopicsForm
  Left = 0
  Top = 0
  Caption = 'DiscourseTopicsForm'
  ClientHeight = 367
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object lvTopics: TListView
    Left = 0
    Top = 0
    Width = 536
    Height = 367
    Align = alClient
    Columns = <
      item
        Caption = 'TopicId'
        Width = 80
      end
      item
        Caption = 'Title'
        Width = 300
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
end
