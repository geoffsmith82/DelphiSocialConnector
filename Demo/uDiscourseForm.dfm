object FormDiscourse: TFormDiscourse
  Left = 0
  Top = 0
  Caption = 'FormDiscourse'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 624
    Height = 441
    ActivePage = tsCategories
    Align = alClient
    TabOrder = 0
    object tsLog: TTabSheet
      Caption = 'Log'
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 616
        Height = 411
        Align = alClient
        Lines.Strings = (
          'Memo1')
        TabOrder = 0
        ExplicitLeft = 176
        ExplicitTop = 104
        ExplicitWidth = 185
        ExplicitHeight = 89
      end
    end
    object tsUsers: TTabSheet
      Caption = 'Users'
      ImageIndex = 1
      object lvUsers: TListView
        Left = 0
        Top = 0
        Width = 616
        Height = 411
        Align = alClient
        Columns = <
          item
            Caption = 'UserId'
          end
          item
            Caption = 'Username'
            Width = 100
          end
          item
            Caption = 'Name'
            Width = 140
          end>
        DoubleBuffered = True
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tsGroups: TTabSheet
      Caption = 'Groups'
      ImageIndex = 2
      object lvGroups: TListView
        Left = 0
        Top = 0
        Width = 616
        Height = 411
        Align = alClient
        Columns = <
          item
            Caption = 'GroupId'
            Width = 80
          end
          item
            Caption = 'Name'
            Width = 200
          end>
        DoubleBuffered = True
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tsCategories: TTabSheet
      Caption = 'Categories'
      ImageIndex = 3
      object lvCategory: TListView
        Left = 0
        Top = 0
        Width = 616
        Height = 411
        Align = alClient
        Columns = <
          item
            Caption = 'CategoryId'
            Width = 80
          end
          item
            Caption = 'Name'
            Width = 200
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
end
