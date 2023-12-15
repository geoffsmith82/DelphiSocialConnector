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
  DesignSize = (
    628
    442)
  TextHeight = 15
  object btnWordpress: TButton
    Left = 536
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Wordpress'
    TabOrder = 0
    OnClick = btnWordpressClick
  end
  object btnTweet: TButton
    Left = 214
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Tweet'
    TabOrder = 1
    OnClick = btnTweetClick
  end
  object btnWebBrowser: TButton
    Left = 111
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Web Browser'
    TabOrder = 2
    OnClick = btnWebBrowserClick
  end
  object btnDiscourse: TButton
    Left = 424
    Top = 8
    Width = 91
    Height = 25
    Caption = 'Discourse'
    TabOrder = 3
    OnClick = btnDiscourseClick
  end
  object PageControl1: TPageControl
    Left = -5
    Top = 39
    Width = 625
    Height = 393
    ActivePage = TsWpUsers
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
    object TabSheet1: TTabSheet
      Caption = 'Log'
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 617
        Height = 363
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object Pages: TTabSheet
      Caption = 'Pages'
      ImageIndex = 1
      object lvPages: TListView
        Left = 0
        Top = 0
        Width = 617
        Height = 363
        Align = alClient
        Columns = <
          item
            Caption = 'PageId'
          end
          item
            Caption = 'Title'
            Width = 400
          end>
        HideSelection = False
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object Posts: TTabSheet
      Caption = 'Posts'
      ImageIndex = 2
      object lvPosts: TListView
        Left = 0
        Top = 0
        Width = 617
        Height = 363
        Align = alClient
        Columns = <
          item
            Caption = 'PostId'
          end
          item
            Caption = 'Title'
            Width = 400
          end>
        DoubleBuffered = True
        HideSelection = False
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Blocks'
      ImageIndex = 3
      object lvBlocks: TListView
        Left = 0
        Top = 0
        Width = 617
        Height = 363
        Align = alClient
        Columns = <
          item
            Caption = 'BlockId'
            Width = 80
          end
          item
            Caption = 'Title'
            Width = 400
          end>
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object TsWpUsers: TTabSheet
      Caption = 'Users'
      ImageIndex = 4
      object lvUsers: TListView
        Left = 0
        Top = 0
        Width = 617
        Height = 363
        Align = alClient
        Columns = <
          item
            Caption = 'UserId'
            Width = 80
          end
          item
            Caption = 'Username'
            Width = 100
          end
          item
            Caption = 'Name'
            Width = 200
          end>
        DoubleBuffered = True
        HideSelection = False
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 423
    Width = 628
    Height = 19
    Panels = <>
  end
end
