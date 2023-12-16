object FormWordpress: TFormWordpress
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
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    628
    442)
  TextHeight = 15
  object btnWebBrowser: TButton
    Left = 111
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Web Browser'
    TabOrder = 0
    OnClick = btnWebBrowserClick
  end
  object PageControl1: TPageControl
    Left = -5
    Top = 39
    Width = 625
    Height = 393
    ActivePage = tsWpMedia
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
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
        Height = 322
        Align = alClient
        Columns = <
          item
            Caption = 'PageId'
          end
          item
            Caption = 'Title'
            Width = 400
          end
          item
            Caption = 'Status'
            Width = 80
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvPagesClick
      end
      object pnlPage: TPanel
        Left = 0
        Top = 322
        Width = 617
        Height = 41
        Align = alBottom
        TabOrder = 1
        object btnDeletePage: TButton
          Left = 520
          Top = 5
          Width = 75
          Height = 25
          Caption = 'Delete Page'
          TabOrder = 0
          OnClick = btnDeletePageClick
        end
        object btnAddPage: TButton
          Left = 432
          Top = 5
          Width = 75
          Height = 25
          Caption = 'Add Page'
          TabOrder = 1
          OnClick = btnAddPageClick
        end
      end
    end
    object Posts: TTabSheet
      Caption = 'Posts'
      ImageIndex = 2
      object lvPosts: TListView
        Left = 0
        Top = 0
        Width = 617
        Height = 322
        Align = alClient
        Columns = <
          item
            Caption = 'PostId'
          end
          item
            Caption = 'Title'
            Width = 400
          end
          item
            Caption = 'Status'
            Width = 80
          end>
        DoubleBuffered = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ViewStyle = vsReport
      end
      object pnlPosts: TPanel
        Left = 0
        Top = 322
        Width = 617
        Height = 41
        Align = alBottom
        TabOrder = 1
        object btnDeletePost: TButton
          Left = 528
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Delete Post'
          TabOrder = 0
          OnClick = btnDeletePostClick
        end
        object btnAddPost: TButton
          Left = 447
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Add Post'
          TabOrder = 1
          OnClick = btnAddPostClick
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Blocks'
      ImageIndex = 3
      object lvBlocks: TListView
        Left = 0
        Top = 0
        Width = 617
        Height = 322
        Align = alClient
        Columns = <
          item
            Caption = 'BlockId'
            Width = 80
          end
          item
            Caption = 'Title'
            Width = 400
          end
          item
            Caption = 'Status'
            Width = 80
          end>
        DoubleBuffered = True
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ViewStyle = vsReport
      end
      object pnlBlock: TPanel
        Left = 0
        Top = 322
        Width = 617
        Height = 41
        Align = alBottom
        TabOrder = 1
        object btnDeleteBlock: TButton
          Left = 528
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Delete Block'
          TabOrder = 0
          OnClick = btnDeleteBlockClick
        end
        object btnAddBlock: TButton
          Left = 447
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Add Block'
          TabOrder = 1
          OnClick = btnAddBlockClick
        end
      end
    end
    object TsWpUsers: TTabSheet
      Caption = 'Users'
      ImageIndex = 4
      object lvUsers: TListView
        Left = 0
        Top = 0
        Width = 617
        Height = 322
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
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ViewStyle = vsReport
      end
      object pnlUser: TPanel
        Left = 0
        Top = 322
        Width = 617
        Height = 41
        Align = alBottom
        TabOrder = 1
        object btnDeleteUser: TButton
          Left = 520
          Top = 5
          Width = 75
          Height = 25
          Caption = 'Delete User'
          TabOrder = 0
          OnClick = btnDeleteUserClick
        end
        object btnAddUser: TButton
          Left = 432
          Top = 5
          Width = 75
          Height = 25
          Caption = 'Add User'
          TabOrder = 1
          OnClick = btnAddUserClick
        end
      end
    end
    object tsWpMedia: TTabSheet
      Caption = 'Media'
      ImageIndex = 5
      object lvMedia: TListView
        Left = 0
        Top = 0
        Width = 617
        Height = 322
        Align = alClient
        Columns = <
          item
            Caption = 'MediaId'
            Width = 80
          end
          item
            Caption = 'Title'
            Width = 400
          end>
        DoubleBuffered = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvMediaDblClick
      end
      object pnlMedia: TPanel
        Left = 0
        Top = 322
        Width = 617
        Height = 41
        Align = alBottom
        TabOrder = 1
        object btnDeleteMedia: TButton
          Left = 520
          Top = 5
          Width = 75
          Height = 25
          Caption = 'Delete Media'
          TabOrder = 0
          OnClick = btnDeleteMediaClick
        end
        object btnAddMedia: TButton
          Left = 432
          Top = 5
          Width = 75
          Height = 25
          Caption = 'Add Media'
          TabOrder = 1
          OnClick = btnAddMediaClick
        end
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
