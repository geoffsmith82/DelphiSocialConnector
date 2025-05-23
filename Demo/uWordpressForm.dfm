object FormWordpress: TFormWordpress
  Left = 0
  Top = 0
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  Caption = 'Social Media Demo'
  ClientHeight = 1105
  ClientWidth = 1610
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -30
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 240
  DesignSize = (
    1610
    1105)
  TextHeight = 41
  object btnWebBrowser: TButton
    Left = 278
    Top = 20
    Width = 242
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Web Browser'
    TabOrder = 0
    OnClick = btnWebBrowserClick
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 99
    Width = 1563
    Height = 982
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    ActivePage = tsTags
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object TabSheet1: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Log'
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 1543
        Height = 908
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object Pages: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Pages'
      ImageIndex = 1
      object lvPages: TListView
        Left = 0
        Top = 0
        Width = 1543
        Height = 805
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        Columns = <
          item
            Caption = 'PageId'
            Width = 125
          end
          item
            Caption = 'Title'
            Width = 1000
          end
          item
            Caption = 'Status'
            Width = 200
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
        Top = 805
        Width = 1543
        Height = 103
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alBottom
        TabOrder = 1
        object btnDeletePage: TButton
          Left = 1300
          Top = 13
          Width = 188
          Height = 62
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Caption = 'Delete Page'
          TabOrder = 0
          OnClick = btnDeletePageClick
        end
        object btnAddPage: TButton
          Left = 1080
          Top = 13
          Width = 188
          Height = 62
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Caption = 'Add Page'
          TabOrder = 1
          OnClick = btnAddPageClick
        end
      end
    end
    object Posts: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Posts'
      ImageIndex = 2
      object lvPosts: TListView
        Left = 0
        Top = 0
        Width = 1543
        Height = 805
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        Columns = <
          item
            Caption = 'PostId'
            Width = 125
          end
          item
            Caption = 'Title'
            Width = 1000
          end
          item
            Caption = 'Status'
            Width = 200
          end>
        DoubleBuffered = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvPostsDblClick
      end
      object pnlPosts: TPanel
        Left = 0
        Top = 805
        Width = 1543
        Height = 103
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alBottom
        TabOrder = 1
        object btnDeletePost: TButton
          Left = 1320
          Top = 15
          Width = 188
          Height = 63
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Caption = 'Delete Post'
          TabOrder = 0
          OnClick = btnDeletePostClick
        end
        object btnAddPost: TButton
          Left = 1118
          Top = 15
          Width = 187
          Height = 63
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Caption = 'Add Post'
          TabOrder = 1
          OnClick = btnAddPostClick
        end
      end
    end
    object TabSheet2: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Blocks'
      ImageIndex = 3
      object lvBlocks: TListView
        Left = 0
        Top = 0
        Width = 1543
        Height = 805
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        Columns = <
          item
            Caption = 'BlockId'
            Width = 200
          end
          item
            Caption = 'Title'
            Width = 1000
          end
          item
            Caption = 'Status'
            Width = 200
          end>
        DoubleBuffered = True
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvBlocksDblClick
      end
      object pnlBlock: TPanel
        Left = 0
        Top = 805
        Width = 1543
        Height = 103
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alBottom
        TabOrder = 1
        object btnDeleteBlock: TButton
          Left = 1320
          Top = 15
          Width = 188
          Height = 63
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Caption = 'Delete Block'
          TabOrder = 0
          OnClick = btnDeleteBlockClick
        end
        object btnAddBlock: TButton
          Left = 1118
          Top = 15
          Width = 187
          Height = 63
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Caption = 'Add Block'
          TabOrder = 1
          OnClick = btnAddBlockClick
        end
      end
    end
    object TsWpUsers: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Users'
      ImageIndex = 4
      object lvUsers: TListView
        Left = 0
        Top = 0
        Width = 1543
        Height = 805
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        Columns = <
          item
            Caption = 'UserId'
            Width = 200
          end
          item
            Caption = 'Username'
            Width = 250
          end
          item
            Caption = 'Name'
            Width = 500
          end>
        DoubleBuffered = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvUsersDblClick
      end
      object pnlUser: TPanel
        Left = 0
        Top = 805
        Width = 1543
        Height = 103
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alBottom
        TabOrder = 1
        object btnDeleteUser: TButton
          Left = 1300
          Top = 13
          Width = 188
          Height = 62
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Caption = 'Delete User'
          TabOrder = 0
          OnClick = btnDeleteUserClick
        end
        object btnAddUser: TButton
          Left = 1080
          Top = 13
          Width = 188
          Height = 62
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Caption = 'Add User'
          TabOrder = 1
          OnClick = btnAddUserClick
        end
      end
    end
    object tsWpMedia: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Media'
      ImageIndex = 5
      object lvMedia: TListView
        Left = 0
        Top = 0
        Width = 1543
        Height = 805
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        Columns = <
          item
            Caption = 'MediaId'
            Width = 200
          end
          item
            Caption = 'Status'
            Width = 200
          end
          item
            Caption = 'Title'
            Width = 1000
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
        Top = 805
        Width = 1543
        Height = 103
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alBottom
        TabOrder = 1
        object btnDeleteMedia: TButton
          Left = 1300
          Top = 13
          Width = 188
          Height = 62
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Caption = 'Delete Media'
          TabOrder = 0
          OnClick = btnDeleteMediaClick
        end
        object btnAddMedia: TButton
          Left = 1080
          Top = 13
          Width = 188
          Height = 62
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Caption = 'Add Media'
          TabOrder = 1
          OnClick = btnAddMediaClick
        end
      end
    end
    object tsCategories: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Categories'
      ImageIndex = 6
      object lvCategories: TListView
        Left = 0
        Top = 0
        Width = 1543
        Height = 908
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        Columns = <
          item
            Caption = 'CategoryID'
            Width = 200
          end
          item
            Caption = 'Name'
            Width = 500
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tsTags: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Tags'
      ImageIndex = 8
      object lvTags: TListView
        Left = 0
        Top = 0
        Width = 1543
        Height = 908
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        Columns = <
          item
            Caption = 'TagID'
            Width = 200
          end
          item
            Caption = 'Name'
            Width = 700
          end
          item
            Caption = 'Slug'
            Width = 400
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tsPlugins: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Plugins'
      ImageIndex = 7
      object lvPlugins: TListView
        Left = 0
        Top = 0
        Width = 1543
        Height = 908
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        Columns = <
          item
            Caption = 'PluginId'
            Width = 200
          end
          item
            Caption = 'Status'
            Width = 200
          end
          item
            Caption = 'Name'
            Width = 750
          end>
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 1058
    Width = 1610
    Height = 47
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Panels = <>
  end
end
