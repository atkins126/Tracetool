inherited frame_Table: Tframe_Table
  Width = 248
  Height = 158
  ExplicitWidth = 248
  ExplicitHeight = 158
  object VstTable: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 248
    Height = 158
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    CheckImageKind = ckDarkCheck
    Color = 16117479
    Colors.BorderColor = clWindowText
    Colors.HotColor = clBlack
    Colors.UnfocusedSelectionColor = clGray
    Colors.UnfocusedSelectionBorderColor = clGray
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Header.AutoSizeIndex = -1
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.Height = 17
    Header.MainColumn = 1
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoVisible]
    HintAnimation = hatNone
    HintMode = hmTooltip
    Indent = 15
    Margin = 0
    NodeAlignment = naFromTop
    ParentFont = False
    ParentShowHint = False
    PopupMenu = PopupDetail
    ScrollBarOptions.AlwaysVisible = True
    ShowHint = True
    TabOrder = 0
    TreeOptions.AnimationOptions = [toAnimatedToggle]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.SelectionOptions = [toExtendedFocus]
    OnChange = VstTableChange
    OnCreateEditor = VstTableCreateEditor
    OnDblClick = VstTableDblClick
    OnEditCancelled = VstTableEditCancelled
    OnEdited = VstTableEdited
    OnFreeNode = VstTableFreeNode
    OnGetText = VstTableGetText
    OnPaintText = VstTablePaintText
    OnKeyAction = VstTableKeyAction
    OnMeasureItem = VstTableMeasureItem
    Columns = <
      item
        Color = 16117479
        MinWidth = 80
        Options = [coAllowClick, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 0
        Width = 150
      end
      item
        Color = 16117479
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 1
        Width = 130
      end
      item
        MinWidth = 300
        Position = 2
        Width = 300
      end>
  end
  object PopupDetail: TPopupMenu
    Images = Frm_Tool.ilActions
    Left = 144
    Top = 30
    object CopyMenu: TMenuItem
      Action = FrmPageContainer.actCopy
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object SelectAllMenu: TMenuItem
      Action = FrmPageContainer.actSelectAll
    end
  end
end
