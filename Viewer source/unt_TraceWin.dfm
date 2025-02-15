object Frm_Trace: TFrm_Trace
  Left = 323
  Top = 184
  Caption = 'TTRACE'
  ClientHeight = 324
  ClientWidth = 568
  Color = clBtnFace
  DragKind = dkDock
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object PanelTTraces: TPanel
    Left = 0
    Top = 0
    Width = 568
    Height = 324
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnCanResize = PanelTTracesCanResize
    ExplicitWidth = 564
    ExplicitHeight = 323
    object VSplitter: TSplitter
      Left = 216
      Top = 22
      Width = 5
      Height = 302
      Align = alRight
      Visible = False
      OnCanResize = VSplitterCanResize
      ExplicitLeft = 220
      ExplicitHeight = 303
    end
    object PanelLeft: TPanel
      Left = 0
      Top = 22
      Width = 216
      Height = 302
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      OnResize = PanelLeftResize
      ExplicitWidth = 212
      ExplicitHeight = 301
      object vstMain: TVirtualStringTree
        Left = 12
        Top = 0
        Width = 204
        Height = 302
        Align = alClient
        BevelInner = bvLowered
        BevelOuter = bvRaised
        Colors.BorderColor = clBlack
        Colors.DisabledColor = clGray
        Colors.DropMarkColor = 15385233
        Colors.DropTargetColor = 15385233
        Colors.DropTargetBorderColor = 15385233
        Colors.FocusedSelectionColor = 15385233
        Colors.FocusedSelectionBorderColor = 15385233
        Colors.GridLineColor = 15987699
        Colors.HeaderHotColor = clBlack
        Colors.HotColor = clBlack
        Colors.SelectionRectangleBlendColor = 15385233
        Colors.SelectionRectangleBorderColor = 15385233
        Colors.SelectionTextColor = clBlack
        Colors.TreeLineColor = 9471874
        Colors.UnfocusedColor = clBlack
        Colors.UnfocusedSelectionColor = clGray
        Colors.UnfocusedSelectionBorderColor = clGray
        DragOperations = []
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Header.AutoSizeIndex = -1
        Header.DefaultHeight = 17
        Header.Height = 17
        Header.MainColumn = 1
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoVisible]
        HintMode = hmTooltip
        Images = Frm_Tool.ilActions
        Indent = 15
        Margin = 0
        ParentFont = False
        ParentShowHint = False
        PopupMenu = PopupTree
        ScrollBarOptions.AlwaysVisible = True
        ScrollBarOptions.HorizontalIncrement = 100
        SelectionBlendFactor = 150
        ShowHint = True
        TabOrder = 0
        TreeOptions.AutoOptions = []
        TreeOptions.MiscOptions = []
        TreeOptions.PaintOptions = []
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect, toSimpleDrawSelection]
        OnAfterCellPaint = vstMainAfterCellPaint
        OnAfterPaint = vstMainAfterPaint
        OnBeforeCellPaint = vstMainBeforeCellPaint
        OnChange = vstMainChange
        OnCompareNodes = vstMainCompareNodes
        OnCreateEditor = vstMainCreateEditor
        OnDblClick = vstMainDblClick
        OnEditCancelled = vstMainEditCancelled
        OnEdited = vstMainEdited
        OnEditing = vstMainEditing
        OnFreeNode = vstMainFreeNode
        OnGetText = vstMainGetText
        OnPaintText = vstMainPaintText
        OnGetImageIndex = vstMainGetImageIndex
        OnGetHint = vstMainGetHint
        OnHeaderDragged = vstMainHeaderDragged
        OnKeyAction = vstMainKeyAction
        OnMeasureItem = vstMainMeasureItem
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coShowDropMark, coVisible]
            Position = 0
            Style = vsOwnerDraw
            Width = 20
          end
          item
            Position = 1
            Text = 'Time'
            Width = 75
          end
          item
            Position = 2
            Text = 'ThId'
          end
          item
            Color = 16705515
            MinWidth = 100
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus]
            Position = 3
            Text = 'Traces'
            Width = 150
          end
          item
            Color = 16705515
            MinWidth = 3000
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus]
            Position = 4
            Text = 'Comment'
            Width = 3000
          end>
      end
      object PanelGutter: TPanel
        Left = 0
        Top = 0
        Width = 12
        Height = 302
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        OnDblClick = PanelGutterDblClick
        ExplicitHeight = 301
      end
    end
    object PanelRight: TPanel
      Left = 221
      Top = 22
      Width = 347
      Height = 302
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
      ExplicitLeft = 217
      ExplicitHeight = 301
    end
    object PanelTop: TPanel
      Left = 0
      Top = 0
      Width = 568
      Height = 22
      Align = alTop
      BevelOuter = bvNone
      Color = clCream
      ParentBackground = False
      TabOrder = 2
      ExplicitWidth = 564
      DesignSize = (
        568
        22)
      object TracesInfo: TLabel
        Left = 3
        Top = 5
        Width = 51
        Height = 13
        Caption = 'TracesInfo'
        OnClick = TracesInfoClick
      end
      object LabelLogFile: TLabel
        Left = 483
        Top = 5
        Width = 60
        Height = 13
        Cursor = crHandPoint
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'LabelLogFile'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = LabelLogFileClick
        ExplicitLeft = 487
      end
      object butClose: TBitBtn
        Left = 545
        Top = 0
        Width = 22
        Height = 22
        Anchors = [akTop, akRight]
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FF000000000000FF00FFFF00FFFF00FFFF00FF000000000000FF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000FF
          00FFFF00FF000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FF000000000000000000000000FF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00
          0000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FF000000000000000000000000FF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000FF
          00FFFF00FF000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FF000000000000FF00FFFF00FFFF00FFFF00FF000000000000FF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        TabOrder = 0
        OnClick = butCloseClick
        ExplicitLeft = 541
      end
    end
  end
  object PopupTree: TPopupMenu
    Images = Frm_Tool.ilActions
    Left = 56
    Top = 46
    object Cut1: TMenuItem
      Action = FrmPageContainer.actCut
    end
    object Copy1: TMenuItem
      Action = FrmPageContainer.actCopy
    end
    object Copycurrentcell1: TMenuItem
      Action = FrmPageContainer.actCopyCurrentCell
    end
    object Delete1: TMenuItem
      Action = FrmPageContainer.actDelete
    end
    object mnuTogglebookmark: TMenuItem
      Action = FrmPageContainer.actToggleBookmark
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuSelectAll: TMenuItem
      Action = FrmPageContainer.actSelectAll
    end
    object mnuExpandAll: TMenuItem
      Caption = 'Expand all'
      OnClick = mnuExpandAllClick
    end
    object mnuCollapseAll: TMenuItem
      Caption = 'Collapse All'
      OnClick = mnuCollapseAllClick
    end
  end
end
