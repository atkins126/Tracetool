object FrmSelectEvent: TFrmSelectEvent
  Left = 508
  Top = 240
  Caption = 'Select Event log'
  ClientHeight = 451
  ClientWidth = 534
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    534
    451)
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 438
    Height = 13
    Caption = 
      'Select Event Log (Note that for some files like '#39'Security'#39', you ' +
      ' need  to run tracetool as admin)'
  end
  object Label2: TLabel
    Left = 8
    Top = 319
    Width = 478
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 
      'Or a custom WQL Query, like "SELECT * FROM Win32_NTLogEvent Wher' +
      'e Logfile = "Application"  '
  end
  object ScrollBox: TScrollBox
    Left = 8
    Top = 33
    Width = 514
    Height = 272
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvSpace
    BevelKind = bkFlat
    Color = 16117479
    ParentColor = False
    TabOrder = 0
  end
  object butOk: TButton
    Left = 16
    Top = 421
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Open'
    ModalResult = 1
    TabOrder = 1
    ExplicitTop = 389
  end
  object butCancel: TButton
    Left = 450
    Top = 421
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ExplicitLeft = 265
    ExplicitTop = 389
  end
  object EditQuery: TMemo
    Left = 8
    Top = 338
    Width = 514
    Height = 77
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
    OnChange = EditQueryChange
  end
end
