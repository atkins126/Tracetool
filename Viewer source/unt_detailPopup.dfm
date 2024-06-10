object DetailPopupForm: TDetailPopupForm
  Left = 0
  Top = 0
  Caption = 'Detail'
  ClientHeight = 369
  ClientWidth = 386
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  inline FrameMemo: TFrameMemo
    Left = 0
    Top = 0
    Width = 386
    Height = 369
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 386
    ExplicitHeight = 369
    inherited SynMemo: TSynEdit
      Width = 386
      Height = 344
      ExplicitWidth = 382
      ExplicitHeight = 343
    end
    inherited PanelTop: TPanel
      Width = 386
      ExplicitWidth = 382
      inherited LabelSelect: TLabel
        Width = 54
        Height = 13
        ExplicitWidth = 54
        ExplicitHeight = 13
      end
    end
  end
end
