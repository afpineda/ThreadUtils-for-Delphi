object Form_main: TForm_main
  Left = 0
  Top = 0
  Caption = 'ThreadUtils: waiting for threads to stop'
  ClientHeight = 270
  ClientWidth = 391
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 407
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 391
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    object Btn_stopBySyncPoint: TButton
      Left = 8
      Top = 10
      Width = 169
      Height = 25
      Caption = 'Stop this mess with TSyncPoint'
      TabOrder = 0
      OnClick = Btn_stopBySyncPointClick
    end
    object Btn_stopByWaitFor: TButton
      Left = 183
      Top = 10
      Width = 194
      Height = 25
      Caption = 'Stop this mess with WaitForAll'
      TabOrder = 1
      OnClick = Btn_stopByWaitForClick
    end
  end
  object Memo_log: TMemo
    Left = 0
    Top = 41
    Width = 391
    Height = 229
    Align = alClient
    Lines.Strings = (
      'Memo_log')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
