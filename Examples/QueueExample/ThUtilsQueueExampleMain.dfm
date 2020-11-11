object Form_main: TForm_main
  Left = 0
  Top = 0
  Caption = 'ThreadUtils: thread-safe queue example'
  ClientHeight = 280
  ClientWidth = 555
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 555
    Height = 57
    Align = alTop
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      555
      57)
    object Lbl_status: TLabel
      Left = 418
      Top = 18
      Width = 110
      Height = 21
      Alignment = taRightJustify
      Anchors = [akTop, akRight, akBottom]
      AutoSize = False
      Caption = 'status'
      Layout = tlCenter
    end
    object Btn_Enqueue: TButton
      Left = 17
      Top = 17
      Width = 65
      Height = 25
      Caption = 'Enqueue'
      Default = True
      TabOrder = 0
      OnClick = Btn_EnqueueClick
    end
    object Btn_Cancel: TButton
      Left = 88
      Top = 17
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = Btn_CancelClick
    end
  end
  object Memo_enqueue: TMemo
    Left = 0
    Top = 57
    Width = 185
    Height = 223
    Align = alLeft
    Lines.Strings = (
      'Memo_enqueue')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitTop = 63
  end
  object Memo_dequeue2: TMemo
    Left = 370
    Top = 57
    Width = 185
    Height = 223
    Align = alRight
    Lines.Strings = (
      'Memo_dequeue2')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Memo_dequeue1: TMemo
    Left = 185
    Top = 57
    Width = 185
    Height = 223
    Align = alClient
    Constraints.MinWidth = 185
    Lines.Strings = (
      'Memo_dequeue1')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
end
