object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 321
  ClientWidth = 678
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btn1: TBitBtn
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 0
    OnClick = btn1Click
  end
  object cfwndwprnt1: TCEFWindowParent
    Left = 8
    Top = 70
    Width = 662
    Height = 243
    TabOrder = 1
  end
  object tmr1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmr1Timer
    Left = 98
    Top = 86
  end
  object chrm1: TChromium
    OnProcessMessageReceived = chrm1ProcessMessageReceived
    OnAfterCreated = chrm1AfterCreated
    Left = 104
    Top = 192
  end
end
