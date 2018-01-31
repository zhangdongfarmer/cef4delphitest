object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 307
  ClientWidth = 599
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object cfwndwprnt1: TCEFWindowParent
    Left = 8
    Top = 24
    Width = 583
    Height = 275
    TabOrder = 0
  end
  object tmr1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmr1Timer
    Left = 208
    Top = 186
  end
  object chrm1: TChromium
    OnAfterCreated = chrm1AfterCreated
    Left = 88
    Top = 128
  end
end
