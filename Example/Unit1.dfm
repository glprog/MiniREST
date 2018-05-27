object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Hello World'
  ClientHeight = 69
  ClientWidth = 230
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object btnStart: TButton
    Left = 8
    Top = 35
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object edtPort: TEdit
    Left = 45
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '8080'
  end
end
