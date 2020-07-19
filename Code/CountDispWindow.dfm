object CountDispFrm: TCountDispFrm
  Left = 29
  Top = 210
  Width = 939
  Height = 556
  Caption = '  Background Counts'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ChannelALabel: TLabel
    Left = 32
    Top = 37
    Width = 672
    Height = 109
    Caption = 'Channel A: 0kHz'
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -96
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
  end
  object ChannelBLabel: TLabel
    Left = 32
    Top = 147
    Width = 667
    Height = 109
    Caption = 'Channel B: 0kHz'
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -96
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
  end
  object ChannelCLabel: TLabel
    Left = 32
    Top = 249
    Width = 667
    Height = 109
    Caption = 'Channel C: 0kHz'
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -96
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
  end
  object CountDurationLabel: TLabel
    Left = 105
    Top = 416
    Width = 303
    Height = 42
    Caption = '0.5s Duration Period'
    Font.Charset = ANSI_CHARSET
    Font.Color = clSilver
    Font.Height = -37
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
  end
  object CountsPerDurLabel: TLabel
    Left = 98
    Top = 360
    Width = 349
    Height = 42
    Caption = '10 Counts Per Duration'
    Font.Charset = ANSI_CHARSET
    Font.Color = clSilver
    Font.Height = -37
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
  end
  object CloseButton: TButton
    Left = 592
    Top = 440
    Width = 107
    Height = 33
    Caption = 'Close'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = CloseButtonClick
  end
end
