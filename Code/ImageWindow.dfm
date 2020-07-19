object ImageForm: TImageForm
  Left = 118
  Top = 205
  Width = 960
  Height = 417
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Image Display'
  Color = clGray
  Constraints.MinHeight = 250
  Constraints.MinWidth = 280
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnCanResize = FormCanResize
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = DetailsMemoKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object ChannelAInfoLabel: TLabel
    Left = 8
    Top = 8
    Width = 89
    Height = 24
    Caption = 'Channel A'
    Color = clAppWorkSpace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Visible = False
  end
  object ChannelBInfoLabel: TLabel
    Left = 320
    Top = 8
    Width = 88
    Height = 24
    Caption = 'Channel B'
    Color = clAppWorkSpace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Visible = False
  end
  object ChannelCInfoLabel: TLabel
    Left = 632
    Top = 8
    Width = 89
    Height = 24
    Caption = 'Channel C'
    Color = clAppWorkSpace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Visible = False
  end
  object ChAImage: TImage
    Left = 8
    Top = 32
    Width = 305
    Height = 305
  end
  object ChBImage: TImage
    Left = 320
    Top = 32
    Width = 305
    Height = 305
  end
  object ChCImage: TImage
    Left = 632
    Top = 32
    Width = 305
    Height = 305
  end
end
