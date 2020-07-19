object ImageCtrlFrm: TImageCtrlFrm
  Left = 2029
  Top = 0
  Width = 496
  Height = 726
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = '  Image Control Window'
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 496
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefault
  ScreenSnap = True
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 0
    Top = 0
    Width = 481
    Height = 481
    Cursor = crCross
    OnMouseDown = ImageMouseDown
    OnMouseMove = ImageMouseMove
  end
  object ControlsPanel: TPanel
    Left = 0
    Top = 480
    Width = 482
    Height = 121
    TabOrder = 0
    object PointCoOrdsLabel: TLabel
      Left = 136
      Top = 61
      Width = 146
      Height = 20
      Caption = 'PointCoOrdsLabel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object FromCenter2Label: TLabel
      Left = 162
      Top = 84
      Width = 79
      Height = 13
      Caption = 'from center point'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object FromCenterLabel: TLabel
      Left = 162
      Top = 36
      Width = 79
      Height = 13
      Caption = 'from center point'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object CoOrdsLabel: TLabel
      Left = 136
      Top = 13
      Width = 105
      Height = 20
      Caption = 'CoOrdsLabel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object MoveToButton: TButton
      Left = 360
      Top = 64
      Width = 113
      Height = 49
      Caption = 'Send To Stage Control'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      WordWrap = True
      OnClick = MoveToButtonClick
    end
    object ChannelRadioGroup: TRadioGroup
      Left = 4
      Top = 8
      Width = 113
      Height = 105
      Caption = 'Select Channel'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ItemIndex = 0
      Items.Strings = (
        'Channel A'
        'Channel B'
        'Channel C')
      ParentFont = False
      TabOrder = 1
      OnClick = ChannelRadioGroupClick
    end
  end
end
