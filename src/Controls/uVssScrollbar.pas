{ ===============================================================================
  uVssScrollbar.pas - Custom Scrollbar/Slider Control

  Part of 3nity Media - Lazarus Edition

  A cross-platform custom scrollbar control with:
  - Click anywhere to seek
  - Smooth dragging
  - Custom colors and styling
  - Double-buffered rendering

  Based on original VssScrollbar by Nicolas DEOUX
  Adapted for LCL/Cross-platform compatibility

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uVssScrollbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, LCLIntf, LMessages;

type
  { TVssScrollbar }
  TVssScrollbar = class(TCustomControl)
  private
    FMax: Integer;
    FMin: Integer;
    FPosition: Integer;
    FSliderHeight: Integer;
    FSliderWidth: Integer;
    FScrolling: Boolean;
    FBorderX: Integer;
    FBorderY: Integer;
    FOnChange: TNotifyEvent;
    FOnScrollEnd: TNotifyEvent;
    FSliderLineHeight: Integer;

    FBitmap: TBitmap;
    FScrollButtonColor: TColor;
    FScrollButtonDirectColor: Boolean;
    FBackgroundColor: TColor;
    FNeedsRedraw: Boolean;

    procedure SetMax(const Value: Integer);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetMin(const Value: Integer);
    procedure SetPosition(Value: Integer);
    function LimitPosition(const Value: Integer): Integer;
    function CalculatePosFromSlider(Pos: Integer): Integer;
    procedure SetSliderHeight(const Value: Integer);
    procedure SetSliderWidth(const Value: Integer);
    procedure SetBorderX(const Value: Integer);
    procedure SetBorderY(const Value: Integer);
    procedure SetSliderLineHeight(const Value: Integer);
    procedure DoOnChange;
    procedure DoOnScrollEnd;
    procedure SetScrollButtonColor(const Value: TColor);
    procedure SetScrollButtonDirectColor(const Value: Boolean);

  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure Resize; override;
    procedure Redraw; virtual;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CancelScroll;

  published
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property Position: Integer read FPosition write SetPosition default 0;
    property SliderWidth: Integer read FSliderWidth write SetSliderWidth default 12;
    property SliderHeight: Integer read FSliderHeight write SetSliderHeight default 18;
    property SliderLineHeight: Integer read FSliderLineHeight write SetSliderLineHeight default 12;
    property Scrolling: Boolean read FScrolling;
    property BorderX: Integer read FBorderX write SetBorderX default 0;
    property BorderY: Integer read FBorderY write SetBorderY default 0;
    property ScrollButtonColor: TColor read FScrollButtonColor write SetScrollButtonColor;
    property ScrollButtonDirectColor: Boolean read FScrollButtonDirectColor write SetScrollButtonDirectColor;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnScrollEnd: TNotifyEvent read FOnScrollEnd write FOnScrollEnd;
    property OnMouseUp;
    property OnMouseDown;
    property OnMouseWheel;
    property Align;
    property Anchors;
    property Enabled;
    property Visible;
    property Hint;
    property ShowHint;
    property PopupMenu;
  end;

implementation

{ Helper functions for color manipulation }

function GetRed(Color: TColor): Byte;
begin
  Color := ColorToRGB(Color);
  Result := Color and $FF;
end;

function GetGreen(Color: TColor): Byte;
begin
  Color := ColorToRGB(Color);
  Result := (Color shr 8) and $FF;
end;

function GetBlue(Color: TColor): Byte;
begin
  Color := ColorToRGB(Color);
  Result := (Color shr 16) and $FF;
end;

function MakeColor(R, G, B: Byte): TColor;
begin
  Result := TColor(R or (G shl 8) or (B shl 16));
end;

function DoColorize(Color, TintColor: TColor): TColor;
var
  GreyLevel: Byte;
begin
  Color := ColorToRGB(Color);
  TintColor := ColorToRGB(TintColor);

  GreyLevel := (GetRed(Color) + GetGreen(Color) + GetBlue(Color)) div 3;
  Result := MakeColor(
    (GreyLevel * GetRed(TintColor) + 127) div 255,
    (GreyLevel * GetGreen(TintColor) + 127) div 255,
    (GreyLevel * GetBlue(TintColor) + 127) div 255
  );
end;

{ TVssScrollbar }

constructor TVssScrollbar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];

  FPosition := 0;
  FMin := 0;
  FMax := 100;
  FSliderWidth := 12;
  FSliderHeight := 18;
  FSliderLineHeight := 12;
  FScrolling := False;
  FBorderX := 0;
  FBorderY := 0;
  FNeedsRedraw := True;

  FScrollButtonColor := MakeColor(192, 192, 255);
  FScrollButtonDirectColor := False;
  FBackgroundColor := clBlack;

  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf32bit;

  Width := 200;
  Height := 24;

  { Initialize bitmap with default size to avoid first-paint issues }
  FBitmap.SetSize(Width, Height);
end;

destructor TVssScrollbar.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

function TVssScrollbar.LimitPosition(const Value: Integer): Integer;
begin
  Result := Value;
  if Value > FMax then
    Result := FMax;
  if Value < FMin then
    Result := FMin;
end;

function TVssScrollbar.CalculatePosFromSlider(Pos: Integer): Integer;
var
  PixelWidth: Integer;
  ValueWidth: Integer;
begin
  ValueWidth := FMax - FMin;
  PixelWidth := Width - FSliderWidth - FBorderX * 2;

  if PixelWidth <= 0 then
  begin
    Result := FMin;
    Exit;
  end;

  Pos := Pos - (FSliderWidth div 2) - FBorderX;
  Result := FMin + (Pos * ValueWidth + (PixelWidth div 2)) div PixelWidth;
  Result := LimitPosition(Result);
end;

procedure TVssScrollbar.CancelScroll;
begin
  if FScrolling then
  begin
    FScrolling := False;
    FNeedsRedraw := True;
    Invalidate;
  end;
end;

procedure TVssScrollbar.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TVssScrollbar.DoOnScrollEnd;
begin
  if Assigned(FOnScrollEnd) then
    FOnScrollEnd(Self);
end;

procedure TVssScrollbar.Redraw;

  procedure FillRectGradient(R: TRect; StartColor, EndColor: TColor; IntSize: Integer);
  var
    I, Max, F: Integer;
    W, H: Integer;
    R1, G1, B1, R2, G2, B2: Byte;
    bOdd: Boolean;
  begin
    StartColor := ColorToRGB(StartColor);
    EndColor := ColorToRGB(EndColor);

    R1 := GetRed(StartColor);
    G1 := GetGreen(StartColor);
    B1 := GetBlue(StartColor);
    R2 := GetRed(EndColor);
    G2 := GetGreen(EndColor);
    B2 := GetBlue(EndColor);

    W := R.Right - R.Left;
    H := R.Bottom - R.Top;

    if W < H then
      Max := W
    else
      Max := H;

    Max := Max - IntSize;
    bOdd := (Max mod 2) = 1;
    Max := Max div 2;

    if Max > 0 then
    begin
      for I := 0 to (Max - 1) do
      begin
        F := Max - I;

        FBitmap.Canvas.Brush.Color := MakeColor(
          (((R1 * F) + (Max div 2)) div Max) + (((R2 * I) + (Max div 2)) div Max),
          (((G1 * F) + (Max div 2)) div Max) + (((G2 * I) + (Max div 2)) div Max),
          (((B1 * F) + (Max div 2)) div Max) + (((B2 * I) + (Max div 2)) div Max)
        );
        FBitmap.Canvas.FrameRect(R);
        Inc(R.Left);
        Inc(R.Top);
        Dec(R.Right);
        Dec(R.Bottom);
      end;
      R.Bottom := R.Top + 1;
    end;

    if bOdd then
    begin
      FBitmap.Canvas.Brush.Color := MakeColor(R2, G2, B2);
      FBitmap.Canvas.FrameRect(R);
    end;
  end;

var
  R: TRect;
  PixelWidth: Integer;
  ValueWidth: Integer;
  MyShadow: TColor;
  MyFace: TColor;
  MyBtnHighlight: TColor;
  MyBtnShadow: TColor;
  MinSize, MaxSize, DifSize: Integer;
begin
  FBitmap.SetSize(Width, Height);

  MyFace := DoColorize(clBtnFace, MakeColor(255, 255, 255));
  MyShadow := DoColorize(clBtnShadow, FScrollButtonColor);

  if FScrolling then
  begin
    MyBtnHighlight := DoColorize(clBtnHighlight, MakeColor(255, 255, 0));
    MyBtnShadow := DoColorize(clBtnShadow, MakeColor(255, 255, 0));
  end
  else
  begin
    MyBtnHighlight := clBtnHighlight;
    MyBtnShadow := clBtnShadow;
  end;

  PixelWidth := Width - FSliderWidth - FBorderX * 2;
  ValueWidth := FMax - FMin;

  { Draw background }
  FBitmap.Canvas.Brush.Color := FBackgroundColor;
  FBitmap.Canvas.Pen.Width := 0;
  FBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));

  { Draw track }
  R.Left := FBorderX;
  R.Right := Width;
  R.Top := (Height - FSliderLineHeight) div 2;
  R.Bottom := R.Top + FSliderLineHeight;

  FillRectGradient(R, MyFace, MyShadow, 0);

  { Draw zero marker if min < 0 }
  if FMin < 0 then
  begin
    FBitmap.Canvas.Brush.Color := clBtnFace;
    R.Left := ((0 - FMin) * PixelWidth + (ValueWidth div 2)) div ValueWidth;
    R.Left := (FBorderX + R.Left + (FSliderWidth div 2)) - 1;
    R.Right := R.Left + 2;
    R.Top := (Height - FSliderLineHeight) div 2;
    R.Bottom := R.Top + FSliderLineHeight;
    FBitmap.Canvas.FillRect(R);
  end;

  { Draw slider thumb if enabled }
  if Enabled and (ValueWidth > 0) then
  begin
    R.Left := ((FPosition - FMin) * PixelWidth + (ValueWidth div 2)) div ValueWidth;
    if R.Left < 0 then
      R.Left := 0;
    if R.Left > PixelWidth then
      R.Left := PixelWidth;
    R.Left := FBorderX + R.Left;

    R.Right := R.Left + FSliderWidth;
    R.Top := (Height - FSliderHeight) div 2;
    R.Bottom := R.Top + FSliderHeight;

    { Outer border }
    FBitmap.Canvas.Brush.Color := MyBtnShadow;
    FBitmap.Canvas.FrameRect(R);

    { Inner border }
    R.Top := R.Top + 1;
    R.Left := R.Left + 1;
    R.Bottom := R.Bottom - 1;
    R.Right := R.Right - 1;
    FBitmap.Canvas.FrameRect(R);

    { Highlight }
    FBitmap.Canvas.Brush.Color := MyBtnHighlight;
    R.Top := R.Top - 1;
    R.Left := R.Left - 1;
    FBitmap.Canvas.FrameRect(R);

    { Fill }
    R.Top := R.Top + 2;
    R.Left := R.Left + 2;
    R.Bottom := R.Bottom - 1;
    R.Right := R.Right - 1;

    if FScrollButtonDirectColor then
      FBitmap.Canvas.Brush.Color := FScrollButtonColor
    else
      FBitmap.Canvas.Brush.Color := MyShadow;
    FBitmap.Canvas.FillRect(R);

    { Special marker at zero position }
    if (FPosition = 0) and (FMin < 0) then
    begin
      if (R.Bottom - R.Top) > (R.Right - R.Left) then
      begin
        MinSize := R.Right - R.Left;
        MaxSize := R.Bottom - R.Top;
        DifSize := (MaxSize - MinSize) div 2;
        R.Top := R.Top + DifSize;
        R.Bottom := R.Bottom - DifSize;
      end
      else
      begin
        MaxSize := R.Right - R.Left;
        MinSize := R.Bottom - R.Top;
        DifSize := (MaxSize - MinSize) div 2;
        R.Left := R.Left + DifSize;
        R.Right := R.Right - DifSize;
      end;
      FBitmap.Canvas.Pen.Color := MyShadow;
      FBitmap.Canvas.Brush.Color := clBtnHighlight;
      FBitmap.Canvas.Ellipse(R);
    end;
  end;

  FNeedsRedraw := False;
end;

procedure TVssScrollbar.Paint;
var
  R: TRect;
begin
  if FBitmap.Width <> Width then
    FNeedsRedraw := True;
  if FBitmap.Height <> Height then
    FNeedsRedraw := True;
  if FNeedsRedraw then
    Redraw;

  R := Rect(0, 0, FBitmap.Width, FBitmap.Height);
  Canvas.CopyRect(R, FBitmap.Canvas, R);
end;

procedure TVssScrollbar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FScrolling := True;  { Set scrolling BEFORE SetPosition so OnChange sees Scrolling=True }
    SetPosition(CalculatePosFromSlider(X));
    FNeedsRedraw := True;
    Invalidate;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TVssScrollbar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FScrolling then
  begin
    SetPosition(CalculatePosFromSlider(X));
    FNeedsRedraw := True;
    Invalidate;
  end;
  inherited MouseUp(Button, Shift, X, Y);
  if FScrolling then
  begin
    FScrolling := False;
    DoOnScrollEnd;
  end;
end;

procedure TVssScrollbar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FScrolling then
    SetPosition(CalculatePosFromSlider(X));
  inherited MouseMove(Shift, X, Y);
end;

procedure TVssScrollbar.SetEnabled(Value: Boolean);
begin
  if Enabled <> Value then
  begin
    inherited SetEnabled(Value);
    FNeedsRedraw := True;
  end;
  if not Enabled then
    if FScrolling then
    begin
      FScrolling := False;
      FNeedsRedraw := True;
    end;
  if FNeedsRedraw then
    Invalidate;
end;

procedure TVssScrollbar.Resize;
begin
  inherited Resize;
  { Force redraw when control is resized }
  FNeedsRedraw := True;
  Invalidate;
end;

procedure TVssScrollbar.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TVssScrollbar.SetMax(const Value: Integer);
begin
  if FMax <> Value then
  begin
    FMax := Value;
    SetPosition(LimitPosition(FPosition));
    FNeedsRedraw := True;
    Invalidate;
  end;
end;

procedure TVssScrollbar.SetMin(const Value: Integer);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    SetPosition(LimitPosition(FPosition));
    FNeedsRedraw := True;
    Invalidate;
  end;
end;

procedure TVssScrollbar.SetPosition(Value: Integer);
begin
  Value := LimitPosition(Value);
  if FPosition <> Value then
  begin
    FPosition := Value;
    DoOnChange;
    FNeedsRedraw := True;
    Invalidate;
  end;
end;

procedure TVssScrollbar.SetScrollButtonColor(const Value: TColor);
begin
  if FScrollButtonColor <> Value then
  begin
    FScrollButtonColor := Value;
    FNeedsRedraw := True;
    Invalidate;
  end;
end;

procedure TVssScrollbar.SetScrollButtonDirectColor(const Value: Boolean);
begin
  if FScrollButtonDirectColor <> Value then
  begin
    FScrollButtonDirectColor := Value;
    FNeedsRedraw := True;
    Invalidate;
  end;
end;

procedure TVssScrollbar.SetSliderHeight(const Value: Integer);
begin
  if FSliderHeight <> Value then
  begin
    FSliderHeight := Value;
    FNeedsRedraw := True;
    Invalidate;
  end;
end;

procedure TVssScrollbar.SetSliderLineHeight(const Value: Integer);
begin
  if FSliderLineHeight <> Value then
  begin
    FSliderLineHeight := Value;
    FNeedsRedraw := True;
    Invalidate;
  end;
end;

procedure TVssScrollbar.SetSliderWidth(const Value: Integer);
begin
  if FSliderWidth <> Value then
  begin
    FSliderWidth := Value;
    FNeedsRedraw := True;
    Invalidate;
  end;
end;

procedure TVssScrollbar.SetBorderX(const Value: Integer);
begin
  if FBorderX <> Value then
  begin
    FBorderX := Value;
    FNeedsRedraw := True;
    Invalidate;
  end;
end;

procedure TVssScrollbar.SetBorderY(const Value: Integer);
begin
  if FBorderY <> Value then
  begin
    FBorderY := Value;
    FNeedsRedraw := True;
    Invalidate;
  end;
end;

procedure TVssScrollbar.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    FNeedsRedraw := True;
    Invalidate;
  end;
end;

function TVssScrollbar.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  Step: Integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    { Calculate step based on range - about 1% of range per wheel notch }
    Step := (FMax - FMin) div 100;
    if Step < 1 then
      Step := 1;

    if WheelDelta > 0 then
      SetPosition(FPosition + Step)
    else
      SetPosition(FPosition - Step);

    Result := True;
  end;
end;

end.
