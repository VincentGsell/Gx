unit Gx.GraphBoard.FMX;

interface

Uses Gx.GraphBoard, Gx.Graph.Core,
     GS.Geometry.Direction,
     System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
     System.Generics.Collections, FMX.Types, FMX.Controls, FMX.Forms,
     FMX.Graphics, FMX.Dialogs, FMX.Objects, System.Math.Vectors;


type
  //Renderer descendant : Specialized in FMX drawing.
  TgxFMXRenderer = class(TgxRenderer)
  private
    FCanvas : TCanvas;
  protected
    procedure BeginDraw(Sender : TObject); Override;
    procedure EndDraw; Override;

    Procedure SetStrokeSize(aSize : Single); Override;
    Procedure SetStrokeDashType(aDashType : TGxRenderDashType); override;
    Procedure SetStrokeType(aStrokeType : TGxRendererFillType); Override;
    Procedure SetStrokeColor(aRed, aGreen, aBlue : Byte; Const aAlpha : Byte = 255); Override;
    Procedure SetFillColor(aRed, aGreen, aBlue  : Byte; Const aAlpha : Byte = 255); override;
    procedure SetFillType(aFillType : TGxRendererFillType); override;

    Procedure Ellipse(aRect : GS.Geometry.Direction.TRct); Override;
    Procedure Rectangle(aRect : GS.Geometry.Direction.TRct); Override;
    procedure Polygone(aCoord : array of GS.Geometry.Direction.TPt); Override;
    Procedure Line(a,b : GS.Geometry.Direction.TPt); override;

    Property Canvas : TCanvas read FCanvas Write FCanvas;
  end;

  TgxFMXGraphBoardControl = class;
  TgxFMX2DGraphBoard = class(TgxGraphBoard)
  protected
    FRenderBackground : Boolean;
  public
    Parent : TgxFMXGraphBoardControl;
    function GetDisplaySize : GS.Geometry.Direction.TPt; override;

    constructor Create; Override;

    procedure Render; override;

    property EnabledRenderBackground : Boolean read FRenderBackground write FRenderBackground;
  end;

  //Pure FMX Control descendant.
  TgxFMXGraphBoardControl = class(TControl)
  private
  protected
    FInternalBoard : TgxFMX2DGraphBoard;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; Override;

    Procedure MouseMove(Shift: TShiftState; X, Y: Single); Override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); Override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); Override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); Override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); Override;

    property Board : TgxFMX2DGraphBoard read FInternalBoard;
  end;

Procedure gxArrayOfPointToPolygon(var anArray : array of GS.Geometry.Direction.TPt; Var aPoly : TPolygon);

implementation

{ TgxFMXRenderer }

Procedure gxArrayOfPointToPolygon(var anArray : array of GS.Geometry.Direction.TPt; Var aPoly : TPolygon);
var i : Integer;
begin
  SetLength(aPoly,Length(anArray));
  for I := 0 to Length(anArray)-1 do
  begin
    aPoly[i].X := anArray[i].X;
    aPoly[i].Y := anArray[i].Y;
  end;
end;



procedure TgxFMXRenderer.BeginDraw(Sender: TObject);
begin
  //Not used in FMX2d.
end;

procedure TgxFMXRenderer.Ellipse(aRect: GS.Geometry.Direction.TRct);
begin
  if FCanvas.Fill.Kind = TBrushKind.Solid then
  begin
    FCanvas.FillEllipse(RectF(aRect.Left,aRect.Top,aRect.Right,aRect.Bottom),1.0);
  end
  else
  begin
    FCanvas.DrawEllipse(RectF(aRect.Left,aRect.Top,aRect.Right,aRect.Bottom),1.0);
  end;
end;


procedure TgxFMXRenderer.EndDraw;
begin
  //Not used in FMX2d.
end;

procedure TgxFMXRenderer.Line(a, b: GS.Geometry.Direction.TPt);
begin
  FCanvas.DrawLine(PointF(a.X,a.Y),PointF(b.X,b.Y),1.0);
end;

procedure TgxFMXRenderer.Polygone(aCoord: array of GS.Geometry.Direction.TPt);
var ld : TPolygon;
begin
  gxArrayOfPointToPolygon(aCoord,ld);
  if FCanvas.Fill.Kind = TBrushKind.Solid then
  begin
    FCanvas.FillPolygon(ld,1.0);
  end
  else
  begin
    FCanvas.DrawPolygon(ld,1.0);
  end;
end;

procedure TgxFMXRenderer.Rectangle(aRect: GS.Geometry.Direction.TRct);
begin
  if FCanvas.Fill.Kind = TBrushKind.Solid then
  begin
    FCanvas.FillRect(Rectf(aRect.Left,aRect.Top,aRect.Right,aRect.Bottom),0,0,[],1.0);
  end
  else
  begin
    FCanvas.DrawRect(Rectf(aRect.Left,aRect.Top,aRect.Right,aRect.Bottom),0,0,[],1.0);
  end;
end;

procedure TgxFMXRenderer.SetFillColor(aRed, aGreen, aBlue: Byte;
  const aAlpha: Byte);
begin
  FCanvas.Fill.Color := TAlphaColorF.Create(aRed/255,aGreen/255,aBlue/255,aAlpha/255).ToAlphaColor;
end;

procedure TgxFMXRenderer.SetFillType(aFillType: TGxRendererFillType);
begin
  case aFillType of
    TGxRendererFillType.none:       FCanvas.Fill.Kind := TBrushKind.None;
    TGxRendererFillType.SolidColor: FCanvas.Fill.Kind := TBrushKind.Solid;
  end;
end;

procedure TgxFMXRenderer.SetStrokeColor(aRed, aGreen, aBlue: Byte;
  const aAlpha: Byte);
begin
  FCanvas.Stroke.Color :=TAlphaColorF.Create(aRed/255, aGreen/255, aBlue/255, aAlpha/255).ToAlphaColor;
end;

procedure TgxFMXRenderer.SetStrokeDashType(aDashType: TGxRenderDashType);
begin
  case aDashType of
    continous: FCanvas.Stroke.Dash := TStrokeDash.Solid;
    dot:       FCanvas.Stroke.Dash := TStrokeDash.Dot;
    dash:      FCanvas.Stroke.Dash := TStrokeDash.Dash;
    dashdot:   FCanvas.Stroke.Dash := TStrokeDash.DashDot;
  end;
end;

procedure TgxFMXRenderer.SetStrokeSize(aSize: Single);
begin
  FCanvas.Stroke.Thickness :=  aSize;
end;

procedure TgxFMXRenderer.SetStrokeType(aStrokeType: TGxRendererFillType);
begin
  case aStrokeType of
  TGxRendererFillType.none :       FCanvas.Stroke.Kind := TBrushKind.None;
  TGxRendererFillType.SolidColor : FCanvas.Stroke.Kind := TBrushKind.Solid;
  end;
end;

constructor TgxFMXGraphBoardControl.Create(AOwner: TComponent);
begin
  inherited;
  FInternalBoard := TgxFMX2DGraphBoard.Create;
  FInternalBoard.Parent := Self; //Important ! GetDisplaySize query.
  FInternalBoard.Renderer := TgxFMXRenderer.Create;
  ClipChildren := true;
end;

destructor TgxFMXGraphBoardControl.Destroy;
begin
  FreeAndNil(FInternalBoard);
  inherited;
end;


procedure TgxFMXGraphBoardControl.MouseClick(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var la : TButtonsState;
    ls : TButtonsStateSet;
begin
  inherited;

  case Button of
    TMouseButton.mbLeft: la := LeftButton;
    TMouseButton.mbRight: la := RightButton;
    TMouseButton.mbMiddle: la := MiddleButton;
  end;

  ls := [];
  if ssRight in Shift then
    ls := ls + [RightButton];
  if ssLeft in Shift then
    ls := ls + [LeftButton];
  if ssMiddle in Shift then
    ls := ls + [MiddleButton];

  FInternalBoard.MouseClickProcess(la,ls,X,Y);
end;

procedure TgxFMXGraphBoardControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var la : TButtonsState;
    ls : TButtonsStateSet;
begin
  inherited;

  case Button of
    TMouseButton.mbLeft: la := LeftButton;
    TMouseButton.mbRight: la := RightButton;
    TMouseButton.mbMiddle: la := MiddleButton;
  end;

  ls := [];
  if ssRight in Shift then
    ls := ls + [RightButton];
  if ssLeft in Shift then
    ls := ls + [LeftButton];
  if ssMiddle in Shift then
    ls := ls + [MiddleButton];
  if ssDouble in Shift then //It seems that we cannot detect that elsewhere. :/
    ls := ls + [PerformDoubleClick];

  FInternalBoard.MouseDownProcess(X,Y,la,ls);
end;

procedure TgxFMXGraphBoardControl.MouseMove(Shift: TShiftState; X, Y: Single);
var ls : TButtonsStateSet;
begin
  inherited;
  ls := [];
  if ssRight in Shift then
    ls := ls + [RightButton];
  if ssLeft in Shift then
    ls := ls + [LeftButton];
  if ssMiddle in Shift then
    ls := ls + [MiddleButton];

  FInternalBoard.MouseMoveProcess(X,Y,ls);

  Repaint;
end;

procedure TgxFMXGraphBoardControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var la : TButtonsState;
    ls : TButtonsStateSet;
begin
  inherited;

  case Button of
    TMouseButton.mbLeft: la := LeftButton;
    TMouseButton.mbRight: la := RightButton;
    TMouseButton.mbMiddle: la := MiddleButton;
  end;

  ls := [];
  if ssRight in Shift then
    ls := ls + [RightButton];
  if ssLeft in Shift then
    ls := ls + [LeftButton];
  if ssMiddle in Shift then
    ls := ls + [MiddleButton];

  FInternalBoard.MouseUpProcess(X,Y,la,ls);
end;

procedure TgxFMXGraphBoardControl.MouseWheel(Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
  FInternalBoard.MouseWheelProcess([],WheelDelta);
end;

procedure TgxFMXGraphBoardControl.Paint;
begin
  Canvas.BeginScene;
  try
    TgxFMXRenderer(FInternalBoard.Renderer).Canvas := Canvas;
    FInternalBoard.Render;
  finally
    Canvas.EndScene;
  end;
end;



{ TgxFMXGraphBoard }


constructor TgxFMX2DGraphBoard.Create;
begin
  inherited;
  FRenderBackground := true;
end;

function TgxFMX2DGraphBoard.GetDisplaySize: GS.Geometry.Direction.TPt;
begin
  Result := GS.Geometry.Direction.Point(Parent.Width, Parent.Height);
end;

procedure TgxFMX2DGraphBoard.Render;
begin
  if FRenderBackGround then
  begin
    Renderer.SetFillColor($AD,$D8,$E6);   //Light blue. $ADD8E6
    Renderer.SetStrokeColor($FF,$FF,$FF); //White.
    Renderer.SetFillType(TGxRendererFillType.SolidColor);
    Renderer.Rectangle(GS.Geometry.Direction.Rect(0,0,GetDisplaySize.x, GetDisplaySize.y));
  end;
  inherited Render;
end;

end.
