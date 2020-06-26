unit Gx.GraphBoard.VCL;

interface

Uses Gx.GraphBoard, Gx.Graph.Core,
     GS.Geometry.Direction,
     System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
     System.Generics.Collections, System.Math.Vectors,
     VCL.Graphics, VCL.Controls;


type
  //Renderer descdant : Specialyzed in VCL drawing.
  TgxVCLRenderer = class(TgxRenderer)
  private
    FCanvas : TCanvas;
  protected
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

  TgxVCLGraphBoardControl = class;
  TgxVCLGraphBoard = class(TgxGraphBoard)
  protected
  public
    Parent : TgxVCLGraphBoardControl;
    function GetDisplaySize : GS.Geometry.Direction.TPt; override;
  end;

  //Pure VCL Control descendant.
  TgxVCLGraphBoardControl = class(TCustomControl)
  private
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X,
      Y: Single);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean);
  protected
    FInternalBoard : TgxVCLGraphBoard;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; Override;

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); Override;

    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
    //procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); Override;
    procedure Click; Override;



    property Board : TgxVCLGraphBoard read FInternalBoard;
  end;

implementation

{ TgxVCLRenderer }


procedure TgxVCLRenderer.Ellipse(aRect: GS.Geometry.Direction.TRct);
begin
//  if FCanvas.Fill.Kind = TBrushKind.Solid then
//  begin
//    FCanvas.FillEllipse(RectF(aRect.Left,aRect.Top,aRect.Right,aRect.Bottom),1.0);
//  end
//  else
//  begin
//    FCanvas.DrawEllipse(RectF(aRect.Left,aRect.Top,aRect.Right,aRect.Bottom),1.0);
//  end;
  FCanvas.Ellipse(Rect(Round(aRect.Left),Round(aRect.Top),Round(aRect.Right),Round(aRect.Bottom)));
end;

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


procedure TgxVCLRenderer.Line(a, b: GS.Geometry.Direction.TPt);
begin
//  FCanvas.DrawLine(PointF(a.X,a.Y),PointF(b.X,b.Y),1.0);
  FCanvas.MoveTo(Round(a.X),Round(a.Y));
  FCanvas.LineTo(Round(b.X),Round(b.Y));
end;

procedure TgxVCLRenderer.Polygone(aCoord: array of GS.Geometry.Direction.TPt);
var larr : Array of TPoint;
    i : integer;
begin
  SetLEngth(larr,Length(aCoord));
  for I := 0 to Length(larr)-1 do
  begin
    larr[i].X := round(acoord[i].X);
    larr[i].Y := round(acoord[i].Y);
  end;


//  if FCanvas.Fill.Kind = TBrushKind.Solid then
//  begin
//    FCanvas.FillPolygon(ld,1.0);
//  end
//  else
//  begin
//    FCanvas.DrawPolygon(ld,1.0);
//  end;
  FCanvas.Polygon(larr);
end;

procedure TgxVCLRenderer.Rectangle(aRect: GS.Geometry.Direction.TRct);
begin
//  if FCanvas.Fill.Kind = TBrushKind.Solid then
//  begin
//    FCanvas.FillRect(Rectf(aRect.Left,aRect.Top,aRect.Right,aRect.Bottom),0,0,[],1.0);
//  end
//  else
//  begin
//    FCanvas.DrawRect(Rectf(aRect.Left,aRect.Top,aRect.Right,aRect.Bottom),0,0,[],1.0);
//  end;
  FCanvas.Rectangle(Rect(Round(aRect.Left),Round(aRect.Top),Round(aRect.Right),Round(aRect.Bottom)));
end;

procedure TgxVCLRenderer.SetFillColor(aRed, aGreen, aBlue: Byte;
  const aAlpha: Byte);
begin
//  FCanvas.Brush.Color := ColorToRGB TAlphaColorF.Create(aRed/255,aGreen/255,aBlue/255,aAlpha/255).ToAlphaColor;
end;

procedure TgxVCLRenderer.SetFillType(aFillType: TGxRendererFillType);
begin
//  case aFillType of
//    TGxRendererFillType.none:       FCanvas.Fill.Kind := TBrushKind.None;
//    TGxRendererFillType.SolidColor: FCanvas.Fill.Kind := TBrushKind.Solid;
//  end;
end;

procedure TgxVCLRenderer.SetStrokeColor(aRed, aGreen, aBlue: Byte;
  const aAlpha: Byte);
begin
//  FCanvas.Stroke.Color :=TAlphaColorF.Create(aRed/255, aGreen/255, aBlue/255, aAlpha/255).ToAlphaColor;
end;

procedure TgxVCLRenderer.SetStrokeDashType(aDashType: TGxRenderDashType);
begin
//  case aDashType of
//    continous: FCanvas.Stroke.Dash := TStrokeDash.Solid;
//    dot:       FCanvas.Stroke.Dash := TStrokeDash.Dot;
//    dash:      FCanvas.Stroke.Dash := TStrokeDash.Dash;
//    dashdot:   FCanvas.Stroke.Dash := TStrokeDash.DashDot;
//  end;
end;

procedure TgxVCLRenderer.SetStrokeSize(aSize: Single);
begin
//  FCanvas.Stroke.Thickness :=  aSize;
end;

procedure TgxVCLRenderer.SetStrokeType(aStrokeType: TGxRendererFillType);
begin
//  case aStrokeType of
//  TGxRendererFillType.none :       FCanvas.Stroke.Kind := TBrushKind.None;
//  TGxRendererFillType.SolidColor : FCanvas.Stroke.Kind := TBrushKind.Solid;
//  end;
end;

procedure TgxVCLGraphBoardControl.Click;
begin
  inherited;

end;

constructor TgxVCLGraphBoardControl.Create(AOwner: TComponent);
begin
  inherited;
  FInternalBoard := TgxVCLGraphBoard.Create;
  FInternalBoard.Parent := Self;
  FInternalBoard.Renderer := TgxVCLRenderer.Create;
//  ClipChildren := true;
end;

destructor TgxVCLGraphBoardControl.Destroy;
begin
  FreeAndNil(FInternalBoard);
  inherited;
end;


procedure TgxVCLGraphBoardControl.MouseClick(Button: TMouseButton;
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

procedure TgxVCLGraphBoardControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TgxVCLGraphBoardControl.MouseMove(Shift: TShiftState; X, Y: Integer);
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
end;

procedure TgxVCLGraphBoardControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
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

procedure TgxVCLGraphBoardControl.MouseWheel(Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
  FInternalBoard.MouseWheelProcess([],WheelDelta);
end;

procedure TgxVCLGraphBoardControl.Paint;
begin
//  Canvas.BeginScene;
  TgxVCLRenderer(FInternalBoard.Renderer).Canvas := Canvas;
  FInternalBoard.Render;
//  Canvas.EndScene;
end;



{ TgxVCLGraphBoard }


function TgxVCLGraphBoard.GetDisplaySize: GS.Geometry.Direction.TPt;
begin
  Result := GS.Geometry.Direction.Point(Parent.Width, Parent.Height);
end;

end.
