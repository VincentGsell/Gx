unit Gx.GraphBoard;

interface

uses GS.Geometry.Direction,
     Gx.Graph.Core,
     Gx.Graph.Controls;

Type
  TgxRenderer = class(TCustomGxRenderer)
  end;

  TButtonsState = (LeftButton, RightButton, MiddleButton, PerformDoubleclick, Button3, Button4, Button5, Button6, Button7, Button8, Button9);
  TButtonsStateSet = Set of TButtonsState;
  TgxGraphBoard = class(TCustomGxGraphBoard)
  private
  protected
    FInput : TGxGraphInputData;
    FEnableGlobalPan: Boolean;
    FEnableGlobalRotate: Boolean;
    FEnableGlobalScale: Boolean;

    Procedure DoShapeItemOnChange;

  public
    constructor Create; Virtual;


    Function AddSelectionPoint(ax,ay : Single; Const aGripSize : Single = 10) : TGxSelectionPoint;
    Function AddSelectionRectangle(ax,ay : Single; Const aWidth : Single = 20; const aHeight : Single = 10) : TGxSelectionRectangle;
//    Function AddVector(ax,ay,Norm,Angle : Single) : TDrawBoardVector;
    Function AddCircle(ax,ay,Radius : Single) : TgxCircle;
    Function AddSquare(ax,ay,Side : Single) : TgxSquare;
    Function AddRectangle(ax,ay,SideW,SideH : Single) : TGxRectangle;

    Function AddObject(anItemObject : TCustomGxGraphItem) : TCustomGxGraphItem;

    Procedure MouseMoveProcess(X, Y: Single; ButtonsState : TButtonsStateSet); virtual;
    Procedure MouseDownProcess(X, Y: Single; Button : TButtonsState; ButtonsDown : TButtonsStateSet); virtual;
    Procedure MouseUpProcess(X, Y: Single; Button : TButtonsState; ButtonsUp : TButtonsStateSet); virtual;
    procedure MouseWheelProcess(ButtonsState : TButtonsStateSet; WheelDelta: Integer); virtual;
    procedure MouseClickProcess(Button : TButtonsState; ButtonsState : TButtonsStateSet; X, Y: Single); virtual;

    Procedure Render; Virtual;

    Procedure Scale(amount : Single; Const CenterX : Single = 0.0; Const CenterY : Single =0.0);
    Procedure Rotate(AngleAmount : Single; Const CenterX : Single = 0.0; Const CenterY : Single = 0.0);
    Procedure MoveTo(FromX,FromY, ToX, ToY : Single);
    Procedure MoveToAbs(aX, aY : Single);

    Property EnableGlobalPan : Boolean read FEnableGlobalPan Write FEnableGlobalPan;
    Property EnableGlobalRotate : Boolean read FEnableGlobalRotate Write FEnableGlobalRotate;
    Property EnableGlobalScale : Boolean read FEnableGlobalScale Write FEnableGlobalScale;
    Property InputData : TGxGraphInputData read FInput;
  end;

implementation


{ TgxGraphBoard }

function TgxGraphBoard.AddCircle(ax, ay, Radius: Single): TgxCircle;
begin
  Result := TGxCircle.Create;
  Result.ParentControl := self;
  Result.PositionX := ax;
  Result.PositionY := ay;
  Result.Radius := Radius;
  FObjects.Add(Result);
end;

function TgxGraphBoard.AddSquare(ax, ay, Side: Single): TgxSquare;
begin
  Result := TGxSquare.Create;
  Result.ParentControl := self;
  Result.PositionX := ax;
  Result.PositionY := ay;
  Result.Radius := Side;
  FObjects.Add(Result);
end;


function TgxGraphBoard.AddObject(anItemObject: TCustomGxGraphItem) : TCustomGxGraphItem;
begin
  Assert(Assigned(anItemObject));
  anItemObject.ParentControl := self;
  FObjects.Add(anItemObject);
  Result := anItemObject;
end;

function TgxGraphBoard.AddRectangle(ax, ay, SideW, SideH: Single): TGxRectangle;
begin
  Result := TGxRectangle.Create;
  Result.ParentControl := self;
  Result.PositionX := ax;
  Result.PositionY := ay;
  Result.RadiusH := SideW/2;
  Result.Radius := SideH/2;
  FObjects.Add(Result);
end;

function TgxGraphBoard.AddSelectionPoint(ax, ay: Single;
  const aGripSize: Single): TGxSelectionPoint;
begin
  Result := TGxSelectionPoint.Create;
  Result.ParentControl := self;
  Result.PositionX := ax;
  Result.PositionY := ay;
  Result.GripSize := aGripSize;
  FObjects.Add(Result);
end;

function TgxGraphBoard.AddSelectionRectangle(ax, ay: Single; const aWidth,
  aHeight: Single): TGxSelectionRectangle;
begin
  Result := TGxSelectionRectangle.Create;
  Result.ParentControl := self;
  Result.PositionX := ax;
  Result.PositionY := ay;
  Result.Width := aWidth;
  Result.Height := aHeight;
  FObjects.Add(Result);
end;


constructor TgxGraphBoard.Create;
begin
  Inherited;
  FEnableGlobalPan := true;
  FEnableGlobalRotate := False;
  FEnableGlobalScale := False;
end;

procedure TgxGraphBoard.DoShapeItemOnChange;
var i : integer;
begin
  for i := 0 to ObjectCount-1 do
  begin
    if Objects[i] is TGxGraphShapeItem then
    begin
      TGxGraphShapeItem(Objects[i]).DoChange;
    end;
  end;
end;

procedure TgxGraphBoard.MouseClickProcess(Button: TButtonsState;
  ButtonsState: TButtonsStateSet; X, Y: Single);
var a : TCustomGxGraphItem;
begin
  inherited;
  FInput.LastMouseX := FInput.MouseX;
  FInput.LastMouseY := FInput.MouseY;
  Finput.MouseX := X;
  Finput.MouseY := Y;
  Finput.MouseButtonLeft := TButtonsState.LeftButton in ButtonsState;
  Finput.MouseButtonRight := TButtonsState.RightButton in ButtonsState;
  Finput.MouseButtonMiddle := TButtonsState.MiddleButton in ButtonsState;
  Finput.MouseButtonLeftClick := Button = LeftButton;
  Finput.MouseButtonRightClick := Button = RightButton;
  //DblClick detect in mouse down only.

  for a in FObjects do
  begin
    a.InputProcess(Finput);
  end;
  Finput.MouseButtonLeftClick := False;
  Finput.MouseButtonRightClick := False;
end;

procedure TgxGraphBoard.MouseDownProcess(X, Y: Single;
  Button: TButtonsState; ButtonsDown : TButtonsStateSet);
var a : TCustomGxGraphItem;
begin
  inherited;
  FOffsetMD := Point(FOffsetX,FOffsetY);
  FInput.LastMouseX := FInput.MouseX;
  FInput.LastMouseY := FInput.MouseY;
  Finput.MouseX := X;
  Finput.MouseY := Y;
  Finput.MouseButtonLeft := TButtonsState.LeftButton in ButtonsDown;
  Finput.MouseButtonRight := TButtonsState.RightButton in ButtonsDown;
  Finput.MouseButtonMiddle := TButtonsState.MiddleButton in ButtonsDown;
  FInput.MouseButtonLeftDblClick := TButtonsState.PerformDoubleclick in ButtonsDown; //Seems to be detect only by mousedown. :/

  Finput.MouseButtonLeftDown := Button = TButtonsState.LeftButton;
  Finput.MouseButtonRightDown := Button = TButtonsState.RightButton;

  if Finput.MouseButtonLeftDown then
  begin
    Finput.LastMouseLeftDown.X := X;
    Finput.LastMouseLeftDown.Y := Y;
  end;

  if Finput.MouseButtonRight then
  begin
    Finput.LastMouseRightDown.X := X;
    Finput.LastMouseRightDown.Y := Y;
  end;


  for a in FObjects do
  begin
    a.InputProcess(Finput);
  end;
  Finput.MouseButtonLeftDown := False;
  Finput.MouseButtonRightDown := False;
  FInput.MouseButtonLeftDblClick := False;
//  Repaint;
end;

procedure TgxGraphBoard.MouseMoveProcess(X, Y: Single;
  ButtonsState: TButtonsStateSet);
var a : TCustomGxGraphItem;
    fOK : Boolean;
begin
  FInput.LastMouseX := FInput.MouseX;
  FInput.LastMouseY := FInput.MouseY;
  Finput.MouseX := X;
  Finput.MouseY := Y;

  fOK := true;
  for a in FObjects do
  begin
    a.InputProcess(Finput);
    if (a.MouseState <> TGxInputItemMachineState.imsScan) then
      fOK := False; //Pan forbidden.
  end;

  if fOK then
  begin
    if Finput.MouseButtonLeft then
    begin
      if FEnableGlobalPan then
        MoveTo(Finput.LastMouseLeftDown.X,Finput.LastMouseLeftDown.Y,Finput.MouseX,Finput.MouseY);
    end
    else
    if FInput.MouseButtonRight then
    begin
      if FEnableGlobalRotate then
        Rotate((Finput.LastMouseRightDown.X-FInput.MouseX)/10,  Finput.LastMouseRightDown.X, Finput.LastMouseRightDown.Y);
    end;
  end;
end;


procedure TgxGraphBoard.MouseUpProcess(X, Y: Single; Button : TButtonsState;
  ButtonsUp: TButtonsStateSet);
var a : TCustomGxGraphItem;
begin
  inherited;
  FInput.LastMouseX := FInput.MouseX;
  FInput.LastMouseY := FInput.MouseY;
  Finput.MouseX := X;
  Finput.MouseY := Y;
  Finput.MouseButtonLeft := False;
  Finput.MouseButtonRight := False;
  Finput.MouseButtonMiddle := False;
  Finput.MouseButtonLeftUp := TButtonsState.LeftButton in ButtonsUp;;
  for a in FObjects do
  begin
    a.InputProcess(Finput);
  end;
  Finput.MouseButtonLeftUp := False;
//  Repaint;
end;

procedure TgxGraphBoard.MouseWheelProcess(ButtonsState: TButtonsStateSet;
  WheelDelta: Integer);
var nF : Double;
begin
  if FEnableGlobalScale then
  begin
    nf := FScale +  WheelDelta/10000;
    Scale(nf,GlobalTransformationCenter.X,GlobalTransformationCenter.Y);
  end;
end;

procedure TgxGraphBoard.Render;
var i : Integer;
    la : TCustomGxGraphItem;
begin
  for i := 0 to ObjectCount-1 do
  begin
    la := Objects[i];
    if la.Visible  then
      la.Draw(FRenderer);
  end;
//  DoAfterDraw; ?
end;


procedure TgxGraphBoard.MoveTo(FromX,FromY, ToX, ToY : Single);
var a : TCustomGxGraphItem;
begin
  FvUtil.Origin := Point(FromX,FromY);
  FvUtil.SetPointedCoord(Point(ToX,ToY));

  FvUtil.Norm := FvUtil.Norm / GlobalScale;
  FvUtil.TurnBy(-GlobalAngle);

  MoveToAbs( FOffsetMD.X + FvUtil.GetPointedCoord.X - FromX,
             FOffsetMD.Y + FvUtil.GetPointedCoord.Y - FromY);

end;


procedure TgxGraphBoard.MoveToAbs(aX, aY: Single);
begin
  FOffsetX := aX;
  FOffsetY := aY;
  DoShapeItemOnChange;
end;

procedure TgxGraphBoard.Rotate(AngleAmount: Single; const CenterX,
  CenterY: Single);
begin
  FAngle := AngleAmount;
  FGlobalTransformationCenter.X := CenterX;
  FGlobalTransformationCenter.Y := CenterY;
  DoShapeItemOnChange;
end;

procedure TgxGraphBoard.Scale(amount : Single; Const CenterX : Single = 0.0; Const CenterY : Single =0.0);
begin
  if Amount>GLB_PrecisionTolerance then
  begin
    FScale := amount;
    FGlobalTransformationCenter.X := CenterX;
    FGlobalTransformationCenter.Y := CenterY;
    DoShapeItemOnChange;
  end;
end;

end.

