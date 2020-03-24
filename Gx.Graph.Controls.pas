unit Gx.Graph.Controls;

interface

uses GS.Geometry.Direction, Gx.Graph.Core, SysUtils;

Const
  CST_TRANSFORMATION_REQUIRED_PARENTCONTROL = 'Transformation required a parent control';
type


  TgxCustomEditableItem = Class(TGxGraphShapeItem)
  private
  protected
    FEditMode : Boolean;
    procedure InternalSetEditMode(const Value: Boolean); Virtual;
    function GetEditMode: Boolean; Virtual;
    procedure SetEditMode(const Value: Boolean); Virtual;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Property EditMode : Boolean read GetEditMode Write SetEditMode;

    Property Visible : Boolean read FVisible Write FVisible;
    property HitTest : Boolean read FHitTest Write FHitTest;
  End;

  TgxCustomEditableItemCenterAndSizeRotate = Class(TgxCustomEditableItem)
  private
  Protected
    FOnCenterClick: TOnGxClick;
    FOnAngleTrack: TOnGxChangeAngleTracking;

    FCenter : TPt;
    FCenterForDraw : TPt;

    FRadius : TDirectionalObject;
    FRadiusD : Single; //Design.
    FRadiusForDraw : TPt;

    FAngle : TDirectionalObject;
    FAngleForDraw : TPt;

    FEditorSelectPointRadius : TGxSelectionPoint;
    FEditorSelectPointCenter : TGxSelectionPoint;
    FEditorSelectPointRotate : TGxSelectionPoint;
    FEditorVector : TDirectionalObject;

    Procedure InternalEditorSetup; Virtual;
    procedure InternalSetEditMode(const Value: Boolean); Override;
      Procedure OnEditorChangeRadius(Sender : Tobject; Var X, Y : Single);
      Procedure OnEditorChangeCenter(Sender : Tobject; Var X, Y : Single);
      Procedure OnEditorChangeRotate(Sender : Tobject; Var X, Y : Single);

    Procedure InternalCenterClick(Sender : TObject);
    Procedure DoOnChangeAngleTracking;


    Procedure InternalChange; Override;

    Function GetBoundRect : TRct; Override;
    procedure SetBoundHeight(const Value: Single); Override;
    procedure SetBoundWidth(const Value: Single); Override;

    function GetDrawBoundRect : TRct; Virtual;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    function GetPointX: Single; Override;
    function GetPointY: Single; Override;
    procedure SetPointX(const Value: Single); Override;
    procedure SetPointY(const Value: Single); Override;

    function GetAngle: Single; Override;
    procedure SetAngle(const Value: Single); Override;
    function GetRadius: Single;
    procedure SetRadius(const Value: Single);

    Procedure Draw(aRenderer : TCustomGxRenderer); Override;
    Procedure InputProcess(Var InPuData : TGxGraphInputData); override;

    Property Radius : Single read GetRadius Write SetRadius;
    Property Angle;

    property DrawBoundrect : TRct read GetDrawBoundRect;

    Property OnCenterClick : TOnGxClick read FOnCenterClick Write FOnCenterClick;

    Property OnAngleTrack : TOnGxChangeAngleTracking read FOnAngleTrack Write FOnAngleTrack;

  End;

  TgxCustomEditableItemFullEditable = Class(TgxCustomEditableItemCenterAndSizeRotate)
  Protected

    FRadiusH : TDirectionalObject;
    FRadiusHD : Single;
    FRadiusForDrawH : TPt;

    FEditorSelectPointRadiusH : TGxSelectionPoint;

    Procedure InternalEditorSetup; Override;
    procedure InternalSetEditMode(const Value: Boolean); Override;
      Procedure OnEditorChangeRadiusH(Sender : Tobject; Var X, Y : Single);

    function GetRadiusH: Single;
    procedure SetRadiusH(const Value: Single);

    Procedure Draw(aRenderer : TCustomGxRenderer); Override;
    Procedure InputProcess(Var InPuData : TGxGraphInputData); Override;

    Procedure InternalChange; Override;

    Function GetBoundRect : TRct; Override;
    procedure SetBoundHeight(const Value: Single); Override;
    procedure SetBoundWidth(const Value: Single); Override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Property RadiusH : Single read GetRadiusH Write SetRadiusH;
  End;

  TGxCircle = Class(TgxCustomEditableItemCenterAndSizeRotate)
  Public
    Procedure Draw(aRenderer : TCustomGxRenderer); Override;
    Property Radius;
    Property Angle;
  End;

  TGxSquare = Class(TgxCustomEditableItemCenterAndSizeRotate)
  Protected
    FBuildVect : TDirectionalObject;
    Coords : Array[0..3] of TPt;
    DrawCoords : Array of TPt;

    Procedure CalculateQuad;

    Procedure InternalChange; Override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure Draw(aRenderer : TCustomGxRenderer); Override;
  End;

  TGxRectangle = Class(TgxCustomEditableItemFullEditable)
  Protected
    FBuildVect : TDirectionalObject;
    Coords : Array[0..3] of TPt;
    DrawCoords : Array of TPt;

    Procedure CalculateRectangle;

    Procedure InternalChange; Override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure Draw(aRenderer : TCustomGxRenderer); Override;
  End;

  TGxGrid = class(TGxRectangle)
  private
  protected
    FStep: Integer;
    fd1,fd2 : TDirectionalObject;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure Draw(aRenderer : TCustomGxRenderer); Override;

    property Step : Integer read FStep Write Fstep;
  end;



implementation

{ TgxCustomEditableItem }

constructor TgxCustomEditableItem.Create;
begin
  inherited;
  FVisible := true;
  FHitTest := true;
end;

destructor TgxCustomEditableItem.Destroy;
begin
  EditMode := False; //Trig the destruction of editor component.
  inherited;
end;


function TgxCustomEditableItem.GetEditMode: Boolean;
begin
  Result := FEditMode;
end;


procedure TgxCustomEditableItem.InternalSetEditMode(
  const Value: Boolean);
begin
  FEditMode := Value;
end;

procedure TgxCustomEditableItem.SetEditMode(const Value: Boolean);
begin
  if FEditMode<>Value then
  begin
    InternalSetEditMode(Value);
  end;
end;

{ TgxCustomEditableItemCenterAndSizeRotate }

constructor TgxCustomEditableItemCenterAndSizeRotate.Create;
begin
  Inherited;
  FAngle := TDirectionalObject.Create(0,0,10);
  FRadius := TDirectionalObject.Create(0,0,10);
end;

destructor TgxCustomEditableItemCenterAndSizeRotate.Destroy;
begin
  FreeAndNil(FAngle);
  FreeAndNil(FRadius);
  inherited;
end;

procedure TgxCustomEditableItemCenterAndSizeRotate.DoOnChangeAngleTracking;
var l : Single;
begin
  if Assigned(FOnAngleTrack) then
  begin
    l := FAngle.AngleInDegree;
    FOnAngleTrack(Self,l);
  end;
end;

procedure TgxCustomEditableItemCenterAndSizeRotate.Draw(aRenderer : TCustomGxRenderer);
begin
  inherited;
  aRenderer.SetStrokeSize(1);
  aRenderer.SetStrokeDashType(TGxRenderDashType.dash);
  aRenderer.Line(GS.Geometry.Direction.Point(FCenterForDraw.X,FCenterForDraw.Y),GS.Geometry.Direction.Point(FAngleForDraw.X,FAngleForDraw.Y));
  aRenderer.SetStrokeDashType(TGxRenderDashType.continous);


  if FEditMode and FHitTest then //And FHitTest because if HitTest not available, no operation possible : Useless to display handle.
  begin
    InternalEditorSetup;

    aRenderer.SetStrokeDashType(TGxRenderDashType.dashdot);
    aRenderer.Line(GS.Geometry.Direction.Point(FCenterForDraw.X,FCenterForDraw.Y),GS.Geometry.Direction.Point(FEditorSelectPointRadius.PositionX,FEditorSelectPointRadius.PositionY));

    aRenderer.SetStrokeDashType(TGxRenderDashType.dash);
    aRenderer.Line(GS.Geometry.Direction.Point(FCenterForDraw.X,FCenterForDraw.Y),GS.Geometry.Direction.Point(FEditorSelectPointRotate.PositionX,FEditorSelectPointRotate.PositionY));

   // aRenderer.SetStrokeDashType(TGxRenderDashType.continous);
    FEditorSelectPointRadius.Draw(aRenderer);
    FEditorSelectPointCenter.Draw(aRenderer);
    FEditorSelectPointRotate.Draw(aRenderer);
  end;
end;

function TgxCustomEditableItemCenterAndSizeRotate.GetAngle: Single;
begin
  Result := FAngle.AngleInDegree;
end;

function TgxCustomEditableItemCenterAndSizeRotate.GetBoundRect: TRct;
begin
  Result := Rect(FCenter.X - Radius, FCenter.Y - Radius, FCenter.X + Radius, FCenter.Y + Radius);
end;

function TgxCustomEditableItemCenterAndSizeRotate.GetDrawBoundRect: TRct;
begin
  Result := Rect( (FCenterForDraw.X-FRadiusD),
                  (FCenterForDraw.Y-FRadiusD),
                  (FCenterForDraw.X+FRadiusD),
                  (FCenterForDraw.Y+FRadiusD)
                );
end;

function TgxCustomEditableItemCenterAndSizeRotate.GetPointX: Single;
begin
  Result := FCenter.X;
end;

function TgxCustomEditableItemCenterAndSizeRotate.GetPointY: Single;
begin
  Result := FCenter.Y;
end;

function TgxCustomEditableItemCenterAndSizeRotate.GetRadius: Single;
begin
  Result := FRadius.Norm;
end;

procedure TgxCustomEditableItemCenterAndSizeRotate.InputProcess(var InPuData: TGxGraphInputData);
var
  P: TPt;
  LocalInput : TGxGraphInputData;
begin
  if not HitTest then
    Exit;
  inherited;

  LocalInput := InPuData;
  P := Point(LocalInput.MouseX,LocalInput.MouseY,0);
  ParentControl.Geometry.TransformeInverse(P);
  LocalInput.MouseX := P.X;
  LocalInput.MouseY := P.Y;

  if FEditMode then
  begin
    FMouseState := imsScan; //Force in this mode.

      begin
        if LocalInput.MouseButtonLeftDown then
        begin
//          if not(PointInControl(LocalInput.MouseX,LocalInput.MouseY)) Then
          begin
            //Just save FMX, FMY in PanMove eventuality.
            FMX := PositionX - LocalInput.MouseX;
            FMY := PositionY - LocalInput.MouseY;
          end;
        end
        else
        if LocalInput.MouseButtonLeftUp then
        begin
//          if not(PointInControl(LocalInput.MouseX,LocalInput.MouseY)) Then
          begin
            FMX := PositionX;
            FMY := PositionY;
          end;
        end;
      end;

    if Assigned(FEditorSelectPointRadius) then
    begin
      FEditorSelectPointRadius.InputProcess(InPuData);
      if (FEditorSelectPointRadius.MouseState <> imsScan) then
         FMouseState := imsProcessing;
    end;
    if Assigned(FEditorSelectPointCenter) then
    begin
      FEditorSelectPointCenter.InputProcess(InPuData);

      //If an event, processed in above InputProcess, push editor mode to false, Feditorxxx does not exists anymore.
      //Todo. Find something better.
      if Not(Assigned(FEditorSelectPointCenter)) then
        Exit;


      if (FEditorSelectPointCenter.MouseState <> imsScan) then
        FMouseState := imsProcessing;
    end;
    if Assigned(FEditorSelectPointRotate) then
    begin
      FEditorSelectPointRotate.InputProcess(InPuData);
      if (FEditorSelectPointRotate.MouseState <> imsScan) then
        FMouseState := imsProcessing;
    end;
  end
  else
  begin
    inherited;
  end;
end;


procedure TgxCustomEditableItemCenterAndSizeRotate.InternalEditorSetup;
begin
  if FEditMode then
  begin
    FEditorVector.Origin := FCenterForDraw;

    //Replace selector.
    FEditorSelectPointCenter.PositionX := FCenterForDraw.X;
    FEditorSelectPointCenter.PositionY := FCenterForDraw.Y;

    //Update radius,
    FEditorVector.SetPointedCoord(FAngleForDraw);
    FEditorVector.Norm := FRadiusD;
    FEditorVector.TurnBy(-90);

    FEditorSelectPointRadius.PositionX := FEditorVector.GetPointedCoord.X;
    FEditorSelectPointRadius.PositionY := FEditorVector.GetPointedCoord.Y;

    //Update Rotate,
    FEditorVector.SetPointedCoord(FAngleForDraw);
    FEditorVector.Norm := FRadiusD + FEditorSelectPointRotate.Width*2;

    FEditorSelectPointRotate.PositionX := FEditorVector.GetPointedCoord.X;
    FEditorSelectPointRotate.PositionY := FEditorVector.GetPointedCoord.Y;
  end;
end;

procedure TgxCustomEditableItemCenterAndSizeRotate.InternalCenterClick(
  Sender: TObject);
begin
  if Assigned(FOnCenterClick) then
  begin
    //redirect on master component.
    FOnCenterClick(Self); //and not sendr. which is the widget center component).
  end;
end;

procedure TgxCustomEditableItemCenterAndSizeRotate.InternalChange;
begin
  Assert(Assigned(ParentControl),CST_TRANSFORMATION_REQUIRED_PARENTCONTROL);
  FCenterForDraw := FCenter;
  ParentControl.Geometry.Transforme(FCenterForDraw);

  FRadius.Origin := FCenter;
  FRadius.AngleInDegree := 0.0;
  FRadiusForDraw := FRadius.GetPointedCoord;
  ParentControl.Geometry.Transforme(FRadiusForDraw);

  FRadiusD := FRadius.Norm * ParentControl.GlobalScale;

  FAngle.Origin := FCenter;
  FAngle.Norm := FRadius.Norm;
  FAngleForDraw := FAngle.GetPointedCoord;
  ParentControl.Geometry.Transforme(FAngleForDraw);
end;

procedure TgxCustomEditableItemCenterAndSizeRotate.InternalSetEditMode(const Value: Boolean);
begin
  Inherited;
  if Value then
  begin
    FEditorVector := TDirectionalObject.Create(0,0,10);
    FEditorSelectPointRadius := TGxSelectionPoint.Create;
    FEditorSelectPointCenter := TGxSelectionPoint.Create;
    FEditorSelectPointRotate := TGxSelectionPoint.Create;
    FEditorSelectPointRadius.OnPositionTrack := OnEditorChangeRadius;
    FEditorSelectPointCenter.OnPositionTrack := OnEditorChangeCenter;
    FEditorSelectPointRotate.OnPositionTrack := OnEditorChangeRotate;

    FEditorSelectPointCenter.OnClick := InternalCenterClick;

    InternalEditorSetup;
  end
  else
  begin
    if Assigned(FEditorSelectPointRadius) then
      FreeAndNil(FEditorSelectPointRadius);
    if Assigned(FEditorSelectPointCenter) then
      FreeAndNil(FEditorSelectPointCenter);
    if Assigned(FEditorSelectPointRotate) then
      FreeAndNil(FEditorSelectPointRotate);
    if Assigned(FEditorVector) then
      FreeAndNil(FEditorVector);
  end;
end;

procedure TgxCustomEditableItemCenterAndSizeRotate.OnEditorChangeCenter(Sender: Tobject; var X,
  Y: Single);
var P : TPt;
begin
  P := Point(X,Y,0);
  ParentControl.Geometry.TransformeInverse(P);
  FCenter := P;
  InternalChange;
  DoOnChangeTracking;
end;

procedure TgxCustomEditableItemCenterAndSizeRotate.OnEditorChangeRadius(Sender: Tobject; var X,
  Y: Single);
var P : TPt;
begin
  P := Point(X,Y,0);
  ParentControl.Geometry.TransformeInverse(P);
  FRadius.SetPointedCoord(P);
  InternalChange;
  DoOnSizeTracking;
end;

procedure TgxCustomEditableItemCenterAndSizeRotate.OnEditorChangeRotate(Sender: Tobject; var X,
  Y: Single);
var P : TPt;
begin
  P := Point(X,Y,0);
  ParentControl.Geometry.TransformeInverse(P);
  FAngle.SetPointedCoord(Point(P.X,P.Y));
  InternalChange;
  DoOnChangeAngleTracking;
end;

procedure TgxCustomEditableItemCenterAndSizeRotate.SetAngle(const Value: Single);
begin
  FAngle.AngleInDegree := Value;
  InternalChange;
end;

procedure TgxCustomEditableItemCenterAndSizeRotate.SetBoundHeight(
  const Value: Single);
begin
  Radius := Value/2;
end;

procedure TgxCustomEditableItemCenterAndSizeRotate.SetBoundWidth(
  const Value: Single);
begin
  Radius := Value/2;
end;

procedure TgxCustomEditableItemCenterAndSizeRotate.SetPointX(const Value: Single);
begin
  FCenter.X := Value;
  InternalChange;
end;

procedure TgxCustomEditableItemCenterAndSizeRotate.SetPointY(const Value: Single);
begin
  FCenter.Y := Value;
  InternalChange;
end;

procedure TgxCustomEditableItemCenterAndSizeRotate.SetRadius(const Value: Single);
begin
  FRadius.Norm := Value;
  InternalChange;
end;

{ TgxCustomEditableItemFullEditable }

constructor TgxCustomEditableItemFullEditable.Create;
begin
  Inherited;
  FRadiusH := TDirectionalObject.Create(0,0,10);
end;

destructor TgxCustomEditableItemFullEditable.Destroy;
begin
  FreeAndNil(FRadiusH);
  inherited;
end;

procedure TgxCustomEditableItemFullEditable.Draw(aRenderer : TCustomGxRenderer);
begin
  inherited;
  if FEditMode And FHitTest then
  begin
//    aCanvas.Stroke.Dash := TStrokeDash.Dot;
//    aCanvas.DrawLine(Pointf(FCenterForDraw.X,FCenterForDraw.Y),Pointf(FEditorSelectPointRadiusH.PositionX,FEditorSelectPointRadiusH.PositionY),0.8);
    FEditorSelectPointRadiusH.Draw(aRenderer);
  end;
end;

function TgxCustomEditableItemFullEditable.GetBoundRect: TRct;
begin
  Result := Rect(FCenter.X - Radius, FCenter.Y - RadiusH, FCenter.X + Radius, FCenter.Y + RadiusH);
end;

function TgxCustomEditableItemFullEditable.GetRadiusH: Single;
begin
  Result := FRadiusH.Norm;
end;

procedure TgxCustomEditableItemFullEditable.InputProcess(
  var InPuData: TGxGraphInputData);
begin
  if not HitTest then
    Exit;
  Inherited;
  if FEditMode then
  begin
    if Assigned(FEditorSelectPointRadiusH) then
    begin
      FEditorSelectPointRadiusH.InputProcess(InPuData);
      if FEditorSelectPointRadiusH.MouseState <> imsScan then
        FMouseState := imsProcessing;
    end;
  end;
end;

procedure TgxCustomEditableItemFullEditable.InternalChange;
begin
  Inherited;
  FRadiusH.Origin := FCenter;
  FRadiusH.AngleInDegree := 0.0;
  FRadiusForDrawH := FRadiusH.GetPointedCoord;
  ParentControl.Geometry.Transforme(FRadiusForDrawH);

  FRadiusHD := FRadiusH.Norm * ParentControl.GlobalScale;

  FAngle.Origin := FCenter;
  FAngle.Norm := FRadiusH.Norm;
  FAngleForDraw := FAngle.GetPointedCoord;
  ParentControl.Geometry.Transforme(FAngleForDraw);

end;

procedure TgxCustomEditableItemFullEditable.InternalEditorSetup;
begin
  inherited;
  if FEditMode then
  begin
    if Assigned(FEditorSelectPointRadiusH) then //Call by Parent before object creation.
    begin
      //Update radiush,
      FEditorVector.SetPointedCoord(FAngleForDraw);
      FEditorVector.Norm := FRadiusHD;
      FEditorVector.TurnBy(-180);

      FEditorSelectPointRadiusH.PositionX := FEditorVector.GetPointedCoord.X;
      FEditorSelectPointRadiusH.PositionY := FEditorVector.GetPointedCoord.Y;

      //Update Rotate,
      FEditorVector.SetPointedCoord(FAngleForDraw);
      FEditorVector.Norm := FRadiusHD + FEditorSelectPointRotate.Width*2;

      FEditorSelectPointRotate.PositionX := FEditorVector.GetPointedCoord.X;
      FEditorSelectPointRotate.PositionY := FEditorVector.GetPointedCoord.Y;

    end;
  end;
end;

procedure TgxCustomEditableItemFullEditable.InternalSetEditMode(
  const Value: Boolean);
begin
  inherited;
  if Value then
  begin
    FEditorSelectPointRadiusH := TGxSelectionPoint.Create;
    FEditorSelectPointRadiusH.OnPositionTrack := OnEditorChangeRadiusH;
    InternalEditorSetup;
  end
  else
  begin
    if Assigned(FEditorSelectPointRadiusH) then
      FreeAndNil(FEditorSelectPointRadiusH);
  end;
end;

procedure TgxCustomEditableItemFullEditable.OnEditorChangeRadiusH(
  Sender: Tobject; var X, Y: Single);
var P : TPt;
begin
  P := Point(X,Y,0);
  ParentControl.Geometry.TransformeInverse(P);
  FRadiusH.SetPointedCoord(P);
  InternalChange;
end;

procedure TgxCustomEditableItemFullEditable.SetBoundHeight(const Value: Single);
begin
  RadiusH := Value;
end;

procedure TgxCustomEditableItemFullEditable.SetBoundWidth(const Value: Single);
begin
  Radius := value;
end;

procedure TgxCustomEditableItemFullEditable.SetRadiusH(const Value: Single);
begin
  FRadiusH.Norm := Value;
  InternalChange;
end;



{ TGxCircle }

procedure TGxCircle.Draw(aRenderer: TCustomGxRenderer);
begin
  Inherited;
  StandartDrawingSetup(aRenderer);
  aRenderer.Ellipse(DrawBoundRect);
end;

{ TGxSquare }

procedure TGxSquare.CalculateQuad;
begin
  FBuildVect.Origin := FCenter;
  FBuildVect.AngleInDegree := FAngle.AngleInDegree;
  FBuildVect.Norm := FRadius.Norm;
  FBuildVect.MoveAhead;

  FBuildVect.TurnLeft;
  FBuildVect.MoveAhead;
  Coords[1] := FBuildVect.Origin;

  FBuildVect.TurnLeft;
  FBuildVect.MoveAhead;
  FBuildVect.MoveAhead;
  Coords[0] := FBuildVect.Origin;

  FBuildVect.TurnLeft;
  FBuildVect.MoveAhead;
  FBuildVect.MoveAhead;
  Coords[3] := FBuildVect.Origin;

  FBuildVect.TurnLeft;
  FBuildVect.MoveAhead;
  FBuildVect.MoveAhead;
  Coords[2] := FBuildVect.Origin;
end;

constructor TGxSquare.Create;
begin
  inherited;
  FBuildVect := TDirectionalObject.Create(0,0,10);
end;

destructor TGxSquare.Destroy;
begin
  FreeAndNil(FBuildVect);
  inherited;
end;

procedure TGxSquare.Draw(aRenderer: TCustomGxRenderer);
begin
  Inherited;
  StandartDrawingSetup(aRenderer);
  //Coordnets are processed in CalculateSquare.
  aRenderer.Polygone(DrawCoords);
end;

procedure TGxSquare.InternalChange;
var P : TPt;
    i : integer;
begin
  inherited;
  CalculateQuad;
  SetLength(DrawCoords,Length(Coords));
  for I := 0 to 3 do
  begin
    DrawCoords[i] := Coords[i];
    ParentControl.Geometry.Transforme(DrawCoords[i]);
  end;
end;

{ TGxRectangle }

procedure TGxRectangle.CalculateRectangle;
begin
  FBuildVect.Origin := FCenter;
  FBuildVect.AngleInDegree := FAngle.AngleInDegree;
  FBuildVect.Norm := FRadiusH.Norm;
  FBuildVect.MoveAhead;

  FBuildVect.TurnLeft;
  FBuildVect.Norm := FRadius.Norm;
  FBuildVect.MoveAhead;
  Coords[1] := FBuildVect.Origin;

  FBuildVect.TurnLeft;
  FBuildVect.Norm := FRadiusH.Norm;
  FBuildVect.MoveAhead;
  FBuildVect.MoveAhead;
  Coords[0] := FBuildVect.Origin;

  FBuildVect.TurnLeft;
  FBuildVect.Norm := FRadius.Norm;
  FBuildVect.MoveAhead;
  FBuildVect.MoveAhead;
  Coords[3] := FBuildVect.Origin;

  FBuildVect.TurnLeft;
  FBuildVect.Norm := FRadiusH.Norm;
  FBuildVect.MoveAhead;
  FBuildVect.MoveAhead;
  Coords[2] := FBuildVect.Origin;
end;

constructor TGxRectangle.Create;
begin
  Inherited;
  FBuildVect := TDirectionalObject.Create(0,0,10);
end;

destructor TGxRectangle.Destroy;
begin
  FreeAndNil(FBuildVect);
  inherited;
end;

procedure TGxRectangle.Draw(aRenderer: TCustomGxRenderer);
begin
  Inherited;
  StandartDrawingSetup(aRenderer);
  aRenderer.Polygone(DrawCoords);
end;

procedure TGxRectangle.InternalChange;
var P : TPt;
    i : integer;
begin
  inherited;
  CalculateRectangle;
  SetLength(DrawCoords,Length(Coords));
  for I := 0 to 3 do
  begin
    DrawCoords[i] := Coords[i];
    ParentControl.Geometry.Transforme(DrawCoords[i]);
  end;
end;

{ TGxGrid }

constructor TGxGrid.Create;
begin
  Inherited;
  fd1 := TDirectionalObject.Create(0,0,10);
  fd2 := TDirectionalObject.Create(0,0,10);
  FStep := 10;
end;

destructor TGxGrid.Destroy;
begin
  FreeAndNil(fd1);
  FreeAndNil(fd2);
  inherited;
end;

procedure TGxGrid.Draw(aRenderer: TCustomGxRenderer);
var i : integer;
begin
  Inherited;
  StandartDrawingSetup(aRenderer);

  //Horiz line.
  Fd1.Origin := DrawCoords[0];
  Fd1.PointAt(DrawCoords[1]);
  Fd2.Origin := DrawCoords[3];
  fd2.PointAt(DrawCoords[2]);

  fd1.Norm := fd1.Norm / FStep; //Step.
  fd2.Norm := fd2.Norm / FStep; //Step.

  for I := 0 to Step-1 do
  begin
    fd1.MoveAhead;
    fd2.MoveAhead;
    aRenderer.line(fd1.Origin,fd2.Origin);
  end;

  //Vert line.
  Fd1.Origin := DrawCoords[0];
  Fd1.PointAt(DrawCoords[3]);
  Fd2.Origin := DrawCoords[1];
  fd2.PointAt(DrawCoords[2]);

  fd1.Norm := fd1.Norm / FStep; //Step.
  fd2.Norm := fd2.Norm / FStep; //Step.

  for I := 0 to Step-1 do
  begin
    fd1.MoveAhead;
    fd2.MoveAhead;
    aRenderer.line(fd1.Origin,fd2.Origin);
  end;

//  aRenderer.Polygone(DrawCoords);
end;

end.
