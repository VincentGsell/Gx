unit Gx.Graph.Core;

interface

Uses SysUtils, Classes,
     Generics.Collections,
     GS.Geometry.Direction;

Type
  TOnGxChangePositionTracking = procedure (Sender: TObject; var X, Y: Single) of object;
  TOnGxChangeSizeTracking = procedure (Sender: TObject; var Width, Height: Single) of object;
  TOnGxChangeAngleTracking = procedure (Sender: TObject; var NewAngle : Single) of object;
  TOnGxClick = procedure(Sender : TObject) of Object;

  TCustomGxGraphBoard = class;
  TCustomGxRenderer = class;

  TGxGraphInputData = Packed record
    MouseX : Single;
    MouseY : Single;
    LastMouseX : Single;
    LastMouseY : Single;

    MouseButtonLeft : Boolean;
    MouseButtonRight : Boolean;
    MouseButtonMiddle : Boolean;

    MouseButtonLeftDown : Boolean;
    MouseButtonLeftUp  : Boolean;
    MouseButtonLeftClick : Boolean;
    MouseButtonLeftDblClick : Boolean;


    MouseButtonRightDown : Boolean;
    MouseButtonRightUp  : Boolean;
    MouseButtonRightClick : Boolean;

    LastMouseLeftDown : TPt;
    LastMouseRightDown : TPt;
  end;

  TGxInputItemMachineState = ( imsScan,
                               imsInputDeviceFlyOverMe,
                               imsInputDevicePushDownOnMe,
                               imsInputDeviceSelectMe,
                               imsProcessing
                             );



  //Base graph item.
  TCustomGxGraphItem = Class abstract
  private
  protected
    FX,FY : Single;
    FMX,FMY : Single; //Last mousedown coords for self modification.
    FMouseState : TGxInputItemMachineState;
    FOnChangeTracking: TOnGxChangePositionTracking;
    FOnChangeSizeTracking : TOnGxChangeSizeTracking;
    FOnClick : TOnGxClick;
    FOnDblClick : TOnGxClick;
    FData2: TObject;
    FData1: TObject;
    FHitTest: Boolean;
    FVisible: Boolean;
    FParentControl : TCustomGxGraphBoard;
    FAlpha: Byte;
    FStrokeSize: Integer;
    FTagStr: String;
    FTag: NativeInt;

    Procedure DoOnChangeTracking;
    Procedure DoOnSizeTracking;
    Procedure DoOnClick;
    Procedure DoOnDblClick;

    function GetPointX: Single; Virtual;
    function GetPointY: Single; Virtual;
    procedure SetPointX(const Value: Single); Virtual;
    procedure SetPointY(const Value: Single); Virtual;
    function GetSelected: Boolean;
    function GetBoundHeight: Single; virtual;
    function GetBoundWidth: Single; virtual;
    procedure SetBoundHeight(const Value: Single); virtual; abstract;
    procedure SetBoundWidth(const Value: Single); virtual; abstract;
    function GetPosition: TPt;
    procedure SetPosition(const Value: TPt);
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetParentControl(const Value: TCustomGxGraphBoard);
  Public


    Function GetBoundRect : TRct; virtual;
    Function PointInControl(ax,ay : Single) : Boolean; Virtual;
    procedure StandartDrawingSetup(aRenderer : TCustomGxRenderer);
    Procedure Draw(aRenderer : TCustomGxRenderer);  Virtual;

    Procedure InputProcess(Var InPuData : TGxGraphInputData); Virtual;

    Constructor Create; Virtual;
  Published
    Property LastLeftMouseDownPositionX : Single read FMX;
    Property LastLeftMouseDownPositionY : Single read FMY;

    property Position : TPt read GetPosition Write SetPosition;
    Property PositionX : Single read GetPointX Write SetPointX;
    Property PositionY : Single read GetPointY Write SetPointY;
    Property Selected : Boolean read GetSelected;

    Property OnPositionTrack : TOnGxChangePositionTracking read FOnChangeTracking Write FOnChangeTracking;
    Property OnSizeTrack : TOnGxChangeSizeTracking read FOnChangeSizeTracking Write FOnChangeSizeTracking;
    Property OnClick : TOnGxClick read FOnclick Write FOnClick;
    Property OnDblClick : TOnGxClick read FOnDblClick Write FOnDblClick;

    Property BoundRect : TRct read GetBoundRect;
    property Width : Single read GetBoundWidth Write SetBoundWidth;
    Property Height : Single read GetBoundHeight Write SetBoundHeight;

    Property MouseState : TGxInputItemMachineState read FMouseState Write FMouseState;

    Property ParentControl : TCustomGxGraphBoard read FParentControl Write SetParentControl;

    //HitTest property : Disable input completely.
    Property HitTest : Boolean read FHitTest Write FHitTest;
    //Visible property : Item in no longer visible, but still present. (HitTest could work !)
    Property Visible : Boolean read FVisible Write FVisible;
    //Enabled is based upon 2 above properties. (i.e. Enabled := HitTest And Visible)
    Property Enabled : Boolean read GetEnabled Write SetEnabled;

    //Visual stuff. Must be managed by inherited stuff.
    property Alpha : Byte read FAlpha Write FAlpha;
    property StrokeSize : Integer read FStrokeSize Write FStrokeSize;
    property TagStr : String read FTagStr Write FTagStr;
    property Tag : NativeInt read FTag Write FTag;
    Property TagData1 : TObject read FData1 Write FData1;
    Property TagData2 : TObject read FData2 Write FData2;
  end;

  //Gui stable shape. (Not transformable)
  TGxStaticCommon = class(TCustomGxGraphItem)
  private
  protected
  public
    Procedure Draw(aRenderer : TCustomGxRenderer);  Override;
  end;

  TGxSingleSidedShape = Class(TGxStaticCommon)
  protected
    FSize : Single;
    Function GetBoundRect : TRct; Override;
    procedure SetBoundHeight(const Value: Single); Override;
    procedure SetBoundWidth(const Value: Single); Override;
  public
    Constructor Create; override;
  End;

  //Gui stable shape. (not transformable)
  TGxBoundedShape = Class(TGxStaticCommon)
  protected
    FWidth : Single;
    FHeight : Single;
    Function GetBoundRect : TRct; Override;
    function GetBoundHeight: Single; Override;
    function GetBoundWidth: Single; Override;
    procedure SetBoundHeight(const Value: Single); Override;
    procedure SetBoundWidth(const Value: Single); Override;
  public
    Constructor Create; override;
  End;

  //Gui stable shape : Not responding to transfomration. (HUD)
  TGxSelectionPoint = Class(TGxSingleSidedShape)
  Private
    function GetGripSize: Single;
    procedure SetGripSize(const Value: Single);

  Public
    Procedure Draw(aRenderer : TCustomGxRenderer);  Override;

  Published
    Property GripSize : Single read GetGripSize Write SetGripSize;
  End;

  TGxSelectionRectangle = class(TGxBoundedShape)
  public
    Procedure Draw(aRenderer : TCustomGxRenderer);  Override;
  end;

  //coub be used for background cell indication (as graph paper).
  //Warning : could no be used with board.Scale or Rotate. Illusion work only with no global transformation.
  TGxSelectionGrid = class(TGxBoundedShape)
  private
  protected
    FStep: Integer;
    FDrawBorder: Boolean;
    FDrawWithParentOffset: Boolean;
    FOffSet: TPt;
    procedure SetStep(const Value: Integer);
  public
    Constructor Create; override;
    Procedure Draw(aRenderer : TCustomGxRenderer);  Override;
    Property StepEveryPixel : Integer read FStep write SetStep;
    property DrawBorder : Boolean read FDrawBorder Write FDrawBorder;

    property DrawWithParentOffset : Boolean read FDrawWithParentOffset Write FDrawWithParentOffset;
    Property OffSet : TPt read FOffSet Write FOffset;
  end;


  //Base object which responds to global transformation.
  TGxGraphShapeItem = class(TCustomGxGraphItem)
  private
  Protected
    Procedure InternalChange; Virtual; Abstract;

    function GetAngle: Single; Virtual; Abstract;
    procedure SetAngle(const Value: Single); Virtual; Abstract;
  Public
    Procedure DoChange; Virtual;

    Property Angle : Single read GetAngle Write SetAngle;
  end;


  TGxRendererFillType = (none, SolidColor);
  TGxRenderDashType = (continous, dot, dash, dashdot);
  TCustomGxRenderer = class abstract
  Public
    procedure BeginDraw(Sender : TObject); virtual; abstract;
    procedure EndDraw; virtual; abstract;

    Procedure SetStrokeSize(aSize : Single); virtual; abstract;
    Procedure SetStrokeDashType(aDashType : TGxRenderDashType); virtual; abstract;
    Procedure SetStrokeType(aStrokeType : TGxRendererFillType); virtual; abstract;
    Procedure SetStrokeColor(aRed, aGreen, aBlue : Byte; Const aAlpha : Byte = 255); virtual; abstract;
    Procedure SetFillColor(aRed, aGreen, aBlue : Byte; Const aAlpha : Byte = 255); virtual; abstract;
    procedure SetFillType(aFillType : TGxRendererFillType); virtual; abstract;


    Procedure Ellipse(aRect : GS.Geometry.Direction.TRct); virtual; abstract;
    Procedure Rectangle(aRect : GS.Geometry.Direction.TRct); virtual; Abstract;
    procedure Polygone(aCoord : array of TPt); virtual; abstract;
    Procedure Line(a,b : TPt); Virtual; Abstract;
  end;

  TGxGraphGeometrie = Class
  Protected
    FVectorUtil : TDirectionalObject;
  Public
    ParentControl : TCustomGxGraphBoard;

    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Transforme(var aP : TPt);         //WorldToScreen.
    Procedure TransformeInverse(var aP : TPt);  //ScreenToWorld.
  End;


  TCustomGxGraphBoard = class
  protected
    FScale: Single;
    FAngle: Single;
    FOffsetX: Single;
    FOffsetY: Single;
    FGlobalTransformationCenter: TPt;
    FvUtil : TDirectionalObject;
    FOffsetMD : TPt;
    FRenderer: TCustomgxRenderer;
    FObjects : TObjectList<TCustomGxGraphItem>;
    FQueryResp : TList<TCustomGxGraphItem>;

    function getObjectcount: Uint32;
    function GetObjects(Index: UINt32): TCustomGxGraphItem;
    //Get all item inside aRect.
//    Function QueryItemInRect(aRectf : TRct) : TList<TCustomGxGraphItem>;
  public
    Geometry : TGxGraphGeometrie;

    constructor Create; Virtual;

    function GetDisplaySize : TPt; Virtual; Abstract;

    Property GlobalOffsetX : Single read FOffsetX write FOffsetX;
    Property GlobalOffsetY : Single read FOffsetY write FOffsetY;
    Property GlobalScale : Single read FScale;
    Property GlobalAngle : Single read FAngle;
    Property GlobalTransformationCenter : TPt read FGlobalTransformationCenter Write FGlobalTransformationCenter;

    property Objects[Index : UINt32] : TCustomGxGraphItem read GetObjects;
    property ObjectCount : Uint32 read getObjectcount;

    Property Renderer : TCustomgxRenderer read FRenderer Write FRenderer;
  end;


implementation

{ TDrawBoardCustomItem }

constructor TCustomGxGraphItem.Create;
begin
  Inherited;
  FMouseState := imsScan;
  Enabled := true;
  FAlpha := 255;
  FStrokeSize := 1;
end;



procedure TCustomGxGraphItem.DoOnChangeTracking;
var ax, ay : Single;
begin
  if Assigned(FOnChangeTracking) then
  begin
    ax := PositionX;
    ay := PositionY;
    FOnChangeTracking(Self,ax,ay);
    PositionX := ax;
    PositionY := ay;
  end;
end;

procedure TCustomGxGraphItem.DoOnClick;
begin
  if Assigned(FOnClick) then
  begin
    FOnClick(Self);
  end;
end;

procedure TCustomGxGraphItem.DoOnDblClick;
begin
  if Assigned(FOnDblClick) then
  begin
    FOnDblClick(Self);
  end;
end;

procedure TCustomGxGraphItem.DoOnSizeTracking;
var aw, ah : Single;
begin
  if Assigned(FOnChangeTracking) then
  begin
    aw := Width;
    ah := Height;
    FOnChangeSizeTracking(Self,aw,ah);
  end;
end;

procedure TCustomGxGraphItem.Draw(aRenderer: TCustomGxRenderer);
begin
  StandartDrawingSetup(aRenderer);
end;

function TCustomGxGraphItem.GetBoundHeight: Single;
var lRect : TRct;
begin
  lrect :=  GetBoundRect;
  Result := Abs(lRect.Bottom - lRect.Top);
end;

function TCustomGxGraphItem.GetBoundRect: TRct;
begin
  Result := Rect(FX,FY,FX + 10, FY + 10);
end;

function TCustomGxGraphItem.GetBoundWidth: Single;
var lRect : TRct;
begin
  lrect :=  GetBoundRect;
  Result := Abs(lRect.Right - lRect.Left);
end;

function TCustomGxGraphItem.GetEnabled: Boolean;
begin
  Result := FVisible and FHitTest;
end;

function TCustomGxGraphItem.GetPointX: Single;
begin
  Result := FX;
end;

function TCustomGxGraphItem.GetPointY: Single;
begin
  result := FY;
end;

function TCustomGxGraphItem.GetPosition: TPt;
begin
  Result := Point(PositionX, PositionY);
end;

function TCustomGxGraphItem.GetSelected: Boolean;
begin
  Result := FMouseState = imsInputDeviceSelectMe;
end;

procedure TCustomGxGraphItem.InputProcess(var InPuData: TGxGraphInputData);
var
  P: TPt;
  LocalInput : TGxGraphInputData;
begin
  if not HitTest then
    Exit;
  LocalInput := InPuData;

  if Self is TGxGraphShapeItem then
  begin
    P := Point(LocalInput.MouseX,LocalInput.MouseY,0);
    ParentControl.Geometry.TransformeInverse(P);
    LocalInput.MouseX := P.X;
    LocalInput.MouseY := P.Y;
  end;

  case FMouseState of
    imsScan :
    begin
      if PointInControl(LocalInput.MouseX,LocalInput.MouseY) And
        Not(LocalInput.MouseButtonLeft) then
      begin
          FMouseState := imsInputDeviceFlyOverMe;
      end
      else
      begin
        if LocalInput.MouseButtonLeftDown then
        begin
          if not(PointInControl(LocalInput.MouseX,LocalInput.MouseY)) Then
          begin
            //Just save FMX, FMY in PanMove eventuality.
            FMX := PositionX - LocalInput.MouseX;
            FMY := PositionY - LocalInput.MouseY;
          end;
        end
        else
        if LocalInput.MouseButtonLeftUp then
        begin
          if not(PointInControl(LocalInput.MouseX,LocalInput.MouseY)) Then
          begin
            FMX := PositionX;
            FMY := PositionY;
          end;
        end;
      end;

    end;

    imsInputDeviceFlyOverMe:
    begin
      if PointInControl(LocalInput.MouseX,LocalInput.MouseY) Then
      begin
        If LocalInput.MouseButtonLeftDown then
        begin
          FMouseState := imsInputDevicePushDownOnMe;
        end;
      end
      else
      begin
        FMouseState := imsScan;
      end;

    end;

    imsInputDevicePushDownOnMe :
    begin
      if PointInControl(LocalInput.MouseX,LocalInput.MouseY) Then
      begin
        If LocalInput.MouseButtonLeft then
        begin
          FMX := PositionX - LocalInput.MouseX;
          FMY := PositionY - LocalInput.MouseY;

          if (Abs(InPuData.LastMouseX-InPuData.MouseX)>0) or (Abs(InPuData.LastMouseY-InPuData.MouseY)>0) then
          begin
            FMouseState := imsInputDeviceSelectMe; //goto drag and drop.
          end;

          if InPuData.MouseButtonLeftDblClick then
          begin
            DoOnDblClick;
            FMouseState := imsScan;
          end
        end
        else
        begin
          if InPuData.MouseButtonLeftClick or InPuData.MouseButtonRightClick then
          begin
            begin
              DoOnClick;
            end;
          end;
          FMouseState := imsInputDeviceFlyOverMe;
        end;
      end
      else
      begin
        FMouseState := imsScan;
      end;
    end;

    imsInputDeviceSelectMe:
    begin
      If LocalInput.MouseButtonLeft then
      begin
        PositionX := LocalInput.MouseX + FMX;
        PositionY := LocalInput.MouseY + FMY;
        DoOnChangeTracking;
      end
      else
      begin
        FMouseState := imsScan;
      end;
    end;
  end;

end;

function TCustomGxGraphItem.PointInControl(ax, ay: Single): Boolean;
var ofs : TPt;
begin
  Result := vPtInRect(Point(ax,ay), BoundRect,ofs);
end;


procedure TCustomGxGraphItem.SetEnabled(const Value: Boolean);
begin
  FHitTest := Value;
  FVisible := Value;
end;

procedure TCustomGxGraphItem.SetPArentControl(const Value: TCustomGxGraphBoard);
begin
  FParentControl := Value;
  PositionX := 0; //trig internal change.
end;

procedure TCustomGxGraphItem.SetPointX(const Value: Single);
begin
  FX := Value;
end;

procedure TCustomGxGraphItem.SetPointY(const Value: Single);
begin
  FY := Value;
end;

procedure TCustomGxGraphItem.SetPosition(const Value: TPt);
begin
  PositionX := Value.X;
  PositionY := Value.Y;
end;

procedure TCustomGxGraphItem.StandartDrawingSetup(aRenderer : TCustomGxRenderer);
begin
  aRenderer.SetStrokeSize(FStrokeSize);
  aRenderer.SetStrokeType(SolidColor);
  aRenderer.SetStrokeColor($10,$72,$C5,FAlpha); //$1072C5;
  aRenderer.SetFillType(none);
end;

{ TGxGraphGeometrie }

constructor TGxGraphGeometrie.Create;
begin
  Inherited Create;
  FVectorUtil := TDirectionalObject.Create(0,0,10);
end;

destructor TGxGraphGeometrie.Destroy;
begin
  FreeAndNil(FVectorUtil);
  inherited;
end;

procedure TGxGraphGeometrie.Transforme(var aP: TPt);
begin
  aP.X := aP.X + ParentControl.GlobalOffsetX;
  aP.Y := aP.Y + ParentControl.GlobalOffsetY;

  FVectorUtil.Origin := ParentControl.GlobalTransformationCenter;
  FVectorUtil.SetPointedCoord(aP);

  FVectorUtil.AngleInDegree := FVectorUtil.AngleInDegree + ParentControl.GlobalAngle;
  FVectorUtil.Norm := FVectorUtil.Norm * ParentControl.GlobalScale;

  aP := FVectorUtil.GetPointedCoord;
end;

procedure TGxGraphGeometrie.TransformeInverse(var aP: TPt);
begin
  FVectorUtil.Origin := ParentControl.GlobalTransformationCenter;
  FVectorUtil.SetPointedCoord(aP);

  FVectorUtil.AngleInDegree := FVectorUtil.AngleInDegree - ParentControl.GlobalAngle;
  FVectorUtil.Norm := FVectorUtil.Norm / ParentControl.GlobalScale;

  aP := FVectorUtil.GetPointedCoord;

  aP.X := aP.X - ParentControl.GlobalOffsetX;
  aP.Y := aP.Y - ParentControl.GlobalOffsetY;
end;

{ TCustomGxGraphBoard }

constructor TCustomGxGraphBoard.Create;
begin
  inherited Create;
  FObjects := TObjectList<TCustomGxGraphItem>.Create; //Owned.
  FQueryResp := TList<TCustomGxGraphItem>.Create;     //Tempo. result a a query.
  FScale := 1.0;
  FAngle := 0.0;
  FGlobalTransformationCenter := Point(0,0);
  FvUtil := TDirectionalObject.Create(0,0,10);
  Geometry := TGxGraphGeometrie.Create;
  Geometry.ParentControl := Self;
end;

function TCustomGxGraphBoard.getObjectcount: Uint32;
begin
  result := FObjects.Count;
end;

function TCustomGxGraphBoard.GetObjects(Index: UINt32): TCustomGxGraphItem;
begin
  Result := FObjects[Index];
end;

{ TGxSelectionPoint }


procedure TGxSelectionPoint.Draw(aRenderer : TCustomGxRenderer);
begin
  Inherited;
  aRenderer.Ellipse(BoundRect);
end;

function TGxSelectionPoint.GetGripSize: Single;
begin
  Result := FSize;
end;

procedure TGxSelectionPoint.SetGripSize(const Value: Single);
begin
  FSize := Value;
end;


{ TGxSingleSidedShape }

constructor TGxSingleSidedShape.Create;
begin
  inherited Create;
  FSize := 10;
end;

function TGxSingleSidedShape.GetBoundRect: TRct;
var fh : Single;
begin
  fh := FSize/2;
  Result := Rect(FX-fh,FY-fh,FX+fh,FY+fh);
end;

procedure TGxSingleSidedShape.SetBoundHeight(const Value: Single);
begin
  FSize := Value;
end;

procedure TGxSingleSidedShape.SetBoundWidth(const Value: Single);
begin
  FSize := Value;
end;

{ TGxGraphShapeItem }

procedure TGxGraphShapeItem.DoChange;
begin
  InternalChange;
end;



{ TGxBoundedShape }

constructor TGxBoundedShape.Create;
begin
  inherited;
  FWidth := 10;
  FHeight := FWidth;
end;


function TGxBoundedShape.GetBoundHeight: Single;
begin
  Result := FHeight;
end;

function TGxBoundedShape.GetBoundRect: TRct;
var fw,fh : Single;
begin
  fw := FWidth/2;
  fh := FHeight/2;
  Result := Rect(FX-fw,FY-fh,FX+fw,FY+fh);
end;

function TGxBoundedShape.GetBoundWidth: Single;
begin
  Result := FWidth;
end;

procedure TGxBoundedShape.SetBoundHeight(const Value: Single);
begin
  FHeight := Value;
end;

procedure TGxBoundedShape.SetBoundWidth(const Value: Single);
begin
  FWidth := Value;
end;


{ TGxSelectionRectangle }

procedure TGxSelectionRectangle.Draw(aRenderer: TCustomGxRenderer);
begin
  inherited;
  aRenderer.Rectangle(BoundRect);
end;

{ TGxSelectionGrid }

constructor TGxSelectionGrid.Create;
begin
  inherited;
  FStep := 50;
  FWidth := 500;
  FHeight := 500;
  FDrawBorder := true;
  FDrawWithParentOffset := false;
  FOffSet := Point(0,0);
end;

procedure TGxSelectionGrid.Draw(aRenderer: TCustomGxRenderer);
var ls : Single;
    ll : Single;
    b1,b2 : Single;
    RealBoundBoxA, RealBoundBoxB : Single;

    Procedure Draw(Const direction : boolean = True);
    begin
      if FDrawWithParentOffset then
        OffSet := Point(Trunc(ParentControl.GlobalOffsetX) mod FStep,Trunc(ParentControl.GlobalOffsetY) mod FStep);

      if direction then
      begin
        ls := ls + OffSet.X;
        ll := ll + OffSet.X;
        while ls<ll do
        begin
          if (ls>RealBoundBoxA) and (ls<=RealBoundBoxB) then
            aRenderer.Line(Point(ls,b1),Point(ls,b2));
          ls := ls + FStep;
        end;
      end
      else
      begin
        ls := ls + OffSet.Y;
        ll := ll + OffSet.Y;
        while ls<ll do
        begin
          if (ls>RealBoundBoxA) and (ls<=RealBoundBoxB) then
            aRenderer.Line(Point(b1,ls),Point(b2,ls));
          ls := ls + FStep;
        end;
      end;
    end;
begin
   inherited;
   ls := BoundRect.Left-FStep;
   ll := BoundRect.Right+FStep;
   RealBoundBoxA := BoundRect.Left;
   RealBoundBoxB := BoundRect.Right;
   b1 := BoundRect.Top;
   b2 := BoundRect.Bottom;
   Draw;

   ls := BoundRect.Top-FStep;
   ll := BoundRect.Bottom+FStep;
   RealBoundBoxA := BoundRect.Top;
   RealBoundBoxB := BoundRect.Bottom;
   b1 := BoundRect.Left;
   b2 := BoundRect.Right;
   Draw(false);

   if FDrawBorder then
   begin
     aRenderer.Rectangle(BoundRect);
   end;
end;

procedure TGxSelectionGrid.SetStep(const Value: Integer);
begin
  FStep := Value;
  if  FStep<1 then
    FStep := 1;
end;

{ TGxStaticCommon }


procedure TGxStaticCommon.Draw(aRenderer: TCustomGxRenderer);
begin
  inherited;
  case FMouseState of
    imsScan: ;
    imsInputDeviceFlyOverMe:
    begin
      aRenderer.SetFillType(SolidColor);
      aRenderer.SetFillColor($FF, $C0, $CB);  //$FFC0CB Pink
    end;
    imsInputDevicePushDownOnMe, imsInputDeviceSelectMe:
    begin
      aRenderer.SetFillType(SolidColor);
      aRenderer.SetFillColor($FF, $00, $00);  //$Red
    end;
  end;
end;

end.
