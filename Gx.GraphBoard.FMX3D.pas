unit Gx.GraphBoard.FMX3D;

interface

Uses System.SysUtils,
     System.Types,
     System.UITypes,
     System.Classes,
     System.Variants,
     System.Generics.Collections,
     System.Math.Vectors,
     FMX.Types,
     FMX.Controls,
     FMX.Forms3D,
     FMX.Types3D,
     FMX.Forms,
     FMX.Graphics,
     FMX.Dialogs,
     Gx.GraphBoard,
     Gx.Graph.Core,
     GS.Direction,
     FMeX.Types3D,
     Gx.GraphBoard.FMX,
     Gx.Graph.Mesh.Geometry;


type
  //Renderer descendant : Specialized in FMX drawing in 3d.
  //Note : We target a low draw call rate. To do that, all
  // drawing primitive is built in mesh array.
  // -> Code logic is not the same than "Standart" renderer.
  TgxFM3DMeshPrimitive = (line, poly, rec, ellipse);
  TgxFMX3DMeshItem = Packed record
    MeshIndex : UInt32;
    PrimitiveType : TgxFM3DMeshPrimitive;
    MeshCount : UInt32;
  end;


  TgxFMX3DRenderer = class(TgxRenderer)
  private
    FContext :  Tcontext3D;
    FTargetCustomMesh: TeCustomMesh;
    FInternalMesh : TMeshData;
    FDesignMode: Boolean;
    procedure SetTargetCustomMesh(const Value: TeCustomMesh);
  protected
    procedure BuildNewMeshFromCoord(const coord : array of GS.Direction.TPt);

  public
    CurrentIndex : integer;
    constructor Create; virtual;
    destructor Destoy; virtual;

    Procedure SetStrokeSize(aSize : Single); Override;
    Procedure SetStrokeDashType(aDashType : TGxRenderDashType); override;
    Procedure SetStrokeType(aStrokeType : TGxRendererFillType); Override;
    Procedure SetStrokeColor(aRed, aGreen, aBlue : Byte; Const aAlpha : Byte = 255); Override;
    Procedure SetFillColor(aRed, aGreen, aBlue  : Byte; Const aAlpha : Byte = 255); override;
    procedure SetFillType(aFillType : TGxRendererFillType); override;

    Procedure Ellipse(aRect : GS.Direction.TRct); Override;
    Procedure Rectangle(aRect : GS.Direction.TRct); Override;
    procedure Polygone(aCoord : array of GS.Direction.TPt); Override;
    Procedure Line(a,b : GS.Direction.TPt); override;

    //Experimental
    procedure preallocate(PrimitiveCount : Uint32; PrimitiveType : TgxFM3DMeshPrimitive);

    Property Context : TContext3D read FContext Write FContext;

    Property TargetCustomMesh : TeCustomMesh read FTargetCustomMesh Write SetTargetCustomMesh;

    Property DesignMode : Boolean read FDesignMode Write FDesignMode;

  end;


  TgxFMX3DGraphBoardControl = class;
  TgxFMX3DGraphBoard  = class(TgxGraphBoard)
  protected
    Primitives : TList<TgxFMX3DMeshItem>;
  public
    Parent : TgxFMX3DGraphBoardControl;
    function GetDisplaySize : GS.Direction.TPt; override;

    procedure Render; Override;

    constructor Create; OVerride;

  end;


  TgxFMX3DGraphBoardControl = class(TeCustomMesh)
  private
  protected
    FInternalBoard : TgxFMX3DGraphBoard;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; override;

    procedure Render; Override;

    //Procedure MouseMove(Shift: TShiftState; X, Y: Single); Override;
    //Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); Override;
    //procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); Override;
    //procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); Override;
    //procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); Override;


    procedure MouseDown3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
    procedure MouseMove3D(Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
    procedure MouseUp3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
//    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); virtual;
//    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); virtual;
//    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); virtual;
//    procedure DialogKey(var Key: Word; Shift: TShiftState); virtual;
//    procedure AfterDialogKey(var Key: Word; Shift: TShiftState); virtual;
//    procedure Click; virtual;
//    procedure DblClick; virtual;
//    procedure DragEnter(const Data: TDragObject; const Point: TPointF); virtual;
//    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); virtual;
//    procedure DragDrop(const Data: TDragObject; const Point: TPointF); virtual;
//    procedure DragLeave; virtual;
//    procedure DragEnd; virtual;


    property Board : TgxFMX3DGraphBoard read FInternalBoard;
  end;

implementation

{ TgxFMXRenderer }


procedure TgxFMX3DRenderer.BuildNewMeshFromCoord(
  const coord: array of GS.Direction.TPt);
var i,ii : integer;
    p : PPoint3D;
begin
  FInternalMesh.Clear;
  FInternalMesh.VertexBuffer.Length := length(Coord);
  FInternalMesh.IndexBuffer.Length := length(Coord)*2;

  ii := 0;
  for i := 0 to Length(Coord)-1 do
  begin
    p := FInternalMesh.VertexBuffer.VerticesPtr[i];
    p.X := Coord[i].X;
    p.Y := Coord[i].Y;
    p.Z := Coord[i].Z;
    FInternalMesh.IndexBuffer.Indices[ii] := i;
    ii := ii+1;
    FInternalMesh.IndexBuffer.Indices[ii] := i+1;
    ii := ii+1;
  end;
  ii := ii - 1;
  FInternalMesh.IndexBuffer.Indices[ii] := 0;
end;

constructor TgxFMX3DRenderer.Create;
begin
  inherited;
  FInternalMesh := TMeshData.Create;
  FTargetCustomMesh := Nil;
  CurrentIndex := -1;
  DesignMode := False;
end;

destructor TgxFMX3DRenderer.Destoy;
begin
  FreeAndNil(FInternalMesh);
  inherited;
end;

procedure TgxFMX3DRenderer.Ellipse(aRect: GS.Direction.TRct);
var i,ii : integer;
    p : PPoint3D;
begin
  if Assigned(FTargetCustomMesh) then
  begin
    if Not(DesignMode) and (CurrentIndex>-1) then
    begin
      //Seek in indexedmesh and change data vertex.
      //ii := 0;
//      ii := CurrentIndex*4;
//      for i := CurrentIndex to (CurrentIndex + Length(aCoord)-1) do
//      begin
//        p := FTargetCustomMesh.Data.VertexBuffer.VerticesPtr[ii];
//        p.X := aCoord[i-CurrentIndex].X;
//        p.Y := aCoord[i-CurrentIndex].Y;
//        p.Z := aCoord[i-CurrentIndex].Z;
//        ii := ii + 1;
//      end;
    end
    else
    begin
      //Merge a new mesh.
//      TGxMeshDiskGenerator.
//      BuildNewMeshFromCoord(aCoord);
//      FTargetCustomMesh.MergeFrom(FInternalMesh,true);
    end;
  end
  else
  begin
    raise Exception.Create('Error Message');
  end;
end;



procedure TgxFMX3DRenderer.Line(a, b: GS.Direction.TPt);
begin

  //FCanvas.DrawLine(PointF(a.X,a.Y),PointF(b.X,b.Y),1.0);
end;

procedure TgxFMX3DRenderer.Polygone(aCoord: array of GS.Direction.TPt);
var i,ii : integer;
    p : PPoint3D;
begin
  //gxArrayOfPointToPolygon(aCoord,ld);
  if length(aCoord)=0 then
    exit;

  if Assigned(FTargetCustomMesh) then
  begin
    if Not(DesignMode) and (CurrentIndex>-1) then
    begin
      //Seek in indexedmesh and change data vertex.
      //ii := 0;
      ii := CurrentIndex*4;
      for i := CurrentIndex to (CurrentIndex + Length(aCoord)-1) do
      begin
        p := FTargetCustomMesh.Data.VertexBuffer.VerticesPtr[ii];
        p.X := aCoord[i-CurrentIndex].X;
        p.Y := aCoord[i-CurrentIndex].Y;
        p.Z := aCoord[i-CurrentIndex].Z;
        ii := ii + 1;
      end;
    end
    else
    begin
      //Merge a new mesh.
      BuildNewMeshFromCoord(aCoord);
      FTargetCustomMesh.MergeFrom(FInternalMesh,true);
    end;
  end
  else
  begin
    BuildNewMeshFromCoord(aCoord);
    FContext.DrawLines(FInternalMesh.VertexBuffer,FInternalMesh.IndexBuffer,FContext.DefaultMaterial,1.0);
  end;


{
  if FCanvas.Fill.Kind = TBrushKind.Solid then
  begin
    FCanvas.FillPolygon(ld,1.0);
  end
  else
  begin
    FCanvas.DrawPolygon(ld,1.0);
  end;
}
end;

procedure TgxFMX3DRenderer.preallocate(PrimitiveCount: Uint32;
  PrimitiveType: TgxFM3DMeshPrimitive);
var v : Uint32;
begin
  v := 0;
  case PrimitiveType of
    TgxFM3DMeshPrimitive.line: ;
    TgxFM3DMeshPrimitive.poly: ;
    TgxFM3DMeshPrimitive.rec: v := 4;
    TgxFM3DMeshPrimitive.ellipse: ;
  end;
  FTargetCustomMesh.Data.VertexBuffer.Length := 4 * PrimitiveCount;
  FTargetCustomMesh.Data.IndexBuffer.Length := 8 * PrimitiveCount
end;

procedure TgxFMX3DRenderer.Rectangle(aRect: GS.Direction.TRct);
begin
//  FContext.DrawRect(Point3D(aRect.Left,aRect.Top,0),Point3D(aRect.Right,aRect.Bottom,0),1.0,TAlphaColorRec.Blue);
{
  if FCanvas.Fill.Kind = TBrushKind.Solid then
  begin
    FCanvas.FillRect(Rectf(aRect.Left,aRect.Top,aRect.Right,aRect.Bottom),0,0,[],1.0);
  end
  else
  begin
    FCanvas.DrawRect(Rectf(aRect.Left,aRect.Top,aRect.Right,aRect.Bottom),0,0,[],1.0);
  end;
  }
end;

procedure TgxFMX3DRenderer.SetFillColor(aRed, aGreen, aBlue: Byte;
  const aAlpha: Byte);
begin
//  FCanvas.Fill.Color := TAlphaColorF.Create(aRed/255,aGreen/255,aBlue/255,aAlpha/255).ToAlphaColor;
end;

procedure TgxFMX3DRenderer.SetFillType(aFillType: TGxRendererFillType);
begin
{
  case aFillType of
    TGxRendererFillType.none:       FCanvas.Fill.Kind := TBrushKind.None;
    TGxRendererFillType.SolidColor: FCanvas.Fill.Kind := TBrushKind.Solid;
  end;
}
end;


procedure TgxFMX3DRenderer.SetStrokeColor(aRed, aGreen, aBlue: Byte;
  const aAlpha: Byte);
begin
{
  FCanvas.Stroke.Color :=TAlphaColorF.Create(aRed/255, aGreen/255, aBlue/255, aAlpha/255).ToAlphaColor;
}
end;

procedure TgxFMX3DRenderer.SetStrokeDashType(aDashType: TGxRenderDashType);
begin
{
  case aDashType of
    continous: FCanvas.Stroke.Dash := TStrokeDash.Solid;
    dot:       FCanvas.Stroke.Dash := TStrokeDash.Dot;
    dash:      FCanvas.Stroke.Dash := TStrokeDash.Dash;
    dashdot:   FCanvas.Stroke.Dash := TStrokeDash.DashDot;
  end;
}
end;

procedure TgxFMX3DRenderer.SetStrokeSize(aSize: Single);
begin
//  FCanvas.Stroke.Thickness :=  aSize;
end;

procedure TgxFMX3DRenderer.SetStrokeType(aStrokeType: TGxRendererFillType);
begin
{
  case aStrokeType of
  TGxRendererFillType.none :       FCanvas.Stroke.Kind := TBrushKind.None;
  TGxRendererFillType.SolidColor : FCanvas.Stroke.Kind := TBrushKind.Solid;
  end;
}
end;

procedure TgxFMX3DRenderer.SetTargetCustomMesh(const Value: TeCustomMesh);
begin
  FTargetCustomMesh := Value;
  FTargetCustomMesh.Data.Clear;
end;

constructor TgxFMX3DGraphBoardControl.Create(AOwner: TComponent);
begin
  inherited;
  Width := 6;
  Height := 4;
  Depth := 0.1;
  FInternalBoard := TgxFMX3DGraphBoard.Create;
  FInternalBoard.Parent := Self;
  FInternalBoard.Renderer := TgxFMX3DRenderer.Create;

  TgxFMX3DRenderer(FInternalBoard.Renderer).TargetCustomMesh := Self;

  DrawFrameType :=  TeCustomMeshDrawType.mdtWireFrame;
  DrawOverlapShape := true;
  //ClipChildren := true;
end;

destructor TgxFMX3DGraphBoardControl.Destroy;
begin
  FreeAndNil(FInternalBoard);
  inherited;
end;



function TgxFMX3DGraphBoardControl.DoRayCastIntersect(const RayPos,
  RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
var
  INear, IFar: TPoint3D;
begin
//  Result := RayCastCuboidIntersect(RayPos, RayDir, Point3D(0,0,0), Width, Height, Depth, INear, IFar) > 0;
  Result := RayCastPlaneIntersect(RayPos,RayDir,Point3D(0,0,0),Point3D(0,0,-1),Intersection);
//  Intersection := Inear;
//  if Result then
//    Intersection := LocalToAbsoluteVector(IFar);
end;

procedure TgxFMX3DGraphBoardControl.MouseDown3D(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
var la : TButtonsState;
    ls : TButtonsStateSet;
    I : TPoint3D;
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

  //FInternalBoard.MouseDownProcess(X,Y,la,ls);
   if RayCastIntersect(RayPos,RayDir,I) then
   begin
     I := TPoint3D(LocalToAbsoluteVector(I));
    FInternalBoard.MouseDownProcess(I.X,I.Y,la,ls);
   end;
end;



procedure TgxFMX3DGraphBoardControl.MouseMove3D(Shift: TShiftState; X, Y: Single;
  RayPos, RayDir: TVector3D);
var ls : TButtonsStateSet;
    i : TPoint3D;
begin
  inherited;
  ls := [];
  if ssRight in Shift then
    ls := ls + [RightButton];
  if ssLeft in Shift then
    ls := ls + [LeftButton];
  if ssMiddle in Shift then
    ls := ls + [MiddleButton];

  //FInternalBoard.MouseMoveProcess(X,Y,ls);
   if RayCastIntersect(RayPos,RayDir,I) then
   begin
     I := TPoint3D(LocalToAbsoluteVector(I));
     FInternalBoard.MouseMoveProcess(I.X,I.Y,ls);
   end;

//  Repaint;
end;
procedure TgxFMX3DGraphBoardControl.MouseUp3D(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
var la : TButtonsState;
    ls : TButtonsStateSet;
    I : TPoint3D;
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

  if RayCastIntersect(RayPos,RayDir,I) then
  begin
    I := TPoint3D(LocalToAbsoluteVector(I));
    FInternalBoard.MouseUpProcess(I.X,I.Y,la,ls);
  end;
end;

procedure TgxFMX3DGraphBoardControl.Render;
begin
//  Context.SetMatrix(AbsoluteMatrix);
//  Context.SetContextState(TContextState.csZWriteOn);
//  Context.DrawCube(NullPoint3D, TPoint3D.Create(Width + 0.01, Height + 0.01, Depth + 0.01), 0.4, $B2005ACC);

  TgxFMX3DRenderer(FInternalBoard.Renderer).Context := Context;

  FInternalBoard.Render;

  if Assigned(TgxFMX3DRenderer(FInternalBoard.Renderer).TargetCustomMesh) then
  begin
    Assert(self = TgxFMX3DRenderer(FInternalBoard.Renderer).TargetCustomMesh);
    Inherited Render;
  end;
end;



{ TgxFMXGraphBoard }


constructor TgxFMX3DGraphBoard.Create;
begin
  inherited;
  EnableGlobalPan := false; //by Camera ! :)
end;

function TgxFMX3DGraphBoard.GetDisplaySize: GS.Direction.TPt;
begin
  Result := GS.Direction.Point(Parent.Width, Parent.Height);
end;

procedure TgxFMX3DGraphBoard.Render;
var i : Integer;
    la : TCustomGxGraphItem;
begin
  for i := 0 to ObjectCount-1 do
  begin
    TgxFMX3DRenderer(FRenderer).CurrentIndex := i;
    la := Objects[i];
    if la.Visible  then
      la.Draw(FRenderer);
  end;
end;

end.
