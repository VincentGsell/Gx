unit Gx.Graph.Mesh.Geometry;

Interface

uses gx.Graph.core, System.SysUtils, GS.Geometry.Direction;

Type

TGxMeshTriangle = Record
  P1,P2,P3 : TPt;
End;
TGxMeshTriangleArray = Array of TGxMeshTriangle;

TGxMeshEdgeCode = (fecDraw, fecIgnore);
TGxMeshEdge = Record
  P1 : TPt;
  Code : TGxMeshEdgeCode;
End;
TGxMeshEdgeArray = Array of TGxMeshEdge;


TGxMeshGeometry2DData = Class
Private
  Procedure ClearArray;
Public
  Mesh : TGxMeshTriangleArray;
  Border : TGxMeshEdgeArray;

  Procedure Clear;
  Procedure SetCapacity(aCapa : Integer);

  Procedure Scale(xScale, yScale : Single);
  Procedure Translate(xAmount, yAmount : Single);

//  Function ToClipperPaths : TPaths;
//  Procedure FromClipperPaths(aClipperPaths : TPaths); //Use Delaunay for Mesh rebuild.
End;

TGxMeshGeometryGenerator = Class
Public
End;

TGxMesh2DGeometryGenerator = Class(TGxMeshGeometryGenerator)
Private
Protected
  LocalGeometryTool : TDirectionalObject;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;
  Procedure Generate(var aData : TGxMeshGeometry2DData); Virtual; Abstract;
End;

//--------------------------------------------------- Disk.
TGxMeshDiskGenerator = Class(TGxMesh2DGeometryGenerator)
Private
  FRadius: Single;
  FSubdivision: Integer;
Public
  Procedure Generate(var aData : TGxMeshGeometry2DData); Override;
  Constructor Create; Override;

Published
  Property Radius : Single read FRadius WRite FRadius;
  Property Subdivision : Integer Read FSubdivision Write FSubdivision;
End;

//-------------------------------------------------- Donut (from Disk).
TGxMeshDiskDonutGenerator = Class(TGxMeshDiskGenerator)
Private
  FInnerRadius: Single;
  FSecondaryGeometryTool : TDirectionalObject;

  procedure SetInnerRadius(const Value: Single);
Public
  Procedure Generate(var aData : TGxMeshGeometry2DData); Override;
  Constructor Create; Virtual;
  Destructor Destroy; Override;
Published
  Property InnerRadius : Single read FInnerRadius WRite SetInnerRadius;
End;

//------------------------------------------------- Sub Operation.
TGxMeshSubOpp = Class(TGxMesh2DGeometryGenerator)
private
  FSubject: TGxMeshGeometry2DData;
  FSubOp: TGxMeshGeometry2DData;
Public
  Procedure Generate(var aData : TGxMeshGeometry2DData); Override;

  Property Subject : TGxMeshGeometry2DData Read FSubject Write FSubject;
  Property SubOp : TGxMeshGeometry2DData read FSubOp Write FSubOp;
End;

implementation

Uses Clipper, ClipperCore;

{ TGxMeshDiskGenerator }

constructor TGxMeshDiskGenerator.Create;
begin
  inherited;
  Subdivision := 30;
  Radius := 1;
end;

procedure TGxMeshDiskGenerator.Generate(var aData : TGxMeshGeometry2DData);
var i : integer;
    FStepAngle : Single;
begin
  aData.SetCapacity(FSubdivision);

  LocalGeometryTool.SetOrigin(0,0,0);
  LocalGeometryTool.Norm := FRadius;
  LocalGeometryTool.Angle := 0;

  FStepAngle := 360/FSubdivision;

  for i := 0 to FSubdivision-1 do
  begin
    aData.Mesh[i].P1 := Point(0,0);
//    aData.Mesh[i].P1_XY_Normal_Angle := 0;
    aData.Mesh[i].P2 := Point(LocalGeometryTool.GetPointedCoord.X,LocalGeometryTool.GetPointedCoord.Y);
//    aData.Mesh[i].P2_XY_Normal_Angle := LocalGeometryTool.AngleInDegree;
    LocalGeometryTool.TurnBy(FStepAngle);
    aData.Mesh[i].P3 := Point(LocalGeometryTool.GetPointedCoord.X,LocalGeometryTool.GetPointedCoord.Y);
//    aData.Mesh[i].P3_XY_Normal_Angle := LocalGeometryTool.AngleInDegree;

    aData.Border[i].P1 := aData.Mesh[i].P2;
  end;
end;

{ TGxMeshDiskData }

procedure TGxMeshGeometry2DData.Clear;
begin
  ClearArray;
end;


procedure TGxMeshGeometry2DData.ClearArray;
var i : Integer;
begin
  for i := Low(Mesh) to High(Mesh) do
  begin
    Mesh[i].P1:= Point(0,0);
    Mesh[i].P2:= Point(0,0);
    Mesh[i].P3:= Point(0,0);
  end;
  for i := Low(Border) to High(Border) do
  begin
    Border[i].P1:= Point(0,0);
    Border[i].Code := fecDraw;
  end;
end;

procedure TGxMeshGeometry2DData.Scale(xScale, yScale: Single);
var i : Integer;
begin
  for i := Low(Mesh) to High(Mesh) do
  begin
    Mesh[i].P1:= Point(Mesh[i].P1.X * xScale, Mesh[i].P1.Y * yScale);
    Mesh[i].P2:= Point(Mesh[i].P2.X * xScale, Mesh[i].P2.Y * yScale);
    Mesh[i].P3:= Point(Mesh[i].P3.X * xScale, Mesh[i].P3.Y * yScale);
  end;
  for i := Low(Border) to High(Border) do
  begin
    Border[i].P1:= Point(Border[i].P1.X * xScale, Border[i].P1.Y * yScale);
    //Border[i].Code := fecDraw;
  end;
end;

procedure TGxMeshGeometry2DData.SetCapacity(aCapa: Integer);
begin
  SetLength(Mesh,aCapa);
  SetLength(Border,aCapa);
  ClearArray;
end;

procedure TGxMeshGeometry2DData.Translate(xAmount, yAmount: Single);
var i : Integer;
begin
  for i := Low(Mesh) to High(Mesh) do
  begin
    Mesh[i].P1:= Point(Mesh[i].P1.X + xAmount, Mesh[i].P1.Y + yAmount);
    Mesh[i].P2:= Point(Mesh[i].P2.X + xAmount, Mesh[i].P2.Y + yAmount);
    Mesh[i].P3:= Point(Mesh[i].P3.X + xAmount, Mesh[i].P3.Y + yAmount);
  end;
  for i := Low(Border) to High(Border) do
  begin
    Border[i].P1:= Point(Border[i].P1.X + xAmount, Border[i].P1.Y + yAmount);
    //Border[i].Code := fecDraw;
  end;
end;

{ TGxMeshDiskDonutGenerator }

constructor TGxMeshDiskDonutGenerator.Create;
begin
  Inherited;
  FSecondaryGeometryTool := TDirectionalObject.Create(0,0,1);
  InnerRadius := 0.5;
end;

destructor TGxMeshDiskDonutGenerator.Destroy;
begin
  FreeAndNil(FSecondaryGeometryTool);
  inherited;
end;

procedure TGxMeshDiskDonutGenerator.Generate(var aData: TGxMeshGeometry2DData);
var i,j : integer;
    FStepAngle : Single;
begin
  aData.SetCapacity(FSubdivision*2); //2 Circles. = twice triangle quantity, and twice border long.

  LocalGeometryTool.SetOrigin(0,0,0);
  LocalGeometryTool.Norm := FRadius;
  LocalGeometryTool.Angle := 0;

  FSecondaryGeometryTool.SetOrigin(0,0,0);
  FSecondaryGeometryTool.Norm := FInnerRadius;
  FSecondaryGeometryTool.Angle := 0;


  FStepAngle := 360/FSubdivision;

  //Outer circle.
  j := 0;
  for i := 0 to FSubdivision-1 do
  begin
    aData.Border[j].Code := fecDraw;
    aData.Border[j].P1 := Point(LocalGeometryTool.GetPointedCoord.X,LocalGeometryTool.GetPointedCoord.Y);
    LocalGeometryTool.TurnBy(FStepAngle);
    inc(j);
  end;

  //Jump !
  aData.Border[j].P1 := Point(FSecondaryGeometryTool.GetPointedCoord.X,FSecondaryGeometryTool.GetPointedCoord.Y);
  aData.Border[j].Code := fecIgnore;
  FSecondaryGeometryTool.TurnBy(FStepAngle);
  inc(j);

  //Inner circle.
  for i := 0 to FSubdivision-1 do
  begin
    aData.Border[j].Code := fecDraw;
    aData.Border[j].P1 := Point(FSecondaryGeometryTool.GetPointedCoord.X,FSecondaryGeometryTool.GetPointedCoord.Y);
    FSecondaryGeometryTool.TurnBy(FStepAngle);
    inc(j);
  end;
end;

procedure TGxMeshDiskDonutGenerator.SetInnerRadius(const Value: Single);
begin
  FInnerRadius := Value;
  if FInnerRadius>(Radius - 0.001*Radius) then
    FInnerRadius := Radius - 0.001*Radius;
end;



{ TGxMesh2DGeometryGenerator }

constructor TGxMesh2DGeometryGenerator.Create;
begin
  Inherited;
  LocalGeometryTool := TDirectionalObject.Create(0,0,1);
end;

destructor TGxMesh2DGeometryGenerator.Destroy;
begin
  FreeAndNil(LocalGeometryTool);
  inherited;
end;

{ TGxMeshSubOpp }

procedure TGxMeshSubOpp.Generate(var aData: TGxMeshGeometry2DData);
var c : TClipper;
    a,b,r : TPaths;

    i : integer;
begin
  Assert(Assigned(Subject));
  Assert(Assigned(SubOp));

  //Todo : Paths with other dim (Jump sequence in border)
  SetLength(a, 1);
  SetLength(a[0], Length(Subject.Border));
  SetLength(b, 1);
  SetLength(b[0], Length(SubOp.Border));

  for I := Low(Subject.Border) to High(Subject.Border) do
  begin
    a[0][i].X := Round(Subject.Border[i].P1.X*1000);
    a[0][i].Y := Round(Subject.Border[i].P1.Y*1000);
  end;

  for I := Low(SubOp.Border) to High(SubOp.Border) do
  begin
    b[0][i].X := Round(SubOp.Border[i].P1.X*1000);
    b[0][i].Y := Round(SubOp.Border[i].P1.Y*1000);
  end;

  //Use Clipper lib to generate aData starting from Subject and SubOp.
  c := TClipper.Create;
  c.AddPaths(a,ptSubject,true);
  c.AddPaths(b,ptClip,true);
  c.Execute(ctDifference,TFillRule.frNonZero,r);

  aData.SetCapacity(LEngth(r[0]));
  for I := Low(r[0]) to High(r[0]) do
  begin
    aData.Border[i].P1.X := r[0][i].X/1000;
    aData.Border[i].P1.Y := r[0][i].Y/1000;
  end;

  c.Free;
end;

end.
