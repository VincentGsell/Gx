//Gx : Wedding between FMeX and Gx (old one)
//
unit Gx.Types;

interface

uses
  GS.Direction;

Type
TeRect = record
  top,left,right,bottom : single;
end;

TeBox = record
  left,top,near : single;
  right,bottom,far : single;
end;

TeRawMesh = record
  meshes : Array of TPt;
  Indexes : Array of Integer;

//  procedure SetCapacity(aMeshCount : UInt32);
  procedure BuildLine(const x,y,x2,y2 : single; const Resolution : Uint32 = 1; const Thikness : Single = 2.0);

  //Generate to triangle, in a square shape, upper of the x,y,x2,y2 line.
  procedure GenerateSquareMesh(const xy,x2y2 : TPt; const Thikness : Single = 2.0; const Up : Boolean = true);
end;

implementation

{ TeRawMesh }

procedure TeRawMesh.BuildLine(const x, y, x2, y2: single; const Resolution : UInt32;
  const Thikness: Single);
var l : TDirectionalObject;
begin
  l := TDirectionalObject.Create(x,y,Thikness);
  try
    //To do : Resolution ?
    l.PointAt(Point(x2,y2));
    GenerateSquareMesh(l.Origin,l.GetPointedCoord,Thikness);
    GenerateSquareMesh(l.Origin,l.GetPointedCoord,Thikness,false);
  finally
    l.Free;
  end;
end;

procedure TeRawMesh.GenerateSquareMesh(const xy, x2y2: TPt;
  const Thikness: Single;const Up : Boolean);
var l : TDirectionalObject;
    a,b,c,d : TPt;
    m : Single;
    idx : Uint32;
begin
  l := TDirectionalObject.Create(xy.x,xy.y,Thikness/2);
  try
    l.LookAt(x2y2);
    m := l.Angle;

    a := l.Origin;
    b := x2y2;

    if Up then
      l.TurnLeft
    else
      l.TurnRight;

    l.MoveAhead;
    c := l.Origin;

    l.SetOrigin(x2y2.X,x2y2.Y,0);
    l.Norm := Thikness/2;
    l.LookAt(xy);

    if Up then
      l.TurnRight
    else
      l.TurnLeft;

    l.MoveAhead;
    d := l.Origin;

    idx := Length(meshes);
    SetLength(meshes,Length(meshes)+4);
    SetLength(Indexes,Length(Indexes)+6);
    meshes[idx] := a;
    meshes[idx+1] := b;
    meshes[idx+2] := c;
    meshes[idx+3] := d;

    Indexes[idx] := idx;
    Indexes[idx+1] := idx+1;
    Indexes[idx+2] := idx+2;
    Indexes[idx+3] := idx+2;
    Indexes[idx+4] := idx+3;
    Indexes[idx+5] := idx+1;

  finally
    l.Free;
  end;

end;

end.
