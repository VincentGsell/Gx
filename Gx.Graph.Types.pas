unit Gx.Graph.Types;


interface

Uses
  System.Types,
  System.Classes,
  System.UITypes,
  System.SysUtils,
  System.Generics.Collections,
  System.Math,
  System.Math.Vectors,
  Gx.Types;

Type
TeGraphElement = class;
TeGraphElement2d = class;
TeGraphElement3d = class;
TeGraph2d = class;

TeGraphElement = class abstract
end;

TeGraphElement2d = class(TeGraphElement)
private
protected
  FLocalMatrix : TMatrix;
  FX: single;
  FY: single;
  FWidth: single;
  FHeight: single;
  FAngle: single;
  FData : TeRawMesh;

  procedure InternalMatrixProcess;
  function GetBoundingRect: TeRect;
  function GetPosition: TPointF;
  procedure SetHeight(const Value: single);
  procedure SetPosition(const Value: TPointF);
  procedure SetWidth(const Value: single);
  procedure SetX(const Value: single);
  procedure SetY(const Value: single);
  procedure SetAngle(const Value: single);
Public
  constructor create; virtual;

  property Position : TPointF read GetPosition Write SetPosition;
  property X : single read FX Write SetX;
  property Y : single read FY Write SetY;
  property angle : single read FAngle Write SetAngle;

  property BoundingBox : TeRect read GetBoundingRect;
  property Width : single read FWidth Write SetWidth;
  property Height : single read FHeight Write SetHeight;

  property localMatrix : TMatrix read FLocalMatrix;
end;

TeGraphElement3D = class(TeGraphElement)
  private
protected
  FLocalMatrix : TMatrix3D;
  FQr : TQuaternion3D;
  FZ: single;
  FX: single;
  FY: single;
  FWidth: single;
  FDepth: single;
  FHeight: single;

  FXR, FXRS: single;
  FYR, FYRS: single;
  FZR, FZRS: single;

  procedure InternalMatrixProcess;

  procedure SetX(const Value: single);
  procedure SetY(const Value: single);
  procedure SetZ(const Value: single);

  procedure SetXR(const Value: single);
  procedure SetYR(const Value: single);
  procedure SetZR(const Value: single);

  procedure SetDepth(const Value: single);
  procedure SetHeight(const Value: single);
  procedure SetWidth(const Value: single);

  function GetPosition: TPoint3D;
  procedure SetPosition(const Value: TPoint3D);

  function GetDepth: single; virtual; abstract;
  function GetHeight: single; virtual; abstract;
  function GetWidth: single; virtual; abstract;

  function GetBoundingBox: TeBox; virtual;
public
  constructor create; virtual;

  property Position : TPoint3D read GetPosition Write SetPosition;
  property X : single read FX Write SetX;
  property Y : single read FY Write SetY;
  property Z : single read FZ Write SetZ;

  Property XRotate : single read FXR Write SetXR;
  Property YRotate : single read FYR Write SetYR;
  Property ZRotate : single read FZR Write SetZR;


  property Box : TeBox read GetBoundingBox;
  property Width : single read FWidth Write SetWidth;
  property Height : single read FHeight Write SetHeight;
  property Depth : single read FDepth Write SetDepth;

  property localMatrix : TMatrix3D read FLocalMatrix;
end;

TeGraph2d = class
private
protected
  Elems : TObjectList<TeGraphElement2d>;
public
  constructor Create; Virtual;
  destructor Destroy; Override;
  function Add(anElement : TeGraphElement2d; const AllowDuplicate : Boolean = true) : TeGraphElement2d; Virtual;
end;


implementation


{ TeGraphElement3D }


constructor TeGraphElement3D.create;
begin
  inherited;
  FLocalMatrix := TMatrix3D.Identity;
  FQr := TQuaternion3D.Identity;;
  Position := NullPoint3D;
  FWidth := 1;
  FHeight := 1;
  FDepth := 1;

  InternalMatrixProcess;
end;

function TeGraphElement3D.GetBoundingBox: TeBox;
begin
  result.left := FX - FWidth/2;
  result.top := FY - FHeight/2;
  result.near := FZ - FDepth/2;
  result.right := FX + FWidth/2;
  result.bottom := FY + FHeight/2;
  result.far := FZ + FDepth/2;
end;

function TeGraphElement3D.GetPosition: TPoint3D;
begin
  result := Point3D(FX,FY,FZ);
end;


procedure TeGraphElement3D.InternalMatrixProcess;
procedure RotationProcess;
var
  a: Single;
//  l: TPoint3D;
begin
  if not(SameValue(FXR, 0, TEpsilon.Vector) and SameValue(FXR, 0, TEpsilon.Vector) and
    SameValue(FZR, 0, TEpsilon.Vector)) then
  begin
//    FQr := TQuaternion3D.Identity;
    //X rotate.
    a := DegNormalize(FXR - FXRS);
    if a <> 0 then
    begin
      FQr := FQr * TQuaternion3D.Create(Point3D(1, 0, 0), DegToRad(a));
      FXR := DegNormalize(FXR);
      FXRS := FXR;
    end;

    //Y rotate.
    a := DegNormalize(FYR - FYRS);
    if a <> 0 then
    begin
      FQr := FQr * TQuaternion3D.Create(Point3D(0, 1, 0), DegToRad(a));
      FYR := DegNormalize(FYR);
      FYRS := FYR;
    end;


    //Z rotate.
    a := DegNormalize(FZR - FZRS);
    if a <> 0 then
    begin
      FQr := FQr * TQuaternion3D.Create(Point3D(0, 0, 1), DegToRad(a));
      FZR := DegNormalize(FZR);
      FZRS := FZR;
    end;


    FLocalMatrix := FLocalMatrix * FQr;
  end


//  a := DegNormalize(RotationAngle.Y - FSavedRotationAngle.Y);
//  if a <> 0 then
//  begin
//    FQuaternion := FQuaternion * TQuaternion3D.Create(Point3D(0, 1, 0) { AbsoluteDirection }, DegToRad(a));
//    NeedChange := True;
//    NewValue.Y := DegNormalize(RotationAngle.Y);
//  end;

//  a := DegNormalize(RotationAngle.Z - FSavedRotationAngle.Z);
//  if a <> 0 then
//  begin
//    FQuaternion := FQuaternion * TQuaternion3D.Create(Point3D(0, 0, 1) { AbsoluteUp }, DegToRad(a));
//    NeedChange := True;
//    NewValue.Z := DegNormalize(RotationAngle.Z);
//  end;
//  if NeedChange then
//  begin
//    FSavedRotationAngle := RotationAngle.Point;
//    RotationAngle.SetPoint3DNoChange(NewValue);
//    MatrixChanged(Sender);
//  end;
end;

begin
  FLocalMatrix := TMatrix3D.Identity;
  RotationProcess;
  //Translation.
  FLocalMatrix := FLocalMatrix * TMatrix3D.CreateTranslation(Position); { TODO : create a translation matrix "FTranslationMatrix }
//  InternalRecalcAbsolute;
end;


procedure TeGraphElement3D.SetDepth(const Value: single);
begin
  FDepth := Value;
end;

procedure TeGraphElement3D.SetHeight(const Value: single);
begin
  FHeight := Value;
end;

procedure TeGraphElement3D.SetPosition(const Value: TPoint3D);
begin
  if not( SameValue(FX, Value.X, TEpsilon.Position) and
          SameValue(FY, Value.Y, TEpsilon.Position) and
          SameValue(FZ, Value.Z, TEpsilon.Position) ) then
  begin
    FX := Value.X;
    FY := Value.Y;
    FZ := Value.Z;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement3D.SetWidth(const Value: single);
begin
  FWidth := Value;
end;

procedure TeGraphElement3D.SetX(const Value: single);
begin
  if not(SameValue(FX, Value, TEpsilon.Position)) Then
  begin
    FX := Value;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement3D.SetXR(const Value: single);
begin
  if not(SameValue(FXR, Value, TEpsilon.Angle)) Then
  begin
    FXR := Value;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement3D.SetY(const Value: single);
begin
  if not(SameValue(FY, Value, TEpsilon.Position)) Then
  begin
    FY := Value;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement3D.SetYR(const Value: single);
begin
  if not(SameValue(FYR, Value, TEpsilon.Angle)) Then
  begin
    FYR := Value;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement3D.SetZ(const Value: single);
begin
  if not(SameValue(FZ, Value, TEpsilon.Position)) Then
  begin
    FZ := Value;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement3D.SetZR(const Value: single);
begin
  if not(SameValue(FZR, Value, TEpsilon.Angle)) Then
  begin
    FZR := Value;
    InternalMatrixProcess;
  end;
end;

{ TeGraphElement2d }

constructor TeGraphElement2d.create;
begin
  inherited;
  FLocalMatrix := TMatrix.Identity;
  Position := Point(0,0);
  FWidth := 1;
  FHeight := 1;
  FAngle := 0;
  InternalMatrixProcess;
end;

function TeGraphElement2d.GetBoundingRect: TeRect;
begin
  result.left := FX - FWidth/2;
  result.top := FY - FHeight/2;
  result.right := FX + FWidth/2;
  result.bottom := FY + FHeight/2;
end;

function TeGraphElement2d.GetPosition: TPointF;
begin
  result := Pointf(FX,FY);
end;

procedure TeGraphElement2d.InternalMatrixProcess;
begin
  FLocalMatrix := TMatrix.Identity;
  FLocalMatrix := FLocalMatrix * TMatrix.CreateTranslation(FX,FY)*TMatrix.CreateRotation(FAngle);
end;

procedure TeGraphElement2d.SetAngle(const Value: single);
begin
  if not(SameValue(FAngle, Value, TEpsilon.Position)) Then
  begin
    FAngle := Value;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement2d.SetHeight(const Value: single);
begin
  FHeight := Value;
end;

procedure TeGraphElement2d.SetPosition(const Value: TPointF);
begin
  if not( SameValue(FX, Value.X, TEpsilon.Position) and
          SameValue(FY, Value.Y, TEpsilon.Position) ) then
  begin
    FX := Value.X;
    FY := Value.Y;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement2d.SetWidth(const Value: single);
begin
  FWidth := Value;
end;

procedure TeGraphElement2d.SetX(const Value: single);
begin
  if not(SameValue(FX, Value, TEpsilon.Position)) Then
  begin
    FX := Value;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement2d.SetY(const Value: single);
begin
  if not(SameValue(FY, Value, TEpsilon.Position)) Then
  begin
    FY := Value;
    InternalMatrixProcess;
  end;
end;

{ TeGraph2d }

function TeGraph2d.Add(anElement: TeGraphElement2d; const AllowDuplicate : Boolean = true) : TeGraphElement2d;
begin
  Assert(Assigned(anElement));
  result := anElement;
  If not AllowDuplicate then
  begin
    if Elems.IndexOf(anElement)>-1 then
      Exit;
  end;
  Elems.Add(anElement);
end;

constructor TeGraph2d.Create;
begin
  inherited;
  Elems := TObjectList<TeGraphElement2d>.Create;
end;

destructor TeGraph2d.Destroy;
begin
  FreeAndNil(Elems);
  inherited;
end;

end.
