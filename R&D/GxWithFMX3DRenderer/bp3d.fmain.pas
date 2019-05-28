unit bp3d.fmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls3D, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs,
  Gx.GraphBoard, Gx.Graph.Controls, Gx.Graph.Mesh.Controls,
  Gx.GraphBoard.FMX3D, System.Math.Vectors, FMX.Objects3D;

type
  TForm24 = class(TForm3D)
    Timer1: TTimer;
    Camera1: TCamera;
    procedure Form3DCreate(Sender: TObject);
    procedure Cube1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single;
      RayPos, RayDir: TVector3D);
    procedure Cube1MouseLeave(Sender: TObject);
    procedure Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Form3DKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    CurrentFPS : Double;
    { Private declarations }
  public
    g : TgxFMX3DGraphBoardControl;
    { Public declarations }
    procedure myIdling(Sender: TObject; var Done: Boolean);
  end;

var
  Form24: TForm24;

implementation

{$R *.fmx}

procedure TForm24.Cube1MouseLeave(Sender: TObject);
begin
  caption := '';
end;

procedure TForm24.Cube1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single; RayPos, RayDir: TVector3D);
  var I : TPoint3D;
begin
//  TControl3D(Sender).RayCastIntersect(RayPos,RayDir,I);
//  Caption := Sender.ClassName+' '+Format('%f %f %f %f Intersection : (%f,%f) [FPS:%f]',[X,Y, RayDir.X,RayDir.Y,I.X,I.Y,Context.FPS]);
end;

procedure TForm24.Form3DCreate(Sender: TObject);
var j,i : integer;
    qte : Uint32;
    lo : TgxCustomEditableItem;
begin
  g := TgxFMX3DGraphBoardControl.create(nil);
  g.HitTest := true;
  g.Parent := Form24;

  Application.OnIdle := myIdling;

  g.OnMouseMove := Cube1MouseMove;

  //g.RotationAngle.z := -25;

  AddObject(g);

  Qte := 50;
  TgxFMX3DRenderer(g.Board.Renderer).preallocate(Qte*Qte,rec); //
//  g.Board.AddRectangle(0,0,1,1);
//  g.Board.AddRectangle(-1,-1,1,1);
//  g.Board.AddRectangle(-0.2,0.2,3,2);
//  g.Board.AddRectangle(-0.4,0.2,1,4);


  for i := 1 to Qte do
  for j := 1 to Qte do
  begin
    lo := g.Board.AddRectangle(j-(Qte div 2),i-(Qte div 2),0.9,0.9);
  end;

//  g.Board.AddObject(TGxMeshCircle.Create());

  lo.EditMode := true;



  TgxFMX3DRenderer(g.Board.Renderer).DesignMode := true;
  g.Board.Render; //Build mesh.
  TgxFMX3DRenderer(g.Board.Renderer).DesignMode := false;




//  Camera1.RotationAngle.Y := 50;
end;

procedure TForm24.Form3DKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var speed : Single;
begin
  speed := 0.25;
  if ssCtrl in Shift then
    speed := speed * 10;

  if Key = vkLeft then
    Camera1.Position.X := Camera1.Position.X + speed;
  if Key = vkRight then
    Camera1.Position.X := Camera1.Position.X - speed;
  if Key = vkUp then
    Camera1.Position.Y := Camera1.Position.Y + speed;
  if Key = vkDown then
    Camera1.Position.Y := Camera1.Position.Y - speed;
end;

procedure TForm24.Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  Camera1.Position.Z := Camera1.Position.Z + WheelDelta / 25;
end;

procedure TForm24.myIdling(Sender: TObject; var Done: Boolean);
begin
  Invalidate;
  CurrentFPS := context.FPS;
  Caption := Format('[FPS:%f]',[Context.FPS]);
end;

end.
