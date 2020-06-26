unit fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Gx.GraphBoard.VCL, GS.Geometry.Direction,
  Gx.Graph.Core,
  Gx.Graph.Controls,      //Classic light control.
  Gx.Graph.Mesh.Controls; //Mesh oriented control

type
  TForm8 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    graph : TgxVCLGraphBoardControl;
    BackGroundGrid : TGxSelectionGrid;
  public
    { Public declarations }
    Procedure MyAppIdle(sender : TObject; Var Done : Boolean);
  end;

var
  Form8: TForm8;

implementation

{$R *.dfm}

procedure TForm8.FormCreate(Sender: TObject);
var i,j : integer;
    lo : TCustomGxGraphItem;
begin
  graph := TgxVCLGraphBoardControl.Create(nil);
  graph.Board.EnableGlobalScale := true;
  graph.Board.EnableGlobalRotate := true;
  graph.Parent := Self;
  graph.Align :=  TAlign.alClient;
  graph.DoubleBuffered := true;

  BackGroundGrid := TGxSelectionGrid.Create;
  graph.Board.AddObject(BackGroundGrid);
  BackGroundGrid.HitTest := false; //No mouse (aka inpiut interation)
  BackGroundGrid.DrawWithParentOffset := true; //Follow pan simulation.
  BackGroundGrid.Alpha := 100; //grid appearance
  BackGroundGrid.Alpha := 30;

// code in resize event handler  :
//  BackGroundGrid.PositionX := ClientWidth/2;
//  BackGroundGrid.PositionY := ClientHeight/2;
//  BackGroundGrid.Width := ClientWidth;
//  BackGroundGrid.Height := ClientHeight;

  for I := 0 to 100 do
  begin

    lo := TGxMeshCircle.Create;
    graph.Board.AddObject(lo);
    TGxMeshCircle(lo).EditMode := false;
    TGxMeshCircle(lo).CircleSubdivideResolution := 3 + Random(10); //20 by default.
    TGxMeshCircle(lo).Radius := 10+Random(50);
    lo.PositionX := 50*i;
    lo.PositionY := 600 + Random(25);

  end;


  for I := 0 to 10 do
  graph.Board.AddSelectionPoint(100+i*20,100);
  for I := 0 to 10 do
  graph.Board.AddSelectionRectangle(100+i*30,120,20,10 +Random(10));


  lo := graph.Board.AddCircle(275,250,35);
  TGxCircle(lo).EditMode := true;
  TGxCircle(lo).StrokeSize := 3;

  lo := graph.Board.AddCircle(370,300,35);
  TGxCircle(lo).EditMode := true;
  lo.HitTest := False;

  for I := 0 to 9 do
    for j := 0 to 9 do
      graph.Board.AddCircle(450+i*30,300+j*30,15).StrokeSize := 2;

  for I := 0 to 9 do
    for j := 0 to 9 do
    begin
      lo := graph.Board.AddSquare(20+i*20,300+j*20,10);
      TCustomGxGraphItem(lo).StrokeSize := 5;
      TCustomGxGraphItem(lo).Alpha := Random(205)+50;
    end;

  for I := 0 to 2 do
    for j := 0 to 2 do
    begin
      lo := graph.Board.AddRectangle(500+i*60,10+j*20,30,10);
      tgxRectangle(lo).EditMode := true;
    end;


  lo := TGxGrid.Create;
  graph.Board.AddObject(lo);
//  TGxGrid(lo).EditMode := true;
  TGxGrid(lo).Angle := 25;
  TGxGrid(lo).PositionX := 950;
  TGxGrid(lo).PositionY := 350;
  TGxGrid(lo).RadiusH := 150;
  TGxGrid(lo).Radius := 150;
  TGxGrid(lo).Step := 10;

  Application.OnIdle := MyAppIdle;


end;

procedure TForm8.MyAppIdle(sender: TObject; var Done: Boolean);
begin
//  TGxGrid(lo).Angle :=   TGxGrid(lo).Angle + 1;

  graph.Repaint;
  Done := False;
end;

end.
