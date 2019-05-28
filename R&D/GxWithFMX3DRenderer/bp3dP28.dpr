program bp3dP28;

uses
  System.StartUpCopy,
  FMX.Forms,
  Gx.Graph.Controls in '..\..\Gx.Graph.Controls.pas',
  Gx.Graph.Core in '..\..\Gx.Graph.Core.pas',
  Gx.Graph.Mesh.Controls in '..\..\Gx.Graph.Mesh.Controls.pas',
  Gx.Graph.Mesh.Geometry in '..\..\Gx.Graph.Mesh.Geometry.pas',
  Gx.GraphBoard.FMX in '..\..\Gx.GraphBoard.FMX.pas',
  Gx.GraphBoard in '..\..\Gx.GraphBoard.pas',
  clipper in '..\..\ThirdPart\clipper.pas',
  GS.Direction in '..\..\..\GS\GS.Core\GS.Direction.pas',
  Gx.GraphBoard.FMX3D in '..\..\Gx.GraphBoard.FMX3D.pas',
  FMeX.Types3D in '..\..\..\FMeX\Sources\FMeX.Types3D.pas',
  bp3d.fmain in 'bp3d.fmain.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm24, Form24);
  Application.Run;
end.
