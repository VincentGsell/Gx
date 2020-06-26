program BasicgxGraphDemoFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmain in 'fmain.pas' {Form7},
  Gx.Graph.Core in '..\..\..\..\..\Gx.Graph.Core.pas',
  Gx.GraphBoard in '..\..\..\..\..\Gx.GraphBoard.pas',
  Gx.Graph.Controls in '..\..\..\..\..\Gx.Graph.Controls.pas',
  Gx.Graph.Mesh.Geometry in '..\..\..\..\..\Gx.Graph.Mesh.Geometry.pas',
  Gx.Graph.Mesh.Controls in '..\..\..\..\..\Gx.Graph.Mesh.Controls.pas',
  Gx.GraphBoard.FMX in '..\..\..\..\..\Gx.GraphBoard.FMX.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
