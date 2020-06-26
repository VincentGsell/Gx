program BasicgxGraphDemoVCL;

uses
  Vcl.Forms,
  fmain in 'fmain.pas' {Form8},
  Gx.Graph.Controls in '..\..\..\..\..\Gx.Graph.Controls.pas',
  Gx.Graph.Core in '..\..\..\..\..\Gx.Graph.Core.pas',
  Gx.Graph.Mesh.Controls in '..\..\..\..\..\Gx.Graph.Mesh.Controls.pas',
  Gx.Graph.Mesh.Geometry in '..\..\..\..\..\Gx.Graph.Mesh.Geometry.pas',
  Gx.GraphBoard in '..\..\..\..\..\Gx.GraphBoard.pas',
  Gx.GraphBoard.VCL in '..\..\..\..\..\Gx.GraphBoard.VCL.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
