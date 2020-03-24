unit Gx.Graph.Mesh.Controls;

interface

Uses System.SysUtils,
     Gx.Graph.Mesh.Geometry,
     gx.Graph.core,
     gx.Graph.Controls,
     GS.Geometry.Direction;

type
  TCustomGraphMeshShape = class(TgxCustomEditableItemCenterAndSizeRotate)
  protected
    DataModel : TGxMeshGeometry2DData;
    DataModelRadius : Single;

    FTool : TDirectionalObject;


    DrawCoord : array of TPt;
  public
    Procedure BuildModel; virtual;
    Procedure InternalChange; Override;

    constructor Create; override;
    destructor destroy; override;
  end;

  TGxMeshCircle = class(TCustomGraphMeshShape)
  private
    procedure SetResolution(const Value: Integer);
  protected
    FResolution: Integer;
    procedure BuildModel; override;
  public
    constructor Create; override;

    Procedure Draw(aRenderer : TCustomGxRenderer); Override;

    Property CircleSubdivideResolution : Integer read FResolution Write SetResolution;
  end;

implementation

{ TCustomGraphMeshShape }

procedure TCustomGraphMeshShape.BuildModel;
begin
  DataModelRadius := Radius;
  //In inherited, rebuild the model.
end;

constructor TCustomGraphMeshShape.Create;
begin
  inherited;
  FTool := TDirectionalObject.Create(0,0,10);
  DataModel := TGxMeshGeometry2DData.Create;
end;

destructor TCustomGraphMeshShape.destroy;
begin
  FreeAndNil(Ftool);
  FreeAndNil(DataModel);
  inherited;
end;

procedure TCustomGraphMeshShape.InternalChange;
var i : Integer;
    p : TPt;
begin
  inherited;

  if DataModelRadius<>Radius then
  begin
    BuildModel; //rebuild model to earn accuracy.
  end;

  { TODO : Prendre en compte DataModel.Border[i].Code. (livrer un tableau de polygon...) }

  SetLength(DrawCoord,Length(DataModel.Border));
  for I := 0 to Length(DrawCoord)-1 do
  begin
    FTool.SetOrigin(0,0,0);
    FTool.SetPointedCoord(DataModel.Border[i].P1);
    FTool.SetOrigin(FCenterForDraw.X,FCenterForDraw.Y,0);
//    FTool.Norm := Radius; Not needed since we rebuild model on radius change.
    FTool.TurnBy(Angle);
    FTool.Norm := ParentControl.GlobalScale * FTool.Norm;
    DrawCoord[i] := FTool.GetPointedCoord;
  end;
end;

{ TGxMeshCircle }

procedure TGxMeshCircle.BuildModel;
var ltool : TGxMeshDiskGenerator;
begin
  inherited;
  ltool := TGxMeshDiskGenerator.Create;
  try
    ltool.Radius := Radius;
    ltool.Subdivision := FResolution;
    ltool.Generate(DataModel);
  finally
    FreeAndNil(ltool);
  end;
end;

constructor TGxMeshCircle.Create;
begin
  inherited;
  FResolution := 20;
  BuildModel;
end;

procedure TGxMeshCircle.Draw(aRenderer: TCustomGxRenderer);
begin
  inherited;
  StandartDrawingSetup(aRenderer);
  aRenderer.Polygone(DrawCoord);
//  aRenderer.Rectangle(DrawBoundrect);
end;

procedure TGxMeshCircle.SetResolution(const Value: Integer);
begin
  FResolution := Value;
  if FResolution <3 then
    FResolution := 3;
  BuildModel;
  InternalChange;
end;

end.
