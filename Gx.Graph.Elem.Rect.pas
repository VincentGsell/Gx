unit Gx.Graph.Elem.Rect;

interface

Uses
  System.Types,
  System.Classes,
  System.UITypes,
  System.SysUtils,
  System.Generics.Collections,
  System.Math,
  System.Math.Vectors,
  Gx.Types,
  Gx.Graph.Types;

type
  teLine = class(TeGraphElement2d)
    constructor Create(x,y,x2,y2 : single; const thikness : single = 3.0); reintroduce;
  end;

  TeFilledRect = class(TeGraphElement2d)
  end;

  TeFrameRect = class(TeGraphElement2d)
  end;


implementation

{ teLine }

constructor teLine.Create(x, y, x2, y2: single; const thikness: single);
begin
  inherited create;
  FData.BuildLine(x,y,x2,y2,1,thikness);
end;

end.
