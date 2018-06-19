unit gControls;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avMiniControls, avCanvas, mutils, gTypes;

type
  { TGameButton }

  TGameButton = class (TavmCustomButton)
  protected
    procedure DoValidate; override;
  end;

  { TGamePanel }

  TGamePanel = class (TavmCustomControl)
  protected
    procedure DoValidate; override;
  end;

implementation

{ TGameButton }

procedure TGameButton.DoValidate;
var
  txt: ITextLines;
begin
  Canvas.Clear;
  if Downed then
    Canvas.Brush.Color := Vec(0.8,1.0,0.6,1.0)
  else
  begin
    if not Moved then
      Canvas.Brush.Color := Vec(0.1,0.3,0.1,1.0)
    else
      Canvas.Brush.Color := Vec(0.4,0.7,0.3,1.0);
  end;
  Canvas.AddFill(Vec(0, 0), Size);

  if Length(Text) > 0 then
  begin
    Canvas.Font.Name := cGameFont;
    Canvas.Font.Color := Vec(0,0,0,1);
    Canvas.Font.Size := 24;
    with Canvas.TextBuilder do
    begin
      Align := laCenter;
      WriteLn(Text);
      txt := Finish();
      txt.VAlign := 0.5;
      txt.BoundsX := Vec(0, Size.x);
      txt.BoundsY := Vec(0, Size.y);
      Canvas.AddText(txt);
    end;
  end;
end;

{ TGamePanel }

procedure TGamePanel.DoValidate;
begin
  Canvas.Clear;
  Canvas.Brush.Color := Vec(0.0,0.0,0.0,0.25);
  Canvas.AddFill(Vec(0, 0), Size);
  Canvas.AddRectangle(Vec(0, 0), Size);
end;

end.

