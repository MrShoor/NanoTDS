unit gInGameUI;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avMiniControls, avCanvas, mutils, gTypes;

type

  { THUD }

  THUD = class(TavmCustomControl)
  private
    FElapsedTime: Integer;
    FHP: Integer;
    FScore: Integer;
    procedure SetElapsedTime(const AValue: Integer);
    procedure SetHP(const AValue: Integer);
    procedure SetScore(const AValue: Integer);
  protected
    procedure DoValidate; override;
    procedure AfterRegister; override;
  public
    property ElapsedTime: Integer read FElapsedTime write SetElapsedTime;
    property HP: Integer read FHP write SetHP;
    property Score: Integer read FScore write SetScore;
  end;

implementation

{ THUD }

procedure THUD.SetScore(const AValue: Integer);
begin
  if FScore = AValue then Exit;
  FScore := AValue;
  Invalidate;
end;

procedure THUD.DoValidate;
var min, sec: Integer;
    txt: ITextLines;
begin
  inherited DoValidate;
  Canvas.Font.Name := cGameFont;
  Canvas.Font.Size := 48;
  Canvas.Clear;
  with Canvas.TextBuilder do
  begin
    Align := laRight;
    min := FElapsedTime div 60000;
    sec := (FElapsedTime mod 60000) div 1000;
    WriteLn( Format('Elapsed time: %d:%.2d', [min, sec]) );
    WriteLn( Format('Score: %.3d', [FScore]) );
    WriteLn( Format('Health: %d/5',[FHP]) );

    txt := Finish();
    txt.BoundsX := Vec(0, Size.x);
    Canvas.AddText(txt);
  end;
end;

procedure THUD.AfterRegister;
begin
  inherited AfterRegister;
  Size := Vec(300, 300);
  Origin := Vec(1,0);
end;

procedure THUD.SetHP(const AValue: Integer);
begin
  if FHP = AValue then Exit;
  FHP := AValue;
  Invalidate;
end;

procedure THUD.SetElapsedTime(const AValue: Integer);
begin
  if FElapsedTime = AValue then Exit;
  FElapsedTime := AValue;
  Invalidate;
end;

end.

