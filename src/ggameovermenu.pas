unit gGameOverMenu;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avMiniControls, avCanvas, mutils, gTypes, gControls;

type
  { TGameOverPanel }

  TGameOverPanel = class(TavmCustomControl)
  private
    FElapsedTime: Integer;
    FScore: Integer;

    FRestartBtn: TGameButton;
    FExitBtn   : TGameButton;

    procedure SetElapsedTime(const AValue: Integer);
    procedure SetScore(const AValue: Integer);
  protected
    procedure DoValidate; override;
    procedure AfterRegister; override;
  public
    property BtnRestart: TGameButton read FRestartBtn;
    property BtnExit: TGameButton read FExitBtn;

    property ElapsedTime: Integer read FElapsedTime write SetElapsedTime;
    property Score: Integer read FScore write SetScore;
  end;

implementation

{ TGameOverPanel }

procedure TGameOverPanel.SetElapsedTime(const AValue: Integer);
begin
  if FElapsedTime = AValue then Exit;
  FElapsedTime := AValue;
  Invalidate;
end;

procedure TGameOverPanel.SetScore(const AValue: Integer);
begin
  if FScore = AValue then Exit;
  FScore := AValue;
  Invalidate;
end;

procedure TGameOverPanel.DoValidate;
var min, sec: Integer;
    txt: ITextLines;
begin
  inherited DoValidate;
  Canvas.Clear;

  Canvas.Brush.Color := Vec(0,0,0,0.25);
  Canvas.AddFill(Vec(0,0), Size);

  Canvas.Font.Name := cGameFont;
  Canvas.Font.Size := 48;
  Canvas.Font.Color := Vec(1,0.5,0.5,1);
  with Canvas.TextBuilder do
  begin
    Align := laCenter;
    min := FElapsedTime div 60000;
    sec := (FElapsedTime mod 60000) div 1000;
    Font.Size := 64;
    WriteLn('Game Over');
    Font.Color := Vec(1,1,1,1);
    WriteLn( Format('Elapsed time: %d:%.2d', [min, sec]) );
    WriteLn( Format('Score: %d', [FScore]) );

    txt := Finish();
    txt.BoundsX := Vec(0, Size.x);
    Canvas.AddText(txt);
  end;
end;

procedure TGameOverPanel.AfterRegister;
begin
  inherited AfterRegister;
  Origin := Vec(0.5, 0.5);
  Size := Vec(500, 400);

  FRestartBtn := TGameButton.Create(Self);
  FRestartBtn.Size := Vec(200, 60);
  FRestartBtn.Pos := Vec(Size.x/4, 300);
  FRestartBtn.Origin := Vec(0.5,0.0);
  FRestartBtn.Text := 'Restart';

  FExitBtn    := TGameButton.Create(Self);
  FExitBtn.Size := Vec(200, 60);
  FExitBtn.Pos := Vec(Size.x/4*3, 300);
  FExitBtn.Origin := Vec(0.5,0.0);
  FExitBtn.Text := 'Exit';
end;

end.

