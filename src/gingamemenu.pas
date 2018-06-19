unit gInGameMenu;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avMiniControls, avRes, gControls, mutils;

type

  { TInGameMenu }

  TInGameMenu = class (TavMainRenderChild)
  private
    FBackPanel: TGamePanel;
    FResume   : TGameButton;
    FRestart  : TGameButton;
    FExit     : TGameButton;

    FOnResume: TNotifyEvent;
    FOnRestart: TNotifyEvent;
    FOnExit: TNotifyEvent;

    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
  protected
    procedure ResumeClick(ASender: TObject);
    procedure RestartClick(ASender: TObject);
    procedure ExitClick(ASender: TObject);
    procedure AfterRegister; override;
  public
    property Visible: Boolean read GetVisible write SetVisible;

    procedure Draw;

    property OnResume: TNotifyEvent read FOnResume write FOnResume;
    property OnRestart: TNotifyEvent read FOnRestart write FOnRestart;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
  end;

implementation

{ TInGameMenu }

function TInGameMenu.GetVisible: Boolean;
begin
  Result := FBackPanel.Visible;
end;

procedure TInGameMenu.SetVisible(const AValue: Boolean);
begin
  FBackPanel.Visible := AValue;
end;

procedure TInGameMenu.ResumeClick(ASender: TObject);
begin
  if Assigned(FOnResume) then FOnResume(Self);
end;

procedure TInGameMenu.RestartClick(ASender: TObject);
begin
  if Assigned(FOnRestart) then FOnRestart(Self);
end;

procedure TInGameMenu.ExitClick(ASender: TObject);
begin
  if Assigned(FOnExit) then FOnExit(Self);
end;

procedure TInGameMenu.AfterRegister;
begin
  inherited AfterRegister;
  FBackPanel := TGamePanel.Create(Self);
  FBackPanel.Origin := Vec(0.5,0.5);
  FBackPanel.Size := Vec(160, 200);

  FResume := TGameButton.Create(FBackPanel);
  FResume.Origin := Vec(0.5, 0.5);
  FResume.Size := Vec(120, 40);
  FResume.Pos := Vec(FBackPanel.Size.x * 0.5, 40);
  FResume.Text := 'Resume';
  FResume.OnClick := {$IfDef FPC}@{$EndIf}ResumeClick;

  FRestart := TGameButton.Create(FBackPanel);
  FRestart.Origin := Vec(0.5,0.5);
  FRestart.Size := Vec(120, 40);
  FRestart.Pos := Vec(FBackPanel.Size.x * 0.5, 100);
  FRestart.Text := 'Restart';
  FRestart.OnClick := {$IfDef FPC}@{$EndIf}RestartClick;

  FExit := TGameButton.Create(FBackPanel);
  FExit.Origin := Vec(0.5,0.5);
  FExit.Size := Vec(120, 40);
  FExit.Pos := Vec(FBackPanel.Size.x * 0.5, 160);
  FExit.Text := 'Exit';
  FExit.OnClick := {$IfDef FPC}@{$EndIf}ExitClick;
end;

procedure TInGameMenu.Draw;
begin
  FBackPanel.Pos := Main.WindowSize * 0.5;
  FBackPanel.Draw();
end;

end.
