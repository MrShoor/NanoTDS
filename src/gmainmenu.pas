unit gMainMenu;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avMiniControls, avRes, avCanvas, gControls, mutils;

type

  { TGameMainMenu }

  TGameMainMenu = class (TavMainRenderChild)
  private
    FBackPanel: TGamePanel;
    FNewGame  : TGameButton;
    FExit     : TGameButton;
    FHighQ    : TGameCheckButton;
    FLowQ     : TGameCheckButton;

    FOnExit: TNotifyEvent;
    FOnStartLevel: TNotifyEvent;

    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
  protected
    procedure NewGameClick(ASender: TObject);
    procedure ExitClick(ASender: TObject);
    procedure AfterRegister; override;
  public
    function IsHighQuality: Boolean;
    property Visible: Boolean read GetVisible write SetVisible;

    procedure Draw;

    property OnStartLevel: TNotifyEvent read FOnStartLevel write FOnStartLevel;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
  end;

implementation

{ TGameMainMenu }

function TGameMainMenu.GetVisible: Boolean;
begin
  Result := FBackPanel.Visible;
end;

procedure TGameMainMenu.SetVisible(const AValue: Boolean);
begin
  FBackPanel.Visible := AValue;
end;

procedure TGameMainMenu.NewGameClick(ASender: TObject);
begin
  if Assigned(FOnStartLevel) then FOnStartLevel(Self);
end;

procedure TGameMainMenu.ExitClick(ASender: TObject);
begin
  if Assigned(FOnExit) then FOnExit(Self);
end;

procedure TGameMainMenu.AfterRegister;
begin
  inherited AfterRegister;
  FBackPanel := TGamePanel.Create(Self);
  FBackPanel.Origin := Vec(0.5,0.5);
  FBackPanel.Size := Vec(160, 220);

  FNewGame := TGameButton.Create(FBackPanel);
  FNewGame.Size := Vec(120, 40);
  FNewGame.Pos := Vec(FBackPanel.Size.x * 0.5, 40);
  FNewGame.Text := 'New game';
  FNewGame.OnClick := {$IfDef FPC}@{$EndIf}NewGameClick;

  FExit := TGameButton.Create(FBackPanel);
  FExit.Size := Vec(120, 40);
  FExit.Pos := Vec(FBackPanel.Size.x * 0.5, 100);
  FExit.Text := 'Exit';
  FExit.OnClick := {$IfDef FPC}@{$EndIf}ExitClick;

  with TGameLabel.Create(FBackPanel) do
  begin
    Origin := Vec(0.5, 0.5);
    Pos := Vec(FBackPanel.Size.x * 0.5, 160);
    Text := 'Graphics:';
    Align := laCenter;
  end;

  FLowQ := TGameCheckButton.Create(FBackPanel);
  FLowQ.Size := Vec(50,25);
  FLowQ.Pos := Vec(45, 190);
  FLowQ.Text := 'Low';

  FHighQ := TGameCheckButton.Create(FBackPanel);
  FHighQ.Size := Vec(50,25);
  FHighQ.Pos := Vec(FBackPanel.Size.x-45, 190);
  FHighQ.Text := 'High';
  FHighQ.Checked := True;
end;

function TGameMainMenu.IsHighQuality: Boolean;
begin
  Result := FHighQ.Checked;
end;

procedure TGameMainMenu.Draw;
begin
  FBackPanel.Pos := Main.WindowSize * 0.5;
  FBackPanel.Draw();
end;

end.

