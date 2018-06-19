unit gMainMenu;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avMiniControls, avRes, gControls, mutils;

type

  { TGameMainMenu }

  TGameMainMenu = class (TavMainRenderChild)
  private
    FBackPanel: TGamePanel;
    FNewGame  : TGameButton;
    FExit     : TGameButton;

    FOnExit: TNotifyEvent;
    FOnStartLevel: TNotifyEvent;

    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
  protected
    procedure NewGameClick(ASender: TObject);
    procedure ExitClick(ASender: TObject);
    procedure AfterRegister; override;
  public
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
  FBackPanel.Size := Vec(160, 140);

  FNewGame := TGameButton.Create(FBackPanel);
  FNewGame.Origin := Vec(0.5,0.5);
  FNewGame.Size := Vec(120, 40);
  FNewGame.Pos := Vec(FBackPanel.Size.x * 0.5, 40);
  FNewGame.Text := 'New game';
  FNewGame.OnClick := {$IfDef FPC}@{$EndIf}NewGameClick;

  FExit := TGameButton.Create(FBackPanel);
  FExit.Origin := Vec(0.5,0.5);
  FExit.Size := Vec(120, 40);
  FExit.Pos := Vec(FBackPanel.Size.x * 0.5, 100);
  FExit.Text := 'Exit';
  FExit.OnClick := {$IfDef FPC}@{$EndIf}ExitClick;
end;

procedure TGameMainMenu.Draw;
begin
  FBackPanel.Pos := Main.WindowSize * 0.5;
  FBackPanel.Draw();
end;

end.

