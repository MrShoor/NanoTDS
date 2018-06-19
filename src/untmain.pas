unit untmain;
{$I avConfig.inc}

interface

uses
  {$IfDef FPC}
  LCLType,
  FileUtil,
  {$Else}
  Windows,
  Messages,
  AppEvnts,
  {$EndIf}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  gMainMenu, gLevel, gTypes,
  avCanvas,
  avRes, avTypes, avTess, mutils;

type
  { TfrmMain }

  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FMain: TavMainRender;
    FMainMenuFBO: TavFrameBuffer;

    FMainMenu: TGameMainMenu;
    FGameLevel: TGameLevel;

    FFPSCounter: Integer;
    FFPSMeasureTime: Integer;

    procedure Idle(Sender: TObject; var Done: Boolean);

    procedure PrecacheFont;
    procedure DrawMainMenu;

    procedure StartGame(ASender: TObject);
    procedure ExitGame(ASender: TObject);
  public
    {$IfDef FPC}
    procedure EraseBackground(DC: HDC); override;
    {$EndIf}
    {$IfDef DCC}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    {$EndIf}
  end;

var
  frmMain: TfrmMain;

implementation

{$IfnDef notDCC}
  {$R *.dfm}
{$EndIf}

{$IfDef FPC}
  {$R *.lfm}
{$EndIf}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  SetCurrentDir(ParamStr(0));

  FMain := TavMainRender.Create(Nil);
  FMain.Window := Handle;
  FMain.Init3D(apiDX11);
  FMain.Projection.DepthRange := Vec(1, 0);
  FMain.Projection.NearPlane := 0.01;
  FMain.Projection.FarPlane := 50;
  FMain.Projection.Fov := Pi * 0.25;
  FMain.States.DepthFunc := cfGreater;
  FMain.UpdateStatesInterval := 8;

  FMainMenu := TGameMainMenu.Create(FMain);
  FMainMenu.Visible := True;
  FMainMenu.OnStartLevel := {$IfDef FPC}@{$EndIf}StartGame;
  FMainMenu.OnExit := {$IfDef FPC}@{$EndIf}ExitGame;

  FMainMenuFBO := Create_FrameBuffer(FMain, TTextureFormat.RGBA, [true]);

  Application.OnIdle := {$IfDef FPC}@{$EndIf}Idle;

  PrecacheFont;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMain);
end;

procedure TfrmMain.FormPaint(Sender: TObject);

  procedure UpdateFPS;
  var measureTime: Int64;
  begin
    measureTime := FMain.Time64 div 100;
    if measureTime > FFPSMeasureTime then
    begin
      FFPSMeasureTime := measureTime;
      Caption := 'FPS:' + IntToStr(FFPSCounter*10 + Random(10));
      FFPSCounter := 0;
    end
    else
      Inc(FFPSCounter);
  end;

begin
  if FMain = nil then Exit;

  if FMain.Bind then
  try
    if Assigned(FGameLevel) then
      FGameLevel.Draw
    else
      DrawMainMenu;
    FMain.Present;
  finally
    FMain.Unbind;
  end;
  UpdateFPS;
end;

procedure TfrmMain.Idle(Sender: TObject; var Done: Boolean);
begin
  FMain.InvalidateWindow;
  Done := False;
end;

procedure TfrmMain.PrecacheFont;
var s: UnicodeString;
  xxx: TVec3;
  yyyy: TVec4;
  i: Integer;
begin
  s := 'ResumeRestartExitNew game';
  for i := 1 to Length(s) do
    GetCanvasCommonData(FMain).GetGlyphImage(cGameFont, s[i], [], xxx, yyyy);
end;

procedure TfrmMain.DrawMainMenu;
begin
  FMainMenuFBO.FrameRect := RectI(Vec(0,0), FMain.WindowSize);
  FMainMenuFBO.Select();
  FMainMenuFBO.Clear(0, Vec(0,0.1,0,0));
  FMainMenu.Draw;
  FMainMenuFBO.BlitToWindow();
end;

procedure TfrmMain.StartGame(ASender: TObject);
begin
  FreeAndNil(FGameLevel);
  FGameLevel := TGameLevel.Create(FMain);
  FGameLevel.LoadLevel();
  FGameLevel.OnRestart := {$IfDef FPC}@{$EndIf}StartGame;
  FGameLevel.OnExit := {$IfDef FPC}@{$EndIf}ExitGame;
end;

procedure TfrmMain.ExitGame(ASender: TObject);
begin
  Close;
end;

{$IfDef FPC}
procedure TfrmMain.EraseBackground(DC: HDC);
begin
  //inherited EraseBackground(DC);
end;
{$EndIf}
{$IfDef DCC}
procedure TfrmMain.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;
{$EndIf}

end.

