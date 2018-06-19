unit untmain;
{$I avConfig.inc}

interface

uses
  {$IfDef FPC}
  Windows,
  LCLType,
  FileUtil,
  LMessages,
  {$Else}
  Windows,
  Messages,
  AppEvnts,
  {$EndIf}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  gMainMenu, gLevel, gTypes,
  avCanvas,
  avRes, avTypes, avTess, mutils,
  bBassLight;

const
  WM_RESTART_LEVEL = WM_USER + 356;

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

    FWARP: Boolean;

    procedure Idle(Sender: TObject; var Done: Boolean);

    procedure PrecacheFont;
    procedure DrawMainMenu;

    procedure StartGame(ASender: TObject);
    procedure ExitGame(ASender: TObject);

    procedure WMRestartLevel(var msg: Cardinal); message WM_RESTART_LEVEL;
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
begin
  SetCurrentDir(ParamStr(0));

  FMain := TavMainRender.Create(Nil);
  FMain.Window := Handle;
  try
    FMain.Init3D(apiDX11);
  except
    on E: ECreateContextFailed do
    begin
      FWARP := True;
      FMain.Init3D(apiDX11_WARP);
    end;
  end;
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

  FMainMenuFBO := Create_FrameBuffer(FMain, [TTextureFormat.RGBA], [true]);

  Application.OnIdle := {$IfDef FPC}@{$EndIf}Idle;

  PrecacheFont;

  GetLightPlayer.GetStream('sounds\music.ogg').Play(True);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMain);
end;

procedure TfrmMain.FormPaint(Sender: TObject);

  procedure UpdateFPS;
  var measureTime: Int64;
      s: string;
  begin
    measureTime := FMain.Time64 div 1000;
    if measureTime > FFPSMeasureTime then
    begin
      FFPSMeasureTime := measureTime;
      s := 'FPS:' + IntToStr(FFPSCounter);
      if FWARP then
        Caption := 'WARP DEVICE! ' + s
      else
        Caption := s;
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
  s := 'ResumeRestartExitNew gameScoreElapsed timeHealth/5';
  for i := 1 to Length(s) do
    GetCanvasCommonData(FMain).GetGlyphImage(cGameFont, s[i], [], xxx, yyyy);
end;

procedure TfrmMain.DrawMainMenu;
begin
  FMainMenuFBO.FrameRect := RectI(Vec(0,0), FMain.WindowSize);
  FMainMenuFBO.Select();
  FMainMenuFBO.Clear(0, cGameClearColor);
  FMainMenu.Draw;
  FMainMenuFBO.BlitToWindow();
end;

procedure TfrmMain.StartGame(ASender: TObject);
begin
  PostMessage(Handle, WM_RESTART_LEVEL, 0, 0);
end;

procedure TfrmMain.ExitGame(ASender: TObject);
begin
  Close;
end;

procedure TfrmMain.WMRestartLevel(var msg: Cardinal);
begin
  FreeAndNil(FGameLevel);
  FGameLevel := TGameLevel.Create(FMain);
  FGameLevel.LoadLevel();
  FGameLevel.OnRestart := {$IfDef FPC}@{$EndIf}StartGame;
  FGameLevel.OnExit := {$IfDef FPC}@{$EndIf}ExitGame;
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

