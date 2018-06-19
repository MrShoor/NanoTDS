unit gLevel;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils,
  avRes, avTypes, avContnrs,
  mutils,
  bWorld,
  bLights,
  bUtils,
  gInGameMenu;

type

  { TbRobot }

  TbRobot = class(TbGameObject)
  private
    FLookAtXZ: TVec2;
    FLastPos : TVec3;
    FPosAccum: TVec3;
    procedure SetLookAtXZ(const AValue: TVec2);
  protected
    procedure AfterRegister; override;
    procedure UpdateStep; override;
  public
    property LookAtXZ: TVec2 read FLookAtXZ write SetLookAtXZ;
  end;

  { TbBotSpawner }

  TbBotSpawner = class(TbGameObject)
  private
    FNextIn : Int64;
    FIntensity: array of Integer;
    FIntervals: array of Integer;
    FOnSpawn: TNotifyEvent;
  protected
    procedure AfterRegister; override;
    procedure UpdateStep; override;
  public
    procedure SetSpawnRules(const AFirstDelay: Integer; const AIntervals: array of Integer; const AIntensityTimes: array of Integer);
    property OnSpawn: TNotifyEvent read FOnSpawn write FOnSpawn;
  end;

  TbBot = class(TbRobot)
  private
    FHP: Integer;
    FRoute: TVec3Arr;
    FRoutePos: TPathPos;
    FRouteVelocity: Single;
  protected
    procedure UpdateStep; override;
  public
    procedure SetRoute(const ARoute: TVec3Arr; const AVelocity: Single);
    property HP: Integer read FHP write FHP;
  end;
  IbBotArr = {$IfDef FPC}specialize{$EndIf}IArray<TbBot>;
  TbBotArr = {$IfDef FPC}specialize{$EndIf}TArray<TbBot>;

  IBoxes = {$IfDef FPC}specialize{$EndIf}IArray<TRectF>;
  TBoxes = {$IfDef FPC}specialize{$EndIf}TArray<TRectF>;

  { TGameLevel }

  TGameLevel = class(TavMainRenderChild)
  private
    FOnExit: TNotifyEvent;
    FOnRestart: TNotifyEvent;
    FInGameMenu: TInGameMenu;
  protected
    procedure MenuResume(ASender: TObject);
    procedure MenuRestart(ASender: TObject);
    procedure MenuExit(ASender: TObject);
  private
    FWorld: TbWorld;
    FGlobalLight: IavPointLight;

    FBotRoutes: array[0..5] of TVec3Arr;
    FBotRedSpawner: TbBotSpawner;
    FBotGreenSpawner: TbBotSpawner;
    FBots: IbBotArr;

    FBoxes: IBoxes;

    FPlayer: TbRobot;

    procedure DoSpawnReds(ASender: TObject);
    procedure DoSpawnGreen(ASender: TObject);
  protected
    procedure ResolveCollisions;
    procedure ProcessInput;
    procedure EMKeyDown(var AMsg: TavKeyDownMessage); message EM_KEYDOWN;
    procedure EMUps(var AMsg: TavMessage); message EM_UPS;
  public
    procedure LoadLevel();
    procedure Draw;

    property OnRestart: TNotifyEvent read FOnRestart write FOnRestart;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
  end;

implementation

uses
  Math, avPlatform;

const
  VK_ESCAPE = 27;

  cFristSpawnDelay = 0;
  cRedIntervals      : array [0..3] of Integer = (5000, 4000, 3000, 2000);
  cGreenIntervals    : array [0..3] of Integer = (7500, 6000, 4500, 3000);
  cIntencityTimes: array [0..3] of Integer = (0, 60000, 120000, 180000);

{ TbBot }

procedure TbBot.UpdateStep;
var lookDir: TVec3;
begin
  inherited UpdateStep;
  Pos := TravelByPath(FRoute, FRouteVelocity, FRoutePos, false);
  lookDir := normalize(FRoute[FRoutePos.Idx+1] - FRoute[FRoutePos.Idx]);
  LookAtXZ := Lerp(LookAtXZ, Vec(lookDir.x, lookDir.z), 0.05);
end;

procedure TbBot.SetRoute(const ARoute: TVec3Arr; const AVelocity: Single);
begin
  FRoute := ARoute;
  FRoutePos.Idx := 0;
  FRoutePos.Pos := 0;
  FRouteVelocity := AVelocity * Main.UpdateStatesInterval / 1000;
  Pos := ARoute[0];
end;

{ TbRobot }

procedure TbRobot.SetLookAtXZ(const AValue: TVec2);
begin
  if FLookAtXZ = AValue then Exit;
  if LenSqr(FLookAtXZ) = 0 then Exit;
  FLookAtXZ := AValue;
end;

procedure TbRobot.AfterRegister;
begin
  inherited AfterRegister;
  SubscribeForUpdateStep;
  FLookAtXZ.x := 1;
  FLookAtXZ.y := 0;
end;

procedure TbRobot.UpdateStep;
var moveStep: TVec3;
    look: TQuat;
    pitch: TQuat;
    n: TVec3;
begin
  inherited UpdateStep;
  pitch.v4 := Vec(0,0,0,1);

  moveStep := Pos - FLastPos;
  if Len(moveStep) < 0.3 then
  begin
    FPosAccum := FPosAccum * 0.93;
    FPosAccum := FPosAccum + moveStep;
    if LenSqr(FPosAccum) > 0 then
    begin
      n := normalize( cross(Vec(0,1,0), FPosAccum) );
      pitch := Quat(n, Len(FPosAccum)*0.75);
    end;
  end;
  FLastPos := Pos;

  look := Quat(Vec(0,-1,0), arctan2(FLookAtXZ.y, FLookAtXZ.x));
  Rot := pitch * look;
end;

{ TbBotSpawner }

procedure TbBotSpawner.AfterRegister;
begin
  inherited AfterRegister;
  SubscribeForUpdateStep;
end;

procedure TbBotSpawner.UpdateStep;
var time: Int64;
    intensity: Integer;
begin
  inherited UpdateStep;
  time := World.GameTime * Main.UpdateStatesInterval;
  if FNextIn < time then
  begin
    intensity := 0;
    while (intensity < Length(FIntensity)) and (time > FIntensity[intensity]) do
      Inc(intensity);
    intensity := Min(Length(FIntervals)-1, intensity);

    Inc(FNextIn, FIntervals[intensity]);

    if Assigned(FOnSpawn) then FOnSpawn(Self);
  end;
end;

procedure TbBotSpawner.SetSpawnRules(const AFirstDelay: Integer; const AIntervals: array of Integer; const AIntensityTimes: array of Integer);
var i: Integer;
begin
  FNextIn := AFirstDelay;
  SetLength(FIntensity, Length(AIntensityTimes));
  for i := 0 to Length(FIntensity) - 1 do FIntensity[i] := AIntensityTimes[i];
  SetLength(FIntervals, Length(AIntervals));
  for i := 0 to Length(FIntervals) - 1 do FIntervals[i] := AIntervals[i];
end;

{ TGameLevel }

procedure TGameLevel.MenuResume(ASender: TObject);
begin
  FInGameMenu.Visible := False;
end;

procedure TGameLevel.MenuRestart(ASender: TObject);
begin
  if Assigned(FOnRestart) then FOnRestart(ASender);
end;

procedure TGameLevel.MenuExit(ASender: TObject);
begin
  if Assigned(FOnExit) then FOnExit(ASender);
end;

procedure TGameLevel.DoSpawnReds(ASender: TObject);
var bot: TbBot;
begin
  bot := TbBot.Create(FWorld);
  bot.HP := 3;
  bot.SetRoute(FBotRoutes[Random(2)], 2);
  bot.AddModel('Enemy_red');
  FBots.Add(bot);

  bot := TbBot.Create(FWorld);
  bot.HP := 3;
  bot.SetRoute(FBotRoutes[Random(2)+2], 2);
  bot.AddModel('Enemy_red');
  FBots.Add(bot);
end;

procedure TGameLevel.DoSpawnGreen(ASender: TObject);
var bot: TbBot;
begin
  bot := TbBot.Create(FWorld);
  bot.HP := 2;
  bot.SetRoute(FBotRoutes[Random(2)+4], 3);
  bot.AddModel('Enemy_green');
  FBots.Add(bot);
end;

procedure TGameLevel.ResolveCollisions;
const cRobotRadius = 0.5;
  procedure PushOut(AObj: TbGameObject; ARad: Single; ABox: TRectF);
  var pushDir : TVec2;
      minPushDir: TVec2;
  begin
    ABox := ABox.Expand(cRobotRadius);
    if ABox.PtInRect(Vec(AObj.Pos.x, AObj.Pos.z)) then
    begin
      minPushDir := Vec(ABox.min.x - AObj.Pos.x, 0);
      pushDir := Vec(ABox.max.x - AObj.Pos.x, 0);
      if LenSqr(pushDir) < LenSqr(minPushDir) then minPushDir := pushDir;
      pushDir := Vec(0, ABox.min.y - AObj.Pos.z);
      if LenSqr(pushDir) < LenSqr(minPushDir) then minPushDir := pushDir;
      pushDir := Vec(0, ABox.max.y - AObj.Pos.z);
      if LenSqr(pushDir) < LenSqr(minPushDir) then minPushDir := pushDir;
      AObj.Pos := AObj.Pos + Vec(minPushDir.x, 0, minPushDir.y);
    end;
  end;
var i: Integer;
begin
  for i := 0 to FBoxes.Count - 1 do
    PushOut(FPlayer, cRobotRadius, FBoxes[i]);
end;

procedure TGameLevel.ProcessInput;
const cPlayerSpeed = 3;
var lookPt : TVec3;
    speed  : Single;
    moveDir: TVec3;
begin
  if not IsFocusedWindow(Main.Window) then Exit;

  speed := cPlayerSpeed * Main.UpdateStatesInterval / 1000;
  moveDir := Vec(0,0,0);
  if IsKeyPressed(Ord('W')) then moveDir.z := moveDir.z + 1;
  if IsKeyPressed(Ord('A')) then moveDir.x := moveDir.x - 1;
  if IsKeyPressed(Ord('S')) then moveDir.z := moveDir.z - 1;
  if IsKeyPressed(Ord('D')) then moveDir.x := moveDir.x + 1;
  if LenSqr(moveDir) > 0 then
    FPlayer.Pos := FPlayer.Pos + SetLen(moveDir, speed);

  if Intersect(Plane(0,1,0,-1), Main.Cursor.Ray, lookPt) then
  begin
    lookPt := lookPt - FPlayer.Pos;
    FPlayer.LookAtXZ := Vec(lookPt.x, lookPt.z);
  end;
end;

procedure TGameLevel.EMKeyDown(var AMsg: TavKeyDownMessage);
begin
  if AMsg.Key = VK_ESCAPE then
    FInGameMenu.Visible := not FInGameMenu.Visible;
end;

procedure TGameLevel.EMUps(var AMsg: TavMessage);
begin
  if not FInGameMenu.Visible then
  begin
    FWorld.UpdateStep;
    ResolveCollisions;
    ProcessInput;
  end;
end;

procedure TGameLevel.LoadLevel;
  procedure SetRoute(var Arr: TVec3Arr; V: array of TVec3);
  var i: Integer;
  begin
    SetLength(Arr, Length(V));
    for i := 0 to Length(V) - 1 do Arr[i] := V[i];
  end;
  function ColliderBox(const ATileCoord: TVec2i): TRectF;
  begin
    Result.min := ATileCoord * 2.0;
    Result.max := Result.min + Vec(2.0, 2.0);
  end;
var i: Integer;
begin
  Main.Camera.At := Vec(8,0,8);
  Main.Camera.Eye := Main.Camera.At + Vec(0,sin(deg2rad*60),-cos(deg2rad*60))*22;

  FWorld := TbWorld.Create(Self);
  FWorld.Renderer.PreloadModels(['level0\model.avm']);
  FWorld.Renderer.PreloadModels(['units\model.avm']);
  with TbGameObject.Create(FWorld) do
  begin
    AddModel('level0');
  end;

  FGlobalLight := FWorld.Renderer.CreatePointLight();
  FGlobalLight.Pos := Vec(0, 20, 0);
  FGlobalLight.Color := Vec(1,1,1);
  FGlobalLight.Radius := 40;
  FGlobalLight.CastShadows := True;

  FPlayer := TbRobot.Create(FWorld);
  FPlayer.AddModel('Player');
  FPlayer.Pos := Vec(8,0,2);

  FInGameMenu := TInGameMenu.Create(Self);
  FInGameMenu.Visible := True;
  FInGameMenu.OnResume := {$IfDef FPC}@{$EndIf}MenuResume;
  FInGameMenu.OnRestart := {$IfDef FPC}@{$EndIf}MenuRestart;
  FInGameMenu.OnExit := {$IfDef FPC}@{$EndIf}MenuExit;

  SetRoute(FBotRoutes[0], [Vec(3,0,15), Vec(1,0,15), Vec(1,0,1), Vec(7,0,1)]);
  SetRoute(FBotRoutes[1], [Vec(3,0,15), Vec(5,0,15), Vec(5,0,1), Vec(7,0,1)]);
  SetRoute(FBotRoutes[2], [Vec(13,0,15), Vec(15,0,15), Vec(15,0,1), Vec(9,0,1)]);
  SetRoute(FBotRoutes[3], [Vec(13,0,15), Vec(11,0,15), Vec(11,0,1), Vec(9,0,1)]);
  SetRoute(FBotRoutes[4], [Vec(8,0,15), Vec( 5,0,15), Vec( 5,0,3), Vec(7,0,3)]);
  SetRoute(FBotRoutes[5], [Vec(8,0,15), Vec(11,0,15), Vec(11,0,3), Vec(9,0,3)]);

  FBots := TbBotArr.Create();
  FBotRedSpawner := TbBotSpawner.Create(FWorld);
  FBotRedSpawner.OnSpawn := {$IfDef FPC}@{$EndIf}DoSpawnReds;
  FBotRedSpawner.SetSpawnRules(cFristSpawnDelay, cRedIntervals, cIntencityTimes);
  FBotGreenSpawner := TbBotSpawner.Create(FWorld);
  FBotGreenSpawner.OnSpawn := {$IfDef FPC}@{$EndIf}DoSpawnGreen;
  FBotGreenSpawner.SetSpawnRules(cFristSpawnDelay, cGreenIntervals, cIntencityTimes);

  FBoxes := TBoxes.Create();
  FBoxes.Add(ColliderBox(Vec(1, 2)));
  FBoxes.Add(ColliderBox(Vec(1, 5)));
  FBoxes.Add(ColliderBox(Vec(6, 2)));
  FBoxes.Add(ColliderBox(Vec(6, 5)));
  FBoxes.Add(ColliderBox(Vec(3, 3)));
  FBoxes.Add(ColliderBox(Vec(4, 3)));
  for i := 0 to 8 do FBoxes.Add(ColliderBox(Vec(-1, i)));
  for i := 0 to 8 do FBoxes.Add(ColliderBox(Vec(i, 8)));
  for i := 0 to 8 do FBoxes.Add(ColliderBox(Vec(8, 7-i)));
  for i := 0 to 8 do FBoxes.Add(ColliderBox(Vec(7-i,-1)));
end;

procedure TGameLevel.Draw;
begin
  FWorld.Renderer.PrepareToDraw;
  FWorld.Renderer.DrawWorld;
  if FInGameMenu.Visible then
    FInGameMenu.Draw;
  Main.ActiveFrameBuffer.BlitToWindow();
end;

end.

