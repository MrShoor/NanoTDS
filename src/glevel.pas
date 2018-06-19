unit gLevel;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils,
  avRes, avTypes, avContnrs, avModel,
  mutils,
  bWorld,
  bLights,
  bUtils,
  gInGameMenu,
  gInGameUI,
  gGameOverMenu,
  gTypes,
  bBassLight;

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

  TbBotKind = (bkGreen, bkRed);

  TbBot = class(TbRobot)
  private
    FBotKind: TbBotKind;
    FHP: Integer;
    FRoute: TVec3Arr;
    FRoutePos: TPathPos;
    FRouteVelocity: Single;

    FHittedTime  : Integer;
    FHittedModels: IavModelInstanceArr;
    FNormalModels: IavModelInstanceArr;
  protected
    procedure AfterRegister; override;
    procedure UpdateStep; override;
  public
    procedure AddModel(const AName: string; AType: TModelType = mtDefault); override;

    procedure AddDamage(const ADir: TVec3);
    procedure SetRoute(const ARoute: TVec3Arr; const AVelocity: Single);
    property HP: Integer read FHP write FHP;
    property BotKind: TbBotKind read FBotKind write FBotKind;
  end;
  IbBotArr = {$IfDef FPC}specialize{$EndIf}IArray<TbBot>;
  TbBotArr = {$IfDef FPC}specialize{$EndIf}TArray<TbBot>;

  IBoxes = {$IfDef FPC}specialize{$EndIf}IArray<TRectF>;
  TBoxes = {$IfDef FPC}specialize{$EndIf}TArray<TRectF>;

  { TbBullet }

  TbBullet = class(TbGameObject)
  private
    FSpeedXZ: TVec2;
  protected
    procedure AfterRegister; override;
    procedure UpdateStep; override;
  public
    property SpeedXZ: TVec2 read FSpeedXZ write FSpeedXZ;
  end;
  IbBulletArr = {$IfDef FPC}specialize{$EndIf}IArray<TbBullet>;
  TbBulletArr = {$IfDef FPC}specialize{$EndIf}TArray<TbBullet>;

  { TGameLevel }

  TGameLevel = class(TavMainRenderChild)
  private
    FOnExit: TNotifyEvent;
    FOnRestart: TNotifyEvent;
    FInGameMenu: TInGameMenu;
    FGameOverMenu: TGameOverPanel;
  protected
    procedure MenuResume(ASender: TObject);
    procedure MenuRestart(ASender: TObject);
    procedure MenuExit(ASender: TObject);
  private
    FWorld: TbWorld;
    FGlobalLight: IavPointLight;

    FHUD : THUD;
    FBaseHP: Integer;
    FScores: Integer;

    FBotRoutes: array[0..5] of TVec3Arr;
    FBotRedSpawner: TbBotSpawner;
    FBotGreenSpawner: TbBotSpawner;
    FBots: IbBotArr;

    FBoxes: IBoxes;
    FBaseBoxes: IBoxes;

    FPlayer: TbRobot;
    FShootNextTime: Integer;
    FBullets: IbBulletArr;

    function IsGameOver(): Boolean;
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
  VK_LBUTTON = 1;

  cFristSpawnDelay = 5000;
  cRedIntervals      : array [0..3] of Integer = (5000, 4000, 3000, 2000);
  cGreenIntervals    : array [0..3] of Integer = (7500, 6000, 4500, 3000);
  cIntencityTimes    : array [0..3] of Integer = (0, 60000, 120000, 180000);

{ TbBullet }

procedure TbBullet.AfterRegister;
begin
  inherited AfterRegister;
  SubscribeForUpdateStep;
end;

procedure TbBullet.UpdateStep;
var speed: TVec3;
begin
  inherited UpdateStep;
  speed := Vec(FSpeedXZ.x, 0, FSpeedXZ.y) * (Main.UpdateStatesInterval / 1000);
  Pos := Pos + speed;
end;

{ TbBot }

procedure TbBot.AfterRegister;
begin
  inherited AfterRegister;
  FHittedModels := World.Renderer.CreateModelInstances(['Enemy_hitted']);
end;

procedure TbBot.UpdateStep;
var lookDir: TVec3;
begin
  inherited UpdateStep;
  Pos := TravelByPath(FRoute, FRouteVelocity, FRoutePos, false);
  lookDir := normalize(FRoute[FRoutePos.Idx+1] - FRoute[FRoutePos.Idx]);
  LookAtXZ := Lerp(LookAtXZ, Vec(lookDir.x, lookDir.z), 0.05);

  if (World.GameTime * Main.UpdateStatesInterval < FHittedTime) then
    FModels := FHittedModels
  else
    FModels := FNormalModels;
end;

procedure TbBot.AddModel(const AName: string; AType: TModelType);
begin
  inherited AddModel(AName, AType);
  FNormalModels := FModels;
end;

procedure TbBot.AddDamage(const ADir: TVec3);
begin
  Dec(FHP);
  FPosAccum := FPosAccum + ADir*0.1;
  FHittedTime := World.GameTime * Main.UpdateStatesInterval + 200;
  GetLightPlayer.GetStream('sounds\hit.wav').Play();
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

function TGameLevel.IsGameOver: Boolean;
begin
  Result := FGameOverMenu.Visible;
end;

procedure TGameLevel.DoSpawnReds(ASender: TObject);
var bot: TbBot;
begin
  bot := TbBot.Create(FWorld);
  bot.HP := 3;
  bot.SetRoute(FBotRoutes[Random(2)], 2);
  bot.AddModel('Enemy_red');
  bot.BotKind := bkRed;
  FBots.Add(bot);

  bot := TbBot.Create(FWorld);
  bot.HP := 3;
  bot.SetRoute(FBotRoutes[Random(2)+2], 2);
  bot.AddModel('Enemy_red');
  bot.BotKind := bkRed;
  FBots.Add(bot);
end;

procedure TGameLevel.DoSpawnGreen(ASender: TObject);
var bot: TbBot;
begin
  bot := TbBot.Create(FWorld);
  bot.HP := 2;
  bot.SetRoute(FBotRoutes[Random(2)+4], 3);
  bot.AddModel('Enemy_green');
  bot.BotKind := bkGreen;
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
var i, j: Integer;
begin
  for i := 0 to FBoxes.Count - 1 do
    PushOut(FPlayer, cRobotRadius, FBoxes[i]);

  for i := 0 to FBoxes.Count - 1 do
    for j := FBullets.Count - 1 downto 0 do
    begin
      if FBoxes[i].PtInRect(Vec(FBullets[j].Pos.x, FBullets[j].Pos.z)) then
      begin
        FBullets[j].Free;
        FBullets.DeleteWithSwap(j);
      end;
    end;

  for i := FBots.Count - 1 downto 0 do
    for j := FBullets.Count - 1 downto 0 do
      if LenSqr(FBots[i].Pos - FBullets[j].Pos) < Sqr(cRobotRadius) then
      begin
        FBots[i].AddDamage(Vec(FBullets[j].SpeedXZ.x, 0, FBullets[j].SpeedXZ.y));
        FBullets[j].Free;
        FBullets.DeleteWithSwap(j);

        if FBots[i].HP <= 0 then
        begin
          case FBots[i].BotKind of
            bkRed : GetLightPlayer.GetStream('sounds\red_death.wav').Play();
            bkGreen : GetLightPlayer.GetStream('sounds\green_death.wav').Play();
          end;
          Inc(FScores);
          FBots[i].Free;
          FBots.DeleteWithSwap(i);
          Break;
        end;
      end;

  for i := 0 to FBaseBoxes.Count - 1 do
    for j := FBots.Count - 1 downto 0 do
      if FBaseBoxes[i].PtInRect(Vec(FBots[j].Pos.x, FBots[j].Pos.z)) then
      begin
        Dec(FBaseHP);
        FBots[j].Free;
        FBots.DeleteWithSwap(j);
      end;

  if (FBaseHP <= 0) and not IsGameOver() then
  begin
    FreeAndNil(FBotRedSpawner);
    FreeAndNil(FBotGreenSpawner);
    FGameOverMenu.ElapsedTime := FWorld.GameTime * Main.UpdateStatesInterval;
    FGameOverMenu.Score := FScores;
    FGameOverMenu.Visible := True;
    FHUD.HP := 0;
    GetLightPlayer.GetStream('sounds\gameover.wav').Play();
  end;
end;

procedure TGameLevel.ProcessInput;

  procedure DoShoot;
  var time: Int64;
      bullet: TbBullet;
  begin
    time := FWorld.GameTime * Main.UpdateStatesInterval;
    if time >= FShootNextTime then
    begin
      bullet := TbBullet.Create(FWorld);
      bullet.AddModel('Bullet');
      bullet.SpeedXZ := SetLen(FPlayer.LookAtXZ, 4*2);
      bullet.Pos := FPlayer.Pos;
      bullet.Rot := Quat(Vec(0,-1,0), arctan2(FPlayer.LookAtXZ.y, FPlayer.LookAtXZ.x));
      FBullets.Add(bullet);
      FShootNextTime := time + (1000 div 5);

      GetLightPlayer.GetStream('sounds\shot.wav').Play();
    end;
  end;

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

  if IsKeyPressed(VK_LBUTTON) then DoShoot;
end;

procedure TGameLevel.EMKeyDown(var AMsg: TavKeyDownMessage);
begin
  if not IsGameOver() then
    if AMsg.Key = VK_ESCAPE then
      FInGameMenu.Visible := not FInGameMenu.Visible;
end;

procedure TGameLevel.EMUps(var AMsg: TavMessage);
var
  i: Integer;
begin
  for i := 0 to AMsg.param - 1 do
    if not FInGameMenu.Visible then
    begin
      FWorld.UpdateStep;
      ResolveCollisions;
      if not IsGameOver() then
      begin
        ProcessInput;

        FHUD.HP := FBaseHP;
        FHUD.Score := FScores;
        FHUD.ElapsedTime := FWorld.GameTime * Main.UpdateStatesInterval;
      end;
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

  FBaseHP := 5;

  FGlobalLight := FWorld.Renderer.CreatePointLight();
  FGlobalLight.Pos := Vec(0, 20, 0);
  FGlobalLight.Color := Vec(1,1,1);
  FGlobalLight.Radius := 40;
  FGlobalLight.CastShadows := True;

  FPlayer := TbRobot.Create(FWorld);
  FPlayer.AddModel('Player');
  FPlayer.Pos := Vec(8,0,2);

  FInGameMenu := TInGameMenu.Create(Self);
  FInGameMenu.Visible := False;
  FInGameMenu.OnResume := {$IfDef FPC}@{$EndIf}MenuResume;
  FInGameMenu.OnRestart := {$IfDef FPC}@{$EndIf}MenuRestart;
  FInGameMenu.OnExit := {$IfDef FPC}@{$EndIf}MenuExit;

  FGameOverMenu := TGameOverPanel.Create(Self);
  FGameOverMenu.BtnRestart.OnClick := {$IfDef FPC}@{$EndIf}MenuRestart;
  FGameOverMenu.BtnExit.OnClick := {$IfDef FPC}@{$EndIf}MenuExit;
  FGameOverMenu.Visible := False;

  FHUD := THUD.Create(Self);
  FHUD.HP := FBaseHP;
  FHUD.Score := FScores;
  FHUD.ElapsedTime := 0;

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

  FBaseBoxes := TBoxes.Create();
  FBaseBoxes.Add(ColliderBox(Vec(3, 0)));
  FBaseBoxes.Add(ColliderBox(Vec(4, 0)));
  FBaseBoxes.Add(ColliderBox(Vec(3, 1)));
  FBaseBoxes.Add(ColliderBox(Vec(4, 1)));

  FBullets := TbBulletArr.Create();
end;

procedure TGameLevel.Draw;
begin
  FWorld.Renderer.PrepareToDraw;
  Main.Clear(cGameClearColor, True, Main.Projection.DepthRange.y, True);
  FWorld.Renderer.DrawWorld;

  FHUD.Pos := Vec(Main.WindowSize.x-1.0, 0);
  FHUD.Draw;
  FInGameMenu.Draw;
  FGameOverMenu.Pos := Main.WindowSize * 0.5;
  FGameOverMenu.Draw;
  Main.ActiveFrameBuffer.BlitToWindow();
end;

end.

