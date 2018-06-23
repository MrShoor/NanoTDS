unit gLevel;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils,
  avRes, avTypes, avContnrs, avModel, avMesh,
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
  TModelQuality = (mqLow, mqHigh);

  { TbRobot }

  TbRobot = class(TbGameObject)
  protected
    FLookAtXZ: TVec2;
    FLastPos : TVec3;
    FPosAccum: TVec3;
    procedure SetLookAtXZ(const AValue: TVec2);
  protected
    procedure AccumulatePos;
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
    FHittedEmissiveModels: IavModelInstanceArr;

    FNormalModels: IavModelInstanceArr;
    FNormalEmissive: IavModelInstanceArr;

    FAnimation: IavAnimationController;
  protected
    procedure AfterRegister; override;
    procedure UpdateStep; override;
  public
    procedure AddModel(const AName: string; AType: TModelType = mtDefault); override;

    procedure LoadModels(Q: TModelQuality; ABotKind: TbBotKind);

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
    FLightSrc: IavPointLight;
  protected
    procedure AfterRegister; override;
    procedure UpdateStep; override;
  public
    procedure LoadModels(Q: TModelQuality);
    property SpeedXZ: TVec2 read FSpeedXZ write FSpeedXZ;
  end;
  IbBulletArr = {$IfDef FPC}specialize{$EndIf}IArray<TbBullet>;
  TbBulletArr = {$IfDef FPC}specialize{$EndIf}TArray<TbBullet>;

  { TbPlayer }

  TbPlayer = class(TbRobot)
  private
    FQuality: TModelQuality;
    FHQTop: IavModelInstance;
    FHQBot: IavModelInstance;
    FBotAnimation: IavAnimationController;
    FTopAnimation: IavAnimationController;

    FShootNextTime: Integer;
  protected
    procedure UpdateStep; override;
  public
    procedure WriteModels(const ACollection: IavModelInstanceArr; AType: TModelType); override;
    function  TryShoot(): TbBullet;
    procedure LoadModels(Q: TModelQuality);
  end;

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
    FModelQuality: TModelQuality;
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

    FPlayer: TbPlayer;
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
    procedure LoadLevel(AQuality: TModelQuality);
    procedure Draw;

    property ModelQuality: TModelQuality read FModelQuality;

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

{ TbPlayer }

procedure TbPlayer.UpdateStep;
var movedir: TVec2;
begin
  if Assigned(FHQTop) then
  begin
    AccumulatePos;

    FHQTop.Mesh.Transform := Mat4(Quat(Vec(0,-1,0), arctan2(FLookAtXZ.y, FLookAtXZ.x)), Pos+Vec(0,0.1,0));
    movedir := Vec(FPosAccum.x, FPosAccum.z);
    if LenSqr(movedir) = 0 then movedir := FLookAtXZ;
    FHQBot.Mesh.Transform := Mat4(Quat(Vec(0,-1,0), arctan2(movedir.y, movedir.x)), Pos+Vec(0,0.1,0));

    if LenSqr(FPosAccum) < 0.05 then
      FBotAnimation.AnimationStop('Walk')
    else
      FBotAnimation.AnimationStart('Walk');
    if FWorld.GameTime + 100 < FShootNextTime then FTopAnimation.AnimationStop('Shoot0');

    if Assigned(FBotAnimation) then
      FBotAnimation.SetTime(FWorld.GameTime);
    if Assigned(FTopAnimation) then
      FTopAnimation.SetTime(FWorld.GameTime);
  end
  else
    inherited UpdateStep;
end;

procedure TbPlayer.WriteModels(const ACollection: IavModelInstanceArr; AType: TModelType);
begin
  if Assigned(FHQTop) then
  begin
    if AType in [mtDefault, mtEmissive] then
    begin
      ACollection.Add(FHQTop);
      ACollection.Add(FHQBot);
    end;
  end
  else
    inherited WriteModels(ACollection, AType);
end;

function TbPlayer.TryShoot: TbBullet;
var time: Int64;
    bullet: TbBullet;
begin
  Result := nil;
  time := FWorld.GameTime;
  if time >= FShootNextTime then
  begin
    Result := TbBullet.Create(FWorld);
    Result.LoadModels(FQuality);
    Result.SpeedXZ := SetLen(LookAtXZ, 4*2);
    Result.Pos := Pos;
    Result.Rot := Quat(Vec(0,-1,0), arctan2(LookAtXZ.y, LookAtXZ.x));
    FShootNextTime := time + (1000 div 5);

    GetLightPlayer.GetStream('sounds\shot.wav').Play();
    if Assigned(FTopAnimation) then FTopAnimation.AnimationStart('Shoot0');
  end;
end;

procedure TbPlayer.LoadModels(Q: TModelQuality);
begin
  FQuality := Q;
  case Q of
    mqHigh:
      begin
        FHQBot := World.Renderer.CreateModelInstances(['BotBottomBody'])[0];
        FHQBot.Mesh.Transform := Transform();
        FHQTop := World.Renderer.CreateModelInstances(['BotTopBody'])[0];
        FHQTop.Mesh.Transform := Transform();
        FBotAnimation := Create_IavAnimationController(FHQBot.Mesh.Pose, FWorld.GameTime);
        FTopAnimation := Create_IavAnimationController(FHQTop.Mesh.Pose, FWorld.GameTime);
      end;
    mqLow: AddModel('Player');
  end;
end;

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

  if FLightSrc <> nil then
    FLightSrc.Pos := Pos + Vec(0,0.5,0);
end;

procedure TbBullet.LoadModels(Q: TModelQuality);
begin
  AddModel('Bullet');
  if Q = mqHigh then
  begin
    AddModel('Bullet', mtEmissive);
    FLightSrc := World.Renderer.CreatePointLight();
    FLightSrc.Radius := 4;
    FLightSrc.Color := Vec(0.8,0.65,0.0)*0.25;
  end;
end;

{ TbBot }

procedure TbBot.AfterRegister;
begin
  inherited AfterRegister;

end;

procedure TbBot.UpdateStep;
var lookDir: TVec3;
begin
  inherited UpdateStep;
  if FHP > 0 then
  begin
    Pos := TravelByPath(FRoute, FRouteVelocity, FRoutePos, false);
    lookDir := normalize(FRoute[FRoutePos.Idx+1] - FRoute[FRoutePos.Idx]);
    LookAtXZ := Lerp(LookAtXZ, Vec(lookDir.x, lookDir.z), 0.05);
  end;

  if Assigned(FAnimation) then FAnimation.SetTime(World.GameTime);

  if (World.GameTime < FHittedTime) and (FHP > 0) then
  begin
    FModels := FHittedModels;
    FEmissive := FHittedEmissiveModels;
  end
  else
  begin
    FModels := FNormalModels;
    FEmissive := FNormalEmissive;
  end;

  if (World.GameTime >= FHittedTime) and (FHP <= 0) then World.SafeDestroy(Self);
end;

procedure TbBot.AddModel(const AName: string; AType: TModelType);
begin
  inherited AddModel(AName, AType);
  FNormalModels := FModels;
  FNormalEmissive := FEmissive;
end;

procedure TbBot.LoadModels(Q: TModelQuality; ABotKind: TbBotKind);
  const cBotKindSuffix: array [TbBotKind] of string = ('_green', '_red');
  const cQualitySuffix: array [TModelQuality] of string = ('', 'HQ');
var s: string;
begin
  s := 'Enemy'+cQualitySuffix[Q]+cBotKindSuffix[ABotKind];
  AddModel(s);
  if Q = mqHigh then
  begin
    AddModel(s, mtEmissive);
    FHittedModels := FNormalModels;
    FHittedEmissiveModels := World.Renderer.CreateModelInstances(['Enemy_hitted']);
    FAnimation := Create_IavAnimationController(FModels[0].Mesh.Pose, FWorld.GameTime);
  end
  else
  begin
    FHittedModels := World.Renderer.CreateModelInstances(['Enemy_hitted']);
    FHittedEmissiveModels := TavModelInstanceArr.Create();
  end;
  FBotKind := ABotKind;
end;

procedure TbBot.AddDamage(const ADir: TVec3);
begin
  Dec(FHP);
  FPosAccum := FPosAccum + ADir*0.1;
  FHittedTime := World.GameTime + 200;

  GetLightPlayer.GetStream('sounds\hit.wav').Play();
  if FHP = 0 then
  begin
    case FBotKind of
      bkRed : GetLightPlayer.GetStream('sounds\red_death.wav').Play();
      bkGreen : GetLightPlayer.GetStream('sounds\green_death.wav').Play();
    end;
  end;

  if FHP = 0 then
    if Assigned(FAnimation) then
    begin
      FAnimation.AnimationStart('Death');
      FNormalEmissive.Clear();
      FHittedTime := FHittedTime + 800;
    end
    else
      World.SafeDestroy(Self);
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

procedure TbRobot.AccumulatePos;
var moveStep: TVec3;
    n: TVec3;
begin
  moveStep := Pos - FLastPos;
  if Len(moveStep) < 0.3 then
  begin
    FPosAccum := FPosAccum * 0.93;
    FPosAccum := FPosAccum + moveStep;
  end;
  FLastPos := Pos;
end;

procedure TbRobot.AfterRegister;
begin
  inherited AfterRegister;
  SubscribeForUpdateStep;
  FLookAtXZ.x := 1;
  FLookAtXZ.y := 0;
end;

procedure TbRobot.UpdateStep;
var look: TQuat;
    pitch: TQuat;
    n: TVec3;
begin
  inherited UpdateStep;
  AccumulatePos;
  pitch.v4 := Vec(0,0,0,1);
  if LenSqr(FPosAccum) > 0 then
  begin
    n := normalize( cross(Vec(0,1,0), FPosAccum) );
    pitch := Quat(n, Len(FPosAccum)*0.75);
  end;
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
  time := World.GameTime;
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
  bot.LoadModels(FModelQuality, bkRed);
  FBots.Add(bot);

  bot := TbBot.Create(FWorld);
  bot.HP := 3;
  bot.SetRoute(FBotRoutes[Random(2)+2], 2);
  bot.LoadModels(FModelQuality, bkRed);
  FBots.Add(bot);
end;

procedure TGameLevel.DoSpawnGreen(ASender: TObject);
var bot: TbBot;
begin
  bot := TbBot.Create(FWorld);
  bot.HP := 2;
  bot.SetRoute(FBotRoutes[Random(2)+4], 3);
  bot.LoadModels(FModelQuality, bkGreen);
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
      if FBoxes[i].PtInRect(Vec(FBullets[j].Pos.x, FBullets[j].Pos.z)) then
      begin
        FBullets[j].Free;
        FBullets.DeleteWithSwap(j);
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
          Inc(FScores);
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
    FGameOverMenu.ElapsedTime := FWorld.GameTime;
    FGameOverMenu.Score := FScores;
    FGameOverMenu.Visible := True;
    FHUD.HP := 0;
    GetLightPlayer.GetStream('sounds\gameover.wav').Play();
  end;
end;

procedure TGameLevel.ProcessInput;
const cPlayerSpeed = 3;
var lookPt : TVec3;
    speed  : Single;
    moveDir: TVec3;
    bullet: TbBullet;
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

  if IsKeyPressed(VK_LBUTTON) then
  begin
    bullet := FPlayer.TryShoot();
    if Assigned(bullet) then FBullets.Add(bullet);
  end;
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
  if IsKeyPressed(Ord('R')) then FWorld.Renderer.InvalidateShaders;

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
        FHUD.ElapsedTime := FWorld.GameTime;
      end;
    end;
end;

procedure TGameLevel.LoadLevel(AQuality: TModelQuality);
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
  FModelQuality := AQuality;

  Main.Camera.At := Vec(8,0,6.5);
  Main.Camera.Eye := Main.Camera.At + Vec(0,sin(deg2rad*60),-cos(deg2rad*60))*17;

  FWorld := TbWorld.Create(Self);
  case FModelQuality of
    mqHigh:
      begin
        FWorld.Renderer.PreloadModels(['level0_hq\model.avm', 'units_hq\player.avm', 'units_hq\enemies.avm', 'units\model.avm']);
        with TbGameObject.Create(FWorld) do
        begin
          AddModel('border');
          AddModel('cube');
          AddModel('floor');
          AddModel('base_decal', mtTransparent);
          AddModel('spawn_decal_green', mtTransparent);
          AddModel('spawn_decal_red', mtTransparent);
        end;
      end;
    mqLow:
      begin
        FWorld.Renderer.PreloadModels(['level0\model.avm', 'units\model.avm']);
        with TbGameObject.Create(FWorld) do
        begin
          AddModel('level0');
        end;
      end;
  end;

  FBaseHP := 5;

  FGlobalLight := FWorld.Renderer.CreatePointLight();
  FGlobalLight.Pos := Vec(0, 20, 0);
  FGlobalLight.Color := Vec(1,1,1);
  FGlobalLight.Radius := 40;
  FGlobalLight.CastShadows := True;

  FPlayer := TbPlayer.Create(FWorld);
  FPlayer.LoadModels(FModelQuality);
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

  FHUD.Pos := Vec(Main.WindowSize.x-15.0, 0);
  FHUD.Draw;
  FInGameMenu.Draw;
  FGameOverMenu.Pos := Main.WindowSize * 0.5;
  FGameOverMenu.Draw;
  Main.ActiveFrameBuffer.BlitToWindow();
end;

end.
