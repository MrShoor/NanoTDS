unit gControls;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avMiniControls, avCanvas, mutils, gTypes;

type
  { TGameButton }

  TGameButton = class (TavmCustomButton)
  protected
    procedure AfterRegister; override;
    function CurrentColor: TVec4; virtual;
    procedure DoValidate; override;
  end;

  { TGamePanel }

  TGamePanel = class (TavmCustomControl)
  protected
    procedure DoValidate; override;
  end;

  { TGameLabel }

  TGameLabel = class (TavmCustomControl)
  private
    FAlign: TLineAlign;
    FText: string;
    function GetFont: TFontStyle;
    procedure SetAlign(const AValue: TLineAlign);
    procedure SetText(const AValue: string);
  protected
    procedure DoValidate; override;
    procedure AfterRegister; override;
  public
    property  Text: string read FText write SetText;
    property  Align: TLineAlign read FAlign write SetAlign;
    property  Font: TFontStyle read GetFont;
  end;

  { TGameCheckButton }

  TGameCheckButton = class (TGameButton)
  private
    FChecked: Boolean;
    procedure SetChecked(const AValue: Boolean);
    procedure ProcessClick(ASender: TObject);
  protected
    function CurrentColor: TVec4; override;
    procedure AfterRegister; override;
  public
    property Checked: Boolean read FChecked write SetChecked;
  end;

implementation

{ TGameCheckButton }

procedure TGameCheckButton.SetChecked(const AValue: Boolean);
var
  i: Integer;
  checkFound: Boolean;
begin
  if FChecked = AValue then Exit;
  FChecked := AValue;
  Invalidate;
  if FChecked then
  begin
    for i := 0 to Parent.ChildCount - 1 do
      if (Parent.Child[i] <> Self) and (Parent.Child[i] is TGameCheckButton) then
        TGameCheckButton(Parent.Child[i]).Checked := False;
  end
  else
  begin
    checkFound := False;
    for i := 0 to Parent.ChildCount - 1 do
      if (Parent.Child[i] is TGameCheckButton) then
        checkFound := checkFound or TGameCheckButton(Parent.Child[i]).Checked;
    if not checkFound then
      FChecked := True;
  end;
end;

procedure TGameCheckButton.ProcessClick(ASender: TObject);
begin
  Checked := Not Checked;
end;

function TGameCheckButton.CurrentColor: TVec4;
begin
  if Checked then
    Result := Vec(0.8,1.0,0.6,1.0)
  else
    Result := Vec(0.1,0.3,0.1,1.0);
end;

procedure TGameCheckButton.AfterRegister;
begin
  inherited AfterRegister;
  OnClick := {$IfDef FPC}@{$EndIf}ProcessClick;
end;

{ TGameLabel }

procedure TGameLabel.SetText(const AValue: string);
begin
  if FText = AValue then Exit;
  FText := AValue;
  Invalidate;
end;

procedure TGameLabel.SetAlign(const AValue: TLineAlign);
begin
  if FAlign = AValue then Exit;
  FAlign := AValue;
  Invalidate;
end;

function TGameLabel.GetFont: TFontStyle;
begin
  Result := Canvas.Font;
end;

procedure TGameLabel.DoValidate;
var
  txt: ITextLines;
begin
  Canvas.Clear;
  if Length(Text) > 0 then
  begin
    Canvas.Font.Name := cGameFont;
    with Canvas.TextBuilder do
    begin
      Align := FAlign;
      WriteLn(Text);
      txt := Finish();
      txt.VAlign := 0.5;
      txt.BoundsX := Vec(0, 0);
      txt.BoundsY := Vec(0, Size.y);
      Canvas.AddText(txt);
    end;
  end;
end;

procedure TGameLabel.AfterRegister;
begin
  inherited AfterRegister;
  Canvas.Font.Color := Vec(1,1,1,1);
  Canvas.Font.Size := 24;
end;

{ TGameButton }

procedure TGameButton.AfterRegister;
begin
  inherited AfterRegister;
  Origin := Vec(0.5, 0.5);
end;

function TGameButton.CurrentColor: TVec4;
begin
  if Downed then Exit(Vec(0.8,1.0,0.6,1.0));
  if Moved then Exit(Vec(0.4,0.7,0.3,1.0));
  Result := Vec(0.1,0.3,0.1,1.0);
end;

procedure TGameButton.DoValidate;
var
  txt: ITextLines;
begin
  Canvas.Clear;
  Canvas.Brush.Color := CurrentColor;
  Canvas.AddFill(Vec(0, 0), Size);

  if Length(Text) > 0 then
  begin
    Canvas.Font.Name := cGameFont;
    Canvas.Font.Color := Vec(0,0,0,1);
    Canvas.Font.Size := 24;
    with Canvas.TextBuilder do
    begin
      Align := laCenter;
      WriteLn(Text);
      txt := Finish();
      txt.VAlign := 0.5;
      txt.BoundsX := Vec(0, Size.x);
      txt.BoundsY := Vec(0, Size.y);
      Canvas.AddText(txt);
    end;
  end;
end;

{ TGamePanel }

procedure TGamePanel.DoValidate;
begin
  Canvas.Clear;
  Canvas.Brush.Color := Vec(0.0,0.0,0.0,0.25);
  Canvas.AddFill(Vec(0, 0), Size);
  Canvas.AddRectangle(Vec(0, 0), Size);
end;

end.

