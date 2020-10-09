unit Text.Attributes;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Math,
  FMX.Graphics,
  FMX.Objects,
  FMX.TextLayout;

function GetTextLayout(Text: TText): TTextLayout;
procedure ClearTextAttribute(Text: TText);
procedure AddTextAttribute(Text: TText; Pos,Length: Integer; Font: TFont; const FontColor: TAlphaColor);

implementation

type
  TTextAccess = class(TText);

function GetTextLayout(Text: TText): TTextLayout;
begin
  Result:=TTextAccess(Text).Layout;
end;

procedure ClearTextAttribute(Text: TText);
begin
  GetTextLayout(Text).ClearAttributes;
end;

procedure AddTextAttribute(Text: TText; Pos,Length: Integer; Font: TFont;
  const FontColor: TAlphaColor);
begin
  GetTextLayout(Text).AddAttribute(TTextRange.Create(Pos,Length),
    TTextAttribute.Create(Font,FontColor));
end;

end.
