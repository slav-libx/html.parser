unit Parser.HTML;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.JSON,
  FMX.Utils;

function HTMLParse(const Source: string): TJSONObject;

implementation

// https://css-live.ru/verstka/do-not-close-tags.html

type
  TTag = record
    Name: string;
    Attributes: TArray<string>;
    Comment: string;
    Closed: Boolean;
    SelfClosed: Boolean;
    StartPos: Integer;
    EndPos: Integer;
    function Source: string;
  end;

function TTag.Source: string;
begin
  Result:='<'+Name;
  for var S in Attributes do Result:=Result+' '+S;
  if Comment<>'' then
    Result:=Result+' '+Comment+' -->'
  else
  if Name.StartsWith('!') then
    Result:=Result+'>'
  else
  if SelfClosed then
    Result:=Result+' />'
  else
    Result:=Result+'>';
end;

function GetOpenTag(const Content: string; P: Integer): Integer;
begin
  Result:=Content.IndexOf('<',P);
  if Result=-1 then Result:=Content.Length;
end;

function ReadText(const Content,EndText: string; var P: Integer): string;
var E: Integer;
begin
  Result:='';
  E:=Content.IndexOf(EndText,P);
  if E=-1 then
    P:=Content.Length
  else begin
    Result:=Content.Substring(P,E-P);
    P:=E+EndText.Length;
  end;
end;

function ReadName(const Content: string; var P: Integer): string;
begin
  Result:='!--';
  if Content.Substring(P,3)=Result then
    Inc(P,Result.Length)
  else
  if Content.Substring(P,1)='/' then
  begin
    Inc(P);
    Result:='/'+GetToken(P,Content,#8#9#10#13' ','/>');
  end else
    Result:=GetToken(P,Content,#8#9#10#13' ','/>');
end;

function ReadAttribute(const Content: string; var P: Integer): string;
var
  Value: string;
  I: Integer;
  Q: Char;
begin
  Result:=GetToken(P,Content,#8#9#10#13' ','=>');
  if P<Content.Length then
  if Content.Chars[P]='=' then
  begin
    Inc(P); // '='
    Value:=GetToken(P,Content,#8#9#10#13' ','"''>'); // value unquoted
    if Value.IsEmpty then
    begin
      Q:=Content.Chars[P]; // value quoted "value"|'value'
      I:=Content.IndexOfAnyUnquoted(#8#9#10#13' />'.ToCharArray,Q,Q,P);
      if I>P then
      begin
        Value:=Content.Substring(P,I-P);
        P:=I;
      end;
    end;
    Result:=Result+'='+Value; // attr=value|"value"|'value'
  end;
end;

function ReadTag(const Content: string; var P: Integer): TTag;
var A: string;
begin

  Result:=Default(TTag);

  Result.StartPos:=P;
  Inc(P); // '<'
  Result.Name:=ReadName(Content,P);
  Result.Closed:=Result.Name.StartsWith('/');

  if Result.Name='!--' then
    Result.Comment:=ReadText(Content,'-->',P).Trim

  else begin

    while not Result.SelfClosed do
    begin
      A:=ReadAttribute(Content,P);
      if A.IsEmpty then Break;
      if A='/' then
        Result.SelfClosed:=True
      else
        Result.Attributes:=Result.Attributes+[A];
    end;

    Inc(P); // '>'

  end;

  Result.EndPos:=P;

end;

function CreateAttribute(const Attribute: string): TJSONPair;
var P: Integer;
begin

  P:=Attribute.IndexOf('=');

  if P=-1 then
    Result:=TJSONPair.Create(Attribute,TJSONNull.Create)
  else
    Result:=TJSONPair.Create(Attribute.Substring(0,P),Attribute.Substring(P+1).Trim(['"','''']));

end;

function CreateTag(const Tag: TTag; const XPath: string): TJSONObject;
begin

  Result:=TJSONObject.Create;

  Result.AddPair('__source',Tag.Source);
  Result.AddPair('__pos',TJSONNumber.Create(Tag.StartPos));
  Result.AddPair('__name',Tag.Name);
  Result.AddPair('__xpath',XPath);

  if Tag.Comment<>'' then
    Result.AddPair('__value',Tag.Comment);

  for var A in Tag.Attributes do
    Result.AddPair(CreateAttribute(A));

end;

procedure HTMLGet(Result: TJSONObject; const Content: string);
var Stack: TList<TJSONObject>;

  function GetXPath: string;
  begin
    Result:='';
    for var I:=1 to Stack.Count-1 do
      Result:=Result+Stack[I].GetValue('__name','')+'/';
  end;

  function Last: TJSONObject;
  begin
    Result:=Stack.Last;
  end;

  procedure Push(const T: TTag);
  var Tag: TJSONObject;
  begin
    Tag:=CreateTag(T,GetXPath);
    Last.AddPair('__tag',Tag);
    Stack.Add(Tag);
  end;

  procedure Pop;
  begin
    Stack.Count:=Stack.Count-1;
  end;

  procedure Close(const T: string);
  var N: string;
  begin
    while Stack.Count>0 do
    begin
      N:='/'+Last.GetValue('__name','').ToLower;
      if T=N then
      begin
        Pop;
        Break;
      end else begin
        if T='/p' then Break;
        if T='/li' then Break;
        if T='/td' then Break;
        if T='/dd' then Break;
        if T='/i' then Break;
        if T='/tr' then Break;
        if T='/span' then Break;

        Pop;
      end;
    end;
  end;

var
  Require,TagName,Text: string;
  StartIndex,TagIndex: Integer;
  T: TTag;
begin

  Stack:=TList<TJSONObject>.Create;
  try

  Stack.Add(Result);

  StartIndex:=0;

  while Stack.Count>0 do
  begin

    TagIndex:=StartIndex;

    StartIndex:=GetOpenTag(Content,StartIndex);

    if StartIndex>=Content.Length then Break;

    Text:=Content.Substring(TagIndex,StartIndex-TagIndex).Trim;

    if Text<>'' then Last.AddPair('__text',Text);

    T:=ReadTag(Content,StartIndex);

    TagName:=T.Name.ToLower;

    if T.Closed then
    begin
      Close(TagName);
      Continue;
    end;

    if ',address,article,aside,blockquote,div,dl,fieldset,footer,form,h1,h2,h3,h4,h5,h6,header,hr,menu,nav,ol,pre,section,table,ul,p,'.
      Contains(TagName) then
    if ',p,'.Contains(Last.GetValue('__name','').ToLower) then
      Pop;

    if ',option,'.Contains(TagName) then
    if ',option,'.Contains(Last.GetValue('__name','').ToLower) then
      Pop;

    if ',li,'.Contains(TagName) then
    if ',li,'.Contains(Last.GetValue('__name','').ToLower) then
      Pop;

    if ',dd,td,'.Contains(TagName) then
    if ',dd,td,'.Contains(Last.GetValue('__name','').ToLower) then
      Pop;

    Push(T);

    if T.SelfClosed then
      Pop
    else

    if ',script,style,textarea,'.Contains(','+TagName+',') then
    begin
      Text:=ReadText(Content,'</'+TagName+'>',StartIndex).Trim;
      if Text<>'' then
      if TagName='textarea' then
        Last.AddPair('__text',Text)
      else
        Last.AddPair('__value',Text);
      Pop;
    end else

    if TagName.StartsWith('!') then
      Pop else

    if ',link,meta,img,br,hr,input,'.Contains(','+TagName+',') then
      Pop;

  end;

  if Stack.Count<>1 then raise Exception.Create('error document structure');

  finally
    Stack.Free;
  end;

end;

function HTMLParse(const Source: string): TJSONObject;
begin
  Result:=TJSONObject.Create;
  try
    HTMLGet(Result,Source);
  except
    Result.Free;
    raise;
  end;
end;

end.
