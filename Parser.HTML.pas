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
var
  Tag: TJSONObject;
  Require,TagName,Text: string;
  Stack: TList<TJSONObject>;
  StartIndex,TagIndex: Integer;
  T: TTag;

  function GetXPath: string;
  begin
    Result:='';
    for var I:=1 to Stack.Count-1 do
      Result:=Result+Stack[I].GetValue('__name','')+'/';
  end;

  procedure Close(const T: string);
  var N: string;
  begin
    while Stack.Count>0 do
    begin
      N:='/'+Stack.Last.GetValue('__name','').ToLower;
      if T=N then
      begin
        Stack.Count:=Stack.Count-1;
        Break;
      end else begin
        if T='/p' then Break;
        if T='/li' then Break;
        if T='/td' then Break;
        if T='/dd' then Break;
        if T='/i' then Break;
        if T='/tr' then Break;
        if T='/span' then Break;
        Stack.Count:=Stack.Count-1;
      end;
    end;
  end;

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

    if Text<>'' then Stack.Last.AddPair('__text',Text);

    T:=ReadTag(Content,StartIndex);

    if T.Closed then
    begin

      Close(T.Name);
      Continue;

      Require:='/'+Stack.Last.GetValue('__name','').ToLower;
      if Require<>T.Name then
      begin

        if Require='/span' then
        if T.Name='/div' then
          Stack.Count:=Stack.Count-1;

        if Require='/div' then
        if T.Name='/span' then
        begin
          Stack.Count:=Stack.Count-1;
          Stack.Count:=Stack.Count-1;
          Continue;
        end;

        if Require='/p' then
          Stack.Count:=Stack.Count-1
        else
          Continue;

      end;
      Stack.Count:=Stack.Count-1;
      Continue;
    end;

    TagName:=T.Name.ToLower;

    if ',address,article,aside,blockquote,div,dl,fieldset,footer,form,h1,h2,h3,h4,h5,h6,header,hr,menu,nav,ol,pre,section,table,ul,p,'.
      Contains(Tag.GetValue('__name','').ToLower) then
    if ',p,'.Contains(Stack.Last.GetValue('__name','').ToLower) then
      Stack.Count:=Stack.Count-1;

    if ',option,'.Contains(Tag.GetValue('__name','').ToLower) then
    if ',option,'.Contains(Stack.Last.GetValue('__name','').ToLower) then
      Stack.Count:=Stack.Count-1;

    if ',li,'.Contains(Tag.GetValue('__name','').ToLower) then
    if ',li,'.Contains(Stack.Last.GetValue('__name','').ToLower) then
      Stack.Count:=Stack.Count-1;

    if ',dd,td,'.Contains(Tag.GetValue('__name','').ToLower) then
    if ',dd,td,'.Contains(Stack.Last.GetValue('__name','').ToLower) then
      Stack.Count:=Stack.Count-1;

    Tag:=CreateTag(T,GetXPath);

    Stack.Last.AddPair('__tag',Tag);

    Stack.Add(Tag);

    if T.SelfClosed then
      Stack.Count:=Stack.Count-1
    else

    if ',script,style,textarea,'.Contains(','+TagName+',') then
    begin
      Text:=ReadText(Content,'</'+TagName+'>',StartIndex).Trim;
      if Text<>'' then
      if TagName='textarea' then
        Stack.Last.AddPair('__text',Text)
      else
        Stack.Last.AddPair('__value',Text);
      Stack.Count:=Stack.Count-1;
    end else

    if TagName.StartsWith('!') then
      Stack.Count:=Stack.Count-1 else

    if ',link,meta,img,br,hr,input,'.Contains(','+TagName+',') then
      Stack.Count:=Stack.Count-1;

  end;

  if Stack.Count<>1 then
    raise Exception.Create('error document structure');

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
