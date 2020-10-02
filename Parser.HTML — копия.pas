unit Parser.HTML;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.JSON;

function HTMLParse(const Source: string): TJSONObject;

implementation

// https://css-live.ru/verstka/do-not-close-tags.html

function GetOpenTag(const Content: string; P: Integer): Integer;
begin
  Result:=Content.IndexOf('<',P);
  if Result=-1 then Result:=Content.Length;
end;

function ReadTag(const Content: string; var P: Integer): string;
var
  StartIndex: Integer;
  CloseText: string;
begin

  StartIndex:=P;

  if Content.Substring(StartIndex,4)='<!--' then
  begin
    CloseText:='-->';
    P:=Content.IndexOf(CloseText,StartIndex);
  end else begin
    CloseText:='>';
    P:=Content.IndexOfAnyUnquoted(['>'],'"','"',P);
  end;

  if P<0 then
    P:=Content.Length
  else
    P:=P+CloseText.Length;

  Result:=Content.Substring(StartIndex,P-StartIndex);

end;

function ReadText(const Content,Tag: string; var P: Integer): string;
var E: Integer;
begin
  Result:='';
  E:=Content.IndexOf('</'+Tag+'>',P);
  if E=-1 then
    P:=Content.Length
  else begin
    Result:=Content.Substring(P,E-P);
    P:=E+Tag.Length+3;
  end;
end;

function SplitAttributes(const Attributes: string): TArray<string>;
begin
  Result:=Attributes.Trim([' ']).Split([' ',#10,#13,#8,')','(','[',']'],
    '"','"',TStringSplitOptions.ExcludeEmpty);
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

function CreateTag(const Tag: string; SourcePos: Integer): TJSONObject;
var S,E,L: Integer;
begin

  Result:=TJSONObject.Create;
  Result.AddPair('__source',Tag);
  Result.AddPair('__pos',TJSONNumber.Create(SourcePos));

  L:=Tag.Length;

  if Tag.StartsWith('<!') then
  begin
    S:=1;
    if Tag.StartsWith('<!--') then
      E:=4 else E:=Tag.IndexOfAny([' ','[','/',#10,#13],S);
    if E=-1 then
      Result.AddPair('__name',Tag.Substring(S,L-S))
    else begin
      Result.AddPair('__name',Tag.Substring(S,E-S));
      S:=E;
      E:=L-1;
      if Tag.EndsWith('-->') then E:=E-2;
      Result.AddPair('__value',Tag.Substring(S,E-S).Trim);
    end;
  end else begin
    S:=1;
    E:=L-1;
    if Tag.EndsWith('/>') then E:=E-1;
    for var A in SplitAttributes(Tag.Substring(S,E-S).Trim) do
    if Result.Count=2 then
      Result.AddPair('__name',A)
    else
      Result.AddPair(CreateAttribute(A));
  end;

end;

procedure HTMLGet(Result: TJSONObject; const Content: string);
var
  Tag: TJSONObject;
  S,M,N,Text: string;
  P: Integer;
  Stack: TStack<TJSONObject>;
  StartIndex,TagIndex: Integer;

  function GetXPath: string;
  begin
    Result:='';
    for var I:=1 to Stack.Count-1 do
      Result:=Result+Stack.List[I].GetValue('__name','')+'.';
  end;

begin

  Stack:=TStack<TJSONObject>.Create;

  Stack.Push(Result);

  StartIndex:=0;

  while Stack.Count>0 do
  begin

    TagIndex:=GetOpenTag(Content,StartIndex);

    if TagIndex>=Content.Length then Break;

    P:=TagIndex;

    Text:=Content.Substring(StartIndex,TagIndex-StartIndex).Trim;

    if Text<>'' then Stack.Peek.AddPair('__text',Text);

    S:=ReadTag(Content,TagIndex);

    StartIndex:=TagIndex;

    if S.StartsWith('</') then
    begin
      M:='</'+Stack.Peek.GetValue('__name','').ToLower+'>';
      if M<>S then
      begin
        if M='</p>' then
          Stack.Pop
        else
          Continue;
      end;
      Stack.Pop;
      Continue;
    end;

    Tag:=CreateTag(S,P);

    if ',dl,dd,td,'.Contains(Stack.Peek.GetValue('__name','').ToLower) then
    if ',dl,dd,td,'.Contains(Tag.GetValue('__name','').ToLower) then
      Stack.Pop;

    Tag.AddPair('__xpath',GetXPath);

    Stack.Peek.AddPair('__tag',Tag);

    Stack.Push(Tag);

    N:=Stack.Peek.GetValue('__name','').ToLower;

    if ',script,style,textarea,'.Contains(','+N+',') and not S.EndsWith('/>') then
    begin
      Text:=ReadText(Content,N,StartIndex).Trim;
      if Text<>'' then
      if N='textarea' then
        Stack.Peek.AddPair('__text',Text)
      else
        Stack.Peek.AddPair('__value',Text);
      Stack.Pop;
    end else

    if N.StartsWith('!') then
      Stack.Pop else

    if ',link,meta,img,br,hr,input,'.Contains(','+N+',') then
      Stack.Pop else

    if S.EndsWith('/>') then
      Stack.Pop;

  end;

  if Stack.Count<>1 then raise Exception.Create('error document structure');

  Stack.Free;

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
