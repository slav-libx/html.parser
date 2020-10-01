unit Parser.HTML;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.JSON;

function HTMLParse(const Source: string): TJSONObject;

implementation

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
    P:=Content.IndexOfAnyUnquoted(['>'],'<','>',P);
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
  Result:=Attributes.Trim([' ']).Split([' ',#10,#13,#8,')','(','[',']'],'"','"',TStringSplitOptions.ExcludeEmpty);





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

function HTMLGet(Parent: TJSONObject; const Content: string; StartIndex: Integer): Integer;
var Tag: TJSONObject;
  S,M,N,Text: string;
  P: Integer;
begin

  Result:=GetOpenTag(Content,StartIndex);

  if Result>=Content.Length then Exit;

  P:=Result;

  Text:=Content.Substring(StartIndex,Result-StartIndex).Trim;

  if Text<>'' then Parent.AddPair('__text',Text);

  S:=ReadTag(Content,Result);

//  if string.Compare('</',0,Content,Result,2,[])=0 then Exit;

  if S.StartsWith('</') then Exit;

  Tag:=CreateTag(S,P);

  Parent.AddPair('__tag',Tag);

  N:=Tag.GetValue('__name','').ToLower;

  if ',script,style,textarea,'.Contains(','+N+',') and not S.EndsWith('/>') then
  begin
    Text:=ReadText(Content,N,Result).Trim;
    if Text<>'' then
    if N='textarea' then
      Tag.AddPair('__text',Text)
    else
      Tag.AddPair('__value',Text);
  end else
  if not ',!doctype,link,meta,img,br,input,!--,!,'.Contains(','+N+',') and not S.EndsWith('/>') then
    Result:=HTMLGet(Tag,Content,Result);

  Result:=HTMLGet(Parent,Content,Result);

end;

procedure HTMLGet2(Result: TJSONObject; const Content: string);
var
  Tag: TJSONObject;
  S,M,N,Text: string;
  P: Integer;
  Stack: TList<TJSONObject>;
  StartIndex,TagIndex: Integer;

  function GetXPath: string;
  begin
    Result:='';
    for var I:=1 to Stack.Count-1 do
      Result:=Result+Stack[I].GetValue('__name','')+'.';
  end;

begin

  Stack:=TList<TJSONObject>.Create;

  Stack.Add(Result);

  StartIndex:=0;

  while Stack.Count>0 do
  begin

    TagIndex:=GetOpenTag(Content,StartIndex);

    if TagIndex>=Content.Length then Break;

    P:=TagIndex;

    Text:=Content.Substring(StartIndex,TagIndex-StartIndex).Trim;

    if Text<>'' then Stack.Last.AddPair('__text',Text);

    S:=ReadTag(Content,TagIndex);

    StartIndex:=TagIndex;

    if S.StartsWith('</') then
    begin
      M:='</'+Stack.Last.GetValue('__name','').ToLower+'>';
      if M<>S then
      begin
        if M='</p>' then
          Stack.Count:=Stack.Count-1
        else
          Continue;
      end;
      Stack.Count:=Stack.Count-1;
      Continue;
    end;

    Tag:=CreateTag(S,P);

    if ',dl,dd,td,'.Contains(Stack.Last.GetValue('__name','').ToLower) then
    if ',dl,dd,td,'.Contains(Tag.GetValue('__name','').ToLower) then
      Stack.Count:=Stack.Count-1;

    Tag.AddPair('__xpath',GetXPath);

    Stack.Last.AddPair('__tag',Tag);
    Stack.Add(Tag);

    N:=Stack.Last.GetValue('__name','').ToLower;

    if ',script,style,textarea,'.Contains(','+N+',') and not S.EndsWith('/>') then
    begin
      Text:=ReadText(Content,N,StartIndex).Trim;
      if Text<>'' then
      if N='textarea' then
        Stack.Last.AddPair('__text',Text)
      else
        Stack.Last.AddPair('__value',Text);
      Stack.Count:=Stack.Count-1;
    end else

    if ',!doctype,link,meta,img,br,hr,input,!--,!,'.Contains(','+N+',') then
      Stack.Count:=Stack.Count-1 else

    if S.EndsWith('/>') then
      Stack.Count:=Stack.Count-1;

  end;

  if Stack.Count<>1 then raise Exception.Create('error document structure');

  Stack.Free;

end;

function HTMLParse(const Source: string): TJSONObject;
begin
  Result:=TJSONObject.Create;
  try
    //HTMLGet(Result,Source,0);
    HTMLGet2(Result,Source);
  except
    Result.Free;
    raise;
  end;
end;

end.
