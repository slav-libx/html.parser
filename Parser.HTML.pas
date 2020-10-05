unit Parser.HTML;

interface

uses
  System.SysUtils,
  System.Character,
  System.Generics.Collections,
  System.JSON,
  FMX.Utils;

function HTMLParse(const Source: string): TJSONObject;

implementation

{ helpful links:

https://css-live.ru/verstka/do-not-close-tags.html

}

type
  TTag = record
    Name: string;
    Attributes: TArray<string>;
    Comment: string;
    Closed: Boolean;
    SelfClosed: Boolean;
    StartPos: Integer;
    EndPos: Integer;
    function AsText: string;
  end;

function TTag.AsText: string;
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

const
  CHARS_WHITESPACES = #8#9#10#13' ';
  CHARS_QUOTED = '''"';

function ReadName(const Content: string; var P: Integer): string;
begin
  Result:='!--';
  if Content.Substring(P,3)=Result then
    Inc(P,Result.Length)
  else
  if Content.Substring(P,1)='/' then
  begin
    Inc(P);
    Result:='/'+GetToken(P,Content,CHARS_WHITESPACES,'/>');
  end else
    Result:=GetToken(P,Content,CHARS_WHITESPACES,'/>');
end;

function ReadToken(const Content: string; var P: Integer; const StopChars: string=''): string;
var
  I: Integer;
  Q: Char;
begin

  Result:=GetToken(P,Content,CHARS_WHITESPACES,CHARS_QUOTED+'>'+StopChars); // unquoted token

  if P<Content.Length then
  if Result.IsEmpty then
  begin
    Q:=Content.Chars[P];
    if Q.IsInArray(CHARS_QUOTED.ToCharArray) then // quoted token
    begin
      I:=Content.IndexOfAnyUnquoted((CHARS_WHITESPACES+'/>').ToCharArray,Q,Q,P);
      if I>P then
      begin
        Result:=Content.Substring(P,I-P);
        P:=I;
      end;
    end;
  end;

end;

function ReadAttribute(const Content: string; var P: Integer): string;
begin

  Result:=ReadToken(Content,P,'=');

  if P<Content.Length then
  if Content.Chars[P]='=' then
  begin
    Inc(P); // '='
    Result:=Result+'='+ReadToken(Content,P);
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

  if Result.Closed then
    Result.Name:=Result.Name.Substring(1); // trim "/"

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

procedure AddAttribute(Tag: TJSONObject; const Attribute: string);
var P: Integer;
begin

  P:=Attribute.IndexOfAnyUnquoted(['='],'"','"');

  if P=-1 then
    Tag.AddPair(TJSONPair.Create(Attribute.Trim(CHARS_QUOTED.ToCharArray),TJSONNull.Create))
  else
    Tag.AddPair(TJSONPair.Create(Attribute.Substring(0,P).Trim(CHARS_QUOTED.ToCharArray),
      Attribute.Substring(P+1).Trim(CHARS_QUOTED.ToCharArray)));

end;

function CreateTag(const Tag: TTag; const XPath: string): TJSONObject;
begin

  Result:=TJSONObject.Create;

  Result.AddPair('__source',Tag.AsText);
  Result.AddPair('__pos',TJSONNumber.Create(Tag.StartPos));
  Result.AddPair('__name',Tag.Name);
  Result.AddPair('__xpath',XPath);

  if Tag.Comment<>'' then
    Result.AddPair('__value',Tag.Comment);

  for var A in Tag.Attributes do
    AddAttribute(Result,A);

end;

type
  THTMLStack = class(TList<TJSONObject>)
  strict private
    function GetXPath(const D: string='/'): string;
  public
    function LastName: string;
    procedure Push(const T: TTag);
    procedure Close;
    procedure CloseTo(const T: string);
  end;

function TagIn(const Tag,Tags: string): Boolean;
begin
  Result:=(','+Tags+',').Contains(','+Tag+',');
end;

function Join(const D,S1,S2: string): string;
begin
  if S1.IsEmpty then
    Result:=S2
  else
  if S2.IsEmpty then
    Result:=S1
  else
    Result:=S1+D+S2;
end;

function THTMLStack.GetXPath(const D: string='/'): string;
begin
  Result:='';
  for var I:=1 to Count-1 do
    Result:=Join(D,Result,Items[I].GetValue('__name',''));
end;

function THTMLStack.LastName: string;
begin
  Result:=Last.GetValue('__name','').ToLower;
end;

procedure THTMLStack.Push(const T: TTag);
var Tag: TJSONObject;
begin

  Tag:=CreateTag(T,GetXPath);

  Last.AddPair('__tag',Tag);

  Add(Tag);

end;

procedure THTMLStack.Close;
begin
  Count:=Count-1;
end;

procedure THTMLStack.CloseTo(const T: string);
const
  TAGS_CLOSE_EXPLICITLY = 'form,table,ul,form,section,select,table,div';
var I: Integer; N: string;
begin

  I:=Count-1;

  while I>0 do
  begin
    N:=Items[I].GetValue('__name','').ToLower;
    if N=T then
    begin
      Count:=I;
      Exit;
    end else
    if TagIn(N,TAGS_CLOSE_EXPLICITLY) then
      Exit;
    Dec(I);
  end;

end;

procedure HTMLGet(Result: TJSONObject; const Content: string);
var
  Stack: THTMLStack;
  TagName,Text: string;
  StartIndex,TagIndex: Integer;
  Tag: TTag;
begin

  Stack:=THTMLStack.Create;
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

      Tag:=ReadTag(Content,StartIndex);

      TagName:=Tag.Name.ToLower;

      if Tag.Closed then // </tag>
        Stack.CloseTo(TagName)

      else begin

        { implicit closing tags }

        if TagIn(TagName,'script') and TagIn(Stack.LastName,'div') then  // ?
          Stack.Close                                                    // ?
        else

        if TagIn(TagName,'address,article,aside,blockquote,div,dl,fieldset,'+
          'footer,form,h1,h2,h3,h4,h5,h6,header,hr,menu,nav,ol,pre,section,'+
          'table,ul,p') and TagIn(Stack.LastName,'p') then
          Stack.Close
        else

        if TagIn(TagName,'option') and TagIn(Stack.LastName,'option') then
          Stack.Close
        else

        if TagIn(TagName,'li') and TagIn(Stack.LastName,'li') then
          Stack.Close
        else

        if TagIn(TagName,'dd,td') and TagIn(Stack.LastName,'dd,td') then
          Stack.Close;

        { add new tag }

        Stack.Push(Tag);

        { immediately close new tag if necessary }

        if Tag.SelfClosed then // <tag />
          Stack.Close
        else

        if TagName.StartsWith('!') then // <!-- comment --> <!doctype > etc.
          Stack.Close
        else

        if TagIn(TagName,'link,meta,img,br,hr,input') then
          Stack.Close
        else

        if TagIn(TagName,'script,style,textarea') then
        begin

          Text:=ReadText(Content,'</'+TagName+'>',StartIndex).Trim;

          if Text<>'' then
          if TagName='textarea' then
            Stack.Last.AddPair('__text',Text)
          else
            Stack.Last.AddPair('__value',Text);

          Stack.Close;

        end;

      end;

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
