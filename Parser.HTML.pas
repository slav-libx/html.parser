unit Parser.HTML;

interface

uses
  System.SysUtils,
  System.Character,
  System.Generics.Collections,
  System.JSON,
  FMX.Utils;

function HTMLParse(const Source: string): TJSONObject;
function TagIn(const Tag,Tags: string): Boolean;

implementation

{ helpful links:

https://css-live.ru/verstka/do-not-close-tags.html

}

const
  CHARS_WHITESPACES = #8#9#10#13' ';

function GetBeginTag(const Content: string; P: Integer): Integer;
begin
  Result:=Content.IndexOf('<',P);
  if Result=-1 then Result:=Content.Length;
end;

function ReadTextTo(const Content,EndText: string; var P: Integer): string;
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

function ReadEnclosedTextContent(const Content: string; var P: Integer): string;
var E: Integer;
begin
  Result:='';
  while (P<Content.Length) and (Content.Chars[P]<>'<') do
  begin
    E:=Content.IndexOfAny(#10#13'<'.ToCharArray,P);
    if E=-1 then
    begin
      P:=Content.Length;
      Exit;
    end else if E<>P then
    begin
      Result:=Result+Content.Substring(P,E-P);
      P:=E;
    end;
    while (P<Content.Length) and CHARS_WHITESPACES.Contains(Content.Chars[P]) and
      (Content.Chars[P]<>'<') do Inc(P);
  end;
end;

function ReadPreEnclosedTextContent(const Content: string; var P: Integer): string;
var E: Integer;
begin
  E:=Content.IndexOf('<',P);
  if E=-1 then E:=Content.Length;
  Result:=Content.Substring(P,E-P);
  P:=E;
end;

function ReadName(const Content: string; var P: Integer): string;
begin
  Result:='!--';
  if Content.Substring(P,3).Equals(Result) then
    Inc(P,Result.Length)
  else
  if Content.Substring(P,1).Equals('/') then
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

  Result:=GetToken(P,Content,CHARS_WHITESPACES,'''">'+StopChars); // unquoted token

  if P<Content.Length then
  if Result.IsEmpty then
  begin
    Q:=Content.Chars[P];
    if Q.IsInArray('''"'.ToCharArray) then // quoted token
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

type
  TTag = record
    Name: string;
    Attributes: TArray<string>;
    Comment: string;
    Closing: Boolean;
    SelfClosed: Boolean;
    StartPos: Integer;
    EndPos: Integer;
    function AsText: string;
    procedure Read(const Content: string; var P: Integer);
  end;

function TTag.AsText: string;
begin
  Result:='<'+Name;
  for var S in Attributes do Result:=Result+' '+S;
  if not Comment.IsEmpty then
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

procedure TTag.Read(const Content: string; var P: Integer);
var A: string;
begin

  Self:=Default(TTag);

  StartPos:=P;

  Inc(P); // '<'

  Name:=ReadName(Content,P);
  Closing:=Name.StartsWith('/');

  if Closing then
    Name:=Name.Substring(1); // trim "/"

  if Name.Equals('!--') then
    Comment:=ReadTextTo(Content,'-->',P).Trim

  else begin

    while True do
    begin
      A:=ReadAttribute(Content,P);
      if A.IsEmpty then
        Break
      else
        if A.Equals('/') then
          SelfClosed:=True
        else
          Attributes:=Attributes+[A];
    end;

    Inc(P); // '>'

  end;

  EndPos:=P;

end;

procedure AddAttribute(Tag: TJSONObject; const Attribute: string);
var P: Integer;
begin

  P:=Attribute.IndexOfAnyUnquoted(['='],'"','"');

  if P=-1 then
    Tag.AddPair(TJSONPair.Create(Attribute.Trim('''"'.ToCharArray),TJSONNull.Create))
  else
    Tag.AddPair(TJSONPair.Create(Attribute.Substring(0,P).Trim('''"'.ToCharArray),
      Attribute.Substring(P+1).Trim('''"'.ToCharArray)));

end;

function CreateTag(const Tag: TTag; const XPath: string): TJSONObject;
begin

  Result:=TJSONObject.Create;

  Result.AddPair('__source',Tag.AsText);
  Result.AddPair('__pos',TJSONNumber.Create(Tag.StartPos));
  Result.AddPair('__name',Tag.Name);
  Result.AddPair('__xpath',XPath);

  if not Tag.Comment.IsEmpty then
    Result.AddPair('__value',Tag.Comment);

  for var A in Tag.Attributes do
    AddAttribute(Result,A);

end;

type
  THTMLStack = class(TList<TJSONObject>)
  strict private
    FNames: array of string;
    function GetXPath(const D: string='/'): string;
    function GetNames(Index: Integer): string;
  public
    function LastName: string;
    procedure Push(O: TJSONObject; const Name: string); overload;
    procedure Push(const T: TTag); overload;
    procedure Close;
    procedure CloseTo(const T: string);
    function Exists(const Name: string): Boolean;
    property Names[Index: Integer]: string read GetNames;
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
    Result:=Join(D,Result,Names[I]);
end;

function THTMLStack.GetNames(Index: Integer): string;
begin
  Result:=FNames[Index];// Items[Index].GetValue('__name','');
end;

function THTMLStack.LastName: string;
begin
  Result:=Names[Count-1];
end;

procedure THTMLStack.Push(O: TJSONObject; const Name: string);
begin
  Add(O);
  FNames:=FNames+[Name.ToLower];
end;

procedure THTMLStack.Push(const T: TTag);
var Tag: TJSONObject;
begin

  Tag:=CreateTag(T,GetXPath);

  Last.AddPair('__tag',Tag);

  Push(Tag,T.Name);

end;

procedure THTMLStack.Close;
begin
  Count:=Count-1;
  SetLength(FNames,Count);
end;

procedure THTMLStack.CloseTo(const T: string);
var I: Integer;
begin

  I:=Count-1;

  while I>0 do
  begin
    if Names[I].Equals(T) then
    begin
      Count:=I;
      SetLength(FNames,Count);
      Exit;
    end else
    if TagIn(Names[I],'form,table,ul,form,section,select,table,div') then // these tags must be explicitly closed
      Exit;
    Dec(I);
  end;

end;

function THTMLStack.Exists(const Name: string): Boolean;
var I: Integer;
begin

  I:=Count-1;

  while I>0 do if Name.Equals(Names[I]) then
    Exit(True)
  else
    Dec(I);

  Result:=False;

end;

procedure HTMLGet(Result: TJSONObject; const Content: string);
var
  Stack: THTMLStack;
  TagName,Text: string;
  StartIndex: Integer;
  Tag: TTag;
begin

  Stack:=THTMLStack.Create;
  try

    Stack.Push(Result,'');

    StartIndex:=0;

    while Stack.Count>0 do
    begin

      if Stack.Exists('pre') then
        Text:=ReadPreEnclosedTextContent(Content,StartIndex)
      else
        Text:=ReadEnclosedTextContent(Content,StartIndex);

      if Text<>'' then Stack.Last.AddPair('__text',Text);

      if StartIndex>=Content.Length then Break;

      Tag.Read(Content,StartIndex);

      TagName:=Tag.Name.ToLower;

      if Tag.Closing then // </tag>
        Stack.CloseTo(TagName)

      else begin

        { implicit closing tags }

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

          Text:=ReadTextTo(Content,'</'+TagName+'>',StartIndex).Trim;

          if not Text.IsEmpty then
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
