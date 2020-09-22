unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils, System.JSON,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  Parser.HTML, System.NetEncoding, FMX.Edit, FMX.Objects, FMX.ListBox;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Layout1: TRectangle;
    Label1: TLabel;
    Edit1: TEdit;
    SearchEditButton1: TSearchEditButton;
    Rectangle1: TRectangle;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem0: TListBoxItem;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SearchEditButton1Click(Sender: TObject);
    procedure ListBoxItem1Click(Sender: TObject);
    procedure ListBoxItem2Click(Sender: TObject);
    procedure ListBoxItem3Click(Sender: TObject);
    procedure ListBoxItem4Click(Sender: TObject);
    procedure ListBoxItem5Click(Sender: TObject);
    procedure ListBoxItem6Click(Sender: TObject);
    procedure ListBoxItem0Click(Sender: TObject);
  private
    Source: string;
    DOM: TJSONObject;
    procedure SetEnabledContent(Value: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetEnabledContent(False);
end;

procedure TForm1.SetEnabledContent(Value: Boolean);
begin
  ListBox1.Enabled:=Value;
  Memo1.Enabled:=Value;
end;

procedure TForm1.Button1Click(Sender: TObject);
var C: Cardinal;
begin

//  Source:='programming-csharp.ru.html';
//  Source:='googd.html';
  //Source:='yout.html';
//  Source:='page_google.html';
 // Source:='page_habrahabr-70330.html';
  Source:='page_habrahabr-index.html';
//  Source:='page_wikipedia.html';

  SetEnabledContent(False);

  DOM.Free;
  Dom:=nil;

  Source:=TFile.ReadAllText(Source,TEncoding.UTF8);

  C:=TThread.GetTickCount;
  DOM:=HTMLParse(Source);
  C:=TThread.GetTickCount-C;

  SetEnabledContent(True);

  Label1.Text:='Parsing time: '+C.ToString+' ms';

  ListBoxItem1.SetIsSelectedInternal(True,False);
  ListBoxItem1Click(nil);

end;

type
  TEnumProc = reference to procedure(Parent: TJSONObject; Pair: TJSONPair);

procedure DOMEnum(DOM: TJSONObject; EnumProc: TEnumProc);
begin
  for var Pair in DOM do
  begin
    EnumProc(DOM,Pair);
    if Pair.JsonValue is TJSONObject then
      DOMEnum(TJSONObject(Pair.JsonValue),EnumProc);
  end;
end;

procedure TForm1.ListBoxItem1Click(Sender: TObject);
begin
  Memo1.Text:=DOM.Format;
end;

procedure TForm1.ListBoxItem2Click(Sender: TObject);
var S: string;
begin

  S:='';

  DOMEnum(DOM,
  procedure(Parent: TJSONObject; Pair: TJSONPair)
  begin
    if Pair.JsonString.Value='__text' then
      S:=S+TNetEncoding.HTML.Decode(Pair.JsonValue.Value)+Memo1.Lines.LineBreak;
  end);

  Memo1.Text:=S;

end;

procedure TForm1.ListBoxItem3Click(Sender: TObject);
var S,V: string;
begin

  S:='';

  DOMEnum(DOM,
  procedure(Parent: TJSONObject; Pair: TJSONPair)
  begin
    if Pair.JsonValue.GetValue('__name','')='a' then
    begin
      V:=Pair.JsonValue.GetValue('href','');
      if V<>'' then
      begin
        S:=S+V;
        V:=Pair.JsonValue.GetValue('__text','');
        if V='' then
          V:=Pair.JsonValue.GetValue('title','');
        if V<>'' then
          S:=S+' "'+V+'"';
        S:=S+Memo1.Lines.LineBreak;
      end;
    end;
  end);

  Memo1.Text:=S;

end;

procedure TForm1.ListBoxItem4Click(Sender: TObject);
var S,V: string;
begin

  S:='';

  DOMEnum(DOM,
  procedure(Parent: TJSONObject; Pair: TJSONPair)
  begin
    if Pair.JsonValue.GetValue('__name','')='img' then
    begin
      S:=S+Pair.JsonValue.GetValue('src','');
      V:=Pair.JsonValue.GetValue('alt','');
      if V<>'' then
        S:=S+' "'+V+'"'
      else begin
        V:=Pair.JsonValue.GetValue('title','');
        if V<>'' then S:=S+' "'+V+'"';
      end;
      S:=S+' '+Pair.JsonValue.GetValue<string>('width','0')+'x'+
        Pair.JsonValue.GetValue<string>('height','0')+Memo1.Lines.LineBreak;
    end;
  end);

  Memo1.Text:=S;

end;

procedure TForm1.ListBoxItem5Click(Sender: TObject);
var S,V: string;
begin

  S:='';

  DOMEnum(DOM,
  procedure(Parent: TJSONObject; Pair: TJSONPair)
  begin
    if Pair.JsonValue.GetValue('__name','')='script' then
    begin
      V:=Pair.JsonValue.GetValue('src','');
      if V<>'' then S:=S+V+Memo1.Lines.LineBreak;
      V:=Pair.JsonValue.GetValue('__value','');
      if V<>'' then S:=S+V+Memo1.Lines.LineBreak;
      S:=S+Memo1.Lines.LineBreak;
    end;
  end);

  Memo1.Text:=S;

end;

procedure TForm1.ListBoxItem6Click(Sender: TObject);
var S,V: string;
begin

  S:='';

  DOMEnum(DOM,
  procedure(Parent: TJSONObject; Pair: TJSONPair)
  begin
    if Pair.JsonValue.GetValue('__name','')='style' then
    begin
      V:=Pair.JsonValue.GetValue('__value','');
      if V<>'' then S:=S+V+Memo1.Lines.LineBreak+Memo1.Lines.LineBreak;
    end else
    if Pair.JsonValue.GetValue('__name','')='link' then
    if Pair.JsonValue.GetValue('rel','')='stylesheet' then
    begin
      V:=Pair.JsonValue.GetValue('href','');
      if V<>'' then S:=S+V+Memo1.Lines.LineBreak+Memo1.Lines.LineBreak;
    end;
  end);

  Memo1.Text:=S;

end;

procedure TForm1.ListBoxItem0Click(Sender: TObject);
begin
  Memo1.Text:=Source;
end;

procedure TForm1.SearchEditButton1Click(Sender: TObject);
begin
  Memo1.SelStart:=Memo1.Lines.Text.IndexOf(Edit1.Text,Memo1.SelStart+Memo1.SelLength);
  Memo1.SelLength:=Edit1.Text.Length;
end;

end.
