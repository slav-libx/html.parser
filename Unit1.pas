unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.UIConsts, System.Classes,
  System.Variants, System.IOUtils, System.JSON,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  Parser.HTML, System.NetEncoding, FMX.Edit, FMX.Objects, FMX.ListBox,
  FMX.TreeView, System.ImageList, FMX.ImgList, FMX.ComboEdit, FMX.Styles.Objects;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Layout1: TRectangle;
    Label1: TLabel;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem0: TListBoxItem;
    ComboBox1: TComboBox;
    TreeView1: TTreeView;
    Layout2: TLayout;
    ListBoxItem7: TListBoxItem;
    ComboEdit1: TComboEdit;
    SearchEditButton2: TSearchEditButton;
    Layout3: TLayout;
    StatusBar1: TStatusBar;
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
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure ComboBox1Change(Sender: TObject);
    procedure ListBoxItem7Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboEdit1ApplyStyleLookup(Sender: TObject);
  private
    Source: string;
    DOM: TJSONObject;
    procedure SetEnabledContent(Value: Boolean);
    procedure ShowViews(MemoParent,TreeParent: TControl);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

type
  TControlAccess = class(TControl);

procedure Click(Control: TControl);
begin
  TControlAccess(Control).Click;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  for var S in TDirectory.GetFiles('html','*.*') do
    ComboBox1.Items.Add(S);
  ComboBox1.ItemIndex:=0;
  ComboBox1.ItemIndex:=ComboBox1.Items.IndexOf(
  //'html\banketservice.ru.html');
  //'html\scr.html');
  //'html\page_wikipedia.html');
  'html\intuit.lecture_1413.html');
  SetEnabledContent(False);
  Memo1.Parent:=nil;
  TreeView1.Parent:=nil;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DOM.Free;
end;

procedure TForm1.SetEnabledContent(Value: Boolean);
begin
  ListBox1.Enabled:=Value;
  Memo1.Enabled:=Value;
  TreeView1.Enabled:=Value;
end;

procedure TForm1.ShowViews(MemoParent,TreeParent: TControl);
begin
  Memo1.Parent:=MemoParent;
  TreeView1.Parent:=TreeParent;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  SetEnabledContent(False);
  Memo1.Text:='';
  DOM.Free;
  Dom:=nil;
  ShowViews(nil,nil);
end;

procedure TForm1.ComboEdit1ApplyStyleLookup(Sender: TObject);
begin
  ComboEdit1.StylesData['content.Margins.Left']:=25;
end;

procedure TForm1.Edit1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key=vkReturn then
  begin
    Key:=0;
    Click(SearchEditButton2);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var C: Cardinal;
begin

  ShowViews(nil,nil);

 // Source:='programming-csharp.ru.html';
  Source:='googd.html';
//  Source:='yout.html';
 // Source:='page_google.html';
// Source:='page_habrahabr-70330.html';
//  Source:='page_habrahabr-index.html';
//  Source:='page_wikipedia.html';
//  Source:='banketservice.ru.html';
//  Source:='dt.html';

  Source:=ComboBox1.Selected.Text;

  SetEnabledContent(False);
  Memo1.Text:='';
  TreeView1.Clear;
  DOM.Free;
  Dom:=nil;

  Label1.Text:='Parsing...';

  TThread.CreateAnonymousThread(procedure
  begin

    Source:=TFile.ReadAllText(Source,TEncoding.UTF8);

    C:=TThread.GetTickCount;
    try
    DOM:=HTMLParse(Source);
    except on E: Exception do
    begin
      DOM:=TJSONObject.Create;
      DOM.AddPair('error',E.Message);
    end end;
    C:=TThread.GetTickCount-C;

    TThread.Synchronize(nil,procedure
    begin

      Label1.Text:='Parsing time: '+C.ToString+' ms';

      SetEnabledContent(True);

      ListBoxItem1.SetIsSelectedInternal(True,False);

      Click(ListBoxItem1);

    end);

  end).Start;

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

function GetDiplayText(const Source: string): string;
begin
  Result:=Source.Replace(#13,' ').Replace(#10,' ');
end;

procedure SetItemColor(Item: TTreeViewItem; const Color: TAlphaColor);
begin
  Item.StyledSettings:=Item.StyledSettings-[TStyledSetting.FontColor];
  Item.TextSettings.FontColor:=Color;
end;

procedure AddTreeView(Parent: TControl; jsObject: TJSONObject);
var Item: TTreeViewItem;
begin
  for var P in jsObject do
  begin
    if P.JsonString.Value='__tag' then
    begin
      Item:=TTreeViewItem.Create(Parent.Owner);
      Item.Parent:=Parent;
      Item.Text:=GetDiplayText(P.JsonValue.GetValue('__source',''));
      if P.JsonValue.GetValue('__name','').StartsWith('!--') then SetItemColor(Item,claGray);
      AddTreeView(Item,TJSONObject(P.JsonValue));
    end else
    if P.JsonString.Value='__text' then
    begin
      Item:=TTreeViewItem.Create(Parent.Owner);
      Item.Parent:=Parent;
      Item.Text:=TNetEncoding.HTML.Decode(GetDiplayText(P.JsonValue.Value));
      SetItemColor(Item,claBlue);
    end;
  end;
end;

procedure TForm1.ListBoxItem0Click(Sender: TObject);
begin
  ShowViews(Layout2,nil);
  Memo1.Text:=Source;
end;

procedure TForm1.ListBoxItem7Click(Sender: TObject);
begin
  ShowViews(nil,Layout2);
  if TreeView1.Count=0 then AddTreeView(TreeView1,DOM);
end;

procedure TForm1.ListBoxItem1Click(Sender: TObject);
begin
  ShowViews(Layout2,nil);
  Memo1.Text:=DOM.Format;
end;

procedure TForm1.ListBoxItem2Click(Sender: TObject);
var S: string;
begin

  ShowViews(Layout2,nil);

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

  ShowViews(Layout2,nil);

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

  ShowViews(Layout2,nil);

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

  ShowViews(Layout2,nil);

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

  ShowViews(Layout2,nil);

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

procedure AddStrings(Strings: TStrings; const Text: string);
var P: Integer;
begin
  Strings.BeginUpdate;
  P:=Strings.IndexOf(Text);
  if P<>-1 then Strings.Delete(P);
  if Text<>'' then Strings.Insert(0,Text);
  Strings.EndUpdate;
end;

procedure TForm1.SearchEditButton1Click(Sender: TObject);
var P: Integer;
begin

  AddStrings(ComboEdit1.Items,ComboEdit1.Text);

  P:=Memo1.Lines.Text.IndexOf(ComboEdit1.Text,Memo1.SelStart+Memo1.SelLength);

  if P>=0 then
  begin
    Memo1.SelStart:=P;
    Memo1.SelLength:=ComboEdit1.Text.Length;
  end else begin
    Memo1.SelLength:=0;
    Beep;
  end;

end;

end.
