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
    ListBoxItem8: TListBoxItem;
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
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure ListBoxItem8Click(Sender: TObject);
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

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  case Key of
  vkF:
    if ssCtrl in Shift then
    if not ComboEdit1.IsFocused then
    begin
      Key:=0;
      KeyChar:=#0;
      ComboEdit1.SetFocus;
    end;
  vkF3:
    begin
      Key:=0;
      KeyChar:=#0;
      Click(SearchEditButton2);
    end;
  end;
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

  SetEnabledContent(False);

  Memo1.Text:='';
  TreeView1.Clear;

  DOM.Free;
  DOM:=nil;

  Source:=ComboBox1.Selected.Text;

  Label1.Text:='Parsing...';

  TThread.Queue(nil,procedure
  begin

    Source:=TFile.ReadAllText(Source,TEncoding.UTF8);

    C:=TThread.GetTickCount;

    DOM:=HTMLParse(Source);

    Label1.Text:=Format('Parsing time: %d ms',[TThread.GetTickCount-C]);

    SetEnabledContent(True);

    ListBoxItem1.SetIsSelectedInternal(True,False);

    Click(ListBoxItem1);

  end);

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
    if P.JsonString.Value.Equals('__tag') then
    begin
      Item:=TTreeViewItem.Create(Parent.Owner);
      Item.Parent:=Parent;
      Item.Text:=GetDiplayText(P.JsonValue.GetValue('__source',''));
      if P.JsonValue.GetValue('__name','').StartsWith('!--') then SetItemColor(Item,claGray);
      AddTreeView(Item,TJSONObject(P.JsonValue));
    end else
    if P.JsonString.Value.Equals('__text') then
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

type
  TEnumProc = reference to procedure(Pair: TJSONPair);

procedure DOMEnum(DOM: TJSONObject; EnumProc: TEnumProc);
var Pair: TJSONPair;
begin
  for Pair in DOM do
  begin
    EnumProc(Pair);
    if Pair.JsonValue is TJSONObject then
      DOMEnum(TJSONObject(Pair.JsonValue),EnumProc);
  end;
end;

function HTMLTextDecode(const Text: string): string;
begin
  Result:=TNetEncoding.HTML.Decode(Text).
    // need optimized
    Replace('&nbsp;',Chr(160)).Replace('&larr;',Chr($2190)).
    Replace('&copy;',Chr(169)).Replace('&uarr;',Chr($2191)).
    Replace('&rarr;',Chr($2192)).Replace('&ldquo',Chr($201C)).
    Replace('&rdquo;',Chr($201D)).Replace('&hearts;',Chr($2665)).
    Replace('&times;',Chr(215)).Replace('&reg',Chr(174)).
    Replace('&ndash;',Chr($2013)).Replace('&laquo;',Chr(171)).
    Replace('&raquo;',Chr(187)).Replace('&hellip;',Chr($2026)).
    Replace('&middot;',Chr(183)).Replace('&mdash;',Chr($2014));
end;

procedure TForm1.ListBoxItem2Click(Sender: TObject);
var S: string;
begin

  ShowViews(Layout2,nil);

  S:='';

  DOMEnum(DOM,procedure(Pair: TJSONPair)
  begin
    if Pair.JsonString.Value.Equals('__text') then
      S:=S+HTMLTextDecode(Pair.JsonValue.Value)+Memo1.Lines.LineBreak;
  end);

  Memo1.Text:=S;

end;

procedure TForm1.ListBoxItem8Click(Sender: TObject);
var S: string; B: Boolean;

  procedure DoEnum(Tag: TJSONObject);
  var N: string;
  begin

    N:=Tag.GetValue('__name','');

    if TagIn(N,'svg,img') then
      S:=S+'['+N+']'
    else
    if TagIn(N,'article,pre,textarea') then
      S:=S+Memo1.Lines.LineBreak
    else
    if B then
    if TagIn(N,'br,div,ul,p,pre,option,dt,td,h1,h2,h3,h4,h5,h6') then
    begin
      B:=False;
      S:=S+Memo1.Lines.LineBreak;
    end;

    for var Pair in Tag do
    begin
      if Pair.JsonValue is TJSONObject then
        DoEnum(TJSONObject(Pair.JsonValue))
      else
      if Pair.JsonString.Value.Equals('__text') then
      begin
        B:=True;
        S:=S+HTMLTextDecode(Pair.JsonValue.Value);
      end
    end;

    if TagIn(N,'tab') then
      S:=S+' | '
    else
    if TagIn(N,'pre,textarea') then
      S:=S+Memo1.Lines.LineBreak
    else
    if B then
    if TagIn(N,'title,div,ul,li,p,pre,option,dt,td,h1,h2,h3,h4,h5,h6') then
    begin
      B:=False;
      S:=S+Memo1.Lines.LineBreak;
    end;

  end;

begin

  ShowViews(Layout2,nil);

  S:='';

  DoEnum(DOM);

  Memo1.Text:=S;

end;

procedure TForm1.ListBoxItem3Click(Sender: TObject);
var S,V: string;
begin

  ShowViews(Layout2,nil);

  S:='';

  DOMEnum(DOM,procedure(Pair: TJSONPair)
  begin
    if Pair.JsonValue.GetValue('__name','').Equals('a') then
    begin
      V:=Pair.JsonValue.GetValue('href','');
      if not V.IsEmpty then
      begin
        S:=S+V;
        V:=Pair.JsonValue.GetValue('__text','');
        if V.IsEmpty then
          V:=Pair.JsonValue.GetValue('title','');
        if not V.IsEmpty then
          S:=S+' '+V.QuotedString('"');
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

  DOMEnum(DOM,procedure(Pair: TJSONPair)
  begin
    if Pair.JsonValue.GetValue('__name','').Equals('img') then
    begin
      S:=S+Pair.JsonValue.GetValue('src','');
      V:=Pair.JsonValue.GetValue('alt','');
      if not V.IsEmpty then
        S:=S+' "'+V+'"'
      else begin
        V:=Pair.JsonValue.GetValue('title','');
        if not V.IsEmpty then S:=S+' "'+V+'"';
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

  DOMEnum(DOM,procedure(Pair: TJSONPair)
  begin
    if Pair.JsonValue.GetValue('__name','').Equals('script') then
    begin
      V:=Pair.JsonValue.GetValue('src','');
      if not V.IsEmpty then S:=S+V+Memo1.Lines.LineBreak;
      V:=Pair.JsonValue.GetValue('__value','');
      if not V.IsEmpty then S:=S+V+Memo1.Lines.LineBreak;
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

  DOMEnum(DOM,procedure(Pair: TJSONPair)
  begin
    if Pair.JsonValue.GetValue('__name','').Equals('style') then
    begin
      V:=Pair.JsonValue.GetValue('__value','');
      if not V.IsEmpty then S:=S+V+Memo1.Lines.LineBreak+Memo1.Lines.LineBreak;
    end else
    if Pair.JsonValue.GetValue('__name','')='link' then
    if Pair.JsonValue.GetValue('rel','')='stylesheet' then
    begin
      V:=Pair.JsonValue.GetValue('href','');
      if not V.IsEmpty then S:=S+V+Memo1.Lines.LineBreak+Memo1.Lines.LineBreak;
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
  if not Text.IsEmpty then Strings.Insert(0,Text);
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
