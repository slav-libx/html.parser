unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.UIConsts, System.Classes,
  System.Variants, System.IOUtils, System.JSON, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  Parser.HTML, System.NetEncoding, FMX.Edit, FMX.Objects, FMX.ListBox,
  FMX.TreeView, System.ImageList, FMX.ImgList, FMX.ComboEdit, FMX.Styles.Objects,
  Text.Attributes, FMX.TextLayout, FMX.Platform;

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
    Text1: TText;
    ScrollBox1: TScrollBox;
    Label2: TLabel;
    Button2: TButton;
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
    procedure Text1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Text1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure Text1Painting(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure ComboEdit1ChangeTracking(Sender: TObject);
  private
    Source: string;
    SourceLower: string;
    DOM: TJSONObject;
    procedure SetEnabledContent(Value: Boolean);
    procedure ShowView(Control: TControl);
  private
    Selected: TRegion;
    SelectedRange: TTextRange;
    SearchRanges: array of TTextRange;
    Search: TRegion;
    SearchIndex: Integer;
    SearchText: string;
    SearchMatchCase: Boolean;
    function SelectedText: string;
    function TextLayout: TTextLayout;
    function NeedDoSearchText(const Text: string; MatchCase: Boolean): Boolean;
    procedure DoSearchText(const Text: string; MatchCase: Boolean);
    procedure ScrollToRect(const R: TRectF);
    procedure ScrollToSearch;
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

procedure CopyToClipboard(const Text: string);
var ClipService: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService,ClipService) then
    ClipService.SetClipboard(Text);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  for var S in TDirectory.GetFiles('html','*.*') do
    ComboBox1.Items.Add(S);
  Text1.AutoCapture:=True;
  ComboBox1.ItemIndex:=0;
  ComboBox1.ItemIndex:=ComboBox1.Items.IndexOf(
  //'html\banketservice.ru.html');
  //'html\scr.html');
  //'html\page_wikipedia.html');
  'html\intuit.lecture_1413.html');
  ScrollBox1.CanFocus:=True;
  SetEnabledContent(False);
  ShowView(nil);
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
  vkC,vkInsert:
    if ssCtrl in Shift then
    if SelectedRange.Length>0 then
      CopyToClipboard(SelectedText);
  vkF3:
    begin
      Key:=0;
      KeyChar:=#0;
      Click(SearchEditButton2);
    end;
  end;
end;

procedure TForm1.Text1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in Shift then
  begin
    Selected:=nil;
    SelectedRange.Pos:=TextLayout.PositionAtPoint(PointF(X,Y));
    SelectedRange.Length:=0;
    SearchIndex:=-1;
    Text1.Repaint;
  end;
end;

procedure TForm1.Text1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if Text1.Pressed then
  begin
    SelectedRange.Length:=TextLayout.PositionAtPoint(PointF(X,Y))-SelectedRange.Pos;
    Selected:=TextLayout.RegionForRange(SelectedRange,False);
    Text1.Repaint;
  end;
end;

procedure TForm1.Text1Painting(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin

  Canvas.Fill.Kind:=TBrushKind.Solid;

  Canvas.Fill.Color:=claLightskyblue;

  for var R in Selected do
    Canvas.FillRect(R,0,0,AllCorners,1);

  Canvas.Fill.Color:=MakeColor(claWheat,0.4);

  for var I:=0 to High(Search) do
  if SearchIndex<>I then
    Canvas.FillRect(Search[I],0,0,AllCorners,1);

end;

function TForm1.SelectedText: string;
begin
  Result:=Source.Substring(SelectedRange.Pos,SelectedRange.Length);
end;

function TForm1.NeedDoSearchText(const Text: string; MatchCase: Boolean): Boolean;
begin
  Result:=not SearchText.Equals(Text) or (SearchMatchCase<>MatchCase);
  SearchText:=Text;
  SearchMatchCase:=MatchCase;
end;

procedure TForm1.DoSearchText(const Text: string; MatchCase: Boolean);
var
  R: TTextRange;
  P: Integer;
  SearchSource: string;
  SearchText: string;
begin

  SelectedRange.Length:=0;

  if NeedDoSearchText(Text,MatchCase) then
  begin

    if MatchCase then
    begin
      SearchSource:=Source;
      SearchText:=Text;
    end else begin
      SearchSource:=SourceLower;
      SearchText:=Text.ToLower;
    end;

    Search:=nil;
    SearchRanges:=nil;

    P:=0;

    R.Length:=SearchText.Length;

    while R.Length>0 do
    begin
      R.Pos:=SearchSource.IndexOf(SearchText,P);
      if R.Pos=-1 then Break;
      SearchRanges:=SearchRanges+[R];
      Search:=Search+TextLayout.RegionForRange(R,False);
      P:=R.Pos+R.Length;
    end;

    SearchIndex:=0;

  end else

    if InRange(SearchIndex,0,High(Search)) then
      Inc(SearchIndex)
    else
      SearchIndex:=0;

  if InRange(SearchIndex,0,High(Search)) then
  begin
    SelectedRange:=SearchRanges[SearchIndex];
    Selected:=[Search[SearchIndex]];
  end else begin
    SelectedRange:=Default(TTextRange);
    Selected:=nil;
  end;

  Label2.Text:=Format('Search: %d of %d',[Min(SearchIndex+1,Length(Search)),Length(Search)]);

  ScrollToSearch;

  Text1.Repaint;

end;

function ViewToRect(const View,R: TRectF): TRectF;
begin
  Result:=View;
  if R.Top<Result.Top then Result.SetLocation(Result.Left,R.Top);
  if R.Bottom>Result.Bottom then Result.SetLocation(Result.Left,R.Top);
  if R.Left<Result.Left then Result.SetLocation(0,Result.Top);
  if R.Right>Result.Right then Result.SetLocation(R.Right-Result.Width,Result.Top);
end;

procedure TForm1.ScrollToRect(const R: TRectF);
var ContentRect,ViewRect: TRectF;
begin
  ContentRect:=ScrollBox1.Content.LocalRect;
  ContentRect.Offset(ScrollBox1.ViewportPosition);
  ViewRect:=R;
  ViewRect.Inflate(R.Height,R.Height);
  ViewRect:=ViewToRect(ContentRect,ViewRect);
  if not ContentRect.EqualsTo(ViewRect) then
    ScrollBox1.ViewportPosition:=ViewRect.TopLeft;
end;

procedure TForm1.ScrollToSearch;
begin
  if InRange(SearchIndex,0,High(Search)) then
    ScrollToRect(Search[SearchIndex]);
end;

function TForm1.TextLayout: TTextLayout;
begin
  Result:=GetTextLayout(Text1);
end;

procedure TForm1.SetEnabledContent(Value: Boolean);
begin
  ListBox1.Enabled:=Value;
  Memo1.Enabled:=Value;
  TreeView1.Enabled:=Value;
end;

procedure TForm1.ShowView(Control: TControl);
begin
  for var C in Layout2.Children.ToArray do
  if C<>Control then C.Parent:=nil;
  if Assigned(Control) then Control.Parent:=Layout2;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  SetEnabledContent(False);
  Memo1.Text:='';
  DOM.Free;
  Dom:=nil;
  ShowView(nil);
  DoSearchText('',Button2.IsPressed);
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

  ShowView(nil);

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
    SourceLower:=Source.ToLower;

    C:=TThread.GetTickCount;

    DOM:=DOMCreate(Source);

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

procedure AddTreeView(Parent: TControl; Tag: TJSONObject);
var
  Item: TTreeViewItem;
  Text: string;
begin
  for var P in Tag do
  begin
    if P.JsonString.Value.Equals('__tag') then
    begin
      Item:=TTreeViewItem.Create(Parent.Owner);
      Item.Parent:=Parent;
      Item.Text:=GetDiplayText(P.JsonValue.GetValue('__source.text',''));
      if P.JsonValue.GetValue('__name','').StartsWith('!--') then SetItemColor(Item,claGray);
      AddTreeView(Item,TJSONObject(P.JsonValue));
    end else
    if P.JsonString.Value.Equals('__text') then
    begin
      Text:=P.JsonValue.GetValue('text','');
      if Text.IsEmpty then Continue;
      Text:=TNetEncoding.HTML.Decode(GetDiplayText(Text)).Trim;
      Item:=TTreeViewItem.Create(Parent.Owner);
      Item.Parent:=Parent;
      Item.Text:=Text;
      SetItemColor(Item,claBlue);
    end;
  end;
end;

procedure TForm1.ListBoxItem0Click(Sender: TObject);
begin
  ScrollBox1.ViewportPosition:=TPointF.Zero;
  GetTextLayout(Text1).BeginUpdate;
  GetTextLayout(Text1).MaxSize:=PointF(MaxInt,MaxInt);
  GetTextLayout(Text1).Text:=Source;
  ClearTextAttribute(Text1);
  DOMEnum(DOM,procedure(Pair: TJSONPair)
  var Source: TJSONObject;
  begin

    //  highlight text

    if Pair.JsonString.Value.Equals('__text') then
      AddTextAttribute(Text1,
        Pair.JsonValue.GetValue<Integer>('pos'),
        Pair.JsonValue.GetValue<Integer>('len'),
        Text1.Font,claGreen);

    if Pair.JsonString.Value.Equals('__tag') then
    begin

      //  highlight scripts

      if Pair.JsonValue.GetValue('__name','').Equals('script') then
      if Pair.JsonValue.TryGetValue('__value',Source) then
        AddTextAttribute(Text1,
          Source.GetValue<Integer>('pos'),
          Source.GetValue<Integer>('len'),
          Text1.Font,claRed);

      //  highlight styles

      if Pair.JsonValue.GetValue('__name','').Equals('style') then
      if Pair.JsonValue.TryGetValue('__value',Source) then
        AddTextAttribute(Text1,
          Source.GetValue<Integer>('pos'),
          Source.GetValue<Integer>('len'),
          Text1.Font,claBlue);

      //  highlight comments

      if Pair.JsonValue.GetValue('__name','').Equals('!--') then
      if Pair.JsonValue.TryGetValue('__source',Source) then
        AddTextAttribute(Text1,
          Source.GetValue<Integer>('pos'),
          Source.GetValue<Integer>('len'),
          Text1.Font,claGray);

      //  highlight tags

//      if Pair.JsonValue.TryGetValue('__source',Source) then
//      begin
//        AddTextAttribute(Text1, // opening tag
//          Source.GetValue<Integer>('pos'),
//          Source.GetValue<Integer>('len'),
//          Text1.Font,claRed);
//        if Source.GetValue<Integer>('cpos',0)>0 then
//        AddTextAttribute(Text1, // closing tag
//          Source.GetValue<Integer>('cpos'),
//          Source.GetValue<Integer>('clen'),
//          Text1.Font,claRed);
//      end;

    end;

  end);
  GetTextLayout(Text1).EndUpdate;
  Text1.SetBounds(0,0,GetTextLayout(Text1).TextWidth,GetTextLayout(Text1).TextHeight);
  ShowView(ScrollBox1);
end;

procedure TForm1.ListBoxItem7Click(Sender: TObject);
begin
  ShowView(TreeView1);
  if TreeView1.Count=0 then AddTreeView(TreeView1,DOM);
end;

procedure TForm1.ListBoxItem1Click(Sender: TObject);
begin
  ShowView(Memo1);
  Memo1.Text:=DOM.Format;
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

  ShowView(Memo1);

  S:='';

  DOMEnum(DOM,procedure(Pair: TJSONPair)
  begin
    if Pair.JsonString.Value.Equals('__text') then
      S:=S+HTMLTextDecode(Pair.JsonValue.GetValue('text',''))+Memo1.Lines.LineBreak;
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
      if Pair.JsonString.Value.Equals('__tag') then
        DoEnum(TJSONObject(Pair.JsonValue))
      else
      if Pair.JsonString.Value.Equals('__text') then
      begin
        B:=True;
        S:=S+HTMLTextDecode(Pair.JsonValue.GetValue('text',''));
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

  ShowView(Memo1);

  S:='';

  DoEnum(DOM);

  Memo1.Text:=S;

end;

procedure TForm1.ListBoxItem3Click(Sender: TObject);
var S,V: string;
begin

  ShowView(Memo1);

  S:='';

  DOMEnum(DOM,procedure(Pair: TJSONPair)
  begin
    if Pair.JsonValue.GetValue('__name','').Equals('a') then
    begin
      V:=Pair.JsonValue.GetValue('__attr.href','');
      if not V.IsEmpty then
      begin
        S:=S+V;
        V:=Pair.JsonValue.GetValue('__text.text','');
        if V.IsEmpty then
          V:=Pair.JsonValue.GetValue('__attr.title','');
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

  ShowView(Memo1);

  S:='';

  DOMEnum(DOM,procedure(Pair: TJSONPair)
  begin
    if Pair.JsonValue.GetValue('__name','').Equals('img') then
    begin
      S:=S+Pair.JsonValue.GetValue('__attr.src','');
      V:=Pair.JsonValue.GetValue('__attr.alt','');
      if not V.IsEmpty then
        S:=S+' "'+V+'"'
      else begin
        V:=Pair.JsonValue.GetValue('__attr.title','');
        if not V.IsEmpty then S:=S+' "'+V+'"';
      end;
      S:=S+' '+Pair.JsonValue.GetValue<string>('__attr.width','0')+'x'+
        Pair.JsonValue.GetValue<string>('__attr.height','0')+Memo1.Lines.LineBreak;
    end;
  end);

  Memo1.Text:=S;

end;

procedure TForm1.ListBoxItem5Click(Sender: TObject);
var S,V: string;
begin

  ShowView(Memo1);

  S:='';

  DOMEnum(DOM,procedure(Pair: TJSONPair)
  begin
    if Pair.JsonValue.GetValue('__name','').Equals('script') then
    begin
      V:=Pair.JsonValue.GetValue('__attr.src','');
      if not V.IsEmpty then S:=S+V+Memo1.Lines.LineBreak;
      V:=Pair.JsonValue.GetValue('__value.text','');
      if not V.IsEmpty then S:=S+V+Memo1.Lines.LineBreak;
      S:=S+Memo1.Lines.LineBreak;
    end;
  end);

  Memo1.Text:=S;

end;

procedure TForm1.ListBoxItem6Click(Sender: TObject);
var S,V: string;
begin

  ShowView(Memo1);

  S:='';

  DOMEnum(DOM,procedure(Pair: TJSONPair)
  begin
    if Pair.JsonValue.GetValue('__name','').Equals('style') then
    begin
      V:=Pair.JsonValue.GetValue('__value.text','');
      if not V.IsEmpty then S:=S+V+Memo1.Lines.LineBreak+Memo1.Lines.LineBreak;
    end else
    if Pair.JsonValue.GetValue('__name','').Equals('link') then
    if Pair.JsonValue.GetValue('__attr.rel','').Equals('stylesheet') then
    begin
      V:=Pair.JsonValue.GetValue('__attr.href','');
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

  DoSearchText(ComboEdit1.Text,Button2.IsPressed);

end;

procedure TForm1.ComboEdit1ChangeTracking(Sender: TObject);
begin
  DoSearchText(ComboEdit1.Text,Button2.IsPressed);
end;

end.
