unit fMain;

{ Viewer for Multilog messages

  Copyright (C) 2006 Luiz Am�rico Pereira C�mara
  pascalive@bol.com.br

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface
//todo: - Use only one StringGrid for Watches (???)
//      - Optimize Watch update (Cache current values?)

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, StdCtrls, MultiLog, VirtualTrees, ComCtrls, Buttons, simpleipc, WatchList,
  Menus, ATBinHex, ToggleLabel, SearchEdit;

type
  TMessageSet = 0..31;

  { TfrmMain }

  TfrmMain = class(TForm)
    BinHexViewer: TATBinHex;
    butSelectAll: TButton;
    butUnSelectAll: TButton;
    checkHeapInfo: TCheckBox;
    checkInfo: TCheckBox;
    checkMemory: TCheckBox;
    checkCustomData: TCheckBox;
    checkBitmap: TCheckBox;
    checkWarning: TCheckBox;
    checkError: TCheckBox;
    checkValue: TCheckBox;
    checkConditional: TCheckBox;
    checkCheckPoint: TCheckBox;
    checkStrings: TCheckBox;
    checkObject: TCheckBox;
    checkException: TCheckBox;
    checkCallStack: TCheckBox;
    ComboWatchHistory: TComboBox;
    imgToolbar: TImageList;
    ImgViewer: TImage;
    imgMessages: TImageList;
    Label1: TLabel;
    lbMemorySize: TLabel;
    MainMenu1: TMainMenu;
    memoViewer: TMemo;
    MIAbout: TMenuItem;
    MISep1: TMenuItem;
    MIClearAll: TMenuItem;
    MIExit: TMenuItem;
    MIHelp: TMenuItem;
    MIFile: TMenuItem;
    nbWatches: TPageControl;
    nbViewer: TNotebook;
    PageHistory: TTabSheet;
    PageBitmap: TPage;
    PageHexViewer: TPage;
    pageNull: TPage;
    pageText: TPage;
    pageSelected: TTabSheet;
    pageLastest: TTabSheet;
    PanelImageViewer: TPanel;
    panelFilter: TPanel;
    panelMessages: TPanel;
    panelViewer: TPanel;
    panelLeft: TPanel;
    panelRight: TPanel;
    EditFilterMessages: TSearchEdit;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    gridCallStack: TStringGrid;
    gridLastestWatch: TStringGrid;
    gridSelectedWatch: TStringGrid;
    Splitter4: TSplitter;
    Splitter5: TSplitter;
    GridWatchHistory: TStringGrid;
    StringGridBitmap: TStringGrid;
    ToggleOptions: TToggleLabel;
    toolbarMain: TToolBar;
    ButClear: TToolButton;
    ButStop: TToolButton;
    ButAlwaysOnTop: TToolButton;
    vtreeMessages: TVirtualStringTree;
    procedure ButAlwaysOnTopClick(Sender: TObject);
    procedure ButStopClick(Sender: TObject);
    procedure EditFilterMessagesExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure butSelectAllClick(Sender: TObject);
    procedure butUnSelectAllClick(Sender: TObject);
    procedure ClearMessages(Sender: TObject);
    procedure ComboWatchHistorySelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImgViewerDblClick(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure nbWatchesPageChanged(Sender: TObject);
    procedure PanelImageViewerDblClick(Sender: TObject);
    procedure QuitApplication(Sender: TObject);
    procedure ToggleOptionsChange(Sender: TObject);
    procedure vtreeMessagesFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vtreeMessagesFocusChanging(Sender: TBaseVirtualTree;
      OldNode: PVirtualNode; NewNode: PVirtualNode; OldColumn: TColumnIndex;
      NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure vtreeMessagesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtreeMessagesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: LongInt);
    procedure vtreeMessagesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: UTF8String);
    procedure vtreeMessagesInitNode(Sender: TBaseVirtualTree;
      ParentNode: PVirtualNode; Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    { private declarations }
    {$ifdef unix}
    FTimer: TTimer;
    {$endif}
    FTitleFilter: String;
    FActiveMessages: set of TMessageSet;
    FMessageCount: LongWord;
    FActiveWatch: TStringGrid;
    FCurrentMsg: TLogMessage;
    FLastParent: PVirtualNode;
    FLastNode: PVirtualNode;
    FIPCServer: TSimpleIPCServer;
    FWatches: TWatchList;
    FExpandParent: Boolean;
    procedure FilterCallback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure SetupFilters;
    procedure ToggleFilterSelected(CheckState:Boolean);
    procedure WatchUpdateCallback(const AVariable,AValue: String);
    procedure NewWatchVariable(const AVariable: String; AIndex: PtrInt);
    procedure ReceiveMessage(Sender: TObject);
    procedure UpdateCallStack(var ANode: PVirtualNode);
    procedure UpdateWatches;
    procedure UpdateWatchHistory;
    procedure ShowBitmapInfo(ABitmap: TBitmap);
    {$ifdef unix}
    procedure GetMessages(Sender: TObject);
    {$endif}
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  StrUtils, LCLIntf, LCLType, fAbout;

type
  TNodeData = record
    Title: String;
    MsgType: Integer;
    MsgData: TStream;
    MsgTime: TDateTime;
    Index: LongWord;
  end;
  PNodeData = ^TNodeData;

const
  PixelFormatNames: array [TPixelFormat] of String =
  (
    'pfDevice',
    'pf1bit',
    'pf4bit',
    'pf8bit',
    'pf15bit',
    'pf16bit',
    'pf24bit',
    'pf32bit',
    'pfCustom'
    );
  HandleTypeNames: array [TBitmapHandleType] of String =
  ('bmDIB',
  'bmDDB');
  
{ TfrmMain }

procedure TfrmMain.butUnSelectAllClick(Sender: TObject);
begin
  ToggleFilterSelected(False);
end;

procedure TfrmMain.ClearMessages(Sender: TObject);
begin
  vtreeMessages.Clear;
  gridCallStack.RowCount := 1;
  gridLastestWatch.RowCount := 1;
  gridSelectedWatch.RowCount := 1;
  GridWatchHistory.RowCount := 1;
  FWatches.Clear;
  ComboWatchHistory.Clear;
  //memoViewer.Lines.Clear;
  nbViewer.PageIndex:=0;//pageNull;
  FMessageCount:=0;
  FLastNode:=nil;
  FLastParent:=nil;
end;

procedure TfrmMain.ComboWatchHistorySelect(Sender: TObject);
begin
  UpdateWatchHistory;
end;

procedure TfrmMain.EditFilterMessagesExecute(Sender: TObject);
begin
  SetupFilters;
  //Scans all tree nodes
  vtreeMessages.IterateSubtree(nil,@FilterCallback,nil);
end;

procedure TfrmMain.ButStopClick(Sender: TObject);
begin
  if ButStop.Down then
    FIPCServer.OnMessage := nil
  else
    FIPCServer.OnMessage := @ReceiveMessage;
end;

procedure TfrmMain.ButAlwaysOnTopClick(Sender: TObject);
begin
  if ButAlwaysOnTop.Down then
    FormStyle := fsSystemStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if MessageDlg('Confirm Exit', 'Quit Multilog Viewer?', mtConfirmation, mbYesNo, 0) = mrNo then
    CanClose := False;
end;

procedure TfrmMain.butSelectAllClick(Sender: TObject);
begin
  ToggleFilterSelected(True);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {$ifdef unix}
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 500;
  FTimer.OnTimer := @GetMessages;
  FTimer.Enabled := True;
  {$endif}
  vtreeMessages.NodeDataSize := SizeOf(TNodeData);
  FWatches := TWatchList.Create;
  FWatches.OnUpdate := @WatchUpdateCallback;
  FWatches.OnNewVariable := @NewWatchVariable;
  FIPCServer := TSimpleIPCServer.Create(nil);
  with FIPCServer do
  begin
    ServerID := 'ipc_log_server';
    Global := True;
    OnMessage := @ReceiveMessage;
    StartServer;
  end;
  SetupFilters;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FIPCServer.Destroy;
  FWatches.Destroy;
end;

procedure TfrmMain.ImgViewerDblClick(Sender: TObject);
begin
  with ImgViewer.Picture.Bitmap do
    PanelImageViewer.Canvas.DrawFocusRect(Rect(0,0,Width + 1,Height + 1));
end;

procedure TfrmMain.MIAboutClick(Sender: TObject);
begin
  with TFormAbout.Create(Self) do
  try
    ShowModal;
  finally
    Destroy;
  end;
end;

procedure TfrmMain.MIExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.nbWatchesPageChanged(Sender: TObject);
begin
  UpdateWatches;
end;

procedure TfrmMain.PanelImageViewerDblClick(Sender: TObject);
begin
  if PanelImageViewer.Color = clBtnFace then
    PanelImageViewer.Color := clWhite
  else
    PanelImageViewer.Color := clBtnFace;
end;

procedure TfrmMain.QuitApplication(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.ToggleOptionsChange(Sender: TObject);
begin
  if ToggleOptions.Expanded then
    panelFilter.Height := checkValue.Top + checkValue.Height + 4
  else
    panelFilter.Height := EditFilterMessages.Top + EditFilterMessages.Height + 4;
end;

procedure TfrmMain.vtreeMessagesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  AStream:TStringStream;
begin
  UpdateWatches;
  with PNodeData(Sender.GetNodeData(Node))^do
  begin
    if MsgData = nil then
    begin
      nbViewer.PageIndex:=0;//pageNull;
      Exit;
    end;
    MsgData.Position:=0;
    case MsgType of
    ltStrings,ltCallStack,ltException,ltHeapInfo,ltCustomData:
    begin
      memoViewer.Lines.LoadFromStream(MsgData);
      nbViewer.PageIndex:=1;//pageText;
    end;
    ltObject:
    begin
      AStream:=TStringStream.Create('');
      ObjectBinaryToText(MsgData,AStream);
      memoViewer.Lines.Text:=AStream.DataString;
      nbViewer.PageIndex:=1;//pageText;
      AStream.Destroy;
    end;
    ltBitmap:
    begin
      ImgViewer.Picture.Bitmap.LoadFromStream(MsgData);
      nbViewer.PageIndex:=2;//PageBitmap;
      ShowBitmapInfo(ImgViewer.Picture.Bitmap);
    end;
    ltMemory:
    begin
      lbMemorySize.Caption := 'Size: ' + IntToStr(MsgData.Size);
      BinHexViewer.OpenStream(MsgData);
      nbViewer.PageIndex:=3;//PageHexViewer;
    end;
    end;
  end;
end;

procedure TfrmMain.vtreeMessagesFocusChanging(Sender: TBaseVirtualTree;
  OldNode: PVirtualNode; NewNode: PVirtualNode; OldColumn: TColumnIndex;
  NewColumn: TColumnIndex; var Allowed: Boolean);
begin
  //Todo: merge with Changed?
  //The CallStack is only updated if the parent changes
  Allowed:=OldNode <> NewNode;
  if Allowed and ((OldNode = nil) or (NewNode = nil) or (OldNode^.Parent<>NewNode^.Parent)) then
    UpdateCallStack(NewNode);
  //warning NewNode value is not more valid after here
end;

procedure TfrmMain.vtreeMessagesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  with PNodeData(Sender.GetNodeData(Node))^ do
  begin
    Title:='';
    if MsgData <> nil then
      MsgData.Destroy;
  end;
end;

procedure TfrmMain.vtreeMessagesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: LongInt);
begin
  ImageIndex:=PNodeData(Sender.GetNodeData(Node))^.MsgType;
end;

procedure TfrmMain.vtreeMessagesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: UTF8String);
begin
  CellText:=PNodeData(Sender.GetNodeData(Node))^.Title;
end;

procedure TfrmMain.vtreeMessagesInitNode(Sender: TBaseVirtualTree;
  ParentNode: PVirtualNode; Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  //WriteLn('InitNode Called');
  with PNodeData(Sender.GetNodeData(Node))^ do
  begin
    Title:=FCurrentMsg.MsgText;
    MsgData:=FCurrentMsg.Data;
    MsgTime:=FCurrentMsg.MsgTime;
    MsgType:=FCurrentMsg.MsgType;
    //In fast computers two or more messages can have the same TimeStamp
    //This leads to conflicts when determining the Watches values
    //Use an unique index instead
    Index:= FMessageCount;
    //Show only what matches filter criterias
    Sender.IsVisible[Node]:= (MsgType in [ltEnterMethod,ltExitMethod]) or
     ((MsgType in FActiveMessages) and IsWild(Title,FTitleFilter,True));
  end;
end;

procedure TfrmMain.FilterCallback(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; var Abort: Boolean);
begin
  with PNodeData(Sender.GetNodeData(Node))^ do
    Sender.IsVisible[Node]:= (MsgType in [ltEnterMethod,ltExitMethod]) or
     ((MsgType in FActiveMessages) and IsWild(Title,FTitleFilter,True));
end;

procedure TfrmMain.SetupFilters;
var
  AControl: TControl;
  i: Integer;
begin
  //Set Active Messages set used to filter
  FActiveMessages:=[4,5];//always show Enter/ExitMethod
  with panelFilter do
  for i:= 0 to ControlCount - 1 do
  begin
    AControl:=Controls[i];
    if (AControl is TCheckBox) and (TCheckBox(AControl)).Checked then
      Include(FActiveMessages, AControl.Tag);
  end;
  //Set Title Filter
  FTitleFilter:=Trim(EditFilterMessages.Text)+'*';
  if Length(FTitleFilter) > 1 then //editFilter is not empty
    FTitleFilter:='*'+FTitleFilter;
  //writeln('FFilter:', FTitleFilter);
end;

procedure TfrmMain.ToggleFilterSelected(CheckState:Boolean);
var
  AControl: TControl;
  i: Integer;
begin
  with panelFilter do
   for i:= 0 to ControlCount - 1 do
   begin
     AControl:=Controls[i];
     if (AControl is TCheckBox) then
        TCheckBox(AControl).Checked := CheckState;
   end;
end;

procedure TfrmMain.WatchUpdateCallback(const AVariable, AValue: String);
begin
  with FActiveWatch do
  begin
    RowCount:=RowCount+1;
    Cells[0,RowCount-1]:=AVariable;
    Cells[1,RowCount-1]:=AValue;
  end;
end;

procedure TfrmMain.NewWatchVariable(const AVariable: String; AIndex: PtrInt);
begin
  ComboWatchHistory.Items.AddObject(AVariable,TObject(AIndex));
end;

procedure TfrmMain.ReceiveMessage(Sender: TObject);
var
  TextSize,DataSize:Integer;
begin
  Inc(FMessageCount);
  with TSimpleIPCServer(Sender).MsgData do
  begin
    Seek(0,soFromBeginning);
    ReadBuffer(FCurrentMsg.MsgType,SizeOf(Integer));
    ReadBuffer(FCurrentMsg.MsgTime,SizeOf(TDateTime));
    ReadBuffer(TextSize,SizeOf(Integer));
    SetLength(FCurrentMsg.MsgText,TextSize);
    ReadBuffer(FCurrentMsg.MsgText[1],TextSize);
    ReadBuffer(DataSize,SizeOf(Integer));
    if DataSize > 0 then
    begin
      FCurrentMsg.Data:=TMemoryStream.Create;
      FCurrentMsg.Data.CopyFrom(TSimpleIPCServer(Sender).MsgData,DataSize);
    end
    else
      FCurrentMsg.Data:=nil;
      
    case FCurrentMsg.MsgType of
    ltEnterMethod:
    begin
      FLastNode:=vtreeMessages.AddChild(FLastParent,nil);
      if FExpandParent then
        vtreeMessages.Expanded[FLastParent]:=True
      else
        FExpandParent:=True;
      FLastParent:=FLastNode;
      vtreeMessages.ValidateNode(FLastNode,False);
    end;
    ltExitMethod:
    begin
      if (FLastParent = nil) or (FLastParent^.Parent = vtreeMessages.RootNode) then
      begin
        FLastNode:=vtreeMessages.AddChild(nil,nil);
        FLastParent:=nil;
      end
      else
      begin
        FLastNode:=vtreeMessages.AddChild(FLastParent^.Parent,nil);
        FLastParent:=FLastNode^.Parent;
      end;
      vtreeMessages.ValidateNode(FLastNode,False);
    end;
    ltWatch, ltCounter:
    begin
      FWatches.Add(FCurrentMsg.MsgText, FMessageCount, FCurrentMsg.MsgType = ltCounter);
      UpdateWatches;
    end;
    ltClear:
    begin
      ClearMessages(nil);
      FLastNode:=nil;
      FLastParent:=nil;
    end
    else
      FLastNode:=vtreeMessages.AddChild(FLastParent,nil);
      vtreeMessages.ValidateNode(FLastNode,False);
      if FExpandParent then
      begin
        vtreeMessages.Expanded[FLastParent]:=True;
        FExpandParent:=False;
      end;
    end;
  end;
end;

procedure TfrmMain.UpdateCallStack(var ANode: PVirtualNode);
var
  i:Integer;
begin
  //Writeln('UpdateCallstack');
  with vtreeMessages, gridCallStack do
  begin
    i:=GetNodeLevel(ANode);
    RowCount:=Succ(i);
    while i > 0 do
    begin
      Cells[0,i]:=PNodeData(GetNodeData(ANode^.Parent))^.Title;
      ANode:=ANode^.Parent;
      Dec(i);
    end;
  end;
end;

procedure TfrmMain.UpdateWatches;
var
  TempIndex: LongWord;
begin
  case nbWatches.PageIndex of
    0{Last},1{Selected}:
    begin
      if nbWatches.PageIndex = 0 then
      begin
        FActiveWatch := gridLastestWatch;
        TempIndex := FMessageCount;
      end
      else
      begin
        FActiveWatch := gridSelectedWatch;
        if vtreeMessages.FocusedNode <> nil then
          TempIndex := PNodeData(vtreeMessages.GetNodeData(vtreeMessages.FocusedNode))^.Index
        else
          TempIndex := 0;
      end;
      FActiveWatch.RowCount:=1;
      FWatches.Update(TempIndex);
    end;
    2{History}:
    begin
      UpdateWatchHistory;
    end;
  end;
end;

procedure TfrmMain.UpdateWatchHistory;
var
  i: Integer;
begin
  with ComboWatchHistory do
  begin
    if ItemIndex = -1 then
      Exit;
    with FWatches[PtrInt(Items.Objects[ItemIndex])] do
    begin
      GridWatchHistory.RowCount := Count + 1;
      for i := 1 to Count do
        GridWatchHistory.Cells[0,i] := Values[i-1];
    end;
  end;
end;

procedure TfrmMain.ShowBitmapInfo(ABitmap: TBitmap);
begin
  with StringGridBitmap, ABitmap do
  begin
    Cells[1,0] := IntToStr(Height);
    Cells[1,1] := IntToStr(Width);
    Cells[1,2] := PixelFormatNames[PixelFormat];
    Cells[1,3] := HandleTypeNames[HandleType];
    Cells[1,4] := '$'+IntToHex(TransparentColor,8);
  end;
end;

{$ifdef unix}
procedure TfrmMain.GetMessages(Sender: TObject);
begin
  while FIPCServer.PeekMessage(1,True) do;
end;
{$endif}

initialization

end.

