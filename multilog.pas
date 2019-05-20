unit MultiLog;
(* nb: this unit is encoded with UTF-8 without BOM, to see this graph of events below; tools: notepad++ or notepadqq *)  

{
  Main unit of the Multilog logging system

  Copyright (C) 2006 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

step ❶:  
          |                                                              |                                    
Calling program sends events;                               There's a distibution of groups of
each event can have its                                     methods too, each group specialized in
*Why*ThisLogging's justification.                           *How* to forward an event type towards its
So, there's a distribution of groups of				channel's target
event's justifications                                                                                                              
                                                                                                              
                                                                                                              
        (lcEvents)→                                                   (lcEvents)(ltEnterMethod)→              
          .../...                                                       .../...                               
        (lcEvents)→                                                   (lcEvents)(ltExitMethod)→               
   (lcNone)→                                                       (lcNone)(ltInfo)→                          
       (lcNone)→                                                     (lcNone)(ltInfo)→                        
  (lcTrackSQLissue)→                                            (lcTrackSQLissue)→                            
     .../...                                                        .../...                                   
     (lcNone)→                                                      (lcNone)(ltValue\@integer)→               
  (lcTrackSQLissue)→                                            (lcTrackSQLissue)→                            
(lcNone)→                                                     (lcNone)(ltValue\@boolean)→                     
                                                                                                              
                                                                                                              


step ❷:
         |                                                            |
There's a distibution of groups of                            ActiveClasses acts like a wall: if the *How* to forward isn't present 
method, each group specialized in                             in the ActiveClasses's "Set Of *How*" type, then, the event doesn't go further.      
*How* to forward an event type towards its channel's target.  Let say that ActiveClasses = [ltEnterMethod, ltExitMethod, ltValue]:                 
																										                                             ↺||
(lcEvents)(ltEnterMethod)→                                              (lcEvents)(ltEnterMethod)→                                            
  .../...                                                            ↺|||
(lcEvents)(ltExitMethod)→                                               (lcEvents)(ltExitMethod)→  
(lcNone)(ltInfo)→        																				                                  ↺||
(lcTrackSQLissue)→      																			                                       ↺||
  .../...                                                            ↺|||
  (lcNone)(ltValue\@integer)→                                           (lcNone)(ltValue\@integer)→  
(lcTrackSQLissue)→              																			                               ↺||
(lcNone)(ltValue\@boolean)→                                             (lcNone)(ltValue\@boolean)→  
                                																									                    ↺||
                 																									                                ↺||



step ❸:
|                                                                                                               |
ActiveClasses acts like a wall: if the *How* to forward isn't present                               For each specialized Channel leading
in the ActiveClasses's "Set Of *How*" type, then, the event doesn't go further.                     to a display medium (TMemo, TFileText, TLogTreeView)                    
Let say that ActiveClasses = [ltEnterMethod, ltExitMethod, ltValue]:                                will receive the msg and display it.       
|
(lcEvents)(ltEnterMethod)→                                                                                    ○ + □
|                                                                                                             ○
(lcEvents)(ltExitMethod)→                                                                                       □ 
|
|
|
(lcNone)(ltValue\@integer)→                                                                                   ○                   
|                                                                                                             ○      
(lcNone)(ltValue\@boolean)→                                                                                   ○ + ▶ 
                                                                                                              ○
}                                        

{$ifdef fpc} 
{$mode objfpc}{$H+}
{$endif}

interface

uses
  {$ifndef fpc}Types, fpccompat,{$endif} Classes, SysUtils, syncobjs;

const
  //MessageTypes
  //mt (Message Type) and lt (Log Type) prefixes are used elsewhere
  //but mt is worse because there's already mtWarning and mtInformation
  //the existing lt* do not makes confusion
  (* -- distribution of function's type groups: HOW \ with WHICH method's group to log  --  *)        
  ltInfo    = 0;
  ltError   = 1;
  ltWarning = 2;
  ltValue   = 3;
  ltEnterMethod = 4;
  ltExitMethod  = 5;
  ltConditional = 6;
  ltCheckpoint = 7;
  ltStrings = 8;
  ltCallStack = 9;
  ltObject = 10;
  ltException = 11;
  ltBitmap = 12;
  ltHeapInfo = 13;
  ltMemory = 14;
  ltCustomData = 15;
  ltWatch = 20;
  ltCounter = 21;


  ltClear = 100;

  //LogClasses, convention with lc prefix
  //it's possible to define the constants to suit any need
  (* -- distribution of statictics classes of WHY, msg are logged -- *)       
  lcAll = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31];
  lcDebug = 0;
  lcError = 1;
  lcInfo = 2;
  lcWarning = 3;
  lcEvents = 4;
  //reserved
  lcUser = 8;
  
type
  TLogger = class;
  
  TDebugClass = 0..31;
  TGroupOfWhyLogMsg = TDebugClass;							// just a statistic's semantic synonym  
  
  TDebugClasses = Set of TDebugClass;
  TDebugClasses = TGroupsOfWhyLogMsg;					// just a statistic's semantic synonym 
  
  TLogMessage = record
    MsgType: Integer;
    WhyThisLogging: Integer; 
    MsgTime: TDateTime;
    MsgText: String;
    Data: TStream;
  end;

  TCustomDataNotify = function (Sender: TLogger; Data: Pointer; var DoSend: Boolean): String of Object;
  TCustomDataNotifyStatic = function (Sender: TLogger; Data: Pointer; var DoSend: Boolean): String;

  { TLogChannel }

  TLogChannel = class
  private
    FActive: Boolean;
  public
    procedure Clear; virtual; abstract;
    procedure Deliver(const AMsg: TLogMessage); virtual; abstract;
    procedure Init; virtual;
    property Active: Boolean read FActive write FActive;
  end;
  
  { TChannelList }

  TChannelList = class
  private
    FList: TFpList;
    function GetCount: Integer; {$ifdef fpc}inline;{$endif}
    function GetItems(AIndex:Integer): TLogChannel; {$ifdef fpc}inline;{$endif}
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AChannel: TLogChannel):Integer;
    procedure Remove(AChannel:TLogChannel);
    property Count: Integer read GetCount;
    property Items[AIndex:Integer]: TLogChannel read GetItems; default;
  end;

  { TLogger }

  TLogger = class
  private
    FMaxStackCount: Integer;
    FChannels: TChannelList;
    FLogStack: TStrings;
    FCheckList: TStringList;
    FCounterList: TStringList;
    FOnCustomData: TCustomDataNotify;
    FLastActiveClasses: TGroupsOfWhyLogMsg;
		FWhyThisMsg: TGroupOfWhyLogMsg;
    FThreadSafe: Boolean;
    class var FDefaultChannels: TChannelList;
    procedure GetCallStack(AStream:TStream);
    procedure SetEnabled(AValue: Boolean);
    class function GetDefaultChannels: TChannelList; static;
    function GetEnabled: Boolean;
    procedure SetMaxStackCount(const AValue: Integer);
    procedure SetThreadSafe(AValue: Boolean);
  protected
    procedure SendStream(AMsgType: Integer; const AText: String; AStream: TStream);
    procedure SendBuffer(AMsgType: Integer; const AText: String; var Buffer; Count: LongWord);
  public
    ActiveClasses: TGroupsOfWhyLogMsg;//Made a public field to allow use of include/exclude functions = [lcDebug, ...] =~ active which can be adjusted contextually in the calling program...
    DefaultClasses: TGroupsOfWhyLogMsg;//																															= [lcDebug, ...] =~ "pass-filter"'s set, blocking - or not - the logging.
    constructor Create;
    destructor Destroy; override;
    function CalledBy(const AMethodName: String): Boolean;
    procedure Clear;
    //Helper functions
    function RectToStr(const ARect: TRect): String; //inline
    function PointToStr(const APoint: TPoint): String; //inline
    //Send functions
    procedure Send(const AText: String); overload; {$ifdef fpc}inline;{$endif}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String);overload;
    procedure Send(const AText: String; Args: array of const);overload; {$ifdef fpc}inline;{$endif}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; Args: array of const);overload;
    procedure Send(const AText, AValue: String);overload; {$ifdef fpc}inline;{$endif}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText,AValue: String); overload;
    procedure Send(const AText: String; AValue: Integer); overload; {$ifdef fpc}inline;{$endif}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Integer);overload;
    {$ifdef fpc}
    procedure Send(const AText: String; AValue: Cardinal); overload; {$ifdef fpc}inline;{$endif}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Cardinal);overload;
    {$endif}
    procedure Send(const AText: String; AValue: Double); overload; {$ifdef fpc}inline;{$endif}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Double);overload;
    procedure Send(const AText: String; AValue: Int64); overload; {$ifdef fpc}inline;{$endif}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Int64);overload;
    procedure Send(const AText: String; AValue: QWord); overload; {$ifdef fpc}inline;{$endif}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: QWord);overload;
    procedure Send(const AText: String; AValue: Boolean); overload; {$ifdef fpc}inline;{$endif}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Boolean);overload;
    procedure Send(const AText: String; const ARect: TRect); overload; {$ifdef fpc}inline;{$endif}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; const ARect: TRect);overload;
    procedure Send(const AText: String; const APoint: TPoint); overload; {$ifdef fpc}inline;{$endif}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; const APoint: TPoint);overload;
    procedure Send(const AText: String; AStrList: TStrings); overload; {$ifdef fpc}inline;{$endif}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AStrList: TStrings);overload;
    procedure Send(const AText: String; AObject: TObject); overload; {$ifdef fpc}inline;{$endif}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AObject: TObject);overload;
    procedure SendPointer(const AText: String; APointer: Pointer); overload; {$ifdef fpc}inline;{$endif}
    procedure SendPointer(Classes: TGroupsOfWhyLogMsg; const AText: String; APointer: Pointer);overload;
    procedure SendCallStack(const AText: String); overload; {$ifdef fpc}inline;{$endif}
    procedure SendCallStack(Classes: TGroupsOfWhyLogMsg; const AText: String);overload;
    procedure SendException(const AText: String; AException: Exception);overload; {$ifdef fpc}inline;{$endif}
    procedure SendException(Classes: TGroupsOfWhyLogMsg; const AText: String; AException: Exception);overload;
    procedure SendHeapInfo(const AText: String); overload; {$ifdef fpc}inline;{$endif}
    procedure SendHeapInfo(Classes: TGroupsOfWhyLogMsg; const AText: String);overload;
    procedure SendMemory(const AText: String; Address: Pointer; Size: LongWord; Offset: Integer = 0); overload; {$ifdef fpc}inline;{$endif}
    procedure SendMemory(Classes: TGroupsOfWhyLogMsg; const AText: String; Address: Pointer; Size: LongWord; Offset: Integer = 0);overload;
    procedure SendIf(const AText: String; Expression: Boolean); overload; {$ifdef fpc}inline;{$endif}
    procedure SendIf(Classes: TGroupsOfWhyLogMsg; const AText: String; Expression: Boolean); overload;
    procedure SendIf(const AText: String; Expression, IsTrue: Boolean); overload; {$ifdef fpc}inline;{$endif}
    procedure SendIf(Classes: TGroupsOfWhyLogMsg; const AText: String; Expression, IsTrue: Boolean);overload;
    procedure SendWarning(const AText: String); overload; {$ifdef fpc}inline;{$endif}
    procedure SendWarning(Classes: TGroupsOfWhyLogMsg; const AText: String);overload;
    procedure SendError(const AText: String); overload; {$ifdef fpc}inline;{$endif}
    procedure SendError(Classes: TGroupsOfWhyLogMsg; const AText: String);overload;
    procedure SendCustomData(const AText: String; Data: Pointer);overload; {$ifdef fpc}inline;{$endif}
    procedure SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotify);overload; {$ifdef fpc}inline;{$endif}
    procedure SendCustomData(Classes: TGroupsOfWhyLogMsg; const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotify);overload;
    procedure SendCustomData(Classes: TGroupsOfWhyLogMsg; const AText: String; Data: Pointer);overload;
    procedure SendCustomData(Classes: TGroupsOfWhyLogMsg; const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotifyStatic);overload;
    procedure SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotifyStatic);overload; {$ifdef fpc}inline;{$endif}
    procedure AddCheckPoint;overload; {$ifdef fpc}inline;{$endif}
    procedure AddCheckPoint(Classes: TGroupsOfWhyLogMsg);overload; {$ifdef fpc}inline;{$endif}
    procedure AddCheckPoint(const CheckName: String);overload; {$ifdef fpc}inline;{$endif}
    procedure AddCheckPoint(Classes: TGroupsOfWhyLogMsg; const CheckName: String);overload;
    procedure IncCounter(const CounterName: String);overload; {$ifdef fpc}inline;{$endif}
    procedure IncCounter(Classes: TGroupsOfWhyLogMsg; const CounterName: String);overload;
    procedure DecCounter(const CounterName: String);overload; {$ifdef fpc}inline;{$endif}
    procedure DecCounter(Classes: TGroupsOfWhyLogMsg; const CounterName: String);overload;
    procedure ResetCounter(const CounterName: String);overload; {$ifdef fpc}inline;{$endif}
    procedure ResetCounter(Classes: TGroupsOfWhyLogMsg; const CounterName: String);overload;
    function GetCounter(const CounterName: String): Integer;
    procedure ResetCheckPoint;overload; {$ifdef fpc}inline;{$endif}
    procedure ResetCheckPoint(Classes: TGroupsOfWhyLogMsg);overload;
    procedure ResetCheckPoint(const CheckName: String);overload; {$ifdef fpc}inline;{$endif}
    procedure ResetCheckPoint(Classes: TGroupsOfWhyLogMsg;const CheckName: String);overload;
    procedure EnterMethod(const AMethodName: String; const AMessage: String = ''); overload; {$ifdef fpc}inline;{$endif}
    procedure EnterMethod(Classes: TGroupsOfWhyLogMsg; const AMethodName: String; const AMessage: String = ''); overload;
    procedure EnterMethod(Sender: TObject; const AMethodName: String; const AMessage: String = ''); overload; {$ifdef fpc}inline;{$endif}
    procedure EnterMethod(Classes: TGroupsOfWhyLogMsg; Sender: TObject; const AMethodName: String; const AMessage: String = '');overload;
    procedure ExitMethod(const AMethodName: String; const AMessage: String = ''); overload; {$ifdef fpc}inline;{$endif}
    procedure ExitMethod(Sender: TObject; const AMethodName: String; const AMessage: String = ''); overload; {$ifdef fpc}inline;{$endif}
    procedure ExitMethod(Classes: TGroupsOfWhyLogMsg; const AMethodName: String; const AMessage: String = ''); overload; {$ifdef fpc}inline;{$endif}
    procedure ExitMethod({%H-}Classes: TGroupsOfWhyLogMsg; Sender: TObject; const AMethodName: String; const AMessage: String = '');overload;
    procedure Watch(const AText, AValue: String); overload; {$ifdef fpc}inline;{$endif}
    procedure Watch(Classes: TGroupsOfWhyLogMsg; const AText,AValue: String);overload;
    procedure Watch(const AText: String; AValue: Integer); overload; {$ifdef fpc}inline;{$endif}
    procedure Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Integer);overload;
    {$ifdef fpc}
    procedure Watch(const AText: String; AValue: Cardinal); overload; {$ifdef fpc}inline;{$endif}
    procedure Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Cardinal);overload;
    {$endif}
    procedure Watch(const AText: String; AValue: Double); overload; {$ifdef fpc}inline;{$endif}
    procedure Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Double);overload;
    procedure Watch(const AText: String; AValue: Boolean); overload; {$ifdef fpc}inline;{$endif}
    procedure Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Boolean);overload;
    class property DefaultChannels: TChannelList read GetDefaultChannels;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Channels: TChannelList read FChannels;
    property LogStack: TStrings read FLogStack;
    property MaxStackCount: Integer read FMaxStackCount write SetMaxStackCount;
    property OnCustomData: TCustomDataNotify read FOnCustomData write FOnCustomData;
    property ThreadSafe: Boolean read FThreadSafe write SetThreadSafe;
    property WhyThisMsg: TGroupOfWhyLogMsg read FWhyThisMsg write FWhyThisMsg default lcNone;
  end;

 { TLogChannelWrapper }

  TLogChannelWrapper = class(TComponent)
  private
    FChannel: TLogChannel;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property Channel: TLogChannel read FChannel write FChannel;
  end;

var
  Logger: TLogger;

implementation

const
  DefaultCheckName = 'CheckPoint';

function FormatNumber(Value: Integer):String;
var
  TempStr:String;
  i, Digits:Integer;
begin
  Digits:=0;
  Result:='';
  TempStr:=IntToStr(Value);
  for i := length(TempStr) downto 1 do
  begin
    //todo: implement using mod() -> get rids of digits
    if Digits = 3 then
    begin
      Digits:=0;
      Result:=DefaultFormatSettings.ThousandSeparator+Result;
    end;
    Result:=TempStr[i]+Result;
    Inc(Digits);
  end;
end;

type

  TFixedCriticalSection = class(TCriticalSection)
  private
    {$WARN 5029 off : Private field "$1.$2" is never used} // FDummy not used anywhere so switch off such warnings
    FDummy: array [0..95] of Byte; // fix multiprocessor cache safety http://blog.synopse.info/post/2016/01/09/Safe-locks-for-multi-thread-applications
  end;

  TGuardian = TFixedCriticalSection;

var
  Guardian: TGuardian;

{ TLogger }

procedure TLogger.GetCallStack(AStream: TStream);
{$ifdef fpc}
var
  i : Longint;
  prevbp : Pointer;
  caller_frame,
  caller_addr,
  bp : Pointer;
  S:String;
{$endif}
begin
  {$ifdef fpc}
  //routine adapted from fpc source

  //This trick skip SendCallstack item
  //bp:=get_frame;
  bp:= get_caller_frame(get_frame);
  try
    prevbp:=bp-1;
    i:=0;
    //is_dev:=do_isdevice(textrec(f).Handle);
    while bp > prevbp Do
     begin
       caller_addr := get_caller_addr(bp);
       caller_frame := get_caller_frame(bp);
       if (caller_addr=nil) then
         break;
       //todo: see what is faster concatenate string and use writebuffer or current
       S:=BackTraceStrFunc(caller_addr)+LineEnding;
       AStream.WriteBuffer(S[1],Length(S));
       Inc(i);
       if (i>=FMaxStackCount) or (caller_frame=nil) then
         break;
       prevbp:=bp;
       bp:=caller_frame;
     end;
   except
     { prevent endless dump if an exception occured }
   end;
  {$endif} 
end;

procedure TLogger.SetEnabled(AValue: Boolean);
begin
  if AValue then
  begin
    if ActiveClasses = [] then
      ActiveClasses := FLastActiveClasses;
  end
  else
  begin
    FLastActiveClasses := ActiveClasses;
    ActiveClasses := [];
  end;
end;

class function TLogger.GetDefaultChannels: TChannelList;
begin
  if FDefaultChannels = nil then
    FDefaultChannels := TChannelList.Create;
  Result := FDefaultChannels;
end;

function TLogger.GetEnabled: Boolean;
begin
  Result:=ActiveClasses <> [];
end;

procedure DispatchLogMessage(Channels: TChannelList; const Msg: TLogMessage);
var
  i: Integer;
  Channel: TLogChannel;
begin
  for i := 0 to Channels.Count - 1 do
  begin
    Channel := Channels[i];
    if Channel.Active then
      Channel.Deliver(Msg);
  end;
end;

procedure TLogger.SendStream(AMsgType: Integer; const AText: String; AStream: TStream);
var
  Msg: TLogMessage;
begin
  with Msg do
  begin
    MsgType := AMsgType;
    WhyThisLogging:= FWhyThisMsg;
    MsgTime := Now;
    MsgText := AText;
    Data := AStream;
  end;
  if FThreadSafe then
    Multilog.Guardian.Enter;	// Yes: it's a global variable created in this unit, and used in this unit only ;-)
  if FDefaultChannels <> nil then
    DispatchLogMessage(FDefaultChannels, Msg);
  DispatchLogMessage(Channels, Msg);
  if FThreadSafe then
    Multilog.Guardian.Leave;
  AStream.Free;
end;

procedure TLogger.SendBuffer(AMsgType: Integer; const AText: String; var Buffer; Count: LongWord);
var
  AStream: TStream;
begin
  if Count > 0 then
  begin
    AStream:=TMemoryStream.Create;
    AStream.Write(Buffer,Count);
  end
  else
    AStream:=nil;
  //nb: SendStream will free AStream
  SendStream(AMsgType,AText,AStream);
end;

procedure TLogger.SetMaxStackCount(const AValue: Integer);
begin
  if AValue < 256 then
    FMaxStackCount := AValue
  else
    FMaxStackCount := 256;
end;

procedure TLogger.SetThreadSafe(AValue: Boolean);
begin
  FThreadSafe := AValue;
  if AValue and not Assigned(Guardian) then
    Multilog.Guardian := TGuardian.Create;
end;

constructor TLogger.Create;
begin
  FChannels := TChannelList.Create;
  FMaxStackCount := 20;
  FLogStack := TStringList.Create;
  FCheckList := TStringList.Create;
  with FCheckList do
  begin
    CaseSensitive := False;
    Sorted := True; //Faster IndexOf?
  end;
  FCounterList := TStringList.Create;
  with FCounterList do
  begin
    CaseSensitive := False;
    Sorted := True; //Faster IndexOf?
  end;
  ActiveClasses := [0]; 		//categor{y|ies} of Why this loging = lcDebug
  DefaultClasses := [0];		//categor{y|ies} of Why this loging = lcDebug
end;

destructor TLogger.Destroy;
begin
  FChannels.Destroy;
  FLogStack.Destroy;
  FCheckList.Destroy;
  FCounterList.Destroy;
end;

function TLogger.CalledBy(const AMethodName: String): Boolean;
begin
  Result:=FLogStack.IndexOf(UpperCase(AMethodName)) <> -1;
end;

procedure ClearChannels(Channels: TChannelList);
var
  i: Integer;
  Channel: TLogChannel;
begin
  for i := 0 to Channels.Count - 1 do
  begin
    Channel := Channels[i];
    if Channel.Active then
      Channel.Clear;
  end;
end;

procedure TLogger.Clear;
begin
  if FDefaultChannels <> nil then
    ClearChannels(FDefaultChannels);
  ClearChannels(Channels);
end;

function TLogger.RectToStr(const ARect: TRect): String;
begin
  with ARect do
    Result:=Format('(Left: %d; Top: %d; Right: %d; Bottom: %d)',[Left,Top,Right,Bottom]);
end;

function TLogger.PointToStr(const APoint: TPoint): String;
begin
  with APoint do
    Result:=Format('(X: %d; Y: %d)',[X,Y]);
end;


{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltInfo's loging method.
-------------------------------------------------------------------}
procedure TLogger.Send(const AText: String);
begin
  Send(DefaultClasses,AText);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltInfo's loging method.
-------------------------------------------------------------------}
procedure TLogger.Send(const AText: String; Args: array of const);
begin
  Send(DefaultClasses,AText,Args);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's loging method.
-------------------------------------------------------------------}
procedure TLogger.Send(const AText, AValue: String);
begin
  Send(DefaultClasses,AText,AValue);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's loging method.
-------------------------------------------------------------------}
procedure TLogger.Send(const AText: String; AValue: Integer);
begin
  Send(DefaultClasses,AText,AValue);
end;

{$ifdef fpc}
{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's loging method.
-------------------------------------------------------------------}
procedure TLogger.Send(const AText: String; AValue: Cardinal);
begin
  Send(DefaultClasses,AText,AValue);
end;
{$endif}

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's loging method.
-------------------------------------------------------------------}
procedure TLogger.Send(const AText: String; AValue: Double);
begin
  Send(DefaultClasses,AText,AValue);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's loging method.
-------------------------------------------------------------------}
procedure TLogger.Send(const AText: String; AValue: Int64);
begin
  Send(DefaultClasses,AText,AValue);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's loging method.
-------------------------------------------------------------------}
procedure TLogger.Send(const AText: String; AValue: QWord);
begin
  Send(DefaultClasses,AText,AValue);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's loging method.
-------------------------------------------------------------------}
procedure TLogger.Send(const AText: String; AValue: Boolean);
begin
  Send(DefaultClasses, AText, AValue);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's loging method.
-------------------------------------------------------------------}
procedure TLogger.Send(const AText: String; const ARect: TRect);
begin
  Send(DefaultClasses,AText,ARect);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's loging method.
-------------------------------------------------------------------}
procedure TLogger.Send(const AText: String; const APoint: TPoint);
begin
  Send(DefaultClasses,AText,APoint);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltStrings's loging method.
-------------------------------------------------------------------}
procedure TLogger.Send(const AText: String; AStrList: TStrings);
begin
  Send(DefaultClasses,AText,AStrList);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltObject's loging method.
-------------------------------------------------------------------}
procedure TLogger.Send(const AText: String; AObject: TObject);
begin
  Send(DefaultClasses,AText,AObject);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's loging method.
-------------------------------------------------------------------}
procedure TLogger.SendPointer(const AText: String; APointer: Pointer);
begin
  SendPointer(DefaultClasses,AText,APointer);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCallStack's loging method.
-------------------------------------------------------------------}
procedure TLogger.SendCallStack(const AText: String);
begin
  SendCallStack(DefaultClasses,AText);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltException's loging method.
-------------------------------------------------------------------}
procedure TLogger.SendException(const AText: String; AException: Exception);
begin
  SendException(DefaultClasses,AText,AException);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltHeapInfo's loging method.
-------------------------------------------------------------------}
procedure TLogger.SendHeapInfo(const AText: String);
begin
  SendHeapInfo(DefaultClasses,AText);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltMemory's loging method.
-------------------------------------------------------------------}
procedure TLogger.SendMemory(const AText: String; Address: Pointer; Size: LongWord; Offset: Integer);
begin
  SendMemory(DefaultClasses,AText,Address,Size,Offset)
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltConditional's loging method.
-------------------------------------------------------------------}
procedure TLogger.SendIf(const AText: String; Expression: Boolean);
begin
  SendIf(DefaultClasses,AText,Expression,True);
end;

{------------------------------------------------------------------
 §Explanations: loging;
this ltConditional method with overloaded specific parameter Classes, that allows us to pass or not [lcDebug, ...],
to verify an existing intersection with ActiveClasses.
-------------------------------------------------------------------}
procedure TLogger.SendIf(Classes: TGroupsOfWhyLogMsg; const AText: String; Expression: Boolean);
begin
  SendIf(Classes,AText,Expression,True);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltConditional's loging method.
-------------------------------------------------------------------}
procedure TLogger.SendIf(const AText: String; Expression, IsTrue: Boolean);
begin
  SendIf(DefaultClasses,AText,Expression,IsTrue);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltWarning's loging method.
-------------------------------------------------------------------}
procedure TLogger.SendWarning(const AText: String);
begin
  SendWarning(DefaultClasses,AText);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltError's loging method.
-------------------------------------------------------------------}
procedure TLogger.SendError(const AText: String);
begin
  SendError(DefaultClasses,AText);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCustomData's loging method.
-------------------------------------------------------------------}
procedure TLogger.SendCustomData(const AText: String; Data: Pointer);
begin
  SendCustomData(DefaultClasses,AText,Data,FOnCustomData);
end;

{------------------------------------------------------------------
 §Explanations: loging;
this ltCustomData method with overloaded specific parameter Classes, that allows us to pass or not [lcDebug, ...],
to verify an existing intersection with ActiveClasses.
-------------------------------------------------------------------}
procedure TLogger.SendCustomData(Classes: TGroupsOfWhyLogMsg; const AText: String; Data: Pointer);
begin
  SendCustomData(Classes,AText,Data,FOnCustomData);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCustomData's loging method.
-------------------------------------------------------------------}
procedure TLogger.SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotify);
begin
  SendCustomData(DefaultClasses,AText,Data,CustomDataFunction);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCustomData's loging method.
-------------------------------------------------------------------}
procedure TLogger.SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotifyStatic);
begin
  SendCustomData(DefaultClasses,AText,Data,CustomDataFunction);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCheckpoint's loging method.
-------------------------------------------------------------------}
procedure TLogger.AddCheckPoint;
begin
  AddCheckPoint(DefaultClasses,DefaultCheckName);
end;

{------------------------------------------------------------------
§Explanations: loging;
This ltCheckpoint method with overloaded specific parameter Classes, that allows us to pass or not [lcDebug, ...],
to verify an existing intersection with ActiveClasses.
-------------------------------------------------------------------}
procedure TLogger.AddCheckPoint(Classes: TGroupsOfWhyLogMsg);
begin
  AddCheckPoint(Classes,DefaultCheckName);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCheckpoint's loging method.
-------------------------------------------------------------------}
procedure TLogger.AddCheckPoint(const CheckName: String);
begin
  AddCheckPoint(DefaultClasses,CheckName);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCheckpoint's loging method.
-------------------------------------------------------------------}
procedure TLogger.ResetCheckPoint;
begin
  ResetCheckPoint(DefaultClasses,DefaultCheckName);
end;

{------------------------------------------------------------------
§Explanations: loging;
This ltCheckpoint method with overloaded specific parameter Classes, that allows us to pass or not [lcDebug, ...],
to verify an existing intersection with ActiveClasses.
-------------------------------------------------------------------}
procedure TLogger.ResetCheckPoint(Classes: TGroupsOfWhyLogMsg);
begin
  ResetCheckPoint(Classes,DefaultCheckName);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCheckpoint's loging method.
-------------------------------------------------------------------}
procedure TLogger.ResetCheckPoint(const CheckName: String);
begin
  ResetCheckPoint(DefaultClasses,CheckName);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCounter's loging method.
-------------------------------------------------------------------}
procedure TLogger.IncCounter(const CounterName: String);
begin
  IncCounter(DefaultClasses,CounterName);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCounter's loging method.
-------------------------------------------------------------------}
procedure TLogger.DecCounter(const CounterName: String);
begin
  DecCounter(DefaultClasses,CounterName);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCounter's loging method.
-------------------------------------------------------------------}
procedure TLogger.ResetCounter(const CounterName: String);
begin
  ResetCounter(DefaultClasses,CounterName);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltEntertMethod's loging method.
-------------------------------------------------------------------}
procedure TLogger.EnterMethod(const AMethodName: String; const AMessage: String);
begin
  EnterMethod(DefaultClasses,nil,AMethodName,AMessage);
end;

{------------------------------------------------------------------
§Explanations: loging;
This ltEntertMethod method with overloaded specific parameter Classes, that allows us to pass or not [lcDebug, ...],
to verify an existing intersection with ActiveClasses.
-------------------------------------------------------------------}
procedure TLogger.EnterMethod(Classes: TGroupsOfWhyLogMsg; const AMethodName: String; const AMessage: String);
begin
  EnterMethod(Classes,nil,AMethodName,AMessage);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltEntertMethod's loging method.
-------------------------------------------------------------------}
procedure TLogger.EnterMethod(Sender: TObject; const AMethodName: String; const AMessage: String);
begin
  EnterMethod(DefaultClasses,Sender,AMethodName,AMessage);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltExitMethod's loging method.
-------------------------------------------------------------------}
procedure TLogger.ExitMethod(const AMethodName: String; const AMessage: String);
begin
  ExitMethod(DefaultClasses,nil,AMethodName,AMessage);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltExitMethod's loging method.
-------------------------------------------------------------------}
procedure TLogger.ExitMethod(Sender: TObject; const AMethodName: String; const AMessage: String);
begin
  ExitMethod(DefaultClasses,Sender,AMethodName,AMessage);
end;

{------------------------------------------------------------------
§Explanations: loging;
This ltExitMethod method with overloaded specific parameter Classes, that allows us to pass or not [lcDebug, ...],
to verify an existing intersection with ActiveClasses.
-------------------------------------------------------------------}
procedure TLogger.ExitMethod(Classes: TGroupsOfWhyLogMsg; const AMethodName: String; const AMessage: String);
begin
  ExitMethod(Classes,nil,AMethodName,AMessage);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltWatch's loging method.
-------------------------------------------------------------------}
procedure TLogger.Watch(const AText, AValue: String);
begin
  Watch(DefaultClasses,AText,AValue);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltWatch's loging method.
-------------------------------------------------------------------}
procedure TLogger.Watch(const AText: String; AValue: Integer);
begin
  Watch(DefaultClasses,AText,AValue);
end;


{$ifdef fpc}
{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCheckpoint's loging method.
-------------------------------------------------------------------}
procedure TLogger.Watch(const AText: String; AValue: Cardinal);
begin
  Watch(DefaultClasses,AText,AValue);
end;
{$endif}

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCheckpoint's loging method.
-------------------------------------------------------------------}
procedure TLogger.Watch(const AText: String; AValue: Double);
begin
  Watch(DefaultClasses,AText,AValue);
end;

{------------------------------------------------------------------
§Explanations: loging; using public DefaultClasses containing Why this loging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCheckpoint's loging method.
-------------------------------------------------------------------}
procedure TLogger.Watch(const AText: String; AValue: Boolean);
begin
  Watch(DefaultClasses,AText,AValue);
end;



(* -- filtred method through intersection of Classes's set and ActivesClasses's set -- *)



{------------------------------------------------------------------
§Explanations: whe log with a ltInfo's method.
-------------------------------------------------------------------}
procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String);
begin
  if Classes * ActiveClasses = [] then Exit;    //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltInfo,AText,nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltInfo's method.
-------------------------------------------------------------------}
procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; Args: array of const);
begin
  if Classes * ActiveClasses = [] then Exit;   //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltInfo, Format(AText,Args),nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltValue's method.
-------------------------------------------------------------------}
procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText, AValue: String);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue,AText+' = '+AValue,nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltValue's method.
-------------------------------------------------------------------}
procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Integer);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue,AText+' = '+IntToStr(AValue),nil);
end;

{$ifdef fpc}
{------------------------------------------------------------------
§Explanations: whe log with a ltValue's method.
-------------------------------------------------------------------}
procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Cardinal);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue,AText+' = '+IntToStr(AValue),nil);
end;
{$endif}

{------------------------------------------------------------------
§Explanations: whe log with a ltValue's method.
-------------------------------------------------------------------}
procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Double);
begin
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue,AText+' = '+FloatToStr(AValue),nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltValue's method.
-------------------------------------------------------------------}
procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Int64);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue,AText+' = '+IntToStr(AValue),nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltValue's method.
-------------------------------------------------------------------}
procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: QWord);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue,AText+' = '+IntToStr(AValue),nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltValue's method.
-------------------------------------------------------------------}
procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Boolean);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue, AText + ' = ' + BoolToStr(AValue, True), nil);
end;


{------------------------------------------------------------------
§Explanations: whe log with a ltValue's method.
-------------------------------------------------------------------}
procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String;const ARect: TRect);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  with ARect do
    SendStream(ltValue,AText+ ' = '+RectToStr(ARect),nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltValue's method.
-------------------------------------------------------------------}
procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; const APoint: TPoint);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue,AText+' = '+PointToStr(APoint),nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltStrings's method.
-------------------------------------------------------------------}
procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AStrList: TStrings);
var
  S:String;
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  if Assigned(AStrList) then
    S:= AStrList.Text
  else
    S:='';
  SendBuffer(ltStrings,AText,S[1],Length(S));
end;

{------------------------------------------------------------------
§Explanations: returns the literal description of the "sender" component that is at the origin of the event.
-------------------------------------------------------------------}
function GetObjectDescription(Sender: TObject): String;
begin
  Result := Sender.ClassName;
  if (Sender is TComponent) and (TComponent(Sender).Name <> '') then
    Result := Result + '(' + TComponent(Sender).Name + ')';
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltObject's method.
-------------------------------------------------------------------}
procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AObject: TObject);
var
  TempStr: String;
  AStream: TStream;
begin
  if Classes * ActiveClasses = [] then Exit;	  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  AStream := nil;
  TempStr := AText + ' [';
  if AObject <> nil then
  begin
    if AObject is TComponent then
    begin
      AStream := TMemoryStream.Create;
      AStream.WriteComponent(TComponent(AObject));
    end
    else
      TempStr := TempStr + GetObjectDescription(AObject) + ' / ';
  end;
  TempStr := TempStr + ('$' + HexStr(AObject) + ']');
  //SendStream free AStream
  SendStream(ltObject, TempStr, AStream);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltValue's method.
-------------------------------------------------------------------}
procedure TLogger.SendPointer(Classes: TGroupsOfWhyLogMsg; const AText: String; APointer: Pointer);
begin
  if Classes * ActiveClasses = [] then Exit;	  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue, AText + ' = $' + HexStr(APointer), nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltCallStack's method.
-------------------------------------------------------------------}
procedure TLogger.SendCallStack(Classes: TGroupsOfWhyLogMsg; const AText: String);
var
  AStream: TStream;
begin
  if Classes * ActiveClasses = [] then Exit;	  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  AStream:=TMemoryStream.Create;
  GetCallStack(AStream);
  //nb: SendStream will free AStream
  SendStream(ltCallStack,AText,AStream);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltException's method.
-------------------------------------------------------------------}
procedure TLogger.SendException(Classes: TGroupsOfWhyLogMsg; const AText: String; AException: Exception);
{$ifdef fpc}
var
  i: Integer;
  Frames: PPointer;
  S:String;
{$endif}
begin
  {$ifdef fpc}
  if Classes * ActiveClasses = [] then Exit;	  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  if (AException <> nil) then
    S:= AException.ClassName+' - '+AException.Message+LineEnding;
  S:= S + BackTraceStrFunc(ExceptAddr);
  Frames:= ExceptFrames;
  for i:= 0 to ExceptFrameCount - 1 do
    S:= S + (LineEnding+BackTraceStrFunc(Frames[i]));
  SendBuffer(ltException,AText,S[1],Length(S));
  {$endif}
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltHeapInfo's method.
-------------------------------------------------------------------}
procedure TLogger.SendHeapInfo(Classes: TGroupsOfWhyLogMsg; const AText: String);
{$ifdef fpc}
var
  S: String;
{$endif}
begin
  {$ifdef fpc}
  if Classes * ActiveClasses = [] then Exit;		  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  with GetFPCHeapStatus do
  begin
    S:= 'All values are in [bytes]:'+LineEnding
    	+'MaxHeapSize: '+FormatNumber(MaxHeapSize)+LineEnding
      +'MaxHeapUsed: '+FormatNumber(MaxHeapUsed)+LineEnding
      +'CurrHeapSize: '+FormatNumber(CurrHeapSize)+LineEnding
      +'CurrHeapUsed: '+FormatNumber(CurrHeapUsed)+LineEnding
      +'CurrHeapFree: '+FormatNumber(CurrHeapFree);
  end;
  SendBuffer(ltHeapInfo,AText,S[1],Length(S));
  {$endif}
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltMemory's method.
-------------------------------------------------------------------}
procedure TLogger.SendMemory(Classes: TGroupsOfWhyLogMsg; const AText: String; Address: Pointer; Size: LongWord; Offset: Integer);
begin
  if Classes * ActiveClasses = [] then Exit; 			  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  if Address <> nil then
  begin
    if Offset <> 0 then
      Address := Address + Offset;
  end
  else
  begin
    //empty
    Address := Self;
    Size := 0;
  end;
  SendBuffer(ltMemory,AText,Address^,Size);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltConditional's method.
-------------------------------------------------------------------}
procedure TLogger.SendIf(Classes: TGroupsOfWhyLogMsg; const AText: String; Expression, IsTrue: Boolean);
begin
  if (Classes * ActiveClasses = []) or (Expression <> IsTrue) then Exit; 			  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltConditional,AText,nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltWarning's method.
-------------------------------------------------------------------}
procedure TLogger.SendWarning(Classes: TGroupsOfWhyLogMsg; const AText: String);
begin
  if Classes * ActiveClasses = [] then Exit;		 			  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltWarning,AText,nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltError's method.
-------------------------------------------------------------------}
procedure TLogger.SendError(Classes: TGroupsOfWhyLogMsg; const AText: String);
begin
  if Classes * ActiveClasses = [] then Exit;		 			  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltError,AText,nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltCustomData's method.
-------------------------------------------------------------------}
procedure TLogger.SendCustomData(Classes: TGroupsOfWhyLogMsg; const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotify);
var
  DoSend: Boolean;
  TempStr: String;
begin
  if (Classes * ActiveClasses = []) or (not Assigned(CustomDataFunction)) then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  DoSend:=True;
  TempStr:=CustomDataFunction(Self,Data,DoSend);
  if DoSend then
    SendBuffer(ltCustomData,AText,TempStr[1],Length(TempStr));
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltCustomData's method.
-------------------------------------------------------------------}
procedure TLogger.SendCustomData(Classes: TGroupsOfWhyLogMsg; const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotifyStatic);
var
  DoSend: Boolean;
  TempStr: String;
begin
  if (Classes * ActiveClasses = []) or (not Assigned(CustomDataFunction)) then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  DoSend:=True;
  TempStr:=CustomDataFunction(Self,Data,DoSend);
  if DoSend then
    SendBuffer(ltCustomData,AText,TempStr[1],Length(TempStr));
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltCheckpoint's method.
-------------------------------------------------------------------}
procedure TLogger.AddCheckPoint(Classes: TGroupsOfWhyLogMsg; const CheckName: String);
var
  i: Integer;
  j: PtrInt;
begin
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  i:=FCheckList.IndexOf(CheckName);
  if i <> -1 then
  begin
    //Add a custom CheckList
    j:=PtrInt(FCheckList.Objects[i])+1;
    FCheckList.Objects[i]:=TObject(j);
  end
  else
  begin
    FCheckList.AddObject(CheckName,TObject(0));
    j:=0;
  end;
  SendStream(ltCheckpoint,CheckName+' #'+IntToStr(j),nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltCounter's method.
-------------------------------------------------------------------}
procedure TLogger.IncCounter(Classes: TGroupsOfWhyLogMsg; const CounterName: String );
var
  i: Integer;
  j: PtrInt;
begin
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  i := FCounterList.IndexOf(CounterName);
  if i <> -1 then
  begin
    j := PtrInt(FCounterList.Objects[i]) + 1;
    FCounterList.Objects[i] := TObject(j);
  end
  else
  begin
    FCounterList.AddObject(CounterName, TObject(1));
    j := 1;
  end;
  SendStream(ltCounter,CounterName+'='+IntToStr(j),nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltCounter's method.
-------------------------------------------------------------------}
procedure TLogger.DecCounter(Classes: TGroupsOfWhyLogMsg; const CounterName: String);
var
  i: Integer;
  j: PtrInt;
begin
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  i := FCounterList.IndexOf(CounterName);
  if i <> -1 then
  begin
    j := PtrInt(FCounterList.Objects[i]) - 1;
    FCounterList.Objects[i] := TObject(j);
  end
  else
  begin
    FCounterList.AddObject(CounterName, TObject(-1));
    j := -1;
  end;
  SendStream(ltCounter,CounterName+'='+IntToStr(j),nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltCounter's method.
-------------------------------------------------------------------}
procedure TLogger.ResetCounter(Classes: TGroupsOfWhyLogMsg; const CounterName: String);
var
  i: Integer;
begin
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  i := FCounterList.IndexOf(CounterName);
  if i <> -1 then
  begin
    FCounterList.Objects[i] := TObject(0);
    SendStream(ltCounter, FCounterList[i] + '=0', nil);
  end;
end;

function TLogger.GetCounter(const CounterName: String): Integer;
var
  i: Integer;
begin
  i := FCounterList.IndexOf(CounterName);
  if i <> -1 then
    Result := PtrInt(FCounterList.Objects[i])
  else
    Result := 0;
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltCheckpoint's method.
-------------------------------------------------------------------}
procedure TLogger.ResetCheckPoint(Classes: TGroupsOfWhyLogMsg; const CheckName:String);
var
  i: Integer;
begin
  if Classes * ActiveClasses = [] then Exit;    //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  i:=FCheckList.IndexOf(CheckName);
  if i <> -1 then
  begin
    FCheckList.Objects[i] := TObject(0);
    SendStream(ltCheckpoint, CheckName+' #0',nil);
  end;
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltEnterMethod's method.
-------------------------------------------------------------------}
procedure TLogger.EnterMethod(Classes: TGroupsOfWhyLogMsg; Sender: TObject; const AMethodName: String; const AMessage: String);
var
  AText: String;
begin
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  FLogStack.Insert(0, UpperCase(AMethodName));
  if AMessage <> '' then
    AText := AMessage
  else if Sender <> nil then
    AText := GetObjectDescription(Sender) + '.' + AMethodName
  else
    AText := AMethodName;
  SendStream(ltEnterMethod, AText, nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltExitMethod's method.
-------------------------------------------------------------------}
procedure TLogger.ExitMethod(Classes: TGroupsOfWhyLogMsg; Sender: TObject; const AMethodName: String; const AMessage: String);
var
  i: Integer;
  AText: String;
begin
  //ensure that ExitMethod will be called always even if there's an unpaired Entermethod (!)
  if FLogStack.Count = 0 then Exit;
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  //todo: see if is necessary to do Uppercase (set case sensitive to false?)
  i := FLogStack.IndexOf(UpperCase(AMethodName));
  if i <> -1 then
    FLogStack.Delete(i)
  else
    Exit;

  if AMessage <> '' then
    AText := AMessage
  else if Sender <> nil then
    AText := GetObjectDescription(Sender) + '.' + AMethodName
  else
    AText := AMethodName;
  SendStream(ltExitMethod, AText, nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltWatch's method.
-------------------------------------------------------------------}
procedure TLogger.Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Integer);
begin
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltWatch,AText+'='+IntToStr(AValue),nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltWatch's method.
-------------------------------------------------------------------}
{$ifdef fpc}
procedure TLogger.Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Cardinal);
begin
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltWatch,AText+'='+IntToStr(AValue),nil);
end;
{$endif}

{------------------------------------------------------------------
§Explanations: whe log with a ltWatch's method.
-------------------------------------------------------------------}
procedure TLogger.Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Double);
begin
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltWatch,AText+'='+FloatToStr(AValue),nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltWatch's method.
-------------------------------------------------------------------}
procedure TLogger.Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Boolean);
begin
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltWatch,AText+'='+BoolToStr(AValue),nil);
end;

{------------------------------------------------------------------
§Explanations: whe log with a ltWatch's method.
-------------------------------------------------------------------}
procedure TLogger.Watch(Classes: TGroupsOfWhyLogMsg; const AText, AValue: String);
begin
  if Classes * ActiveClasses = [] then Exit;    //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltWatch,AText+'='+AValue,nil);
end;


{ TChannelList }

function TChannelList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TChannelList.GetItems(AIndex:Integer): TLogChannel;
begin
  Result := TLogChannel(FList[AIndex]);
end;

constructor TChannelList.Create;
begin
  FList := TFPList.Create;
end;

destructor TChannelList.Destroy;
var
  i: Integer;
begin
  //free the registered channels
  for i := 0 to FList.Count - 1 do
    Items[i].Free;
  FList.Destroy;
end;

function TChannelList.Add(AChannel: TLogChannel):Integer;
begin
  Result := FList.Add(AChannel);
  AChannel.Init;
end;

procedure TChannelList.Remove(AChannel: TLogChannel);
begin
  FList.Remove(AChannel);
end;


{ TLogChannel }

procedure TLogChannel.Init;
begin
		//must be overriden in its descendants
end;


{------------------------------------------------------------------
§Explanations: if AComponent - a specialized channel, like TMemoChannel - is in opRemove,
and if there's a AComponent's FChannelWrapper that is memory managed by AComponent,
then this FChannelWrapper must stop immediately any activity.
-------------------------------------------------------------------}
procedure TLogChannelWrapper.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if	(Operation = opRemove) and
  		(FChannel <> nil) then
    FChannel.Active := False;
end;

initialization
  Logger:=TLogger.Create;
finalization
  TLogger.FDefaultChannels.Free;
  Logger.Free;
  Multilog.Guardian.Free;

end.
                                                                                                                                        
