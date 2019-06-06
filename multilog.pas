unit MultiLog;
{$REGION 'Xls: Comments section'}
{§< @HTML(
<b>Main unit of the Multilog logging system.</b>  <br>
<span style="color: #4169E1;">author of MultiLog: Luiz Américo Pereira Câmara; pascalive@bol.com.br</span>  <br><br>

<div style="border: solid windowtext .5pt; padding: 1.0pt 4.0pt 1.0pt 4.0pt;">
nb1: all units are encoded with UTF-8 without BOM, using EDI "file settings\encoding\UTF-8" contextual menu.  <br>
nb2: this HTML documentation has been made with PasDoc - program named "pasdoc_gui". The comment marker is the character "§",
to include only comments that start with this merker, a documentation tool for the Object Pascal code:
<a href="https://github.com/pasdoc/pasdoc/wiki">https://github.com/pasdoc/pasdoc/wiki</a> . The pasdoc_gui's configuration file,
for those who want to update the documentation, is named "config.pds".
</div>)
}
{$ENDREGION}

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
}



{$ifdef fpc} 
{$mode objfpc}{$H+}
{$endif}

interface

uses
  {$ifndef fpc}Types, fpccompat,{$endif} Classes, SysUtils, syncobjs, math;

{$REGION 'Xls: Comments section'}
{§ MessageTypes  @br
  mt (Message Type) and lt (Log Type) prefixes are used elsewhere
  but mt is worse because there's already mtWarning and mtInformation.  @br
  The existing lt do not makes confusion.  @br
  MessageTypes form a distribution of function's type groups: HOW \ with WHICH method's group to log.
}
{$ENDREGION}
const
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
  {§ We can use ltSubEventBetweenEnterAndExitMethods, to indent the Msg depending of it's level in the callstack, between EnterMethod..ExitMethod }
  ltSubEventBetweenEnterAndExitMethods = 22;



  ltClear = 100;


{$REGION 'Xls: Comments section'}
{§ LogClasses of stats, convention with lc prefix. @br
It's possible to define the constants to suit any need
distribution of statictics classes of WHY, msg are logged. }
{$ENDREGION}
  lcAll = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31];
  lcDebug = 0;
  lcError = 1;
  lcInfo = 2;
  lcWarning = 3;
  lcEvents = 4; lcEvent = 4;
  //above is reserved
  lcUser = 8; lcNone = 8;
  lcStudyChainedEvents = 9;


  
type
  TLogger = class;

  TDebugClass = 0..31;
  TGroupOfWhyLogMsg = TDebugClass;							// just a statistic's semantic synonym

  TGroupsOfWhyLogMsg = Set of TGroupOfWhyLogMsg;
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


{$REGION 'Xls: Comments section'}
{§ Brief summmary of the processing of a TLogger's mathod call: @br
@html(<pre>
<u>step ❶:</u>
          |                                                              |
Calling program sends events;                               There's a distibution of groups of
each event can have its                                     methods too, each group specialized in
*Why*ThisLogging's justification.                           *How* to forward an event type towards its
So, there's a distribution of groups of                          channel's target
event's justifications
.../...
        (lcEvents)→                                                   (lcEvents)(ltEnterMethod)→
          .../...                                                       .../...
        (lcEvents)→                                                   (lcEvents)(ltExitMethod)→
   (lcNone)→                                                       (lcNone)(ltInfo)→
       (lcNone)→                                                     (lcNone)(ltInfo)→
  (lcStudyChainedEvents)(ltSubEventBetweenEnterAndExitMethods)→                                            (lcStudyChainedEvents)(ltSubEventBetweenEnterAndExitMethods)→
     .../...                                                        .../...
     (lcNone)→                                                      (lcNone)(ltValue\@integer)→
  (lcStudyChainedEvents)(ltSubEventBetweenEnterAndExitMethods)→                                           (lcStudyChainedEvents)(ltSubEventBetweenEnterAndExitMethods)→
(lcNone)→                                                     (lcNone)(ltValue\@boolean)→
.../...



<u>step ❷:</u>
         |                                                            |
There's a distibution of groups of                            ActiveClasses acts like a wall: if the *How* to forward isn't present
method, each group specialized in                             in the ActiveClasses's "set Of *How*" type, then, the event doesn't go further.
*How* to forward an event type towards its channel's target.  Let say that ActiveClasses = [ltEnterMethod, ltExitMethod, ltValue]:
.../...                                                               ↺|||
(lcEvents)(ltEnterMethod)→                                              (lcEvents)(ltEnterMethod)→
.../...                                                               ↺|||
(lcEvents)(ltExitMethod)→                                               (lcEvents)(ltExitMethod)→
.../...                                                               ↺|||
(lcStudyChainedEvents)(ltSubEventBetweenEnterAndExitMethods)→         ↺|||
.../...                                                               ↺|||
  (lcNone)(ltValue\@integer)→                                           (lcNone)(ltValue\@integer)→
  (lcStudyChainedEvents)(ltSubEventBetweenEnterAndExitMethods)→       ↺|||
(lcNone)(ltValue\@boolean)→                                             (lcNone)(ltValue\@boolean)→
.../...                                                               ↺|||
.../...                                                               ↺|||



<u>step ❸:</u>
|                                                                                                               |
ActiveClasses acts like a wall: if the *How* to forward isn't present                               For each specialized Channel leading
in the ActiveClasses's "Set Of *How*" type, then, the event doesn't go further.                     to a display medium (TMemo □, TFileText ○,TLogTreeView ▶)
Let say that ActiveClasses = [ltEnterMethod, ltExitMethod, ltValue]:                                will receive the msg and display it.
|
(lcEvents)(ltEnterMethod)→                                                                                    ○ + □
|
(lcEvents)(ltExitMethod)→                                                                                       □
|
|   .../...
|
(lcNone)(ltValue\@integer)→                                                                                    ○
|
(lcNone)(ltValue\@boolean)→                                                                                    ○ + ▶
.../...
</pre>)
}
{$ENDREGION}
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
		{§ public field to allow use of include/exclude functions = [lcDebug, ...] =~ active which can be adjusted contextually in the calling program...}
    ActiveClasses: TGroupsOfWhyLogMsg;
    {§ public field to allow use of include/exclude functions = [lcDebug, ...] =~ ..."pass-filter"'s set, blocking or not the logging.}
    DefaultClasses: TGroupsOfWhyLogMsg;
    constructor Create;
    destructor Destroy; override;
    function CalledBy(const AMethodName: String): Boolean;
    procedure Clear;
    //Helper functions
    function RectToStr(const ARect: TRect): String; //inline
    function PointToStr(const APoint: TPoint): String; //inline
    //Send functions
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing "Why this logging" = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltInfo's logging method.
@param(AText is a string wich is the Msg to send via the TxxxChannel(s) towards their respectful display \ record media)
@returns(nil).
}
{$ENDREGION}
    procedure Send(const AText: String); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltInfo's method.
}
{$ENDREGION}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltInfo's logging method.
}
{$ENDREGION}
    procedure Send(const AText: String; Args: array of const);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
    {§ Explanations: whe log with a ltInfo's method.}
{$ENDREGION}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; Args: array of const);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's logging method.
}
{$ENDREGION}
    procedure Send(const AText, AValue: String);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
    {§ Explanations: whe log with a ltValue's method.}
{$ENDREGION}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText,AValue: String); overload;
{$REGION 'Xls: Comments section'}
    {§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
    to pass this WHICH\HOW ltValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AValue: Integer); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
	{§ Explanations: whe log with a ltValue's method.}
{$ENDREGION}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Integer);overload;
    {$ifdef fpc}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AValue: Cardinal); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a ltValue's method.}
{$ENDREGION}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Cardinal);overload;
    {$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AValue: Double); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a ltValue's method.}
{$ENDREGION}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Double);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AValue: Int64); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a ltValue's method.}
{$ENDREGION}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Int64);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AValue: QWord); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a ltValue's method.}
{$ENDREGION}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: QWord);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AValue: Boolean); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a ltValue's method.}
{$ENDREGION}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Boolean);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; const ARect: TRect); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a ltValue's method.}
{$ENDREGION}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; const ARect: TRect);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; const APoint: TPoint); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a ltValue's method.}
{$ENDREGION}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; const APoint: TPoint);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltStrings's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AStrList: TStrings); overload; {$ifdef fpc}inline;{$endif}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AStrList: TStrings);overload;
{$REGION 'Xls: Comments section'}
	{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
	to pass this WHICH\HOW ltObject's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AObject: TObject); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a ltObject's method.}
{$ENDREGION}
    procedure Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AObject: TObject);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltValue's logging method.}
{$ENDREGION}
    procedure SendPointer(const AText: String; APointer: Pointer); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a ltValue's method.}
{$ENDREGION}
    procedure SendPointer(Classes: TGroupsOfWhyLogMsg; const AText: String; APointer: Pointer);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCallStack's logging method.}
{$ENDREGION}
    procedure SendCallStack(const AText: String); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltCallStack's method.}
{$ENDREGION}
    procedure SendCallStack(Classes: TGroupsOfWhyLogMsg; const AText: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltException's logging method.}
{$ENDREGION}
    procedure SendException(const AText: String; AException: Exception);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltException's method.}
{$ENDREGION}
    procedure SendException(Classes: TGroupsOfWhyLogMsg; const AText: String; AException: Exception);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: check Exception hierarchy (ultimate ancestor=EDatabaseError || EStreamError || ...), to grab its specific fields into a dumped string.}
{$ENDREGION}
    function GetExceptionDescriptionFields(AException: Exception): string;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltHeapInfo's logging method.}
{$ENDREGION}
    procedure SendHeapInfo(const AText: String); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltHeapInfo's method.}
{$ENDREGION}
    procedure SendHeapInfo(Classes: TGroupsOfWhyLogMsg; const AText: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltMemory's logging method.}
{$ENDREGION}
    procedure SendMemory(const AText: String; Address: Pointer; Size: LongWord; Offset: Integer = 0); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltMemory's method.}
{$ENDREGION}
    procedure SendMemory(Classes: TGroupsOfWhyLogMsg; const AText: String; Address: Pointer; Size: LongWord; Offset: Integer = 0);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltConditional's logging method.}
{$ENDREGION}
    procedure SendIf(const AText: String; Expression: Boolean); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging;
this ltConditional method with overloaded specific parameter Classes, that allows us to pass or not [lcDebug, ...],
to verify an existing intersection with ActiveClasses.}
{$ENDREGION}
    procedure SendIf(Classes: TGroupsOfWhyLogMsg; const AText: String; Expression: Boolean); overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltConditional's logging method.}
{$ENDREGION}
    procedure SendIf(const AText: String; Expression, IsTrue: Boolean); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltConditional's method.}
{$ENDREGION}
    procedure SendIf(Classes: TGroupsOfWhyLogMsg; const AText: String; Expression, IsTrue: Boolean);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltWarning's logging method.}
{$ENDREGION}
    procedure SendWarning(const AText: String); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltWarning's method.}
{$ENDREGION}
    procedure SendWarning(Classes: TGroupsOfWhyLogMsg; const AText: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltError's logging method.}
{$ENDREGION}
    procedure SendError(const AText: String); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltError's method.}
{$ENDREGION}
    procedure SendError(Classes: TGroupsOfWhyLogMsg; const AText: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCustomData's logging method.}
{$ENDREGION}
    procedure SendCustomData(const AText: String; Data: Pointer);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCustomData's logging method.}
{$ENDREGION}
    procedure SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotify);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltCustomData's method.}
{$ENDREGION}
    procedure SendCustomData(Classes: TGroupsOfWhyLogMsg; const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotify);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging;
this ltCustomData method with overloaded specific parameter Classes, that allows us to pass or not [lcDebug, ...],
to verify an existing intersection with ActiveClasses.}
{$ENDREGION}
    procedure SendCustomData(Classes: TGroupsOfWhyLogMsg; const AText: String; Data: Pointer);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltCustomData's method.}
{$ENDREGION}
    procedure SendCustomData(Classes: TGroupsOfWhyLogMsg; const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotifyStatic);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCustomData's logging method.}
{$ENDREGION}
    procedure SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotifyStatic);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCheckpoint's logging method.}
{$ENDREGION}
    procedure AddCheckPoint;overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging;
This ltCheckpoint method with overloaded specific parameter Classes, that allows us to pass or not [lcDebug, ...],
to verify an existing intersection with ActiveClasses.}
{$ENDREGION}
    procedure AddCheckPoint(Classes: TGroupsOfWhyLogMsg);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCheckpoint's logging method.}
{$ENDREGION}
    procedure AddCheckPoint(const CheckName: String);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltCheckpoint's method.}
{$ENDREGION}
    procedure AddCheckPoint(Classes: TGroupsOfWhyLogMsg; const CheckName: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCounter's logging method.}
{$ENDREGION}
    procedure IncCounter(const CounterName: String);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltCounter's method.}
{$ENDREGION}
    procedure IncCounter(Classes: TGroupsOfWhyLogMsg; const CounterName: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCounter's logging method.}
{$ENDREGION}
    procedure DecCounter(const CounterName: String);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltCounter's method.}
{$ENDREGION}
    procedure DecCounter(Classes: TGroupsOfWhyLogMsg; const CounterName: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCounter's logging method.}
{$ENDREGION}
    procedure ResetCounter(const CounterName: String);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltCounter's method.}
{$ENDREGION}
    procedure ResetCounter(Classes: TGroupsOfWhyLogMsg; const CounterName: String);overload;
    function GetCounter(const CounterName: String): Integer;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCheckpoint's logging method.}
{$ENDREGION}
    procedure ResetCheckPoint;overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging;
This ltCheckpoint method with overloaded specific parameter Classes, that allows us to pass or not [lcDebug, ...],
to verify an existing intersection with ActiveClasses.}
{$ENDREGION}
    procedure ResetCheckPoint(Classes: TGroupsOfWhyLogMsg);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCheckpoint's logging method.}
{$ENDREGION}
    procedure ResetCheckPoint(const CheckName: String);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltCheckpoint's method.}
{$ENDREGION}
    procedure ResetCheckPoint(Classes: TGroupsOfWhyLogMsg;const CheckName: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltEntertMethod's logging method.}
{$ENDREGION}
    procedure EnterMethod(const AMethodName: String; const AMessage: String = ''); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging;
This ltEntertMethod method with overloaded specific parameter Classes, that allows us to pass or not [lcDebug, ...],
to verify an existing intersection with ActiveClasses.}
{$ENDREGION}
    procedure EnterMethod(Classes: TGroupsOfWhyLogMsg; const AMethodName: String; const AMessage: String = ''); overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltEntertMethod's logging method.}
{$ENDREGION}
    procedure EnterMethod(Sender: TObject; const AMethodName: String; const AMessage: String = ''); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltEnterMethod's method.}
{$ENDREGION}
    procedure EnterMethod(Classes: TGroupsOfWhyLogMsg; Sender: TObject; const AMethodName: String; const AMessage: String = '');overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltExitMethod's logging method.}
{$ENDREGION}
    procedure ExitMethod(const AMethodName: String; const AMessage: String = ''); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltExitMethod's logging method.}
{$ENDREGION}
    procedure ExitMethod(Sender: TObject; const AMethodName: String; const AMessage: String = ''); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging;
This ltExitMethod method with overloaded specific parameter Classes, that allows us to pass or not [lcDebug, ...],
to verify an existing intersection with ActiveClasses.}
{$ENDREGION}
    procedure ExitMethod(Classes: TGroupsOfWhyLogMsg; const AMethodName: String; const AMessage: String = ''); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltExitMethod's method.}
{$ENDREGION}
    procedure ExitMethod({%H-}Classes: TGroupsOfWhyLogMsg; Sender: TObject; const AMethodName: String; const AMessage: String = '');overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltWatch's logging method.}
{$ENDREGION}
    procedure Watch(const AText, AValue: String); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltWatch's method.}
{$ENDREGION}
    procedure Watch(Classes: TGroupsOfWhyLogMsg; const AText,AValue: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltWatch's logging method.}
{$ENDREGION}
    procedure Watch(const AText: String; AValue: Integer); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltWatch's method.}
{$ENDREGION}
    procedure Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Integer);overload;
    {$ifdef fpc}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCheckpoint's logging method.}
{$ENDREGION}
    procedure Watch(const AText: String; AValue: Cardinal); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltWatch's method.}
{$ENDREGION}
    procedure Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Cardinal);overload;
    {$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCheckpoint's logging method.}
{$ENDREGION}
    procedure Watch(const AText: String; AValue: Double); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltWatch's method.}
{$ENDREGION}
    procedure Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Double);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltCheckpoint's logging method.}
{$ENDREGION}
    procedure Watch(const AText: String; AValue: Boolean); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltWatch's method.}
{$ENDREGION}
		procedure Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Boolean);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lcDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltInfo's logging method.}
{$ENDREGION}
    procedure SubEventBetweenEnterAndExitMethods(const AText: String); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltSubEventBetweenEnterAndExitMethods's method.}
{$ENDREGION}
    procedure SubEventMethodBetweenEnterAndExitMethods(Classes: TGroupsOfWhyLogMsg; const AText: String);overload;
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

{$REGION 'Xls: Comments section'}
{§ Explanations: if AComponent - a specialized channel, like TMemoChannel - is in a @code(TComponentState = [opRemove]),
and if there's a AComponent's FChannelWrapper that is memory managed by AComponent,
then this FChannelWrapper must stop immediately any activity.}
{$ENDREGION}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property Channel: TLogChannel read FChannel write FChannel;
  end;


  TFixedCriticalSection = class(TCriticalSection)
  private
    {$WARN 5029 off : Private field "$1.$2" is never used} // FDummy not used anywhere so switch off such warnings
    FDummy: array [0..95] of Byte; // fix multiprocessor cache safety http://blog.synopse.info/post/2016/01/09/Safe-locks-for-multi-thread-applications
  end;

  TGuardian = TFixedCriticalSection;


var
  Logger: TLogger;

implementation

uses
  db;

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


{$REGION 'Xls: Comments section'}
  {§ Explanations: global procedure returns the literal description of the "sender" component that is at the origin of the event.}
{$ENDREGION}
function GetObjectDescription(Sender: TObject): String;
begin
  Result := Sender.ClassName;
  if (Sender is TComponent) and (TComponent(Sender).Name <> '') then
    Result := Result + '(' + TComponent(Sender).Name + ')';
end;


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
  //bp:=get_frame;                                             //get_frame=IP's frame
  bp:= get_caller_frame(get_frame);                            //BP = number of the current base frame
  try
    prevbp:=bp-1;                                              //prev_BP = number of the precedent base frame *)
    i:=0;
    //is_dev:=do_isdevice(textrec(f).Handle);
    while bp > prevbp Do                                       // while we can pop...
     begin
       caller_addr := get_caller_addr(bp);                     //we get the IP's caller
       caller_frame := get_caller_frame(bp);                   //and its BP
       if (caller_addr=nil) then
         break;                                                //We are back at the start point: all has been "poped"
       //todo: see what is faster concatenate string and use writebuffer or current
       S:=BackTraceStrFunc(caller_addr)+LineEnding;            //EI name
       AStream.WriteBuffer(S[1],Length(S));
       Inc(i);
       if (i>=FMaxStackCount) or (caller_frame=nil) then
         break;
       prevbp:=bp;                                             //previous variable is set with the IP
       bp:=caller_frame;                                       //the IP becomes the courent caller of the courent frame: we backward from one call frame
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
  Result:= ActiveClasses <> [];
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
    //nb: this overwrite WhyThisLogging to lcStudyChainedEvents is used\recognized by The TFileChannel's callstack indentation, when ltSubEventBetweenEnterAndExitMethods
    WhyThisLogging := ifthen(AMsgType=ltSubEventBetweenEnterAndExitMethods, lcStudyChainedEvents, FWhyThisMsg);
    MsgTime := Now;
    MsgText := AText;
    Data := AStream;
  end;
  if FThreadSafe then
  	//Yes: it's a global variable created in this unit, and used in this unit only ;-)
    Multilog.Guardian.Enter;
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
    AStream:= TMemoryStream.Create;
    AStream.Write(Buffer,Count);
  end
  else
    AStream:=nil;
  SendStream(AMsgType,AText,AStream);	//nb: SendStream will free AStream
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
  if FThreadSafe and not Assigned(Guardian) then
    Multilog.Guardian := TGuardian.Create
	else if (not FThreadSafe) and Assigned(Guardian) then
  	FreeAndNil(Multilog.Guardian);
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
  ActiveClasses := [0]; 		//categor{y|ies} of Why this logging = lcDebug
  DefaultClasses := [0];		//categor{y|ies} of Why this logging = lcDebug
end;

destructor TLogger.Destroy;
begin
  FChannels.Destroy;
  FLogStack.Destroy;
  FCheckList.Destroy;
  FCounterList.Destroy;
  if Assigned(Guardian) then
    FreeAndNil(Multilog.Guardian);
end;

function TLogger.CalledBy(const AMethodName: String): Boolean;
begin
  Result:= FLogStack.IndexOf(UpperCase(AMethodName)) <> -1;
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
    Result:= Format('(Left: %d; Top: %d; Right: %d; Bottom: %d)',[Left,Top,Right,Bottom]);
end;

function TLogger.PointToStr(const APoint: TPoint): String;
begin
  with APoint do
    Result:= Format('(X: %d; Y: %d)',[X,Y]);
end;



procedure TLogger.Send(const AText: String);
begin
  Send(DefaultClasses,AText);
end;


procedure TLogger.Send(const AText: String; Args: array of const);
begin
  Send(DefaultClasses,AText,Args);
end;


procedure TLogger.Send(const AText, AValue: String);
begin
  Send(DefaultClasses,AText,AValue);
end;


procedure TLogger.Send(const AText: String; AValue: Integer);
begin
  Send(DefaultClasses,AText,AValue);
end;

{$ifdef fpc}
procedure TLogger.Send(const AText: String; AValue: Cardinal);
begin
  Send(DefaultClasses,AText,AValue);
end;
{$endif}


procedure TLogger.Send(const AText: String; AValue: Double);
begin
  Send(DefaultClasses,AText,AValue);
end;


procedure TLogger.Send(const AText: String; AValue: Int64);
begin
  Send(DefaultClasses,AText,AValue);
end;


procedure TLogger.Send(const AText: String; AValue: QWord);
begin
  Send(DefaultClasses,AText,AValue);
end;


procedure TLogger.Send(const AText: String; AValue: Boolean);
begin
  Send(DefaultClasses, AText, AValue);
end;


procedure TLogger.Send(const AText: String; const ARect: TRect);
begin
  Send(DefaultClasses,AText,ARect);
end;


procedure TLogger.Send(const AText: String; const APoint: TPoint);
begin
  Send(DefaultClasses,AText,APoint);
end;


procedure TLogger.Send(const AText: String; AStrList: TStrings);
begin
  Send(DefaultClasses,AText,AStrList);
end;


procedure TLogger.Send(const AText: String; AObject: TObject);
begin
  Send(DefaultClasses,AText,AObject);
end;


procedure TLogger.SendPointer(const AText: String; APointer: Pointer);
begin
  SendPointer(DefaultClasses,AText,APointer);
end;


procedure TLogger.SendCallStack(const AText: String);
begin
  SendCallStack(DefaultClasses,AText);
end;


procedure TLogger.SendException(const AText: String; AException: Exception);
begin
  SendException(DefaultClasses,AText,AException);
end;


procedure TLogger.SendHeapInfo(const AText: String);
begin
  SendHeapInfo(DefaultClasses,AText);
end;


procedure TLogger.SendMemory(const AText: String; Address: Pointer; Size: LongWord; Offset: Integer);
begin
  SendMemory(DefaultClasses,AText,Address,Size,Offset)
end;


procedure TLogger.SendIf(const AText: String; Expression: Boolean);
begin
  SendIf(DefaultClasses,AText,Expression,True);
end;


procedure TLogger.SendIf(Classes: TGroupsOfWhyLogMsg; const AText: String; Expression: Boolean);
begin
  SendIf(Classes,AText,Expression,True);
end;


procedure TLogger.SendIf(const AText: String; Expression, IsTrue: Boolean);
begin
  SendIf(DefaultClasses,AText,Expression,IsTrue);
end;


procedure TLogger.SendWarning(const AText: String);
begin
  SendWarning(DefaultClasses,AText);
end;


procedure TLogger.SendError(const AText: String);
begin
  SendError(DefaultClasses,AText);
end;


procedure TLogger.SendCustomData(const AText: String; Data: Pointer);
begin
  SendCustomData(DefaultClasses,AText,Data,FOnCustomData);
end;


procedure TLogger.SendCustomData(Classes: TGroupsOfWhyLogMsg; const AText: String; Data: Pointer);
begin
  SendCustomData(Classes,AText,Data,FOnCustomData);
end;


procedure TLogger.SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotify);
begin
  SendCustomData(DefaultClasses,AText,Data,CustomDataFunction);
end;


procedure TLogger.SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotifyStatic);
begin
  SendCustomData(DefaultClasses,AText,Data,CustomDataFunction);
end;


procedure TLogger.AddCheckPoint;
begin
  AddCheckPoint(DefaultClasses,DefaultCheckName);
end;


procedure TLogger.AddCheckPoint(Classes: TGroupsOfWhyLogMsg);
begin
  AddCheckPoint(Classes,DefaultCheckName);
end;


procedure TLogger.AddCheckPoint(const CheckName: String);
begin
  AddCheckPoint(DefaultClasses,CheckName);
end;


procedure TLogger.ResetCheckPoint;
begin
  ResetCheckPoint(DefaultClasses,DefaultCheckName);
end;


procedure TLogger.ResetCheckPoint(Classes: TGroupsOfWhyLogMsg);
begin
  ResetCheckPoint(Classes,DefaultCheckName);
end;


procedure TLogger.ResetCheckPoint(const CheckName: String);
begin
  ResetCheckPoint(DefaultClasses,CheckName);
end;


procedure TLogger.IncCounter(const CounterName: String);
begin
  IncCounter(DefaultClasses,CounterName);
end;


procedure TLogger.DecCounter(const CounterName: String);
begin
  DecCounter(DefaultClasses,CounterName);
end;


procedure TLogger.ResetCounter(const CounterName: String);
begin
  ResetCounter(DefaultClasses,CounterName);
end;


procedure TLogger.EnterMethod(const AMethodName: String; const AMessage: String);
begin
  EnterMethod(DefaultClasses,nil,AMethodName,AMessage);
end;


procedure TLogger.EnterMethod(Classes: TGroupsOfWhyLogMsg; const AMethodName: String; const AMessage: String);
begin
  EnterMethod(Classes,nil,AMethodName,AMessage);
end;


procedure TLogger.EnterMethod(Sender: TObject; const AMethodName: String; const AMessage: String);
begin
  EnterMethod(DefaultClasses,Sender,AMethodName,AMessage);
end;


procedure TLogger.ExitMethod(const AMethodName: String; const AMessage: String);
begin
  ExitMethod(DefaultClasses,nil,AMethodName,AMessage);
end;


procedure TLogger.ExitMethod(Sender: TObject; const AMethodName: String; const AMessage: String);
begin
  ExitMethod(DefaultClasses,Sender,AMethodName,AMessage);
end;


procedure TLogger.ExitMethod(Classes: TGroupsOfWhyLogMsg; const AMethodName: String; const AMessage: String);
begin
  ExitMethod(Classes,nil,AMethodName,AMessage);
end;


procedure TLogger.Watch(const AText, AValue: String);
begin
  Watch(DefaultClasses,AText,AValue);
end;


procedure TLogger.Watch(const AText: String; AValue: Integer);
begin
  Watch(DefaultClasses,AText,AValue);
end;


{$ifdef fpc}
procedure TLogger.Watch(const AText: String; AValue: Cardinal);
begin
  Watch(DefaultClasses,AText,AValue);
end;
{$endif}


procedure TLogger.Watch(const AText: String; AValue: Double);
begin
  Watch(DefaultClasses,AText,AValue);
end;


procedure TLogger.Watch(const AText: String; AValue: Boolean);
begin
  Watch(DefaultClasses,AText,AValue);
end;


procedure TLogger.SubEventBetweenEnterAndExitMethods(const AText: String);
begin
  SubEventMethodBetweenEnterAndExitMethods(DefaultClasses,AText);
end;




(* -- filtred method through intersection of Classes's set and ActivesClasses's set -- *)




procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String);
begin
  if Classes * ActiveClasses = [] then Exit;    //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltInfo,AText,nil);
end;


procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; Args: array of const);
begin
  if Classes * ActiveClasses = [] then Exit;   //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltInfo, Format(AText,Args),nil);
end;


procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText, AValue: String);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue,AText+' = '+AValue,nil);
end;


procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Integer);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue,AText+' = '+IntToStr(AValue),nil);
end;

{$ifdef fpc}
procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Cardinal);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue,AText+' = '+IntToStr(AValue),nil);
end;
{$endif}


procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Double);
begin
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue,AText+' = '+FloatToStr(AValue),nil);
end;


procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Int64);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue,AText+' = '+IntToStr(AValue),nil);
end;


procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: QWord);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue,AText+' = '+IntToStr(AValue),nil);
end;


procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Boolean);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue, AText + ' = ' + BoolToStr(AValue, True), nil);
end;



procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String;const ARect: TRect);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  with ARect do
    SendStream(ltValue,AText+ ' = '+RectToStr(ARect),nil);
end;


procedure TLogger.Send(Classes: TGroupsOfWhyLogMsg; const AText: String; const APoint: TPoint);
begin
  if Classes * ActiveClasses = [] then Exit;  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue,AText+' = '+PointToStr(APoint),nil);
end;


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


procedure TLogger.SendPointer(Classes: TGroupsOfWhyLogMsg; const AText: String; APointer: Pointer);
begin
  if Classes * ActiveClasses = [] then Exit;	  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltValue, AText + ' = $' + HexStr(APointer), nil);
end;


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



function TLogger.GetExceptionDescriptionFields(AException: Exception): string;
var
  sDescrFields: string;
begin
  sDescrFields:= '';
  if (AException is EDatabaseError) then
    if (AException is EUpdateError) then begin
	    with (AException as EUpdateError) do begin
        sDescrFields:= sDescrFields + '- Context:' + Context + LineEnding;
        sDescrFields:= sDescrFields + '- ErrorCode:' + IntToStr(ErrorCode) + LineEnding;
        sDescrFields:= sDescrFields + '- PreviousError:' + IntToStr(PreviousError) + LineEnding;
        sDescrFields:= sDescrFields + '- OriginalException:' + OriginalException.ClassName + ' - ' + AException.Message + LineEnding;
        result:= sDescrFields;
        Exit;
      end
		end
{  else if (AException is EDomError) then begin
  	.../...
   end
   else if (AException is EStreamError) .../... then begin
    .../...
   end  }
end;


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
    S:= AException.ClassName + ' - ' + AException.Message + LineEnding + GetExceptionDescriptionFields(AException);
  S:= S + BackTraceStrFunc(ExceptAddr);
  Frames:= ExceptFrames;
  for i:= 0 to ExceptFrameCount - 1 do
    S:= S + (LineEnding + BackTraceStrFunc(Frames[i]));
  SendBuffer(ltException,AText,S[1],Length(S));
  {$endif}
end;



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


procedure TLogger.SendIf(Classes: TGroupsOfWhyLogMsg; const AText: String; Expression, IsTrue: Boolean);
begin
  if (Classes * ActiveClasses = []) or (Expression <> IsTrue) then Exit; 			  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltConditional,AText,nil);
end;


procedure TLogger.SendWarning(Classes: TGroupsOfWhyLogMsg; const AText: String);
begin
  if Classes * ActiveClasses = [] then Exit;		 			  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltWarning,AText,nil);
end;


procedure TLogger.SendError(Classes: TGroupsOfWhyLogMsg; const AText: String);
begin
  if Classes * ActiveClasses = [] then Exit;		 			  //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltError,AText,nil);
end;


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


procedure TLogger.Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Integer);
begin
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltWatch,AText+'='+IntToStr(AValue),nil);
end;


{$ifdef fpc}
procedure TLogger.Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Cardinal);
begin
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltWatch,AText+'='+IntToStr(AValue),nil);
end;
{$endif}


procedure TLogger.Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Double);
begin
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltWatch,AText+'='+FloatToStr(AValue),nil);
end;


procedure TLogger.Watch(Classes: TGroupsOfWhyLogMsg; const AText: String; AValue: Boolean);
begin
  if Classes * ActiveClasses = [] then Exit;	//pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltWatch,AText+'='+BoolToStr(AValue),nil);
end;


procedure TLogger.Watch(Classes: TGroupsOfWhyLogMsg; const AText, AValue: String);
begin
  if Classes * ActiveClasses = [] then Exit;    //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltWatch,AText+'='+AValue,nil);
end;


procedure TLogger.SubEventMethodBetweenEnterAndExitMethods(Classes: TGroupsOfWhyLogMsg; const AText: String);
begin
  if Classes * ActiveClasses = [] then Exit;    //pre-conditions: the intersection of Classes and ActivesClasses must not be empty

  SendStream(ltSubEventBetweenEnterAndExitMethods,AText,nil);
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

end.

