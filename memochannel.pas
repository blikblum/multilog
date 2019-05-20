unit MemoChannel;

{
  Copyright (C) 2006 Luiz Américo Pereira Câmara

  TMemoChannel contributed by Avra (Жељко Аврамовић). TMemoChannel was based on TFileChannel.

  This library is free software; you can redistribute it and/or modify it
  under the terms of the FPC modified LGPL licence which can be found at:
  http://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  {$ifndef fpc}fpccompat,{$endif} Classes, SysUtils, StdCtrls, Forms, Math, MultiLog;

const
  MEMO_MINIMAL_NUMBER_OF_TOTAL_LINES = 200;
  MEMO_MINIMAL_NUMBER_OF_LINES_TO_DELETE_AT_ONCE = 100; // must be lower or equal to MEMO_MINIMAL_NUMBER_OF_TOTAL_LINES

type
  TLogMsgData = record
    Text: string;
  end;
  PLogMsgData = ^TLogMsgData;

  TMemoChannelOption = (fcoShowHeader, fcoShowPrefix, fcoShowTime, fcoUnlimitedBuffer, fcoRotaryBuffer);
  TMemoChannelOptions = set of TMemoChannelOption;


  { TMemoChannel }

  TMemoChannel = class(TLogChannel)
  private
    FMsg: TLogMessage;
    FMemo: TMemo;
    FRelativeIdent: Integer;
    FBaseIdent: Integer;
    FShowHeader: Boolean;
    FShowTime: Boolean;
    FShowPrefix: Boolean;
    FUnlimitedBuffer: Boolean;
    FRotaryBuffer: Boolean;
    FTimeFormat: String;
    FShowWhyThisLogging: Boolean;
    FLogLinesLimit: Integer;
    FWrapper: TLogChannelWrapper;
    procedure SetLogLinesLimit(AValue: Integer);
    procedure SetShowTime(const AValue: Boolean);
    procedure UpdateIdentation;
    procedure Write(const AMsg: string);
    procedure WriteStringsOfMsgDataStream();
    procedure WriteComponentOfMsgDataStream();
  public
    constructor Create(AMemo: TMemo; AChannelOptions: TMemoChannelOptions = [fcoShowHeader, fcoShowTime] );
    destructor Destroy; override;
    procedure WriteAsyncQueue(Data: PtrInt);
    procedure Deliver(const AMsg: TLogMessage); override;
    procedure Init; override;
    procedure Clear; override;
    property ShowHeader: boolean read FShowHeader write FShowHeader;
    property ShowPrefix: boolean read FShowPrefix write FShowPrefix;
    property ShowTime: boolean read FShowTime write SetShowTime;
    property LogLinesLimit: integer read FLogLinesLimit write SetLogLinesLimit;
    property TimeFormat: String read FTimeFormat write FTimeFormat;
  end;

implementation

const
  LogPrefixes: array [ltInfo..ltCounter] of string = (
    'INFO',
    'ERROR',
    'WARNING',
    'VALUE',
    '>>ENTER METHOD',
    '<<EXIT METHOD',
    'CONDITIONAL',
    'CHECKPOINT',
    'STRINGS',
    'CALL STACK',
    'OBJECT',
    'EXCEPTION',
    'BITMAP',
    'HEAP INFO',
    'MEMORY',
    '', '', '', '', '',
    'WATCH',
    'COUNTER');

	QualifiersWhyMsgIsLogged: array [lcDebug..lcTrackSQLissue] of String = (
    'Debug',
    'Error',
    'Info',
    'Warning',
    'Event(s)',
    '', '', '', '',
    'TrackSQLissue');

{ TMemoChannel }

constructor TMemoChannel.Create(AMemo: TMemo; AChannelOptions: TMemoChannelOptions);
begin
  FMemo := AMemo;
  FWrapper := TLogChannelWrapper.Create(nil);
  FWrapper.Channel := Self;
  AMemo.FreeNotification(FWrapper);
  FShowPrefix := fcoShowPrefix in AChannelOptions;
  FShowTime := fcoShowTime in AChannelOptions;
  FShowHeader := fcoShowHeader in AChannelOptions;
  FShowWhyThisLogging:= false;
  FUnlimitedBuffer := fcoUnlimitedBuffer in AChannelOptions;
  FRotaryBuffer := fcoRotaryBuffer in AChannelOptions;
  Active := True;
  FLogLinesLimit := MEMO_MINIMAL_NUMBER_OF_TOTAL_LINES;
  FTimeFormat := 'hh:nn:ss:zzz';
end;

destructor TMemoChannel.Destroy;
begin
  FWrapper.Destroy;
  inherited Destroy;
end;

procedure TMemoChannel.Write(const AMsg: string);
var
  LogMsgToSend: PLogMsgData;
begin
  New(LogMsgToSend);
  LogMsgToSend^.Text := AMsg;
  Application.QueueAsyncCall(@WriteAsyncQueue, PtrInt(LogMsgToSend)); // put log msg into queue that will be processed from the main thread after all other messages
end;

procedure TMemoChannel.WriteAsyncQueue(Data: PtrInt);
var // called from main thread after all other messages have been processed to allow thread safe TMemo access
  ReceivedLogMsg: TLogMsgData;
  LineCount: Integer;
begin
  ReceivedLogMsg := PLogMsgData(Data)^;
  try
    if (FMemo <> nil) and (not Application.Terminated) then
    begin
      if FMemo.Lines.Count > LogLinesLimit then
      begin
        if FRotaryBuffer then
        begin
          FMemo.Lines.BeginUpdate;
          try // less flickering compared to deleting first line for each newly added line
            for LineCount := 1 to Max(LoglinesLimit div 10, MEMO_MINIMAL_NUMBER_OF_LINES_TO_DELETE_AT_ONCE) do
              FMemo.Lines.Delete(0);
          finally
            FMemo.Lines.EndUpdate;
          end;
        end
        else
          if not FUnlimitedBuffer then
            FMemo.Clear; // clear whole buffer when limit is reached
      end;
      FMemo.Append(ReceivedLogMsg.Text)
    end;
  finally
    Dispose(PLogMsgData(Data));
  end;
end;

procedure TMemoChannel.UpdateIdentation;
var
  S: string;
begin
  S := '';
  if FShowTime then
    S := FormatDateTime(FTimeFormat, Time);
  FBaseIdent := Length(S) + 3;
end;

procedure TMemoChannel.SetShowTime(const AValue: Boolean);
begin
  FShowTime := AValue;
  UpdateIdentation;
end;

procedure TMemoChannel.SetLogLinesLimit(AValue: Integer);
begin
  FLogLinesLimit := Max(Abs(AValue), MEMO_MINIMAL_NUMBER_OF_TOTAL_LINES);
end;

procedure TMemoChannel.WriteStringsOfMsgDataStream();
var
  i: integer;
begin
  if FMsg.Data.Size = 0 then Exit;	// pre-condition
  with TStringList.Create do begin
    try
      FMsg.Data.Position:=0;
      LoadFromStream(FMsg.Data);
      for i := 0 to Count - 1 do
       Write(Space(FRelativeIdent+FBaseIdent) + Strings[i]);
    finally
      Destroy;
    end;
  end;
end;

procedure TMemoChannel.WriteComponentOfMsgDataStream();
var
  TextStream: TStringStream;
begin
  TextStream := TStringStream.Create('');
  FMsg.Data.Seek(0, soFromBeginning);
  ObjectBinaryToText(FMsg.Data, TextStream);
  write(TextStream.DataString);	  //todo: better handling of format

  TextStream.Destroy;
end;

procedure TMemoChannel.Deliver(const AMsg: TLogMessage);
var
  WholeMsg: string;
begin
  FMsg:= AMsg;
  FShowWhyThisLogging:= (FMsg.WhyThisLogging <> lcNone);
  WholeMsg := '';
  //Exit method identation must be set before
  if (FMsg.MsgType = ltExitMethod) and (FRelativeIdent >= 2) then
    Dec(FRelativeIdent, 2);

  try
    if FShowTime then
      WholeMsg := FormatDateTime(FTimeFormat, FMsg.MsgTime) + '   ';
    WholeMsg := WholeMsg + Space(FRelativeIdent);
    //FShowPrefix can serve as qualifier for each current msg, allowing further thematic extractions
    if FShowPrefix then
      WholeMsg := WholeMsg + (LogPrefixes[FMsg.MsgType] + ':   ');
    //write qualifier explaining Why this Msg (is | is not) logged?
    if FShowWhyThisLogging then begin
    	if ((FMsg.MsgType = ltEnterMethod) or (FMsg.MsgType = ltExitMethod)) then
      	WholeMsg := WholeMsg + '|' + QualifiersWhyMsgIsLogged[FMsg.WhyThisLogging] + '| '  	// all on one precise visual lines for >>>ltEnterMethod .. <<<ltExitMethod
      else
    		WholeMsg := WholeMsg + LineEnding + '|' + QualifiersWhyMsgIsLogged[FMsg.WhyThisLogging] + '| '
    end;
    WholeMsg:= WholeMsg + FMsg.MsgText;
    write(WholeMsg);
    //if there's a TStream to write
    if FMsg.Data <> nil then
    begin
      case FMsg.MsgType of
        ltStrings, ltCallStack, ltHeapInfo, ltException, ltMemory: WriteStringsOfMsgDataStream();
        ltObject: WriteComponentOfMsgDataStream();
      end;
    end;
  finally
    //Update enter method identation
    if (FMsg.MsgType = ltEnterMethod) then
      Inc(FRelativeIdent, 2);
  end;
end;

procedure TMemoChannel.Init;
var
  BufferLimitedStr: string;
begin
  if FRotaryBuffer or not FUnlimitedBuffer then
    BufferLimitedStr := ' (buffer limited to ' + IntToStr(LogLinesLimit) + ' lines)'
  else
    BufferLimitedStr := '';

  if FShowHeader then
    Write('=== Log Session Started at ' + DateTimeToStr(Now) + ' by ' + ApplicationName + BufferLimitedStr + ' ===');
  UpdateIdentation;
end;

procedure TMemoChannel.Clear;
begin
  // no need to implement this abstract method in TMemoChannel
end;


end.
