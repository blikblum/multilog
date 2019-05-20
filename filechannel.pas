unit FileChannel;

{ Copyright (C) 2006 Luiz Américo Pereira Câmara

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

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
  {$ifndef fpc}fpccompat,{$endif} Classes, SysUtils, multilog;

type

  TFileChannelOption = (fcoShowHeader, fcoShowPrefix, fcoShowTime);
  TFileChannelOptions = set of TFileChannelOption;

  { TFileChannel }

  TFileChannel = class (TLogChannel)
  private
    FMsg: TLogMessage;
    FPathFileName: string;
    	FFileHandle: TextFile;
    FRelativeIdent: Integer;
    FBaseIdent: Integer;
    FShowHeader: Boolean;
    FShowTime: Boolean;
    FShowPrefix: Boolean;
    FTimeFormat: string;
    FShowWhyThisLogging: Boolean;
    procedure SetShowTime(const AValue: Boolean);
    procedure UpdateIdentation;
    procedure WriteStringsOfMsgDataStream();
    procedure WriteComponentOfMsgDataStream();
    (* log size management *)
    function GetFileLogSize: UInt64;
  protected
    property PathFileName: string read FPathFileName;
  public
    constructor Create(const AFileName: String; AChannelOptions: TFileChannelOptions = [fcoShowHeader, fcoShowTime]);
    procedure Clear; override;
    procedure Deliver(const AMsg: TLogMessage);override;
    procedure Init; override;
    property ShowHeader: Boolean read FShowHeader write FShowHeader;
    property ShowPrefix: Boolean read FShowPrefix write FShowPrefix;
    property ShowTime: Boolean read FShowTime write SetShowTime;
    property TimeFormat: String read FTimeFormat write FTimeFormat;
  end;

implementation


const
  LogPrefixes: array [ltInfo..ltCounter] of String = (
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
    '','','','','',
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



{ TFileChannel }

procedure TFileChannel.UpdateIdentation;
var
  S:String;
begin
  S:='';
  if FShowTime then
    S:=FormatDateTime(FTimeFormat,Time);
  FBaseIdent:=Length(S)+3;
end;

procedure TFileChannel.SetShowTime(const AValue: Boolean);
begin
  FShowTime:=AValue;
  UpdateIdentation;
end;

procedure TFileChannel.WriteStringsOfMsgDataStream();
var
  i: Integer;
begin
  if FMsg.Data.Size = 0 then Exit;	// pre-condition
  with TStringList.Create do begin
    try
      FMsg.Data.Position:=0;
      LoadFromStream(FMsg.Data);
      for i:= 0 to Count - 1 do
	      WriteLn(FFileHandle,Space(FRelativeIdent+FBaseIdent) + Strings[i]);
    finally
      Destroy;
    end;
  end;
end;

procedure TFileChannel.WriteComponentOfMsgDataStream();
var
  TextStream: TStringStream;
begin
  TextStream:=TStringStream.Create('');
  FMsg.Data.Seek(0,soFromBeginning);
  ObjectBinaryToText(FMsg.Data,TextStream);
  Write(FFileHandle, TextStream.DataString);  	//todo: better handling of format

  TextStream.Destroy;
end;

{------------------------------------------------------------------
§Explanations: returns de files'size.
-------------------------------------------------------------------}
function TFileChannel.GetFileLogSize: UInt64;
begin
	// comming soon, with FiMaxLogSize: UInt64;	...
end;




constructor TFileChannel.Create(const AFileName: String; AChannelOptions: TFileChannelOptions);
begin
  FShowPrefix := fcoShowPrefix in AChannelOptions;
  FShowTime := fcoShowTime in AChannelOptions;
  FShowHeader := fcoShowHeader in AChannelOptions;
  FShowWhyThisLogging:= false;
  Active := True;
  FTimeFormat := 'hh:nn:ss:zzz';
  FPathFileName := AFileName;
end;

procedure TFileChannel.Clear;
begin
  if FPathFileName <> '' then
    Rewrite(FFileHandle);
end;

procedure TFileChannel.Deliver(const AMsg: TLogMessage);
var
  WholeMsg: string;
begin
  FMsg:= AMsg;
  FShowWhyThisLogging:= (FMsg.WhyThisLogging <> lcNone);
  WholeMsg := '';
  //Exit method identation must be set before
  if (FMsg.MsgType = ltExitMethod) and (FRelativeIdent >= 2) then
    Dec(FRelativeIdent, 2);
  if (FPathFileName <> '') then
    Append(FFileHandle);

  try
    if FShowTime then
      WholeMsg := FormatDateTime(FTimeFormat, FMsg.MsgTime) + ' ';
		WholeMsg := WholeMsg + Space(FRelativeIdent);
    //FShowPrefix can serve as qualifier for each current msg, allowing further thematic extractions
    if FShowPrefix then
      WholeMsg := WholeMsg + (LogPrefixes[FMsg.MsgType] + ':   ');

    //write qualifier explaining Why this Msg (is | is not) logged?
    if FShowWhyThisLogging then begin
      if ((FMsg.MsgType = ltEnterMethod) or (FMsg.MsgType = ltExitMethod)) then
      	 WholeMsg := WholeMsg + '|' + QualifiersWhyMsgIsLogged[FMsg.WhyThisLogging] + '| '
			else
         WholeMsg := WholeMsg + LineEnding + '|' + QualifiersWhyMsgIsLogged[FMsg.WhyThisLogging] + '| '
    end;
    WholeMsg:= WholeMsg + FMsg.MsgText;
    WriteLn(FFileHandle, WholeMsg);
    //if there's a TStream to write
    if FMsg.Data <> nil then
    begin
      case FMsg.MsgType of
        ltStrings, ltCallStack, ltHeapInfo, ltException, ltMemory: WriteStringsOfMsgDataStream();
        ltObject: WriteComponentOfMsgDataStream();
      end;
    end;
  finally
    if (FPathFileName <> '') then
      Close(FFileHandle);
    //Update enter method identation
    if (FMsg.MsgType = ltEnterMethod) then
      Inc(FRelativeIdent, 2);
  end;
end;

procedure TFileChannel.Init;
begin
  if FPathFileName <> '' then
  begin
    Assign(FFileHandle, FPathFileName);
    if FileExists(FPathFileName) then
      Append(FFileHandle)
    else
      Rewrite(FFileHandle);
  end
  else
    FFileHandle := Output;
  if FShowHeader then
    WriteLn(FFileHandle,'=== Log Session Started at ',DateTimeToStr(Now),' by ',ApplicationName,' ===');
  if FPathFileName <> '' then
    Close(FFileHandle);
  UpdateIdentation;
end;

end.


