unit FileChannel;
{$REGION 'Xls: Comments section'}
{§< Unit containing TFileChannel. Updated by devEric69 (Ėric Moutié).@br
@html(<span style="background-color: #ffff00"><u>TMemoChannel can be thread safe!</u>
There is the possibility to see the indentation of events in the calls stack.</u></span>)
}
{$ENDREGION}

{
Copyright (C) 2006 Luiz Américo Pereira Câmara

TFileChannel modified by devEric69 (Éric Moutié):
- it became thread safe, eg TMemoChannel's modifications.
- added the possibility to have an overview, a "film" of the events for which we want to anderstand the order of their inter-calls.

This library is free software; you can redistribute it and/or modify it
under the terms of the FPC modified LGPL licence which can be found at:
http://wiki.lazarus.freepascal.org/FPC_modified_LGPL.
}

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  {$ifndef fpc}fpccompat,{$endif} Classes, SysUtils, db, multilog;

type
  TFileLogThemes = (fltLogNormal, fltLogSQL);
  TFileChannelOption = (fcoShowHeader, fcoShowPrefix, fcoShowTime);
  TFileChannelOptions = set of TFileChannelOption;
  TStatesOfLogstack = record
  	FbEnterMethodCalled: Boolean;
    FbShouldIndentASubEvent: Boolean;
    FbExitMethodCalled: Boolean;
  end;
	PStatesOfLogstack = ^TStatesOfLogstack;


  { TFileChannelDataHelperFunctions }

type
  TFileChannelDataHelperFunctions = class
    private
      FsFilePath: string;
        FiHandleDataFile: THandle;
        FiLockSpecifier: Byte;
    protected
      function GetSize: Int64;
    public
      constructor Create(const sFilePath: string); reintroduce; overload;
      destructor Destroy; override;
      property Size: Int64 read GetSize;
  end;


  { TFileChannel }

  TFileChannel = class (TLogChannel)
  private
    FMsg: TLogMessage;
    {§ (!!!) It must be *full* path like '/home/user_1/.TheAppli/Log.txt' or 'C:\Program Files\TheAppli\Log.txt'.}
    FFullPathFileName: string;
    	FFileHandle: TextFile;
     	FieCurrentFileLogTheme: TFileLogThemes;
    FBaseIdent: Integer;
    	FRelativeIdent: Integer;
 		FbIndentCallstackTriggeredByEntermethod: boolean;
    	FiCallstackIndent: Integer;
      FiAddressEBPcurrentFrame: Pointer;
	    FiLevelStack: integer;
    FShowHeader: Boolean;	//'=== Log Session Started at '.../...
      {§ is set to True when '=== Log Session Started at ' has been written one time in the log normal, at the launch of the application.}
    	FbShowHeaderNormalWritten: boolean;
      {§ is set to True when '=== Log Session Started at ' has been written one time in the log SQL, at the launch of the application.}
      FbShowHeaderSQL_Written: boolean;
    FShowTime: Boolean;
    FShowPrefix: Boolean;
    FTimeFormat: string;
    FpRecStatesOfLogstack: PStatesOfLogstack;
    FiMaxLogSize: int64;
    FbWantSpecificFileForSQLstatements: Boolean;
    procedure SetShowTime(const AValue: Boolean);
    (* identation management *)
    procedure UpdateBaseTimeIdentation;
    procedure UpdateRelativeIndentation;
    procedure UpdateEBPIndentation;
{$REGION 'Xls: Comments section'}
{§ Explanations: @br
@HTML(<u>Warning:</u> it is not used to send a Call Stack back from a crash, which you would like to indent nicely: the crashed Call stack is already recovered in multiLog.) @br
@br
This is used to see the indentation of events, that have an event like @code(send|watch|)... coded between: @br
- the beginning of an event @code(procedure TForm1.btnPostClick(Sender: TObject); enter_METHOD_[>>];)... @br
- ...and the end of the same event @code(exit_METHOD)_[<<]). @br
Indeed, by considering only potentially codable events, such as those used for a database management, they can be encapsulated in a complex way,
between the basic click of a button @code(btnPostData();) that can initiate the request to repaint controls, ..., format the input, ...,
data transfer via a datalink, ..., data verification, ..., related calculations of calculated fields, ..., datSet Post, ... @br
In addition, many events have an onBeforEvent, onEvent and onAfterEvent. @br
@br
==> the view of the graph of the indentation of the events \ methods \... called between @code(enter_METHOD_[>>>];) and @code(exit_METHOD)_[<<<];),
as they are "pushed" \ "poped" into the stack of calls makes it possible to understand through an indented holistic view,
an event management whose inter-calls of sub-events are not understood, if they have been coded in an event called too early or too late,
if an under-event has been called several times by other under-events without regard to context, etc. @br
@br
@HTML(<b><u>It is therefore a means of more in-depth understanding an event programming, and therefore of helping to develop events management:</u></b> @br
<pre>
procedure TForm1.SubLogClick(Sender: TObject);
begin
|--> EnterMethod [&gt;&gt;]: Logger.EnterMethod(Sender,'SubLogClick');
   |--> entrée event 1: Logger.SubEventBetweenEnterAndExitMethods('&gt;event DoSmallCodePieceNum2 + Sender=' + Sender.ClassName);
      |-->
         |-->
      |-->
         |-->
            |--> Logger.SubEventBetweenEnterAndExitMethods('&gt;&lt;event DoSmallCodePieceNum6 + Sender=' + Sender.ClassName + ' (I''m the last push inside the callstack. Now, we pop from the callstack.)');
         |-->
      |-->
   |--> sortie event 1: Logger.SubEventBetweenEnterAndExitMethods('&lt;event DoSmallCodePieceNum1 + Sender=' + Sender.ClassName);
|--> ExitMethod [&lt;&lt;: ExitMethod(Sender,'SubLogClick');
end;
</pre>
)
}
{$ENDREGION}
    function GetEBPIndentation: integer;
    function CallstackSize: int64;
{$REGION 'Xls: Comments section'}
{§ Explanations: returns de files'size; -1 if it doesn't exist.
}
{$ENDREGION}
    function GetFileLogSize: int64;
    (* methods of writing stream-text in the log *)
    procedure WriteStringsOfMsgDataStream();
    procedure WriteComponentOfMsgDataStream();
    function StreamIntoRawString(): string;
    (* log management *)
{$REGION 'Xls: Comments section'}
{§ Explanations: check if the size is larger than the maximum allowed; in this case, the log file est
saved and a new one is created and used.
}
{$ENDREGION}
    procedure CheckIfNecessaryToSaveAndCreateNewFileLog;
{$REGION 'Xls: Comments section'}
{§ Explanations: create a NEW FsPathFileLog file!
}
{$ENDREGION}
    procedure FileLogCreate;
    function GetTimestampFromToday: string;
{$REGION 'Xls: Comments section'}
{§ Explanations: close, remove all lock(s), threads's connexion(s), with FsPathFileLog.
}
{$ENDREGION}
    procedure CloseFileLog;
{$REGION 'Xls: Comments section'}
{§ Explanations: ckeck if FMsg est SQL related.
@returns(returns True, if it's a SQL-oriented message.)
}
{$ENDREGION}
    function IsSQLwordPresentInMsg: Boolean;
{$REGION 'Xls: Comments section'}
{§ Explanations: set the FieCurrentFileLogTheme property. Does not have to be public or published. Usefulness due to its side effects, nothing more.}
{$ENDREGION}
    procedure SetCurrentFileLogTheme(ACurrentFileLogTheme: TFileLogThemes);
    procedure OpenForCurrentFileLogThemeTheRightFileLog;
  protected
		property PathFileName: string read FFullPathFileName;
		property CurrentFileLogTheme: TFileLogThemes read FieCurrentFileLogTheme write SetCurrentFileLogTheme default fltLogNormal;
  public
{$REGION 'Xls: Comments section'}
{§ Explanations: constructor.
@param(AFileName is the *full* path and the name of the logging file.)
@param(AChannelOptions: it's a set of options (timestamp, Why this msg, ...) that qualifies each message written in the log file.)
@param(AbWantSpecificFileForSQLstatements:
=False if you want to @HTML(<u>use a single log file for your whole application</u>), where you will write everything.@br
=True if you want to @HTML(<u>create another SQL-oriented log file</u>), in which only the SQL Exceptions, the msq which may have their dumped
content as text with a substring 'SQL' or 'sql' will be logged. It's an SQL-oriented log file named "AFileName+'_SQL'+.your_possible_file_ext".)
@returns(object instance).
}
{$ENDREGION}
    constructor Create(const AFileName: String; AChannelOptions: TFileChannelOptions = [fcoShowHeader, fcoShowTime]; AbWantSpecificFileForSQLstatements: boolean = false);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Deliver(const AMsg: TLogMessage);override;
{$REGION 'Xls: Comments section'}
{§ Explanations: init, post-constructor; @br
goal: create the log file(s) that will be used by the application,
according to the parameters passed to the constructor.}
{$ENDREGION}
    procedure Init; override;
    property ShowHeader: Boolean read FShowHeader write FShowHeader;
    property ShowPrefix: Boolean read FShowPrefix write FShowPrefix;
    property ShowTime: Boolean read FShowTime write SetShowTime;
    property TimeFormat: String read FTimeFormat write FTimeFormat;
    property iMaxLogSize: int64 read FiMaxLogSize default 1000000; (*1 Mo = 4.10^9 [b]*)
  end;




implementation

uses
	FileUtil, fpccompat, strutils, multilogAsm, math, RtlConsts;


var
  GuardianFile: TGuardian;


const
  LogPrefixes: array [ltInfo..ltSubEventBetweenEnterAndExitMethods] of String = (
    'INFO',
    'ERROR',
    'WARNING',
    'VALUE',
    '>>ENTER METHOD',
    '<<EXIT  METHOD',
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
    'COUNTER',
    'SUBEVENT');

	QualifiersWhyMsgIsLogged: array [lcDebug..lcStudyChainedEvents] of String = (
    'Debug',
    'Error',
    'Info',
    'Warning',
    'Event(s)',
    '', '', '', '',
    'StudyChainedEvents');

  csSuffixForSQLlogFile: string = '_SQL';

{ TFileChannel }


function TFileChannel.CallstackSize: int64;
begin
  result:= int64(StackTop)-int64(StackBottom);	// =~ same information as StackLength, ie it's an offset.
end;


procedure TFileChannel.SetShowTime(const AValue: Boolean);
begin
  FShowTime:= AValue;
end;


procedure TFileChannel.WriteStringsOfMsgDataStream();
var
  i: Integer;
{$IFDEF DEBUG}
sTemp: string;
{$ENDIF}
  oTStringList: TStringList;
begin
  if FMsg.Data.Size = 0 then Exit;	// pre-condition
  oTStringList:= TStringList.Create;
  with oTStringList do begin
    try
      FMsg.Data.Position:=0;
      LoadFromStream(FMsg.Data);
      for i:= 0 to Count - 1 do begin
{$IFDEF DEBUG}
sTemp:= Strings[i];
{$ENDIF}
        WriteLn(FFileHandle,Space(FBaseIdent+FRelativeIdent) + Strings[i]);
      end;
    finally
      FreeAndNil(oTStringList);
    end;
  end;
end;


function TFileChannel.StreamIntoRawString(): string;
begin
  if not Assigned(FMsg.Data) then begin	// pre-condition
		result:= '';
		Exit;
  end;

  with TStringStream.Create('') do begin
    try
      FMsg.Data.Position:= 0;
      CopyFrom(FMsg.Data, FMsg.Data.Size - FMsg.Data.Position);
      Result:= DataString;
    finally
      Free;
    end;
  end;
end;


procedure TFileChannel.WriteComponentOfMsgDataStream();
var
  TextStream: TStringStream;
begin
  TextStream:= TStringStream.Create('');
  FMsg.Data.Seek(0,soFromBeginning);
  ObjectBinaryToText(FMsg.Data,TextStream);
  Write(FFileHandle, TextStream.DataString);  	//todo: better handling of format

  TextStream.Destroy;
end;


procedure TFileChannel.CheckIfNecessaryToSaveAndCreateNewFileLog;

      procedure BackupEtDeleteFileLog;
      {Explanations: Copy the log file with "xxx_timestamp.log.txt.old" extension.
      Then, delete the original saved file.}
      var
        sFullPathFileDest: TFileName;
        sFilePath, sFileOldExt, sFileNewExt, sFileName: string;
      begin
        (*close the log-file, before making its copy*)
        CloseFileLog;

        sFilePath:= ExtractFilePath(FFullPathFileName);
        sFileOldExt:= ExtractFileExt(FFullPathFileName);
        sFileNewExt:= '.old'+ExtractFileExt(FFullPathFileName);
        sFileName:= ExtractFileName(FFullPathFileName);
        sFileName:= LeftStr(sFileName, Length(sFileName)-Length(sFileOldExt));
        (*finally, the backup-file will be named... _AAAAMMJJ_HHMM.old.xxx*)
        sFullPathFileDest:= sFilePath+sFileName+'_'+GetTimestampFromToday+'_'+LeftStr(ReplaceStr(TimeToStr(Now), ':', ''), 4)+sFileNewExt;
        if FileExists(sFullPathFileDest) then
        	SysUtils.DeleteFile(sFullPathFileDest);
        CopyFile(FFullPathFileName, sFullPathFileDest);
        SysUtils.DeleteFile(FFullPathFileName);
      end;

begin
  if (GetFileLogSize > FiMaxLogSize) then begin
    BackupEtDeleteFileLog;
    FileLogCreate;
  end;
end;


procedure TFileChannel.CloseFileLog;
begin
  {$I-}
  try
    AssignFile(FFileHandle, FFullPathFileName);
  finally
    CloseFile(FFileHandle);
    if IOResult = 103 then begin
      (*CloseFile sends IOResult-error 103, if the file wasn't open; reset IOResult with 0.*)
      InOutRes:= 0;
    end;
  end;
  {$I+}
end;


procedure TFileChannel.FileLogCreate;
begin
  Assert(not FileExists(FFullPathFileName), 'TFileChannel.FileLogCreate: File '+FFullPathFileName+' must already be deleted!');	// How to call this method

  AssignFile(FFileHandle, FFullPathFileName);
	ReWrite(FFileHandle);	// Create new log file
  Writeln(FFileHandle, '=== File created at ', DateTimeToStr(Now),' by ', ApplicationName,' ===');
  WriteLn(FFileHandle,'=== Log Session Started at ', DateTimeToStr(Now),' by ', ApplicationName,' ===');
  Close(FFileHandle);
end;



function TFileChannel.GetFileLogSize: int64;
var
  oFileData: TFileChannelDataHelperFunctions;
begin
  oFileData:= TFileChannelDataHelperFunctions.Create(Self.FFullPathFileName);
  Result:= oFileData.Size;
  FreeAndNil(oFileData);
end;


function TFileChannel.GetTimestampFromToday: string;
var
  sTodayDateTime: string;
begin
  sTodayDateTime:= FormatDateTime('yyyy/mm/dd', Now);
  {$if defined(Win64)}
    result:= StringReplace(sTodayDateTime, '/', '-', [rfReplaceAll]); // the slash is not allowed in a Windows file name
  {$ElseIf defined(UNIX)}
    result:= sTodayDateTime;
  {$endif}
end;



constructor TFileChannel.Create(const AFileName: String; AChannelOptions: TFileChannelOptions; AbWantSpecificFileForSQLstatements: boolean = false);
begin
  if Logger.ThreadSafe then
    GuardianFile:= TGuardian.Create;
  FiMaxLogSize:= 1000000;                 		//default property
  FShowTime := fcoShowTime in AChannelOptions;
  FShowHeader := fcoShowHeader in AChannelOptions;
  	FbShowHeaderNormalWritten:= False;
    FbShowHeaderSQL_Written:= False;
  FShowPrefix := fcoShowPrefix in AChannelOptions;
  Active := True;
  FTimeFormat := 'hh:nn:ss:zzz';
  FFullPathFileName := AFileName;
  	FieCurrentFileLogTheme:= fltLogNormal;		//default property
  FbWantSpecificFileForSQLstatements:= AbWantSpecificFileForSQLstatements;
  FbIndentCallstackTriggeredByEntermethod:= false;
	FiAddressEBPcurrentFrame:= StackTop; FiLevelStack:= 0;
  FpRecStatesOfLogstack:= new(PStatesOfLogstack);
  with FpRecStatesOfLogstack^ do begin
    FbEnterMethodCalled:= false;
    FbExitMethodCalled:= false;
  end;
end;


destructor TFileChannel.Destroy;
begin
  if Assigned(FpRecStatesOfLogstack) then
    Dispose(FpRecStatesOfLogstack);
  if Assigned(GuardianFile) then
    FreeAndNil(GuardianFile);
  inherited Destroy;
end;



procedure TFileChannel.Init;
begin
  //Create || Open the log file(s)
  CurrentFileLogTheme:= fltLogNormal;
  UpdateBaseTimeIdentation;
  if (FbWantSpecificFileForSQLstatements) then begin
    CurrentFileLogTheme:= fltLogSQL;
    UpdateBaseTimeIdentation;
  end;
  //we "re-point" to the default fltLogNormal log file
  CurrentFileLogTheme:= fltLogNormal;
end;


procedure TFileChannel.Clear;
begin
  if FFullPathFileName <> '' then
    Rewrite(FFileHandle);
end;



function TFileChannel.IsSQLwordPresentInMsg: Boolean;
var
  bIsSQLMsg: Boolean;
  sTemp: string;
begin
  bIsSQLMsg:= AnsiContainsStr( UpperCase(FMsg.MsgText), 'SQL');
  sTemp:= UpperCase( StreamIntoRawString() );
  bIsSQLMsg:= bIsSQLMsg or AnsiContainsStr(sTemp, 'SQL');
  Result:= bIsSQLMsg;
end;



function TFileChannel.GetEBPIndentation: integer;
var
  i: integer;
  iPt1: pointer; //first pusher entry by current_method, in the stack
begin
  iPt1:= GetEBP;
	if iPt1 < FiAddressEBPcurrentFrame then begin
    (*récup nouvelle base du dernier cadre pushé*)
    FiLevelStack:= FiLevelStack+1;
  end
  else if iPt1 > FiAddressEBPcurrentFrame then begin
    (*récup nouvelle base du dernier cadre en haut de la pile suite au pop du précédent*)
    FiLevelStack:= math.Max(0, FiLevelStack-1);
  end;
  (*le prochain coup que l'on revient dans cette fonction, on connaîtra FiAdresseEBPCadreCourant*)
  FiAddressEBPcurrentFrame:= iPt1;
  result:= 0;
  if FiLevelStack > 0 then begin
    for i:= 0 to FiLevelStack-1 do begin
      result:= result + 4; // How many space(s) should we add after Logger.EnterMethod()?
    end;
  end;
end;


procedure TFileChannel.UpdateBaseTimeIdentation;
var
  S: String;
begin
  S:= '';
  if FShowTime then
    S:= FormatDateTime(FTimeFormat,Time);
  FBaseIdent:= Length(S)+1;
end;


procedure TFileChannel.UpdateRelativeIndentation;
begin
  if (FMsg.MsgType = ltEnterMethod) then begin
    //reference's "screenshot" of the top of callstack
  	FbIndentCallstackTriggeredByEntermethod:= True;
    FiAddressEBPcurrentFrame:= StackTop;
    FiCallstackIndent:= 0; FiLevelStack:= 0;
    //Update EnterMethod identation
    Inc(FRelativeIdent, 3);
    with FpRecStatesOfLogstack^ do begin
    	FbEnterMethodCalled:= True;
    	FbShouldIndentASubEvent:= True;
    	FbExitMethodCalled:= False;
    end;
  end
  else if (FMsg.MsgType = ltExitMethod) then begin
    //whe stop to track the callstack: it's the end of the indentation of sub-events
    FbIndentCallstackTriggeredByEntermethod:= False;		// End of callstack [ltEnterMethod..ltExitMethod] indentation
    FiCallstackIndent:= 0; FiLevelStack:= 0;
    //Update EnterMethod identation
    Dec(FRelativeIdent, 3);
    with FpRecStatesOfLogstack^ do begin
    	FbEnterMethodCalled:= True;
    	FbShouldIndentASubEvent:= False;
    	FbExitMethodCalled:= True;
    end;
  end;
end;


procedure TFileChannel.UpdateEBPIndentation;
begin
  if FbIndentCallstackTriggeredByEntermethod and (FMsg.MsgType = ltSubEventBetweenEnterAndExitMethods) then begin
    //we are always between [ltEnterMethod..ltExitMethod], and that's a sub-event: so, it must be indented via it's level in the callstack
    with FpRecStatesOfLogstack^ do begin
    	FbEnterMethodCalled:= True;
    	FbShouldIndentASubEvent:= True;
    	FbExitMethodCalled:= False;
    end;
    //Update EBPIndentation
    FiCallstackIndent:= GetEBPIndentation;
	end
	else if FbIndentCallstackTriggeredByEntermethod and (FMsg.MsgType <> ltSubEventBetweenEnterAndExitMethods) then begin
    //we are always beteween [ltEnterMethod..ltExitMethod] and that's not a sub-event: so, it must not be indented via it's level in the callstack
    with FpRecStatesOfLogstack^ do begin
			FbEnterMethodCalled:= True;
    	FbShouldIndentASubEvent:= False;
    	FbExitMethodCalled:= False;
    end;
  end
end;


procedure TFileChannel.OpenForCurrentFileLogThemeTheRightFileLog;
var
  iPosSQL: integer;
begin
  iPosSQL:= Pos(csSuffixForSQLlogFile, FFullPathFileName);
  //Check for correct use: FFullPathFileName must be correctly set according with FieCurrentFileLogTheme
  Assert(  	((FieCurrentFileLogTheme=fltLogSQL) and (iPosSQL>0)) or
  					((FieCurrentFileLogTheme=fltLogNormal) and (iPosSQL=0)), 'incoherent FieCurrentFileLogTheme & FFullPathFileName. See SetForCurrentFileLogThemeFullPathFileName...');

  if (FFullPathFileName <> '') then begin
    Assign(FFileHandle, FFullPathFileName);	//(re)-create the file's OS-inode-Handle, in bijection with the assigned file
    if FileExists(FFullPathFileName) then
      Append(FFileHandle)
    else
      Rewrite(FFileHandle);
  end
  else
    FFileHandle := Output;
  //If Asked, Write the start of application's session, but only one time per application's life
  if FShowHeader then begin
    if (FieCurrentFileLogTheme = fltLogNormal) and not FbShowHeaderNormalWritten then begin
			WriteLn(FFileHandle, '=== Log Session Started at ', DateTimeToStr(Now), ' by ', ApplicationName, ' ===');
      FbShowHeaderNormalWritten:= True;
    end
    else if (FieCurrentFileLogTheme = fltLogSQL) and not FbShowHeaderSQL_Written then begin
      WriteLn(FFileHandle, '=== Log Session Started at ', DateTimeToStr(Now), ' by ', ApplicationName, ' ===');
      FbShowHeaderSQL_Written:= True;
    end;
  end;
  if FFullPathFileName <> '' then
    Close(FFileHandle);
end;



procedure TFileChannel.SetCurrentFileLogTheme(ACurrentFileLogTheme: TFileLogThemes);

  							procedure SetTheRightThemeChoiceFileLogToBeWritten;
                begin
                  CloseFileLog;
                  //FFullPathFileName points to the "verbose" log file of the software
                  if (FieCurrentFileLogTheme = fltLogNormal) then
                    FFullPathFileName:= StringReplace(FFullPathFileName, csSuffixForSQLlogFile, '', [rfReplaceAll])
                  //FFullPathFileName points to the file containing SQL \ Exception \ ... queries: database management software crashes 95%, due to contextually false SQL queries
                  else if (FieCurrentFileLogTheme = fltLogSQL) then begin
                    FFullPathFileName:= ExtractFilePath(FFullPathFileName) + StringReplace(ExtractFileName(FFullPathFileName), ExtractFileExt(FFullPathFileName), '', [rfReplaceAll]) + csSuffixForSQLlogFile + ExtractFileExt(FFullPathFileName);
                  end;
                  OpenForCurrentFileLogThemeTheRightFileLog;
                end;

begin
  //in which logging file should we write?
  if (FieCurrentFileLogTheme <> ACurrentFileLogTheme) then
    FieCurrentFileLogTheme:= ACurrentFileLogTheme;
 	SetTheRightThemeChoiceFileLogToBeWritten;
end;


procedure TFileChannel.Deliver(const AMsg: TLogMessage);
var
  sWholeMsg: string;
  bIsSQLwordPresentInMsg: boolean;
begin
  try

    //only one thread at a time, can now execute the code below; the others must be patient
    if Logger.ThreadSafe then
      GuardianFile.Acquire;

    FMsg:= AMsg;
    //has the maximum allowable limit size fot the log file been reached?
		CheckIfNecessaryToSaveAndCreateNewFileLog;
		//we point to the right logging file according to what will be logged, and where it should be logged
    if (FbWantSpecificFileForSQLstatements) then begin
			bIsSQLwordPresentInMsg:= IsSQLwordPresentInMsg;
      if bIsSQLwordPresentInMsg then
        CurrentFileLogTheme:= fltLogSQL
			else
        CurrentFileLogTheme:= fltLogNormal;
		end
    else
	    CurrentFileLogTheme:= fltLogNormal;

    sWholeMsg := '';
    if FShowTime then begin
      sWholeMsg := FormatDateTime(FTimeFormat, FMsg.MsgTime) + ' ';
    	UpdateBaseTimeIdentation;
    end;
    //Update ExitMethod identation
    UpdateRelativeIndentation;
    sWholeMsg:= sWholeMsg + fpccompat.Space(FRelativeIdent);
    //FShowPrefix can serve as prime qualifier for each current msg, allowing further thematic extractions
    if FShowPrefix then
      sWholeMsg := sWholeMsg + (LogPrefixes[FMsg.MsgType] + ':   ');
    //write second qualifier explaining Why this Msg (is | is not) logged?
    if (FMsg.WhyThisLogging <> lcNone) and ((FMsg.MsgType = ltEnterMethod) or (FMsg.MsgType = ltExitMethod)) then
      	 sWholeMsg := sWholeMsg + '|' + QualifiersWhyMsgIsLogged[FMsg.WhyThisLogging] + '| ';
		//Update if there are a sequence of "lcStudyChainedEvents Why messages" (due to a sequence of [ltEnterMethod... ltSubEventBetweenEnterAndExitMethods_, ltSubEventBetweenEnterAndExitMethods_2, etc, ...ltExitMethod]) to study.
		UpdateEBPIndentation;
    sWholeMsg:= sWholeMsg + fpccompat.Space( ifthen(FpRecStatesOfLogstack^.FbShouldIndentASubEvent, FiCallstackIndent, 0) );
    sWholeMsg:= sWholeMsg + FMsg.MsgText;

    if (FFullPathFileName <> '') then
      Append(FFileHandle);
    WriteLn(FFileHandle, sWholeMsg);
    //Update ExitMethod identation
    UpdateRelativeIndentation;
    //if there's a TStream to write
    if FMsg.Data <> nil then
    begin
      case FMsg.MsgType of
        ltStrings, ltCallStack, ltHeapInfo, ltException, ltMemory: WriteStringsOfMsgDataStream();
        ltObject: WriteComponentOfMsgDataStream();
      end;
    end;
  finally
    if (FFullPathFileName <> '') then
      Close(FFileHandle);
    if Logger.ThreadSafe then
      GuardianFile.Release;
  end;
end;


{ TFileChannelDataHelperFunctions }

constructor TFileChannelDataHelperFunctions.Create(const sFilePath: string);
begin
  { It does not yet exist any lock on the file's byte(s) }
  FiLockSpecifier:= 0;
  { Make file if it ain't there }
  if not FileExists(sFilePath) then
    FiHandleDataFile := FileCreate(sFilePath);
  if FiHandleDataFile < 0 then
    raise EFCreateError.CreateFmt(SFCreateError, [sFilePath]);
  { Close handle returned by FileCreate so we can open it in shared mode }
  FileClose(FiHandleDataFile);
  FiHandleDataFile := FileOpen(sFilePath, fmOpenReadWrite or fmShareDenyNone);
  if FiHandleDataFile < 0 then
    raise EFOpenError.CreateFmt(SFOpenError, [sFilePath]);
  FsFilePath:= sFilePath;
end;


destructor TFileChannelDataHelperFunctions.Destroy;
begin
  FileClose(FiHandleDataFile);
  inherited Destroy;
end;


function TFileChannelDataHelperFunctions.GetSize: Int64;
begin
  Result:= FileSize(FsFilePath);
end;


end.

