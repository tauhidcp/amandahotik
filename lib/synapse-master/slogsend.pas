{==============================================================================|
| Project : Ararat Synapse                                       | 002.000.000 |
|==============================================================================|
| Content: SysLog client                                                       |
|==============================================================================|
| Copyright (c)1999-2023, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c)2001-2023.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|    Christian Brosius                                                         |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(BSD SYSLOG protocol)

Used RFC: RFC-3164, RFC-5424 (millisecond grade timestamp, nonASCII support)
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$Q-}
{$H+}

unit slogsend;

interface

uses
  SysUtils, Classes,
  blcksock, synautil;

const
  cSysLogProtocol = '514';

  FCL_Kernel = 0;
  FCL_UserLevel = 1;
  FCL_MailSystem = 2;
  FCL_System = 3;
  FCL_Security = 4;
  FCL_Syslogd = 5;
  FCL_Printer = 6;
  FCL_News = 7;
  FCL_UUCP = 8;
  FCL_Clock = 9;
  FCL_Authorization = 10;
  FCL_FTP = 11;
  FCL_NTP = 12;
  FCL_LogAudit = 13;
  FCL_LogAlert = 14;
  FCL_Time = 15;
  FCL_Local0 = 16;
  FCL_Local1 = 17;
  FCL_Local2 = 18;
  FCL_Local3 = 19;
  FCL_Local4 = 20;
  FCL_Local5 = 21;
  FCL_Local6 = 22;
  FCL_Local7 = 23;

type
  {:@abstract(Define Syslog versions)}
  TSyslogVersion = (RFC3164, RFC5424);

  {:@abstract(Define possible priority of Syslog message)}
  TSyslogSeverity = (Emergency, Alert, Critical, Error, Warning, Notice, Info,
    Debug);

  {:@abstract(encoding or decoding of SYSLOG message)}
  TSyslogMessage = class(TObject)
  private
    FVersion: TSyslogVersion;
    FFacility: Byte;
    FSeverity: TSyslogSeverity;
    FDateTime: TDateTime;
    FTag: String;
    FMessage: String;
    FLocalIP: String;
    FProcID: string;
    FMsgID: string;
    function GetPacketBuf: AnsiString;
    procedure SetPacketBuf(Value: AnsiString);
  public
    {:Reset values to defaults}
    procedure Clear;

    {:Define packet format version.}
    property Version: TSyslogVersion read FVersion write FVersion;

    {:Define facilicity of Syslog message. For specify you may use predefined
     FCL_* constants. Default is "FCL_Local0".}
    property Facility: Byte read FFacility write FFacility;

    {:Define possible priority of Syslog message. Default is "Debug".}
    property Severity: TSyslogSeverity read FSeverity write FSeverity;

    {:date and time of Syslog message}
    property DateTime: TDateTime read FDateTime write FDateTime;

    {:This is used for identify process of this message. Default is filename
     of your executable file.}
    property Tag: String read FTag write FTag;

    {:alias to Tag}
    property AppName: String read FTag write FTag;

    {:Identification of logging process, like handle of process, transaction ID, etc.}
    property ProcID: String read FProcID write FProcID;

    {:Identification of message type category. Messages with same ID should have same semantic.}
    property MsgID: String read FMsgID write FMsgID;

    {:Text of your message for log.}
    property LogMessage:String read FMessage write FMessage;

    {:IP address of message sender.}
    property LocalIP:String read FLocalIP write FLocalIP;

    {:This property holds encoded binary SYSLOG packet.
      Note: writing is deprecated and working for RFC3164 only.}
    property PacketBuf: AnsiString read GetPacketBuf write SetPacketBuf;
  end;

  {:@abstract(This object implement BSD SysLog client)

   Note: Are you missing properties for specify server address and port? Look to
   parent @link(TSynaClient) too!}
  TSyslogSend = class(TSynaClient)
  private
    FSock: TUDPBlockSocket;
    FSysLogMessage: TSysLogMessage;
  public
    constructor Create;
    destructor Destroy; override;
    {:Send Syslog UDP packet defined by @link(SysLogMessage).}
    function DoIt: Boolean;
    {:Syslog message for send}
    property SysLogMessage:TSysLogMessage read FSysLogMessage write FSysLogMessage;
  end;

{:Simply send old RFC-3164 packet to specified Syslog server.}
function ToSysLog(const SyslogServer: string; Facil: Byte;
  Sever: TSyslogSeverity; const Content: string): Boolean;

{:Simply send RFC-5424 version 1 packet to specified Syslog server.}
function ToSysLog1(const SyslogServer: string; Facil: Byte;
  Sever: TSyslogSeverity; const ProcID, MsgID, Content: string): Boolean;

implementation

function TSyslogMessage.GetPacketBuf: AnsiString;
var
  s: ansistring;
begin
  case FVersion of
    RFC3164:
      begin
        Result := '<' + IntToStr((FFacility * 8) + Ord(FSeverity)) + '>';
        Result := Result + CDateTime(FDateTime) + ' ';
        Result := Result + FLocalIP + ' ';
        Result := Result + FTag + ': ' + FMessage;
      end;
    RFC5424:
      begin
        Result := '<' + IntToStr((FFacility * 8) + Ord(FSeverity)) + '>1 ';
        Result := Result + Rfc3339DateTime(FDateTime) + ' ';
        Result := Result + FLocalIP + ' ';
        if FTag = '' then s := '-'
          else s := FTag;
        Result := Result + FTag + ' ';
        if FProcID = '' then s := '-'
          else s := FProcID;
        Result := Result + FProcID + ' ';
        if FMsgID = '' then s := '-'
          else s := FMsgID;
        Result := Result + FMsgID + ' ';
        Result := Result + '- '; //structured data not implemented yet
        Result := Result + #$EF#$BB#$BF + AnsiToUtf8(FMessage); //BOM and UTF8 encoded text
      end;
    else
      Result := '';
  end;
end;

procedure TSyslogMessage.SetPacketBuf(Value: AnsiString);
var StrBuf: AnsiString;
    IntBuf, Pos: Integer;
begin
  if Length(Value) < 1 then exit;
  Pos := 1;
  if Value[Pos] <> '<' then exit;
  Inc(Pos);
  // Facility and Severity
  StrBuf := '';
  while (Value[Pos] <> '>')do
  begin
    StrBuf := StrBuf + Value[Pos];
    Inc(Pos);
  end;
  IntBuf := StrToInt(StrBuf);
  FFacility := IntBuf div 8;
  case (IntBuf mod 8)of
    0:FSeverity := Emergency;
    1:FSeverity := Alert;
    2:FSeverity := Critical;
    3:FSeverity := Error;
    4:FSeverity := Warning;
    5:FSeverity := Notice;
    6:FSeverity := Info;
    7:FSeverity := Debug;
  end;
  // DateTime
  Inc(Pos);
  StrBuf := '';
    // Month
  while (Value[Pos] <> ' ')do
    begin
      StrBuf := StrBuf + Value[Pos];
      Inc(Pos);
    end;
    StrBuf := StrBuf + Value[Pos];
    Inc(Pos);
    // Day
  while (Value[Pos] <> ' ')do
    begin
      StrBuf := StrBuf + Value[Pos];
      Inc(Pos);
    end;
    StrBuf := StrBuf + Value[Pos];
    Inc(Pos);
    // Time
  while (Value[Pos] <> ' ')do
    begin
      StrBuf := StrBuf + Value[Pos];
      Inc(Pos);
    end;
  FDateTime := DecodeRFCDateTime(StrBuf);
  Inc(Pos);

  // LocalIP
  StrBuf := '';
  while (Value[Pos] <> ' ')do
    begin
      StrBuf := StrBuf + Value[Pos];
      Inc(Pos);
    end;
  FLocalIP := StrBuf;
  Inc(Pos);
  // Tag
  StrBuf := '';
  while (Value[Pos] <> ':')do
    begin
      StrBuf := StrBuf + Value[Pos];
      Inc(Pos);
    end;
  FTag := StrBuf;
  // LogMessage
  Inc(Pos);
  StrBuf := '';
  while (Pos <= Length(Value))do
    begin
      StrBuf := StrBuf + Value[Pos];
      Inc(Pos);
    end;
  FMessage := TrimSP(StrBuf);
end;

procedure TSysLogMessage.Clear;
begin
  FVersion := RFC3164;
  FFacility := FCL_Local0;
  FSeverity := Debug;
  FTag := ExtractFileName(ParamStr(0));
  FProcID := '';
  FMsgID := '';
  FMessage := '';
  FLocalIP := '0.0.0.0';
end;

//------------------------------------------------------------------------------

constructor TSyslogSend.Create;
begin
  inherited Create;
  FSock := TUDPBlockSocket.Create;
  FSock.Owner := self;
  FSysLogMessage := TSysLogMessage.Create;
  FSysLogMessage.Clear;
  FTargetPort := cSysLogProtocol;
end;

destructor TSyslogSend.Destroy;
begin
  FSock.Free;
  FSysLogMessage.Free;
  inherited Destroy;
end;

function TSyslogSend.DoIt: Boolean;
var
  s: ansistring;
begin
  Result := False;
  FSysLogMessage.LocalIP := Fsock.ResolveIPToName(FSock.Localname);
  FSysLogMessage.DateTime := Now;
  s := FSysLogMessage.PacketBuf;
  if FSysLogMessage.Version = RFC3164 then
    if Length(s) > 1024 then
      exit; //old format does not allow larger size!
  FSock.Connect(FTargetHost, FTargetPort);
  FSock.SendString(s);
  Result := FSock.LastError = 0;
end;

{==============================================================================}

function ToSysLog(const SyslogServer: string; Facil: Byte;
  Sever: TSyslogSeverity; const Content: string): Boolean;
begin
  with TSyslogSend.Create do
    try
      TargetHost := SyslogServer;
      SysLogMessage.Version := RFC3164;
      SysLogMessage.Facility := Facil;
      SysLogMessage.Severity := Sever;
      SysLogMessage.LogMessage := Content;
      Result := DoIt;
    finally
      Free;
    end;
end;

function ToSysLog1(const SyslogServer: string; Facil: Byte;
  Sever: TSyslogSeverity; const ProcID, MsgID, Content: string): Boolean;
begin
  with TSyslogSend.Create do
    try
      TargetHost := SyslogServer;
      SysLogMessage.Version := RFC5424;
      SysLogMessage.Facility := Facil;
      SysLogMessage.Severity := Sever;
      SysLogMessage.ProcID := ProcID;
      SysLogMessage.MsgID := MsgID;
      SysLogMessage.LogMessage := Content;
      Result := DoIt;
    finally
      Free;
    end;
end;


end.
