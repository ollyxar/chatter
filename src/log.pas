unit log;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, settings;

procedure WriteLog(ALogtype, ASection, AMessage: string);
function GetSettings: TSettings;

implementation

procedure WriteLog(ALogtype, ASection, AMessage: string);
var
  f: TextFile;
  FLogFile: string;
  LogDir, version: string;
begin
  try
    version := 'Chatter v2.0';
{$IFDEF UNIX}
    {if debug param is present, then writeln to stdout}
    if (ParamStr(2) = '-d') or (ParamStr(2) = '-debug') then
    begin
      WriteLn(AMessage);
    end;
{$ENDIF}

{$IFDEF UNIX}
    LogDir := '/var/log/';
{$ENDIF}
{$IFDEF WINDOWS}
    LogDir := ExtractFilePath(ParamStr(0)) + 'log\';
{$ENDIF}

    if not DirectoryExists(LogDir) then
      ForceDirectories(LogDir);

    FLogFile := LogDir + 'chatter.log';

    AssignFile(f, FLogFile);
    if FileExists(FLogFile) then
      Append(f)
    else
      ReWrite(f);

    WriteLn(f, version + #9 + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) +
      #9 + ALogtype + #9 + ASection + #9 + AMessage);

    if (ALogtype = 'error') or (ALogtype = 'crit') then
    begin
      DumpExceptionBackTrace(f);
    end;

    CloseFile(f);
  except
    on E: Exception do
    begin
      WriteLn(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + #9 + AMessage);
    end;
  end;
end;

function GetSettings: TSettings;
var
  ini: TIniFile;
  path: string;
begin
{$IFDEF UNIX}
  path := '/etc/chatter.ini';
{$ENDIF}
{$IFDEF WINDOWS}
  path := ExtractFilePath(ParamStr(0)) + 'chatter.ini';
{$ENDIF}
  ini := TIniFile.Create(path);
  with Result do
  begin
    Port := ini.ReadInteger('main', 'port', 2052);
    Interval := ini.ReadInteger('main', 'interval', 400);
    TimeLimit := ini.ReadInteger('main', 'time_limit', 20000);
    MessagesLimit := ini.ReadInteger('main', 'messages_limit', 50);
    Secret := ini.ReadString('main', 'secret', 'qwerty123');
    PostMaxLength := ini.ReadInteger('main', 'post_max_length', 512);
    WelcomeAvatar := ini.ReadString('welcome', 'avatar', '');
    WelcomeName := ini.ReadString('welcome', 'name', 'SYSTEM');
    WelcomePost := ini.ReadString('welcome', 'post', 'Greetings!');
  end;
end;

end.
