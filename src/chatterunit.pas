unit chatterunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp, fphttpserver, fpjsonrtti, log,
  settings;

type

  { TPostItem }

  TPostItem = class(TCollectionItem)
  private
    FUniqueId: integer;
    FName: string;
    FAvatar: string;
    FPost: string;
    FTime: string;
    FUserId: integer;
  published
    property uniqueId: integer read FUniqueId write FUniqueId;
    property name: string read FName write FName;
    property avatar: string read FAvatar write FAvatar;
    property post: string read FPost write FPost;
    property userId: integer read FUserId write FUserId;
    property time: string read FTime write FTime;
  end;

  { TPostItems }

  TPostItems = class(TPersistent)
  private
    FPosts: TCollection;
    FStreamer: TJSONStreamer;
  public
    constructor Create;
    destructor Destroy; override;
    function AsString: string;
  published
    property Posts: TCollection read FPosts write FPosts;
  end;

  { TChatterServer }

  TChatterServer = class(TFPHTTPServer)
  private
    FPosts: TPostItems;
    FSettings: TSettings;
  public
    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  { THTTPServerThread }

  THTTPServerThread = class(TThread)
  private
    FServer: TChatterServer;
  public
    procedure Execute; override;
    procedure DoTerminate; override;
  end;

  { TChatterDaemon }

  TChatterDaemon = class(TDaemon)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModulePause(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleShutDown(Sender: TCustomDaemon);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
  private
    FServerThread: THTTPServerThread;
  public

  end;

var
  ChatterDaemon: TChatterDaemon;

implementation

function HTMLEncode(const Data: string): string;
var
  iPos, i: Integer;
  procedure Encode(const AStr: String);
  begin
    Move(AStr[1], Result[iPos], Length(AStr) * SizeOf(Char));
    Inc(iPos, Length(AStr));
  end;
begin
  SetLength(Result, Length(Data) * 6);
  iPos := 1;
  for i := 1 to length(Data) do
    case Data[i] of
      '<': Encode('&lt;');
      '>': Encode('&gt;');
      '&': Encode('&amp;');
      '"': Encode('&quot;');
    else
      Result[iPos] := Data[i];
      Inc(iPos);
    end;
  SetLength(Result, iPos - 1);
end;

{ THTTPServerThread }

procedure THTTPServerThread.Execute;
begin
  FServer := TChatterServer.Create(nil);
  try
    FServer.Threaded := True;
    FServer.Active := True;
  finally
    FServer.Free;
  end;
end;

procedure THTTPServerThread.DoTerminate;
begin
  FServer.Active := False;
  FreeAndNil(FServer);
  inherited DoTerminate;
end;

{ TChatterDaemon }

procedure TChatterDaemon.DataModuleCreate(Sender: TObject);
begin
  WriteLog('sys', 'Main', 'Creating daemon...');
end;

procedure TChatterDaemon.DataModulePause(Sender: TCustomDaemon; var OK: Boolean);
begin
  OK := True;
  WriteLog('sys', 'Main', 'Pausing daemon...');
end;

procedure TChatterDaemon.DataModuleShutDown(Sender: TCustomDaemon);
begin
  WriteLog('sys', 'Main', 'Shutting down...');
end;

procedure TChatterDaemon.DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
begin
  WriteLog('sys', 'Main', 'Starting daemon...');
  FServerThread := THTTPServerThread.Create(True);
  FServerThread.Start;
  OK := True;
  WriteLog('sys', 'Main', 'Server started.');
end;

procedure TChatterDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
begin
  WriteLog('sys', 'Main', 'Stopping daemon...');
  OK := True;
  FServerThread.Terminate;
  WriteLog('sys', 'Main', 'Daemon stopped.');
end;

destructor TChatterServer.Destroy;
begin
  FPosts.Free;
  inherited Destroy;
end;

procedure TChatterServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  message_id: integer;
  t: integer = 0;
begin
  AResponse.Server := 'Ollyxar';
  AResponse.Connection := 'Keep-Alive';
  AResponse.SetCustomHeader('Keep-Alive', 'timeout=5, max=100');
  AResponse.SetCustomHeader('Accept-Ranges', 'bytes');
  AResponse.SetCustomHeader('access-control-allow-origin', '*');
  AResponse.SetCustomHeader('Access-Control-Allow-Credentials', 'true');

  if (Pos('/index', ARequest.URI) = 1) then
  begin
    AResponse.ContentType := 'application/json';
    AResponse.Content := FPosts.AsString;
    AResponse.SendContent;
    AResponse.ContentStream := nil;
  end
  else
  if ((Pos('?', ARequest.URI) > 1) and
    (copy(ARequest.URI, 0, Pos('?', ARequest.URI) - 1) = '/pull')) then
  begin
    message_id := StrToIntDef(ARequest.QueryFields.Values['message_id'], 0);
    t := 0;

    while (t < FSettings.TimeLimit) do
    begin
      t += FSettings.Interval;
      if (FPosts.Posts.Count > 0) and (message_id <> 0) and
        (TPostItem(FPosts.Posts.Items[FPosts.Posts.Count - 1]).uniqueId <> message_id) then
      begin
        AResponse.ContentType := 'application/json';
        try
          AResponse.Content := FPosts.AsString;
          AResponse.SendContent;
        except
          on E: Exception do
          WriteLog('crit', 'Pull', 'Cannot read and send content! ... ' + E.Message);
        end;
        break;
      end;
      sleep(FSettings.Interval);
    end;
  end
  else
  if (ARequest.URI = '/post') and (StringReplace(HTMLEncode(ARequest.ContentFields.Values['post']), ' ', '', [rfReplaceAll]) <> '') and
    (ARequest.ContentFields.Values['secret'] = FSettings.Secret) then
  begin
    try
      if (FPosts.Posts.Count > FSettings.MessagesLimit) then
        FPosts.Posts.Delete(0);

      with TPostItem(FPosts.Posts.Add) do
      begin
        if (FPosts.Posts.Count > 0) then
          uniqueId := TPostItem(FPosts.Posts.Items[FPosts.Posts.Count - 2]).uniqueId + 1
        else
          uniqueId := 1;

        name := HTMLEncode(ARequest.ContentFields.Values['name']);
        avatar := HTMLEncode(ARequest.ContentFields.Values['avatar']);
        post := Copy(HTMLEncode(ARequest.ContentFields.Values['post']), 0, FSettings.PostMaxLength);
        userId := StrToIntDef(ARequest.ContentFields.Values['userId'], 0);
        time := formatdatetime('hh:nn', now);
      end;
      AResponse.Content := '1';
      AResponse.SendContent;
    except
      on E: Exception do
      begin
        writelog('crit', 'Post', E.message);
      end;
    end;
    sleep(FSettings.Interval);
  end
  else
  begin
    AResponse.Code := 404;
    AResponse.SendContent;
  end;
end;

constructor TChatterServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSettings := GetSettings;
  Port := FSettings.Port;
  FPosts := TPostItems.Create;

  with TPostItem(FPosts.Posts.Add) do
  begin
    uniqueId := 1;
    name := FSettings.WelcomeName;
    avatar := FSettings.WelcomeAvatar;
    post := FSettings.WelcomePost;
    time := FormatDateTime('hh:nn', Now);
    userId := 0;
  end;
end;

{ TPostItems }

constructor TPostItems.Create;
begin
  FPosts := TCollection.Create(TPostItem);
  FStreamer := TJSONStreamer.Create(nil);
  FStreamer.Options := FStreamer.Options + [jsoTStringsAsArray];
end;

destructor TPostItems.Destroy;
begin
  FStreamer.Free;
  FPosts.Free;
  inherited Destroy;
end;

function TPostItems.AsString: string;
begin
  Result := FStreamer.ObjectToJSONString(self);
end;

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TChatterDaemon)
end;

{$R *.lfm}

initialization
  RegisterDaemon;
end.




