unit uFileLogger;

interface

uses Winapi.Windows, uThreadQueueUtil, System.Classes, System.SyncObjs, Winapi.Messages;

const
  VV_MSG_BASE = WM_USER + 1000;
  VV_MSG_LOGGER = VV_MSG_BASE + 1;


type
  PLogRequest = ^TLogRequest;

  TLogLevel = (llAll, llDebug, llInfo, llWarn, llError, llFatal, llForce);

  TLogRequest = record
    LogLevel: TLogLevel;
    LogText: String;
  end;

  TThreadFileLog = class(TObject)
  private

    Critical: TCriticalSection;
    FLogLevel: TLogLevel;
    FFilePrefix: String;
    FFileFormat: string;
    FThreadPool: TThreadPool;
    FUnHandledLogCount: Integer;
    FThreadCount: Integer;
    procedure HandleLogRequest(Data: Pointer; AThread: TThread);
    procedure LogToFile(const FileName, LogString: String);
    procedure Log(const LogText: string; AAlogLevel: TLogLevel = llFatal);
  public
    //֪ͨ����Ӧ�Ĵ���
    NoticeHandle: THandle;

    property LogLevel: TLogLevel write FLogLevel;

    constructor Create(AThreadCount: Integer = 1; const AFilePrefix: string = '';
          const ADtFormat: string = 'yyyymmddhh'; const ALogLevel: TLogLevel = llAll);
    destructor Destroy; override;
    procedure Debug(const LogText: string);
    procedure Info(const LogText: string);
    procedure Warn(const LogText: string);
    procedure Error(const LogText: string);
    procedure Fatal(const LogText: string);
    procedure Force(const LogText: string);
  end;



implementation

uses System.SysUtils;

  (* Simple reuse of a logtofile function for example *)
procedure TThreadFileLog.LogToFile(const FileName, LogString: String);
var
  F: TextFile;
  LDir, LMsg: string;
begin
  Critical.Enter;
  try
    //�ж��ļ��д治����
    LDir := ExtractFileDir(FileName);
    if not DirectoryExists(LDir) then
    begin
      if not ForceDirectories(LDir) then
      begin
        Critical.Leave;
        Exit;
      end;
    end;

    //����ָ����������������
    AssignFile(F, FileName);

    if not FileExists(FileName) then
      Rewrite(F)
    else
      Append(F);

    LMsg := FormatDateTime('[yyyy.mm.dd hh:nn:ss]', Now) + ': ' + LogString;
    Writeln(F, LMsg);

    CloseFile(F);
    if NoticeHandle > 0 then
      SendMessage(NoticeHandle, VV_MSG_LOGGER, Integer(PChar(LMsg)), 0);
  finally
    Critical.Leave;
  end;
end;

constructor TThreadFileLog.Create(AThreadCount: Integer = 1; const AFilePrefix: string = '';
                const ADtFormat: string = 'yyyymmddhh'; const ALogLevel: TLogLevel = llAll);
begin
  NoticeHandle := 0;
  FLogLevel := ALogLevel;
  FFilePrefix := AFilePrefix;
  FFileFormat := ADtFormat;
  Critical := TCriticalSection.Create;
  FThreadCount := AThreadCount;
  FThreadPool := TThreadPool.Create(HandleLogRequest, FThreadCount);
  FUnHandledLogCount := 0;
end;

destructor TThreadFileLog.Destroy;
begin
  FThreadPool.Free;
  Critical.Free;
  inherited;
end;

procedure TThreadFileLog.HandleLogRequest(Data: Pointer; AThread: TThread);
var
  Request: PLogRequest;
  FileName: string;
  LogTitle: string;
begin
  Request := Data;

  try
    // ����log4delphi�еķ���
    case Request.LogLevel of
      llDebug:
        begin
          LogTitle := 'DEBUG';
        end;
      llInfo:
        begin
          LogTitle := 'INFO';
        end;
      llWarn:
        begin
          LogTitle := 'WARN';
        end;
      llError:
        begin
          LogTitle := 'ERROR';
        end;
      llFatal:
        begin
          LogTitle := 'FATAL';
        end;
      llForce:
        begin
          LogTitle := 'FORCE';
        end;
    end;
    FileName := FFilePrefix +  FormatDateTime(FFileFormat,
        Now) + '.log';

    LogToFile(FileName, '[' + LogTitle + ']: ' + Request^.LogText);
  finally
    Dispose(Request);
    InterlockedDecrement(FUnHandledLogCount);
  end;
end;

// llAll�Ǽ�����͵���Ϣ��ֻ����llAllʱ�Żᱻ��¼
procedure TThreadFileLog.Log(const LogText: string;
  AAlogLevel: TLogLevel = llFatal);
var
  Request: PLogRequest;
begin
  if AAlogLevel < FLogLevel then Exit;
  
  //����������Ϣ��ʧ�������ܹ������ܱ��ֺ�����Ϣ�ĳ�����¼
  //if FUnHandledLogCount < FThreadCount  then
  Begin
    //���������δ�������Ϣ�����м�¼
    New(Request);
    Request^.LogLevel := AAlogLevel;

    if Length(LogText) > 512 then
      Request^.LogText := LogText.Substring(0, 512)
    else
      Request^.LogText := LogText;

    InterlockedIncrement(FUnHandledLogCount);

    //֧�ֵ��߳�ģʽ��Ҳ����������ֱ��д���ļ�
    if FThreadCount = 0 then
      HandleLogRequest(Request, nil)
    else
      FThreadPool.Add(Request);
  End;
end;

procedure TThreadFileLog.Debug(const LogText: string);
begin
  Log(LogText, llDebug);
end;

procedure TThreadFileLog.Info(const LogText: string);
begin
  Log(LogText, llInfo);
end;

procedure TThreadFileLog.Warn(const LogText: string);
begin
  Log(LogText, llWarn);
end;

procedure TThreadFileLog.Error(const LogText: string);
begin
  Log(LogText, llError);
end;

procedure TThreadFileLog.Fatal(const LogText: string);
begin
  Log(LogText, llFatal);
end;

procedure TThreadFileLog.Force(const LogText: string);
begin
  Log(LogText, llForce);
end;

end.
