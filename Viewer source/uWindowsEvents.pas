// https://delphi-bar.blogspot.com/2022/02/writing-to-and-reading-from-windows.html

unit uWindowsEvents;

interface

uses classes, Windows, SvcMgr, Vcl.StdCtrls, Generics.Collections,System.JSON;

type

  tInsertionArray = array of string;
  tDataArray = array of integer;

  { /----------------------------------------------------------------------------------------------------------------- }
  TRBWindowsEvent = class(TObject)
  private
    fCategory:         string;
    fCategoryString:   string;
    fComputerName:     string;
    fTypeString:       string;
    fEventType:        integer;
    fEventCode:        integer;
    fEventIdentifier:  integer;
    fRecordNumber:     integer;
    fMessage:          string;
    fLogFile:          string;
    fUser:             string;
    fSourceName:       string;
    fTimeWritten:      TDateTime;   // TimeGenerated is same as TimeWritten
    fTimeGenerated:    TDateTime;
    fInsertionStrings: tInsertionArray;   // array of string
    fDataArray :       tDataArray;        // array of integer
  public
    property Category:        string           read fCategory         write fCategory;
    property CategoryString:  string           read fCategoryString   write fCategoryString;
    property ComputerName:    string           read fComputerName     write fComputerName;
    property TypeString:      string           read fTypeString       write fTypeString;
    property EventType:       integer          read fEventType        write fEventType;
    property EventCode:       integer          read fEventCode        write fEventCode;
    property EventIdentifier: integer          read fEventIdentifier  write fEventIdentifier;
    property RecordNumber:    integer          read fRecordNumber     write fRecordNumber;
    property Msg:             string           read fMessage          write fMessage;
    property LogFile:         string           read fLogFile          write fLogFile;
    property User:            string           read fUser             write fUser;
    property SourceName:      string           read fSourceName       write fSourceName;
    property TimeWritten:     TdateTime        read fTimeWritten      write fTimeWritten;
    property TimeGenerated:   TdateTime        read fTimeGenerated    write fTimeGenerated;
    property InsertionStrings: tInsertionArray read fInsertionStrings write fInsertionStrings;
    property DataArray:        tDataArray      read fDataArray        write fDataArray;
    //property EventData:       string           read fEventData        write fEventData;

    procedure PopulateFromOleVariant(aEvent: OLEVariant);
  end;

  { /----------------------------------------------------------------------------------------------------------------- }
  TRBWindowsEventLogsReader = class(TObjectList<TRBWindowsEvent>)
  strict private
    fApplicationName: string;
    procedure AddErrorMessage(aErrorMessage: string);
  public
    constructor Create(aApplicationName: string);

    procedure GetWindowsEventLogs (MaxNumberOfEntries: integer);
  end;

  { /----------------------------------------------------------------------------------------------------------------- }
  TRBWindowsEventLogsWriter = class(TObject)
  strict private
    fWindowsEventLogger: TEventLogger;
  public
    constructor Create(aApplicationName: string);
    destructor Destroy; override;

    procedure WriteInformationToWindowsEvents(aMessage: string);
    procedure WriteErrorToWindowsEvents(aMessage: string);
  end;

  { /----------------------------------------------------------------------------------------------------------------- }
  TRBWindowsEventLogs = class(TObject)
  strict private
    fApplicationName: string;

    fWriter: TRBWindowsEventLogsWriter;
    fReader: TRBWindowsEventLogsReader;

  public
    property Writer: TRBWindowsEventLogsWriter read fWriter;
    property Reader: TRBWindowsEventLogsReader read fReader;
    constructor Create(aApplicationName: string);
    destructor Destroy; override;
  end;

implementation

uses SysUtils, StrUtils, ComObj, ActiveX, System.Variants, DateUtils;

{ TRBWindowsEvent }

// https://learn.microsoft.com/en-us/previous-versions/windows/desktop/eventlogprov/win32-ntlogevent

procedure TRBWindowsEvent.PopulateFromOleVariant(aEvent: OLEVariant);
var
  localInsertionArray: array of String;
  localDataArray: array of integer;
  i: integer;
begin
  // Identifies the event within the Windows event log file. This is specific to the log file and is used together with the log file name to uniquely identify an instance of this class.

  // Record numbers are always unique; they are not reset to 1 when an event log is cleared.
  // As a result, the highest record number also indicates the number of records that have
  // been written to the event log since the operating system was installed
  fRecordNumber := integer(aEvent.RecordNumber);

  //Classification of the event as determined by the source.
  // This subcategory is source-specific.
  fCategory := string (aEvent.Category);                // '0'

  // Translation of the subcategory. The translation is source-specific
  if not VarIsNull(aEvent.CategoryString) then
     fCategoryString := string (aEvent.CategoryString); // 'Application Crashing Events'

  // Type of event. This is an enumerated string.
  // It is preferable to use the EventType property rather than the "Type" property.
  // 1:Error,2:Warning,4:Information,8:Security Audit Success,16:Security Audit Failure
  if not VarIsNull(aEvent.Type) then
     fTypeString := string (aEvent.Type);               // 'Information'

  // Type of event.
  // 1:Error,2:Warning,3:Information,4:Security Audit Success,5:Security Audit Failure
  if not VarIsNull(aEvent.EventType) then
     fEventType := aEvent.EventType;

  // User name of the logged-on user when the event occurred.
  // If the user name cannot be determined, this will be NULL.
  if not VarIsNull(aEvent.User) then
     fUser := string (aEvent.User);                     // 'NT AUTHORITY\SYSTEM'

  // Name of the source (application, service, driver, or subsystem) that generated the entry.
  // It is used, together with EventIdentifier to uniquely identify a Windows event type.
  if not VarIsNull(aEvent.SourceName) then
     fSourceName := string (aEvent.SourceName);         // Microsoft-Windows-Security-SPP

  // The time when the event is written to the log file
  if not VarIsNull(aEvent.TimeWritten) then begin
     var dateStr : string := aEvent.TimeWritten ;      // '20240602101605.176003-000'
     var fs: TFormatSettings;
     fs := TFormatSettings.Create;
     fs.DateSeparator := '-';
     fs.ShortDateFormat := 'yyyyMMdd';
     fs.TimeSeparator := ':';
     fs.ShortTimeFormat := 'hhmmss.zzz';
     fs.LongTimeFormat := 'hhmmss.zzz';
     fTimeWritten := StrToDateTime (dateStr,fs);       // 02-06-24 10:16:05:176
     // Time is UTC. convert to local time
     fTimeWritten := TTimeZone.Local.ToLocalTime(fTimeWritten);
  end;

  // The time when the event is generated.
  if not VarIsNull(aEvent.TimeGenerated) then begin
     var dateStr : string := aEvent.TimeGenerated ;      // '20240602101605.176003-000'
     var fs: TFormatSettings;
     fs := TFormatSettings.Create;
     fs.DateSeparator := '-';
     fs.ShortDateFormat := 'yyyyMMdd';
     fs.TimeSeparator := ':';
     fs.ShortTimeFormat := 'hhmmss.zzz';
     fs.LongTimeFormat := 'hhmmss.zzz';
     fTimeGenerated := StrToDateTime (dateStr,fs);       // 02-06-24 10:16:05:176
     // Time is UTC. convert to local time
     fTimeGenerated := TTimeZone.Local.ToLocalTime(fTimeGenerated);
  end;

  // Identifier of the event.
  // This is specific to the source that generated the event log entry and is used, together with SourceName,
  // to uniquely identify a Windows event type.
  if not VarIsNull(aEvent.EventIdentifier) then
     fEventIdentifier := aEvent.EventIdentifier;         // 1073758208

  // Name of the computer that generated this event.
  fComputerName := string (aEvent.ComputerName);

  // Value of the lower 16-bits of the EventIdentifier property.
  // It is present to match the value displayed in the Windows Event Viewer.
  fEventCode := integer(aEvent.EventCode);

  // Event message as it appears in the Windows event log.
  // This is a standard message with zero or more insertion strings supplied by the source of the Windows event.
  // The insertion strings are inserted into the standard message in a predefined format.
  // If there are no insertion strings or there is a problem inserting the insertion strings,
  // only the standard message will be present in this field.
  if not VarIsNull(aEvent.Message) then
     fMessage := string (aEvent.Message)
  else
     fMessage := '';

  // Name of Windows event log file.
  // Together with RecordNumber, this is used to uniquely identify an instance of this class.
  fLogFile := string (aEvent.LogFile);             // Name of Windows event log file

  // List of the insertion strings that accompanied the report of the Windows event.
  if not VarIsNull(aEvent.InsertionStrings) then
  begin
     localInsertionArray := aEvent.InsertionStrings;
     var low: integer := VarArrayLowBound(localInsertionArray, 1);
     var high: integer := VarArrayHighBound(localInsertionArray, 1);
     setLength (fInsertionStrings,High+1-low);
     var index:integer := 0;

     for i := low to High do
     begin
        fInsertionStrings[index] := localInsertionArray[i];
        inc(index);
     end;
  end;

  // List of the binary data that accompanied the report of the Windows event. (uint8 array)
  if not VarIsNull(aEvent.Data) then
  begin
     localDataArray := aEvent.Data;
     var low: integer := VarArrayLowBound(localDataArray, 1);
     var high: integer := VarArrayHighBound(localDataArray, 1);
     setLength (fDataArray,High+1-low);
     var index:integer := 0;

     for i := VarArrayLowBound(localDataArray, 1) to VarArrayHighBound(localDataArray, 1) do
     begin
        fDataArray[index] := localDataArray[i];
        inc(index);
     end;
  end;

end;

{ TRBWindowsEvents }

constructor TRBWindowsEventLogs.Create(aApplicationName: string);
begin
  inherited Create;
  fApplicationName := aApplicationName;
  fWriter := TRBWindowsEventLogsWriter.Create(aApplicationName);
  fReader := TRBWindowsEventLogsReader.Create(aApplicationName);
end;

destructor TRBWindowsEventLogs.Destroy;
begin
  FreeAndNil(fReader);
  FreeAndNil(fWriter);
  inherited;
end;

{ TRBWindowsEventLogsWriter }

constructor TRBWindowsEventLogsWriter.Create(aApplicationName: string);
begin
  inherited Create;
  fWindowsEventLogger := TEventLogger.Create(aApplicationName);
end;

destructor TRBWindowsEventLogsWriter.Destroy;
begin
  FreeAndNil(fWindowsEventLogger);
  inherited;
end;

procedure TRBWindowsEventLogsWriter.WriteErrorToWindowsEvents(aMessage: string);
begin
  fWindowsEventLogger.LogMessage(aMessage, EVENTLOG_ERROR_TYPE);
end;

procedure TRBWindowsEventLogsWriter.WriteInformationToWindowsEvents(aMessage: string);
begin
  fWindowsEventLogger.LogMessage(aMessage, EVENTLOG_INFORMATION_TYPE);
end;

{ TRBWindowsEventLogsReader }

constructor TRBWindowsEventLogsReader.Create(aApplicationName: string);
begin
  inherited Create;
  fApplicationName := aApplicationName;
end;

procedure TRBWindowsEventLogsReader.AddErrorMessage(aErrorMessage: string);
var
  event: TRBWindowsEvent;
begin
  event := TRBWindowsEvent.Create;
  event.Category := 'Error';
  event.Msg := aErrorMessage;
  Add(event);
end;

procedure TRBWindowsEventLogsReader.GetWindowsEventLogs (MaxNumberOfEntries: integer);
const
  wbemForwardOnly = 32;
  wbemReturnImmediately = 16;
  wbemFlagReturnWhenComplete = 0;

begin
  try
    Clear();
    var iCount := 0;
    // https://www.codeproject.com/Articles/42571/WMI-Windows-Event-Logs-and-User-Privileges
    // https://learn.microsoft.com/en-us/windows/win32/wmisdk/swbemlocator-connectserver
    // https://learn.microsoft.com/fr-be/windows/win32/wmisdk/privilege-constants?redirectedfrom=MSDN
    // https://learn.microsoft.com/en-us/windows/win32/wmisdk/swbemservices-execquery
    // https://learn.microsoft.com/en-us/windows/win32/wmisdk/querying-with-wql

    var BindCtx: IBindCtx;
    OleCheck(CreateBindCtx(0, bindCtx));

    var Moniker: IMoniker;
    var chEaten: Integer;
    OleCheck(MkParseDisplayName(BindCtx, StringToOleStr('winmgmts:'), chEaten, Moniker));    // 'winmgmts:\\localhost\root\cimv2'  // {impersonationLevel=impersonate}

    var dispatch: IDispatch;
    OleCheck(Moniker.BindToObject(BindCtx, nil, IDispatch, dispatch));

    var WMIService: OLEVariant := dispatch;

    var eventQuery :=
       'SELECT * FROM Win32_NTLogEvent '
       + 'Where Logfile = "' + fApplicationName + '" '
       + 'AND TimeGenerated >= "' + DateTimeToStr(IncDay(Now(), -10)) + '"'  ;

    var WbemObjectSet: OLEVariant := WMIService.ExecQuery(
       EventQuery,    // text of the query
       'WQL',         // the query language to be used.
       wbemReturnImmediately + wbemForwardOnly); // wbemReturnImmediately + wbemForwardOnly);    // wbemFlagReturnWhenComplete

    var WbemObject: OLEVariant;
    var iValue: LongWord;
    var oEnum: IEnumvariant := IUnknown(WbemObjectSet._NewEnum) as IEnumvariant;

    while oEnum.Next(1, WbemObject, iValue) = 0 do begin
       var event := TRBWindowsEvent.Create;
       event.PopulateFromOleVariant(WbemObject);
       Add(event);
       WbemObject := Unassigned;
       inc(iCount);
       if (MaxNumberOfEntries <> -1) and (iCount > MaxNumberOfEntries) then
          Break;
    end;
  except
    on E: EOleException do
      AddErrorMessage(Format('EOleException %s %x', [E.Message, E.ErrorCode]));
    on E: Exception do
      AddErrorMessage(E.Classname + ':' + E.Message);
  end;
end;

end.
