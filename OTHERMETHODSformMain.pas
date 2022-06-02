unit formMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AdvGlowButton, AdvMemo, JCLFileUtils,
  AdvmES, AdvMemoStylerManager, AdvmWS, AdvUtil, Vcl.Grids, AdvObj, BaseGrid,
  AdvGrid, ExtCtrls, JCLShell, IOUtils, ShellAPI,
    OtlCommon, OtlTask, OtlTaskControl, OtlEventMonitor, OtlComm,
    System.Threading, AdvEdit, StrUtils,
    OtlCollections, OtlParallel;

type
  TfrmMain = class(TForm)
    FileOpenDialog1: TFileOpenDialog;
    memoLog: TAdvMemo;
    AdvStringGrid1: TAdvStringGrid;
    Panel1: TPanel;
    Label1: TLabel;
    Button3: TButton;
    editSimulTasks: TAdvEdit;
    Label2: TLabel;
    Button1: TButton;
    OmniEventMonitor1: TOmniEventMonitor;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure OmniEventMonitor1TaskTerminated(const task: IOmniTaskControl);
    procedure OmniEventMonitor1TaskMessage(const task: IOmniTaskControl;
      const msg: TOmniMessage);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    ExeFiles: TStringList;
    function DoDosboxTask(ItemNum: integer): TProc;
    procedure Log(LogItem: String);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;
  loop : IOmniParallelLoop<integer>;


implementation

{$R *.dfm}

//horrible but if it works...
{procedure Delay(TickTime : Integer);
 var
 Past: longint;
 begin
 Past := GetTickCount;
 repeat
 application.ProcessMessages;
 Until (GetTickCount - Past) >= longint(TickTime);
end;}

procedure ExecuteProcess(
  const ExecutablePath: string;
  const Arguments: string;
  const CurrentDirectory: string;
  const WaitThenTerminate: Boolean;
  const WaitTimeMilliseconds: integer;
  const CreationFlags: DWORD
);
var
  si: TStartupInfo;
  pi: TProcessInformation;
  MyCurrentDirectory: PChar;
begin
  ZeroMemory(@si, SizeOf(si));
  si.cb := SizeOf(si);

  if CurrentDirectory <> '' then begin
    MyCurrentDirectory := PChar(CurrentDirectory);
  end else begin
    MyCurrentDirectory := nil;
  end;

  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_HIDE;

  Win32Check(CreateProcess(
    nil,
    PChar('"' + ExecutablePath + '" ' + Arguments),
    //PChar(ExecutablePath +  ' ' + Arguments),
    nil,
    nil,
    False,
    CreationFlags,
    nil,
    MyCurrentDirectory,
    si,
    pi
  ));
  try
    if WaitThenTerminate then begin
      //WaitUntilSignaled(pi.hProcess, True);
      //Delay(WaitTimeMilliseconds);
      sleep(WaitTimeMilliseconds);
      //PostMessage (pi.hProcess, WM_CLOSE, 0, 0);
      PostThreadMessage (pi.hThread, WM_CLOSE, 0, 0);
      sleep(2000); //if its still not closed then terminate it
      TerminateProcess(pi.hProcess, 0);
      Sleep(1000); //Wait afterwards for process to fully terminate
    end;
  finally
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
  end;
end;


procedure TfrmMain.Button1Click(Sender: TObject);
var
  buffer   : TOmniValue;
  memStr   : TMemoryStream;
  outQueue : IOmniBlockingCollection;
  unwritten: IOmniCounter;
begin
  if FileOpenDialog1.Execute = false then
    exit;

  ExeFiles.Clear;
  Log('Searching for .exe files');

  if AdvBuildFilelist(IncludeTrailingPathDelimiter( FileOpenDialog1.FileName ) + '*.exe' , faAnyFile, ExeFiles, amAny, [flFullNames, flRecursive]) = false then
  begin
    Log('There was an error scanning the folder.' + ' Aborting...');
    Exit;
  end;

  outQueue := TOmniBlockingCollection.Create;
  unwritten := CreateCounter(10);  //CHANGE
  Parallel.ParallelTask.NoWait
    .NumTasks(Environment.Process.Affinity.Count)
    .OnStop(Parallel.CompleteQueue(outQueue))
    .Execute(
      procedure
      var
        buffer      : TMemoryStream;
        bytesToWrite: integer;
        //randomGen   : TGpRandom;
      begin
        //randomGen := TGpRandom.Create;
        //try
        bytesToWrite := 0;
          while unwritten.Take(1, bytesToWrite) do begin
            {buffer := TMemoryStream.Create;
            buffer.Size := bytesToWrite;
            FillBuffer(buffer.Memory, bytesToWrite, randomGen);
            outQueue.Add(buffer);}
            ExecuteProcess(ExtractFilePath(Application.ExeName) + 'dosbox-staging\dosbox.exe', '-noconsole -c "mount d ." -c "mount C ''' + ExtractFilePath(ExeFiles[bytesToWrite]) + '''" -c "c:" -c "' + extractfilename(ExtractShortPathName(ExeFiles[bytesToWrite])) + ' /? >d:\outputs\output' + inttostr(bytesToWrite) + '.txt" -exit', '', True, 8000, 0);
          end;
        //finally FreeAndNil(randomGen); end;
      end
    );
  {for buffer in outQueue do begin
    memStr := buffer.AsObject as TMemoryStream;
    output.CopyFrom(memStr, 0);
    FreeAndNil(memStr);
  end; }
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  //Delete old output files
  DeleteDirectory('outputs', false);
  CreateDir('outputs');

  if FileOpenDialog1.Execute = false then
    exit;

  ExeFiles.Clear;
  Log('Searching for .exe files');

  if AdvBuildFilelist(IncludeTrailingPathDelimiter( FileOpenDialog1.FileName ) + '*.exe' , faAnyFile, ExeFiles, amAny, [flFullNames, flRecursive]) = false then
  begin
    Log('There was an error scanning the folder.' + ' Aborting...');
    Exit;
  end;

  //memoLog.lines.AddStrings(ExeFiles);
  AdvStringGrid1.RowCount := ExeFiles.Count +1;
  AdvStringGrid1.Cols[0].AddStrings(ExeFiles);


  Log('Please wait, running interpreters and parsing their output, this can take a while');

  loop := Parallel.ForEach(0, ExeFiles.Count -1);
  loop.OnStopInvoke(
    procedure
    begin
      log('All done!!!');
      beep;
      loop := nil;
    end
  );
  loop.NoWait.Execute(
    procedure (const task: IOmniTask; const value: integer)
    var
      res: real;
    begin
      ExecuteProcess(ExtractFilePath(Application.ExeName) + 'dosbox-staging\dosbox.exe', '-noconsole -c "mount d ." -c "mount C ''' + ExtractFilePath(ExeFiles[value]) + '''" -c "c:" -c "' + extractfilename(ExtractShortPathName(ExeFiles[value])) + ' /? >d:\outputs\output' + inttostr(value) + '.txt" -exit', '', True, 8000, 0);
      task.Invoke(
        procedure
        var
          outtext: string;
          i: integer;
        begin
          if fileexists('outputs\output' + inttostr(value) + '.txt') then
          begin
            OutText := Trim( TFile.ReadAllText('outputs\output' + inttostr(value) + '.txt') );  //Read the output
            AdvStringGrid1.AllCells[2, value+1] := OutText;
            if OutText.Length = 0 then
              for i := 0 to (AdvStringGrid1.ColCount -1) do
                AdvStringGrid1.Colors[i, value+1] := clRed;

          end;
          AdvStringGrid1.AutoSizeRows(false, 0);
        end
      );
    end
  );
  log('Tasks started');

end;

procedure TfrmMain.Button3Click(Sender: TObject);
var
  i, SimultaneousTasks, CompletedTasks: integer;
  aTasks: array of ITask;
begin
  //Delete old output files
  DeleteDirectory('outputs', false);
  CreateDir('outputs');

  if FileOpenDialog1.Execute = false then
    exit;

  ExeFiles.Clear;
  Log('Searching for .exe files');

  if AdvBuildFilelist(IncludeTrailingPathDelimiter( FileOpenDialog1.FileName ) + '*.exe' , faAnyFile, ExeFiles, amAny, [flFullNames, flRecursive]) = false then
  begin
    Log('There was an error scanning the folder.' + ' Aborting...');
    Exit;
  end;

  //memoLog.lines.AddStrings(ExeFiles);
  AdvStringGrid1.RowCount := ExeFiles.Count +1;
  AdvStringGrid1.Cols[0].AddStrings(ExeFiles);


  Log('Please wait, running interpreters and parsing their output, this can take a while');
  application.ProcessMessages;

  //How many dosbox instances we launch at once.
  SimultaneousTasks := editSimulTasks.Value;

  CompletedTasks := 0;
  while CompletedTasks <> ExeFiles.Count  do
  begin
    SetLength(aTasks, SimultaneousTasks);
    if ExeFiles.Count - CompletedTasks < length(aTasks) then
      SetLength(aTasks, ExeFiles.Count - CompletedTasks);

    for I := Low(aTasks) to High(aTasks) do
    begin
      aTasks[I] := TTask.Create(
      DoDosboxTask( CompletedTasks + i ));
      aTasks[I].Start;
    end;
    //TTask.WaitForAll(aTasks);

    while not TTask.WaitForAll(ATasks, 250) do
    begin
      // process any pending TThread.Synchronize() and TThread.Queue() requests
      CheckSynchronize(0);
      // process any pending UI paint requests, but not other messages
      Application.MainForm.Update;
      //Application.ProcessMessages;
    end;

    inc(CompletedTasks, High(aTasks)+1);

    AdvStringGrid1.AutoSizeRows(false, 0);
    application.ProcessMessages;
  end;
  beep;

end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ExeFiles := TStringList.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ExeFiles.Free;
end;

procedure TfrmMain.Log(LogItem: String);
begin
  memoLog.Lines.Add(LogItem);
  memoLog.GotoEnd;
  //SendMessage(RichEditMemo.Handle, WM_VSCROLL, SB_LINEDOWN, 0);
end;


procedure TfrmMain.OmniEventMonitor1TaskMessage(const task: IOmniTaskControl;
  const msg: TOmniMessage);
begin
  log('oi!');
end;

procedure TfrmMain.OmniEventMonitor1TaskTerminated(
  const task: IOmniTaskControl);
begin
  beep;
end;

function ExtractBetween(const Value, A, B: string): string;
var
  aPos, bPos: Integer;
begin
  result := '';
  aPos := Pos(A, Value);
  if aPos > 0 then begin
    aPos := aPos + Length(A);
    bPos := PosEx(B, Value, aPos);
    if bPos > 0 then begin
      result := Copy(Value, aPos, bPos - aPos);
    end;
  end;
end;

//This is used in an annonymous function. Write it like this to capture the parameter variable as it is when called.
//https://stackoverflow.com/questions/50970467/delphi-ppl-ttask-procedure-with-parameters
//https://stackoverflow.com/questions/51276986/itask-how-to-use-variables-as-parameters-to-ttask-procedure
function TfrmMain.DoDosboxTask(ItemNum: integer): TProc;
begin
  Result :=
    procedure
    var
      OutText: string;
      i: integer;
    begin
      //Mount dosbox dir as drive D so we can pipe the log file to it.
      ExecuteProcess(ExtractFilePath(Application.ExeName) + 'dosbox-staging\dosbox.exe', '-noconsole -c "mount d ." -c "mount C ''' + ExtractFilePath(ExeFiles[ItemNum]) + '''" -c "c:" -c "' + extractfilename(ExtractShortPathName(ExeFiles[ItemNum])) + ' /? >d:\outputs\output' + inttostr(ItemNum) + '.txt" -exit', '', True, 8000, 0);
      //Sleep(1000); //Wait afterwards for process to fully terminate
      if fileexists('outputs\output' + inttostr(ItemNum) + '.txt') then
      begin
        OutText := Trim( TFile.ReadAllText('outputs\output' + inttostr(ItemNum) + '.txt') );  //Read the output
        AdvStringGrid1.AllCells[2, ItemNum+1] := OutText;
        if OutText.Length = 0 then
          for i := 0 to (AdvStringGrid1.ColCount -1) do
            AdvStringGrid1.Colors[i, ItemNum+1] := clRed;

        AdvStringGrid1.AllCells[1, ItemNum+1] := ExtractBetween(OutText, 'Interpreter Verson', 'Unknown flag');
      end;
    end;
end;




  {Thread := TThread.CreateAnonymousThread(
      procedure
      begin
        //RunDosboxAndExtractInfo(i);
      end
    );
    Thread.OnTerminate := QueryFinished;


  for i := 0 to ExeFiles.count -1 do
  begin
    Thread.Queue(nil,
    procedure
    begin
      RunDosboxAndExtractInfo(i);
    end);
  end;
     Thread.Start;
  }




    //Mount dosbox dir as drive D so we can pipe the log file to it.
    //full launch example: dosbox.exe -noconsole -c "mount d ." -c "mount C 'C:\Users\Ben\Desktop\Games\Indiana Jones and the Fate of Atlantis (1992)(Lucasfilm Games LLC) [Adventure]'" -c "c:" -c "atlantis.exe /? >d:\out.txt" -exit

    {if ShellExecAndWait( ExtractFilePath(Application.ExeName) + 'dosbox-staging\dosbox.exe',
      '-noconsole -c "mount d ." -c "mount C ''' + ExtractFilePath(ExeFiles[i]) + '''" -c "c:" -c "'  + extractfilename(ExeFiles[i]) + ' /? >d:\output.txt" -exit'
      , '', SW_MINIMIZE) then}

    //ShellExecute(MyHandle,'open',PChar(ExtractFilePath(Application.ExeName) + 'dosbox-staging\dosbox.exe'),PChar('-noconsole -c "mount d ." -c "mount C ''' + ExtractFilePath(ExeFiles[i]) + '''" -c "c:" -c "'  + extractfilename(ExeFiles[i]) + ' /? >d:\output.txt" -exit'),'',SW_SHOWMINIMIZED);
    //Log('-noconsole -c "mount d ." -c "mount C ''' + ExtractFilePath(ExeFiles[i]) + '''" -c "c:" -c "'  + extractfilename(ExtractShortPathName(ExeFiles[i]))  + ' /? >d:\output.txt" -exit');
    {ExecuteProcess(ExtractFilePath(Application.ExeName) + 'dosbox-staging\dosbox.exe', '-noconsole -c "mount d ." -c "mount C ''' + ExtractFilePath(ExeFiles[i]) + '''" -c "c:" -c "' + extractfilename(ExtractShortPathName(ExeFiles[i])) + ' /? >d:\output.txt" -exit', '', True, 2000, 0);
    Delay(1000); //Wait afterwards for process to fully terminate
    AdvStringGrid1.AllCells[0, i+1] := TFile.ReadAllText('output.txt');  //Read the output from the text file

  end;

  beep;
  Log('All interpreters have been processed.'); }
end.
