unit formMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids,
  System.ImageList, Vcl.ImgList,  Vcl.ExtCtrls, IOUtils, System.Hash, IdHashCRC,
  AdvMemo, AdvObj, AdvGrid, AdvUtil, BaseGrid, System.RegularExpressions,
  JCLFileUtils, JCLShell, JclStrings,
  OtlTask, OtlCollections, OtlParallel, OtlSync, tmsAdvGridExcel,
  uMemReader;

type
  TfrmMain = class(TForm)
    FileOpenDialog1: TFileOpenDialog;
    memoLog: TAdvMemo;
    AdvStringGrid1: TAdvStringGrid;
    Panel1: TPanel;
    Label1: TLabel;
    btnChooseFolder: TButton;
    ImageList1: TImageList;
    btnSave: TButton;
    FileSaveDialog1: TFileSaveDialog;
    AdvGridExcelIO1: TAdvGridExcelIO;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnChooseFolderClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSaveClick(Sender: TObject);
    procedure AdvGridExcelIO1ExportColumnFormat(Sender: TObject; GridCol,
      GridRow, XlsCol, XlsRow: Integer; const Value: WideString;
      var ExportCellAsString: Boolean);
  private
    { Private declarations }
    ExeFiles: TStringList;
    function IsExeInvalid(OutputText: string): boolean;
    function ExtractStringFromResourceFiles(Path, Exename: string): string;
    procedure GetFileChecksums(TheFile: string; var CRC: string; var MD5: string);
    procedure Log(LogItem: String);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;
  loop : IOmniParallelLoop<integer>;
  cancelToken: IOmniCancellationToken;

implementation

{$R *.dfm}

function StripNonAscii(const s: string): string;
var
  i, Count: Integer;
begin
  SetLength(Result, Length(s));
  Count := 0;
  for i := 1 to Length(s) do begin
    if ((s[i] >= #32) and (s[i] <= #127)) or (s[i] in [#10, #13]) then begin
      inc(Count);
      Result[Count] := s[i];
    end;
  end;
  SetLength(Result, Count);
end;

function FindFileHeader(SearchStream: TExplorerMemoryStream;
  StartSearchAt, EndSearchAt: Integer; Header: ansistring): integer;
var
  HeaderLength, Index: integer;
begin
  Result:=-1;
  Index:=1;
  if EndSearchAt > SearchStream.Size then
    EndSearchAt:=SearchStream.Size;

  HeaderLength:=Length(Header);
  if HeaderLength <= 0 then exit;

  SearchStream.Position:=StartSearchAt;
  while SearchStream.Position < EndSearchAt do
  begin
    if AnsiChar(SearchStream.ReadByte) <> Header[Index] then
    begin
      if Index > 1 then
        SearchStream.Position := SearchStream.Position  -1;

      Index:=1;
      continue;
    end;

    inc(Index);
    if index > HeaderLength then
    begin
      Result:=SearchStream.Position - HeaderLength;
      exit;
    end;
  end;
end;

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

function ExtractBetween(const Value, A, B: string): string;
var
  aPos, bPos: Integer;
begin
  result := '';
  aPos := Pos(A, Value);
  if aPos > 0 then begin
    aPos := aPos + Length(A);
    bPos := Pos(B, Value, aPos);
    if bPos > 0 then begin
      result := Copy(Value, aPos, bPos - aPos);
    end;
  end;
end;

procedure TfrmMain.AdvGridExcelIO1ExportColumnFormat(Sender: TObject; GridCol,
  GridRow, XlsCol, XlsRow: Integer; const Value: WideString;
  var ExportCellAsString: Boolean);
begin
  ExportCellAsString  := true;
end;

procedure TfrmMain.btnChooseFolderClick(Sender: TObject);
var
  i: Integer;
begin
  //Is a cancel button if task assigned and begun
  if assigned(loop) then
  begin
    cancelToken.Signal;
    btnChooseFolder.Enabled := false;
    exit;
  end;

  //Delete old output files
  DeleteDirectory('outputs', false);
  CreateDir('outputs');

  if FileOpenDialog1.Execute = false then
    exit;

  AdvStringGrid1.ClearNormalRows(1, AdvStringGrid1.RowCount -1);
  ExeFiles.Clear;
  Log('Searching for .exe files in ' + FileOpenDialog1.FileName);

  if AdvBuildFilelist(IncludeTrailingPathDelimiter( FileOpenDialog1.FileName ) + '*.exe' , faAnyFile, ExeFiles, amAny, [flFullNames, flRecursive]) = false then
  begin
    Log('There was an error scanning the folder.');
    Exit;
  end;

  Log('...done. ' + inttostr(ExeFiles.Count) + ' found.');
  AdvStringGrid1.RowCount := ExeFiles.Count +1;
  AdvStringGrid1.Cols[0].AddStrings(ExeFiles);

  //memoLog.lines.AddStrings(ExeFiles);
  for I := 0 to ExeFiles.Count -1 do
  begin
    AdvStringGrid1.Cols[1].Add( ExtractFileName( ExcludeTrailingPathDelimiter(ExtractFilePath(ExeFiles[i])) ));
    AdvStringGrid1.Cols[2].Add( ExtractFileName(ExeFiles[i] ));
  end;

  Log('Please wait, running interpreters and parsing their output, this will take a while.');

  btnChooseFolder.ImageIndex := 3;
  btnChooseFolder.Caption := 'Cancel';
  cancelToken := CreateOmniCancellationToken;
  btnSave.Enabled :=false;
  loop := Parallel.ForEach(0, ExeFiles.Count -1);
  //loop.PreserveOrder;
  loop.CancelWith(cancelToken);
  loop.OnStopInvoke(
    procedure
    begin
      log('Scanning finished.');
      log('Raw outputs can be found at ' + ExtractFilePath(Application.ExeName) + 'outputs\');
      beep;
      loop := nil;
      cancelToken.Clear;
      btnChooseFolder.ImageIndex := 0;
      btnChooseFolder.Caption := 'Choose a folder';
      btnChooseFolder.Enabled := true;
      btnSave.Enabled := True;
    end
  );
  loop.NoWait.Execute(
    procedure (const task: IOmniTask; const value: integer)
    begin
      //log(inttostr(value));
      //If there's errors in future its dos 8.3 filenames eg outputs\outxx.txt it just under the limit when it was outputxx anying above 99 got ignored.
      ExecuteProcess(ExtractFilePath(Application.ExeName) + 'dosbox-staging\dosbox.exe', '-noconsole -c "mount d ." -c "mount C ''' + ExtractFilePath(ExeFiles[value]) + '''" -c "c:" -c "' + extractfilename(ExtractShortPathName(ExeFiles[value])) + ' /? >d:\outputs\out' + inttostr(value) + '.txt" -exit', '', True, 8000, 0);
      if task.CancellationToken.IsSignalled then exit;

      task.Invoke(
        procedure
        var
          outtext, firstline, versionstring, datetimestring, resfilestring: string;
          CRC32, MD5: string;
          i: integer;
        begin
          if fileexists('outputs\out' + inttostr(value) + '.txt') then
          begin
            OutText := Trim( TFile.ReadAllText('outputs\out' + inttostr(value) + '.txt') );  //Read the output
            AdvStringGrid1.AllCells[8, value+1] := OutText;

            resfilestring := ExtractStringFromResourceFiles( ExtractFilePath(ExeFiles[value]), extractfilename(ExeFiles[value]));
            AdvStringGrid1.AllCells[7, value+1] := resfilestring;

            firstline := StrBefore(sLineBreak, OutText);
            firstline := firstline + sLineBreak; //Add the linebreak back in, we search for it later

            //Spelling of Version differs in some interpreters
            versionstring := ExtractBetween(firstline, 'Interpreter Verson', ' (');
            if versionstring = '' then
              versionstring := ExtractBetween(firstline, 'Interpreter Version', ' (');
            if versionstring = '' then
              versionstring := ExtractBetween(firstline, 'Version', sLineBreak );
            if versionstring = '' then
              versionstring := ExtractBetween(firstline, 'Verson', sLineBreak );
            if versionstring = '' then
              versionstring := ExtractBetween(firstline, 'SPU(tm) version ', ' (');

            datetimestring := ExtractBetween(firstline, '(', ')');
            if datetimestring = 'tm' then //The Dig has SPU(tm) before (datetime)
              datetimestring := ExtractBetween( strafter(')', firstline), '(', ')');

            AdvStringGrid1.AllCells[3, value+1] := versionstring;
            AdvStringGrid1.AllCells[4, value+1] := datetimestring;
          end;
          //else Log(ExeFiles[value]);

          //Invalid exe
          if ((OutText.Length = 0) or (IsExeInvalid(OutText))) and (resfilestring = '') then
          begin
            for i := 0 to (AdvStringGrid1.ColCount -1) do
              AdvStringGrid1.Colors[i, value+1] := $003743ED;//0000009F;
          end;


          GetFileChecksums( ExeFiles[value], CRC32, MD5 );
          AdvStringGrid1.AllCells[5, value+1] := CRC32;
          AdvStringGrid1.AllCells[6, value+1] := MD5;

          AdvStringGrid1.ColumnSize.StretchColumn := AdvStringGrid1.ColCount; //make last column the stretch one
          AdvStringGrid1.AutoSizeRows(false, 0); //Resize the rows
        end
      );
    end
  );
  log('Scanning started.');

end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  if FileSaveDialog1.Execute = false then exit;

  //AdvStringGrid1.SaveToXLS(FileSaveDialog1.FileName);
  advgridexcelio1.XLSExport(FileSaveDialog1.FileName, 'SCUMM Interpreters')
end;

function TfrmMain.ExtractStringFromResourceFiles(Path, Exename: string): string;
const
  ResExtensions: array[0..0] of string = ('.LA0');
  ResourceFileExtensions: array [0..0] of string = ('.LEC');
var
  i, j, k, foundoffset, blocksize: integer;
  MemStream: TExplorerMemoryStream;
  //sr: TSearchRec;
  FoundFiles, FoundStrings: TStringList;
begin
  result :='';

  // >=V5 resource files should always have the same name as the exe with a different extension
  // lflxx and diskxx files are always the same name
    //For Full Throttle and above - in the index file its in the MAXS block
    //So find MAXS, next 4 bytes BE is blocksize, so read blocksize-8 as a string and then trim it

    //For LFL and DISK.LEC files - 0x 2701 seems to be an identifier for start of a string- then string after that, null terminated
    // for .000 001 etc - ?

  //See if any resource files with those extensions exist in the folder
  {for i := 0 to length(ResExtensions)-1 do
  begin
    if fileexists(Path + ChangeFileExt(Exename, ResExtensions[i])) then
    begin
      memstream := TExplorerMemoryStream.Create;
      try
        memstream.LoadFromFile(Path + ChangeFileExt(Exename, ResExtensions[i]));
        foundoffset := FindFileHeader(memstream, 0, memstream.Size, 'MAXS');
        if foundoffset > -1 then
        begin
          memstream.Position := foundoffset + 4;
          blocksize := memstream.ReadDWordBE - 8;
          Result := memstream.ReadString(blocksize);
        end;
      finally
        memstream.Free;
      end;

    end;
  end;}


  for i := 0 to length(ResExtensions)-1 do
  begin
    //if FindFirst(Path + '*'+ ResExtensions[i], 0, sr) = 0 then
    //An exception for comi.exe on the cd. When not installed COMI.EXE is in the Install folder and he index file is in the parent folder.
    if (Exename = 'COMI.EXE') and (fileexists(ExtractFilePath(ExcludeTrailingPathDelimiter(Path)) + '/COMI.LA0')) then
      Path := ExtractFilePath(ExcludeTrailingPathDelimiter(Path) +'/');

    if fileexists(Path + ChangeFileExt(Exename, ResExtensions[i])) then
    begin
      memstream := TExplorerMemoryStream.Create;
      try
        memstream.LoadFromFile(Path + ChangeFileExt(Exename, ResExtensions[i])); //sr.Name);
        foundoffset := FindFileHeader(memstream, 0, memstream.Size, 'MAXS');
        if foundoffset > -1 then
        begin
          memstream.Position := foundoffset + 4;
          blocksize := memstream.ReadDWordBE - 8;
          Result := memstream.ReadString(blocksize);
          //log( IntToStr(StrNIPos(Result, #0, 2)) );  //Find second occurance of null character
        end;
      finally
        memstream.Free;
      end;
    end
    //FindClose(sr);
  end;

  FoundFiles := TStringList.Create;
  try
    memstream := TExplorerMemoryStream.Create;
    try
      for i := 0 to length(ResourceFileExtensions) -1 do
      begin
        //Find all files in the folder with this file extension
        FoundFiles.Clear;
        BuildFileList(IncludeTrailingPathDelimiter(Path) + '*' + ResourceFileExtensions[i], faAnyFile, FoundFiles );
        if FoundFiles.Count = 0 then Continue;

        for j := 0 to FoundFiles.Count -1 do
        begin
          MemStream.Clear;
          MemStream.LoadFromFile(Path + FoundFiles[j]);
          MemStream.SetXORVal($69);

          FoundStrings := TStringList.Create;
          try
            //Search entire file for strings
            foundoffset := 0;
            while foundoffset <> -1 do
            begin
              foundoffset := FindFileHeader(MemStream, foundoffset, MemStream.Size, #$27#$01); //These hex values start strings in the scripts
              if foundoffset > 0 then
              begin
                MemStream.Position := foundoffset;
                FoundStrings.Add(MemStream.ReadNullTerminatedString(100));
                foundoffset := MemStream.Position; //Update pointer with where we are after reading the string
              end;
            end;

            //Now do regex to get the string we want. Search for date strings.
            for k := 0 to FoundStrings.Count -1 do
            begin
              if TRegEx.IsMatch(FoundStrings[k], '[0-3]?[0-9].[0-3]?[0-9].(?:[0-9]{2})?[0-9]{2}', [roNone]) then
              begin
                result := StripNonAscii(FoundStrings[k]);
                log(result);
                Exit;
              end;
            end;

          finally
            FoundStrings.Free;
          end;

        end;
      end;
    finally
      memstream.Free;
    end;
  finally
    FoundFiles.Free;
  end;


end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if assigned(cancelToken) then
    cancelToken.Signal;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ExeFiles := TStringList.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ExeFiles.Free;
end;

procedure TfrmMain.GetFileChecksums(TheFile: string; var CRC: string; var MD5: string);
var
  CRCMaker : TIdHashCRC32;
  CRC32: DWORD;
  Temp: TFileStream;
begin
  Temp := TFileStream.Create(TheFile, fmOpenRead or fmShareDenyWrite);
  try
    CRCMaker := TIdHashCRC32.Create;
    try
      CRC32 := CRCMaker.HashValue(Temp);
      CRC := IntToHex(CRC32); //' 0x' + IntToHex(CRC32) + ' (' + IntToStr( CRC32 ) + ')';
    finally
      CRCMaker.Free;
    end;
  finally
    Temp.Free;
  end;

  MD5 := UpperCase(THashMD5.GetHashStringFromFile( TheFile ))
end;

function TfrmMain.IsExeInvalid(OutputText: string): boolean;
const
  InvalidStringsBeginning: array [0..8] of string = ('UNKNOWN COMMAND LINE PARAMETER /?','INSTALL','ILLEGAL COMMAND','IMUSE(TM) CONFIGURATION UTILITY','DOS/4GW','THIS PROGRAM CANNOT BE RUN IN DOS MODE', 'STUB EXEC FAILED:', 'LHA VERSION 2', 'THIS PROGRAM REQUIRES MICROSOFT WINDOWS');
begin
  result := StrHasPrefix( UpperCase(OutputText), InvalidStringsBeginning);
end;

procedure TfrmMain.Log(LogItem: String);
begin
  memoLog.Lines.Add(LogItem);
  memoLog.GotoEnd;
  //SendMessage(RichEditMemo.Handle, WM_VSCROLL, SB_LINEDOWN, 0);
end;






end.
