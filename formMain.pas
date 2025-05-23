unit formMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids,
  System.ImageList, Vcl.ImgList,  Vcl.ExtCtrls, IOUtils, System.Hash, IdHashCRC,
  AdvObj, AdvGrid, BaseGrid, System.RegularExpressions,  System.Character,
  System.StrUtils,
  JCLFileUtils, JCLShell, JclStrings,
  OtlTask, OtlCollections, OtlParallel, OtlSync, tmsAdvGridExcel,
  uMemReader, AdvGlowButton, JvExStdCtrls, JvRichEdit, AdvUtil, JvExControls,
  JvAnimatedImage, JvGIFCtrl, JvEdit;

type
  TfrmMain = class(TForm)
    FileOpenDialog1: TFileOpenDialog;
    AdvStringGrid1: TAdvStringGrid;
    Panel1: TPanel;
    ImageList1: TImageList;
    FileSaveDialog1: TFileSaveDialog;
    AdvGridExcelIO1: TAdvGridExcelIO;
    btnParseInterpreters: TAdvGlowButton;
    btnScanResourceFiles: TAdvGlowButton;
    btnExportToExcel: TAdvGlowButton;
    btnHideInvalid: TAdvGlowButton;
    JvGIFAnimator1: TJvGIFAnimator;
    Panel2: TPanel;
    editSearch: TJvEdit;
    memoLog: TJvRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AdvGridExcelIO1ExportColumnFormat(Sender: TObject; GridCol,
      GridRow, XlsCol, XlsRow: Integer; const Value: WideString;
      var ExportCellAsString: Boolean);
    procedure btnParseInterpretersClick(Sender: TObject);
    procedure btnScanResourceFilesClick(Sender: TObject);
    procedure btnExportToExcelClick(Sender: TObject);
    procedure AdvStringGrid1GetCellColor(Sender: TObject; ARow, ACol: Integer;
      AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
    procedure btnHideInvalidClick(Sender: TObject);
    procedure editSearchChange(Sender: TObject);
  private
    { Private declarations }
    fHideRows, FilteringInProgress: Boolean;
    ExeFiles: TStringList;
    function IsExeInvalid(OutputText: string): boolean;
    function ExtractStringFromResourceFiles(Path, Exename: string): string;
    function SearchStreamForValidVersionString(TheStream: TExplorerMemoryStream; XORVal: Byte): string;
    function GetRowToMergeWith(Col0Dir: string): Integer;
    procedure GetFileChecksums(TheFile: string; var CRC: string; var MD5: string);
    procedure Log(LogItem: String);
    procedure HideInvalidRows();
    procedure EnableDisableButtons(Value: boolean);
    procedure ShowProgressAnimation(Value: Boolean);
    procedure AddRowValid(aRow: integer);
  public
    { Public declarations }
  end;

const
  InvalidColour = $003743ED;
  DosboxExe = 'dosbox-staging\dosbox.exe';
  ResourceFileExtensions: array [0..3] of string = ('.LA0', '.LEC', '.LFL', '.001');

  ExesToExclude: array[0..52] of string =
    ('8514HERE.EXE','8514HERE.EXE','ACROREAD.EXE','ACRTEST.EXE', 'ALLBOOT.EXE',
    'AR16D30.EXE','AR32D30.EXE','AR32D301.EXE','AR500DEU.EXE','ASSAULT.EXE',
    'AT.EXE','BINMOD.EXE','BOOTDISK.EXE','BOOTDSK2.EXE','BOOTFULL.EXE',
    'BOOTMKR.EXE','CDCOPY.EXE','CDPLAY.EXE','CHECKCD.EXE','CONTROL.EXE',
    'DARK.EXE','DDHELP.EXE','DIGSTART.EXE','DOS4GW.EXE','DOTT.EXE',
    'DPLAYSVR.EXE','DXINFO.EXE','DXSETUP.EXE','DXTOOL.EXE','GRIMOVIE.EXE',
    'HELPME.EXE','IMUSE.EXE','INSTALIT.EXE','INSTALL.EXE','INSTDX50.EXE',
    'INSTI4CD.EXE','LHA.EXE','LHA213.EXE','MOUSECHK.EXE','NECGMMUT.EXE',
    'RBL2DEMO.EXE','README.EXE','REBEL.EXE','REBELSE.EXE','REGISTER.EXE',
    'RUN386.EXE','SDTCT.EXE','SDTCT.EXE','SETMUSE.EXE','SETUP.EXE',
    'T_OAK2.EXE','WEBSITE.EXE','_ISDEL.EXE');
var
  frmMain: TfrmMain;
  InterpretersLoop : IOmniParallelLoop<integer>;
  cancelToken: IOmniCancellationToken;

implementation

{$R *.dfm}

{function StripNonAscii(const s: string): string;
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
end;}

function GetAlphaSubstr2(const Str: string): string;
var
  ActualLength: integer;
  i: Integer;
begin
  SetLength(result, length(Str));
  ActualLength := 0;
  for i := 1 to length(Str) do
    //if TCharHelper.IsLetter(Str[i]) then
    if Str[i].IsLetterOrDigit or Str[i].IsWhiteSpace or Str[i].IsPunctuation then
    begin
      inc(ActualLength);
      result[ActualLength] := Str[i];
    end;
  SetLength(Result, ActualLength);
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

procedure TfrmMain.AddRowValid(aRow: integer);
var
  ValidString: string;
begin
  ValidString := 'Y';

  //Nothing in resource file cell and SCUMM version cell
  if (AdvStringGrid1.Cells[8, Arow] = '') and (AdvStringGrid1.Cells[3, Arow] = '') then
    ValidString := 'N';

  //Invalid exe and no resource file string (COMI has 'cannot be run in dos mode' string but does have a resource file string
  if (IsExeInvalid(AdvStringGrid1.Cells[7, ARow])) and (AdvStringGrid1.Cells[8, Arow] = '') then
    ValidString := 'N';

  //Add to last column
  AdvStringGrid1.Cells[9, Arow] := ValidString;
end;

procedure TfrmMain.AdvGridExcelIO1ExportColumnFormat(Sender: TObject; GridCol,
  GridRow, XlsCol, XlsRow: Integer; const Value: WideString;
  var ExportCellAsString: Boolean);
begin
  ExportCellAsString  := true;
end;


procedure TfrmMain.AdvStringGrid1GetCellColor(Sender: TObject; ARow,
  ACol: Integer; AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
begin
  if ARow = 0 then exit;  //Header row
  if FilteringInProgress then Exit;

  //Use RealRowIndex as if rows are hidden the index change
  //if AdvStringGrid1.IsHiddenRow(AdvStringGrid1.RealRowIndex(ARow)) then exit; //Dont colour hidden rows, it'll colour the next visible one instead
  if AdvStringGrid1.IsHiddenRow(ARow) then Exit;   //Dont colour hidden rows, it'll colour the next visible one instead

  if AdvStringGrid1.AllCells[9, ARow] = 'N' then  //, AdvStringGrid1.RealRowIndex(ARow)] = 'N' then
    ABrush.Color := InvalidColour;

  {
  if IsRowInvalid(AdvStringGrid1.RealRowIndex(ARow)) then
  //if IsRowInvalid(ARow) then
    ABrush.Color := InvalidColour;
    }
end;

procedure TfrmMain.btnExportToExcelClick(Sender: TObject);
begin
  if FileSaveDialog1.Execute = false then exit;
  AdvStringGrid1.AutoSizeCol(7); //Interpreter output sometimes gets cutoff and hidden by cell size in spreadsheet
  advgridexcelio1.XLSExport(FileSaveDialog1.FileName, 'SCUMM Interpreters');
end;

procedure TfrmMain.btnHideInvalidClick(Sender: TObject);
begin
  FilteringInProgress := True;
  editSearch.Clear;
  editSearch.Text:=''; //Makes it show the default 'Search' text
  HideInvalidRows;
  FilteringInProgress := False;
end;

procedure TfrmMain.btnParseInterpretersClick(Sender: TObject);
var
  i: Integer;
begin
  //Is a cancel button if task assigned and begun
  if assigned(InterpretersLoop) then
  begin
    cancelToken.Signal;
    btnParseInterpreters.Enabled := false;
    exit;
  end;

  //Delete old output files
  if DeleteDirectory('outputs', false) = False then
  begin
    Log('Couldnt delete all contents of outputs folder. Check that none of the files in there are in use.');
    Exit;
  end;

  //(Re)create outputs folder
  if CreateDir('outputs') = false then
  begin
    Log('Couldnt create outputs folder, retry and check that this isnt running from a read only location');
    exit;
  end;

  if FileOpenDialog1.Execute = false then
    exit;

  editSearch.Clear;
  editSearch.Text:=''; //Makes it show the default 'Search' text

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

  btnParseInterpreters.ImageIndex := 0;
  btnParseInterpreters.Caption := 'Cancel';
  cancelToken := CreateOmniCancellationToken;
  EnableDisableButtons(False);
  btnParseInterpreters.Enabled := True;
  ShowProgressAnimation(True);
  InterpretersLoop := Parallel.ForEach(0, ExeFiles.Count -1);
  InterpretersLoop.CancelWith(cancelToken);
  InterpretersLoop.OnStopInvoke(
    procedure
    begin
      log('Scanning finished.');
      log('Raw .exe outputs can be found at ' + ExtractFilePath(Application.ExeName) + 'outputs\');
      AdvStringGrid1.ColumnSize.StretchColumn := AdvStringGrid1.ColCount; //make last column the stretch one
      AdvStringGrid1.AutoSizeRows(false, 1); //Resize the rows
      beep;
      InterpretersLoop := nil;
      cancelToken.Clear;
      btnParseInterpreters.ImageIndex := 2;
      btnParseInterpreters.Caption := 'Parse interpreters';
      ShowProgressAnimation(False);
      EnableDisableButtons(True);
    end
  );
  InterpretersLoop.NoWait.Execute(
    procedure (const task: IOmniTask; const value: integer)
    begin
      //log(inttostr(value));
      //If there's errors in future its dos 8.3 filenames eg outputs\outxx.txt it just under the limit when it was outputxx anying above 99 got ignored.
      ExecuteProcess(ExtractFilePath(Application.ExeName) + DosboxExe, '-noconsole -c "mount d ." -c "mount C ''' + ExtractFilePath(ExeFiles[value]) + '''" -c "c:" -c "' + extractfilename(ExtractShortPathName(ExeFiles[value])) + ' /? >d:\outputs\' + inttostr(value) + '.txt" -exit', '', True, 8000, 0);
      if task.CancellationToken.IsSignalled then exit;

      task.Invoke(
        procedure
        var
          outtext, firstline, versionstring, datetimestring: string;
          CRC32, MD5: string;
        begin
          if fileexists('outputs\' + inttostr(value) + '.txt') then
          begin
            OutText := Trim( TFile.ReadAllText('outputs\' + inttostr(value) + '.txt') );  //Read the output
            AdvStringGrid1.AllCells[7, value+1] := OutText;

            //Ignore this exe if in ignore array. Just to speed things up a little.
            if MatchText(extractfilename(ExeFiles[value]), ExesToExclude) then
            begin
              AddRowValid(value+1);
              exit;
            end;

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

          GetFileChecksums( ExeFiles[value], CRC32, MD5 );
          AdvStringGrid1.AllCells[5, value+1] := CRC32;
          AdvStringGrid1.AllCells[6, value+1] := MD5;

          AddRowValid(value+1);

          AdvStringGrid1.AutoSizeRow(value+1); //Resize the row to fit the text
        end
      );
    end
  );
  log('Scanning started.');


end;


procedure TfrmMain.btnScanResourceFilesClick(Sender: TObject);
var
  i, foundindex, TotalNew, TotalUpdated: integer;
  FoundFiles, CompletedDirs: TStringList;
  ResfileString: string;
begin
  TotalNew := 0;
  TotalUpdated := 0;

  if FileOpenDialog1.Execute = false then
    exit;

  editSearch.Clear;
  editSearch.Text:=''; //Makes it show the default 'Search' text

  if fHideRows = true then //Show hidden rows so we can populate them if necessary
    btnHideInvalid.Click;

  Log('Searching for resource files in dir and subdirs of ' + FileOpenDialog1.FileName);
  FoundFiles  := nil;
  CompletedDirs := nil;
  try
    FoundFiles  := TStringList.Create;
    CompletedDirs := TStringList.Create;
    if AdvBuildFilelist(IncludeTrailingPathDelimiter( FileOpenDialog1.FileName ) + '*.*' , faAnyFile, FoundFiles, amAny, [flFullNames, flRecursive]) = false then
    begin
      Log('There was an error scanning the folder.');
      Exit;
    end;

    Log('...done. ');
    Log('Please wait, scanning resource files for version information, this will take a while.');

    EnableDisableButtons(False);
    ShowProgressAnimation(True);

    //Scan all the files found, first see if the file extension matches one of the ones we are looking for
    for i := 0 to FoundFiles.Count -1 do
    begin
      if (MatchText(ExtractFileExt(FoundFiles[i]), ResourceFileExtensions)) and //File extension is a resource file
         (CompletedDirs.IndexOf(ExtractFilePath(FoundFiles[i])) = -1) then      //And isnt in a dir where we've already found a version string in a file
      begin
        ResfileString := ExtractStringFromResourceFiles( ExtractFilePath(FoundFiles[i]), extractfilename(FoundFiles[i]));
        if ResfileString > '' then
        begin
          CompletedDirs.Add( ExtractFilePath(FoundFiles[i]));

          //Does it already exist in the table? Find all matching rows
          foundindex:= GetRowToMergeWith( ExtractFilePath(FoundFiles[i])); //ExtractFileName( ExcludeTrailingPathDelimiter(ExtractFilePath(FoundFiles[i]))) );
          //Try and pick the one that has an interpreter. If not just choose the first



          //Does this dir already exist in the table? Check the basic dir name in column 1
          //foundindex := AdvStringGrid1.Cols[1].IndexOf( ExtractFileName( ExcludeTrailingPathDelimiter(ExtractFilePath(FoundFiles[i]))));

          //Now check if its also the same full path. Check dir in column 0
          //if (foundindex <> -1) and (ExtractFilePath(AdvStringGrid1.AllCells[0, foundindex]) = ExtractFilePath(FoundFiles[i])) then
          if foundindex <> -1 then
          begin
            AdvStringGrid1.AllCells[8, foundindex] :=  ResfileString;
            Inc(TotalUpdated);
            AddRowValid(foundindex);
          end
          else //add it as a new row
          begin
            AdvStringGrid1.AddRow;
            AdvStringGrid1.AllCells[8, AdvStringGrid1.LastRow] := ResfileString;
            AdvStringGrid1.AllCells[0, AdvStringGrid1.LastRow] := ExtractFilePath(FoundFiles[i]);
            AdvStringGrid1.AllCells[1, AdvStringGrid1.LastRow] := ExtractFileName( ExcludeTrailingPathDelimiter(ExtractFilePath(FoundFiles[i])));
            inc(TotalNew);
            AddRowValid(AdvStringGrid1.LastRow);
          end;
          AdvStringGrid1.AutoSizeRows(false, 0); //Resize the rows
        end;
      end;
      application.ProcessMessages; //Horrible, but it'll do for now
    end;

    log('Scanning finished. ' + inttostr(TotalNew) + ' new entries and ' + inttostr(TotalUpdated) + ' existing entries updated.');
    beep;
  finally
    CompletedDirs.Free;
    FoundFiles.Free;
    ShowProgressAnimation(False);
    EnableDisableButtons(True);
  end;
end;

procedure TfrmMain.editSearchChange(Sender: TObject);
begin
  //sometimes it still has focus when view is filtered elsewhere
  if (editSearch.Focused = false) then exit;
  if editSearch.Text = '' then exit;

  FilteringInProgress := true;

  {if fHideRows = true then
    HideInvalidRows;}


  AdvStringGrid1.UnHideColumn(9);

  AdvStringGrid1.RemoveAllFilters;
  AdvStringGrid1.FilterActive := false;
  AdvStringGrid1.Filter.Clear; // clearing any previous filter settings

  if fHideRows = True then
  begin
    with AdvStringGrid1.Filter.Add do
    begin
       Condition := '>""';
       Column := 3;
       Data := fcNormal;
       Operation := foAND; // perform AND with default True result
    end;
    with AdvStringGrid1.Filter.Add do
    begin
       Condition := '>""';
       Column := 8;
       Data := fcNormal;
       Operation := foOR;
    end;
    with AdvStringGrid1.Filter.Add do
    begin
       Condition := '"THIS PROGRAM CANNOT BE RUN IN DOS MODE"';
       Column := 7;
       Data := fcNormal;
       Operation := foOR;
       //RemoveAccented := True; causing crashes, unsure why
       CaseSensitive := False;
    end;
    AdvStringGrid1.ApplyFilter;
  end;


  AdvStringGrid1.FilterActive := True; // applying the filter
  //AdvStringGrid1.NarrowDown(editSearch.Text, True);  the accented characters check is causing crashes
  AdvStringGrid1.NarrowDown(editSearch.Text, False);

  AdvStringGrid1.HideColumn(9);

  {if fHideRows then
  begin
    HideInvalidRows;
    AdvStringGrid1.NarrowDown(editSearch.Text, True);
    HideInvalidRows;
  end
  else
  AdvStringGrid1.NarrowDown(editSearch.Text, True); }

  //AdvStringGrid1.NarrowDown(editSearch.Text, True);
  FilteringInProgress := false;
end;

procedure TfrmMain.EnableDisableButtons(Value: boolean);
begin
  btnParseInterpreters.Enabled := Value;
  btnScanResourceFiles.Enabled := Value;
  btnExportToExcel.Enabled := Value;
  btnHideInvalid.Enabled := Value;
  editSearch.Enabled := Value;
end;

function TfrmMain.ExtractStringFromResourceFiles(Path, Exename: string): string;
var
  i, j, foundoffset, blocksize: integer;
  MemStream: TExplorerMemoryStream;
  FoundFiles: TStringList;
  ParentDir: string;
begin
   {For V5 and above resource files should always have the same name as the exe with a different extension eg monkey2.001
   lflxx and diskxx files are always the same name.

   For V7 and above - version string is in the index file - in the MAXS block.
    So find MAXS, next 4 bytes BE is blocksize, read blocksize-8 as a string and then trim it.

   For LFL and DISK.LEC files - 0x 2701 seems to be an identifier for start of a string - then null terminated string after that.
    Diskxx.LEC files - version string is usually in disk01.lec
    *.lfl files - version string could be in any lfl file

   For .000 .001 files - version string in the .001 file.
   What about amiga, mac where it can be .003, 004 etc? Still in the .001 file?}

  result :='';

  //An exception for comi.exe on the cd. When not installed COMI.EXE is in the Install folder and the index file is in the parent folder.
  ParentDir := IncludeTrailingPathDelimiter(ExtractFilePath(ExcludeTrailingPathDelimiter(Path)));
  if (Exename = 'COMI.EXE') and (FileExists(ParentDir + 'COMI.LA0')) then
    Path := ParentDir;

  FoundFiles:= nil;
  MemStream := nil;
  try
    FoundFiles := TStringList.Create;
    MemStream := TExplorerMemoryStream.Create;

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

        //>=V7 Full Throttle, The Dig, CMI
        if ResourceFileExtensions[i] = '.LA0' then
        begin
          MemStream.SetXORVal($0);
          foundoffset := FindFileHeader(memstream, 0, memstream.Size, 'MAXS');
          if foundoffset > -1 then
          begin
            memstream.Position := foundoffset + 4;
            blocksize := memstream.ReadDWordBE - 8;
            Result := memstream.ReadString(blocksize);
            Exit;
          end;
        end;

        //log('File num: ' +(IntToStr( j)));

        //DISK.LEC .001 and .LFL Files. Search the entire file for strings.
        Result := SearchStreamForValidVersionString(MemStream, $69);
        if Result = '' then
          Result := SearchStreamForValidVersionString(MemStream, $FF); //Last crusade ega uses 0xFF
        if Result = '' then
          Result := SearchStreamForValidVersionString(MemStream, $0); //Last crusade VGA is not xor'ed
        if Result <> '' then exit;   //We found a valid string, dont scan any other files
      end;
    end;
  finally
    MemStream.Free;
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
  fHideRows := false;
  EditSearch.Font.Size:=20;
  FilteringInProgress := false;

  if FileExists(ExtractFilePath(Application.ExeName) + DosboxExe) = False then
  begin
    ShowMessage('Dosbox exe not found! Closing...');
    Application.Terminate;
  end;

  CreateDir('outputs');

  //Add hidden 'valid' column at the end
  AdvStringGrid1.AddColumn;
  AdvStringGrid1.ColumnHeaders.Add('Valid');
  AdvStringGrid1.HideColumn(9);
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

//Search column 0 for the string and try to pick the most appropriate row to merge with.
//If more than one then the one thats got an interpreter entry, otherwise just the first found.
function TfrmMain.GetRowToMergeWith(Col0Dir: string): Integer;
var
  i, firstfoundindex: Integer;
begin
  //result := -1;
  firstfoundindex := -1;
  //Log(Col0Dir);
  for I := 1 to AdvStringGrid1.RowCount -1 do //ignore header row
  begin
    if Col0Dir = ExtractFilePath(AdvStringGrid1.AllCells[0, i]) then //A match, so see if there's an interpreter here
    begin
      if firstfoundindex = -1 then
        firstfoundindex := i; //store index of first match in case we need it later

      if AdvStringGrid1.AllCells[3, i] > '' then //There's a SPUTM version string
      begin
        Result := i;
        exit; //This has an interpreter so is the best match
      end;
    end;
   end;
    //If we get here its searched all the strings and hasnt found an interpreter so use first match (if there is one)
  if firstfoundindex > -1 then
    Result := firstfoundindex
  else
    Result := -1; //no match
end;

procedure TfrmMain.HideInvalidRows;
{var
  i: integer;
  il: TIntList;}
begin
  {When using hidden rows and hidden columns, from the manual:
  The recommended way is to first apply all row hiding and then apply column hiding
  and unhide the columns again before unhiding the rows.}
  AdvStringGrid1.UnHideColumn(9);

  if fHideRows = True then
  begin
    AdvStringGrid1.FilterActive := False;
    AdvStringGrid1.RemoveAllFilters;
    AdvStringGrid1.Filter.Clear;
    fHideRows := False;
    btnHideInvalid.ImageIndex := 7;
    btnHideInvalid.Caption := 'Hide invalid items';
    AdvStringGrid1.HideColumn(9);
    exit;
  end;

  fHideRows := True;
  btnHideInvalid.ImageIndex := 8;
  btnHideInvalid.Caption := 'Show invalid items';
  with AdvStringGrid1 do
  begin
    FilterActive := False;
    Filter.Clear; // clearing any previous filter settings
    with Filter.Add do
    begin
       Condition := '>""';
       Column := 3;
       Data := fcNormal;
       Operation := foAND; // perform AND with default True result
    end;

    with Filter.Add do
    begin
       Condition := '>""';
       Column := 8;
       Data := fcNormal;
       Operation := foOR;
    end;

    with Filter.Add do
    begin
       Condition := '"THIS PROGRAM CANNOT BE RUN IN DOS MODE"';
       Column := 7;
       Data := fcNormal;
       Operation := foOR;
       //RemoveAccented := True;  This was causing crashes - cant work out why right now
       CaseSensitive := False;
    end;

    FilterActive := True; // applying the filter
    AdvStringGrid1.HideColumn(9);
  end;


   { if fHideRows = false then
    begin
    //Create list of rows to hide
    il := TIntList.Create(-1,-1);
    try
      for I := 1 to AdvStringGrid1.AllRowCount -1 do
      begin
        if AdvStringGrid1.AllCells[9, i] = 'N' then
          il.add(i);
      end;

      AdvStringGrid1.HideRowList(il);
    finally
      il.Free;
      fHideRows := True;
      btnHideInvalid.ImageIndex := 8;
      btnHideInvalid.Caption := 'Show invalid items';
    end;
  end
  else
  begin
    AdvStringGrid1.UnHideRowList;
    fHideRows := false;
    btnHideInvalid.ImageIndex := 7;
    btnHideInvalid.Caption := 'Hide invalid items';
  end;
  AdvStringGrid1.HideColumn(9);}
end;

function TfrmMain.IsExeInvalid(OutputText: string): boolean;
const
  InvalidStringsBeginning: array [0..8] of string = ('UNKNOWN COMMAND LINE PARAMETER /?','INSTALL','ILLEGAL COMMAND','IMUSE(TM) CONFIGURATION UTILITY','DOS/4GW','THIS PROGRAM CANNOT BE RUN IN DOS MODE', 'STUB EXEC FAILED:', 'LHA VERSION 2', 'THIS PROGRAM REQUIRES MICROSOFT WINDOWS');
begin
  result := StrHasPrefix( UpperCase(OutputText), InvalidStringsBeginning);
end;

{function TfrmMain.IsRowInvalid(aRow: integer): boolean;
begin
  Result := false;

  //Nothing in resource file cell and SCUMM version cell
  if (AdvStringGrid1.Cells[8, Arow] = '') and (AdvStringGrid1.Cells[3, Arow] = '') then
    result := true;

  //Invalid exe and no resource file string (COMI has 'cannot be run in dos mode' string but does have a resource file string
  if (IsExeInvalid(AdvStringGrid1.Cells[7, ARow])) and (AdvStringGrid1.Cells[8, Arow] = '') then
    result := true;
end; }

procedure TfrmMain.Log(LogItem: String);
begin
  memoLog.Lines.Add(LogItem);
end;


//TODO refactor. All these hacks needs merging at least, or can a reasonable regex be found for version strings without date?
function TfrmMain.SearchStreamForValidVersionString(
  TheStream: TExplorerMemoryStream; XORVal: Byte): string;
var
  foundoffset, i: integer;
  FoundStrings: TStringList;
  Tempstring: string;
begin
  result := '';
  TheStream.SetXORVal(XORVal);
  //foundoffset := 0;

  FoundStrings := TStringList.Create;
  try
    //MI1 EGA Demo has no date string. e
    //'Monkey Island Demo, version 2.0'
    foundoffset := 0;
    while foundoffset <> -1 do
    begin  //These bytes preceed the version string in MI1 eg demo and arent found elsewhere so once found just try and read it
      foundoffset := FindFileHeader(TheStream, foundoffset, TheStream.Size, #$80#$27#$01#$18);
      if foundoffset > 0 then
      begin
        TheStream.Position := foundoffset + 4;
        result := GetAlphaSubstr2( TheStream.ReadNullTerminatedString(100));
        Exit;
      end;
    end;


    foundoffset := 0;
    while foundoffset <> -1 do
    begin
      foundoffset := FindFileHeader(TheStream, foundoffset, TheStream.Size, #$27#$01); //These hex values preceed strings in the scripts
      if foundoffset > 0 then
      begin
        TheStream.Position := foundoffset + 2;
        FoundStrings.Add(TheStream.ReadNullTerminatedString(100));
        foundoffset := TheStream.Position; //Update pointer with where we are after reading the string
      end;
    end;
    //Log(FoundStrings.Text);
    //Now do regex to get the string we want. Search for date strings.
    for i := 0 to FoundStrings.Count -1 do
    begin
      if TRegEx.IsMatch(FoundStrings[i], '[0-3]?[0-9].[0-3]?[0-9][\.\/-](?:[0-9]{2})?[0-9]{2}', [roNone]) then
      begin
        result := GetAlphaSubstr2(FoundStrings[i]);
        Exit;
      end;

      //MI2 German needs different regex 'Monkey 2 (v1.0D 17Feb92) �D'
      if TRegEx.IsMatch(FoundStrings[i], '(\b\d{1,2}\D{0,3})?(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|(Nov|Dec)(?:ember)?)\D?(\d{1,2}(st|nd|rd|th)?)?(([,.\-\/])\D?)?((19[7-9]\d|20\d{2})|\d{2})*', [roNone]) then
      begin
        result := GetAlphaSubstr2(FoundStrings[i]);
        Exit;
      end;
    end;

    //Loom
    foundoffset := 0;
    while foundoffset <> -1 do
    begin
      foundoffset := FindFileHeader(TheStream, foundoffset, TheStream.Size, #$01#$8F);
      if foundoffset > 0 then
      begin
        TheStream.Position := foundoffset + 2;
        Tempstring := TheStream.ReadNullTerminatedString(100);
        Tempstring := Tempstring + TheStream.ReadNullTerminatedString(100); //Loom has version string null terminated and then date as separate null terminated, so have to read both and concatenate
        FoundStrings.Add(Tempstring);
        foundoffset := TheStream.Position;
      end;
    end;

    //Search for date strings '.LOOM]^ 1.0 (..).) 8 Mar 90.'
    for i := 0 to FoundStrings.Count -1 do
    begin
      if TRegEx.IsMatch(FoundStrings[i], '(\b\d{1,2}\D{0,3})?\b(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|(Nov|Dec)(?:ember)?)\D?(\d{1,2}(st|nd|rd|th)?)?(([,.\-\/])\D?)?((19[7-9]\d|20\d{2})|\d{2})*', [roNone]) then
      begin
        result := GetAlphaSubstr2(FoundStrings[i]);
        Exit;
      end;
    end;

    //DOTT
    foundoffset := 0;
    while foundoffset <> -1 do
    begin  //These bytes preceed the version string in DOTT and arent found elsewhere so once found just try and read it
      foundoffset := FindFileHeader(TheStream, foundoffset, TheStream.Size, #$A4#$CD#$E6#$00);
      if foundoffset > 0 then
      begin
        TheStream.Position := foundoffset + 4;
        result := GetAlphaSubstr2( TheStream.ReadNullTerminatedString(100));
        Exit;
      end;
    end;

    //MI1 CD from Monkey Madness - no date string
    foundoffset := 0;
    while foundoffset <> -1 do
    begin  //These bytes preceed the version string  and arent found elsewhere so once found just try and read it
      foundoffset := FindFileHeader(TheStream, foundoffset, TheStream.Size, #$02#$27#$01#$24);
      if foundoffset > 0 then
      begin
        TheStream.Position := foundoffset + 4;
        result := GetAlphaSubstr2( TheStream.ReadNullTerminatedString(100));
        Exit;
      end;
    end;
  finally
    FoundStrings.Free;
  end;

end;

procedure TfrmMain.ShowProgressAnimation(Value: Boolean);
begin
  JvGIFAnimator1.Visible := Value;
  JvGIFAnimator1.Animate := Value;
end;

end.
