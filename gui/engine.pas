{     
  gditools.py GUI are licensed under the GNU General Public License (version 3), 
  a copy of which is provided in the licences folder: GNU_GPL_v3.txt.
  
  SiZiOUS 2015 / www.sizious.com
}
unit Engine;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Contnrs, ComCtrls;

const
  SFILE_BOOTSTRAP             = 'IP.BIN';
  SFILE_SORT                  = 'sorttxt.txt';
  SDIR_DATA_VOLUME_LABEL      = '__volume_label__';

type
  EGDReader = class(Exception);

  { TFileEntry }

  TFileEntry = class(TObject)
  private
    fDirectory: Boolean;
    fFileName: string;
    fFullPath: string;
    fSortHashStr: string;
    procedure Update;
  public
    constructor Create(AFileName, AFullPath: string);
    property FileName: string read fFileName;
    property Directory: Boolean read fDirectory;
  end;

  { TFilesList }

  TFilesList = class(TObject)
  private
    fList: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TFileEntry;
    procedure Add(AFileName, AFullPath: string);
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TFileEntry read GetItem; default;
  end;

  { TGDReader }

  TGDReader = class(TObject)
  private
    fLoadedFileName: TFileName;
    fDirectoriesList: TStringList;
    fFilesListHashTable: TFPDataHashTable;
    fPythonExecutable: TFileName;
    fPythonScriptFileName: TFileName;
    procedure Clear;
    procedure HashTableCleanUp(Item: Pointer; const Key: string; var Continue: Boolean);
    procedure HashTableUpdateDirectoryRecord(Item: Pointer; const Key: string; var Continue: Boolean);
    procedure HashTableSortRecords(Item: Pointer; const Key: string; var Continue: Boolean);
    function RunCommandFileOutput(const CmdSwitch: string; const OutputFileName: TFileName): Boolean;
    function RunCommand(OutputDirectory: TFileName; CommandLine: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetFilesList(const Path: string): TFilesList;
    function Extract(const AFullPath: string; const OutputFileName: TFileName): Boolean;
    procedure ExtractAll(const OutputDirectory: TFileName; const DataFolder: string);
    function ExtractBootstrap(const OutputFileName: TFileName): Boolean;
    function GenerateSortFile(const OutputFileName: TFileName; const DataFolder: string): Boolean;
    procedure LoadFromFile(const AFileName: TFileName);
    procedure FillListView(ListView: TListView; Path: string);
    procedure FillTreeView(TreeView: TTreeView);
    property LoadedFileName: TFileName read fLoadedFileName;
    property PythonExecutable: TFileName read fPythonExecutable write fPythonExecutable;
  end;

implementation

uses
{$IFDEF DEBUG}
  Dialogs,
{$ENDIF}
  Utils,
  Forms,
  Process
{$IF Defined(Unix) OR Defined(Darwin)}
  , UTF8Process
{$ENDIF};

const
  SFILE_GDITOOLS_PY = 'gditools.py';

{ TFileEntry }

procedure TFileEntry.Update;
var
  S: string;

begin
  S := '1';
  if fDirectory then
    S := '0';
  fSortHashStr := S + UpperCase(fFileName);
end;

constructor TFileEntry.Create(AFileName, AFullPath: string);
begin
  fFileName := AFileName;
  fFullPath := AFullPath;
  fDirectory := False;
  Update;
end;

{ TFilesList }

function TFilesList.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TFilesList.GetItem(Index: Integer): TFileEntry;
begin
  Result := TFileEntry(fList[Index]);
end;

procedure TFilesList.Add(AFileName, AFullPath: string);
begin
  fList.Add(TFileEntry.Create(AfileName, AFullPath));
end;

procedure TFilesList.Clear;
var
  i: Integer;

begin
  for i := 0 to fList.Count - 1 do
    TFileEntry(fList[i]).Free;
  fList.Clear;
end;

constructor TFilesList.Create;
begin
  fList := TList.Create;
end;

destructor TFilesList.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
end;

{ TGDReader }

procedure TGDReader.Clear;
begin
  fDirectoriesList.Clear;
  fFilesListHashTable.Iterate(@HashTableCleanUp);
  fFilesListHashTable.Clear;
end;

procedure TGDReader.HashTableCleanUp(Item: Pointer; const Key: string;
  var Continue: Boolean);
begin
  FreeAndNil(Item);
  Continue := True;
end;

// This code was made to determine if a FileEntry is in reality a directory.
// This can be optimized if the gditools.py script prints if the entry is a
// directory or a file.
procedure TGDReader.HashTableUpdateDirectoryRecord(Item: Pointer;
  const Key: string; var Continue: Boolean);
var
  i, Index: Integer;
  FilesList: TFilesList;
  FileEntry: TFileEntry;

begin
  FilesList := TFilesList(Item);

  // for each entry in the current directory...
  for i := 0 to FilesList.Count - 1 do
  begin
    FileEntry := FilesList[i];

    // if the current entry is in fDirectoriesList, then it's a directory.
    if fDirectoriesList.Find(FileEntry.fFullPath + '/', Index) then
    begin
      FileEntry.fDirectory := True;
      FileEntry.Update;
    end;
  end;
  Continue := True;
end;

function __Compare__FileEntry(Item1, Item2: Pointer): Integer;
var
   FileEntry1, FileEntry2: TFileEntry;

begin
  FileEntry1 := TFileEntry(Item1);
  FileEntry2 := TFileEntry(Item2);

  Result := -1;
  if FileEntry1.fSortHashStr > FileEntry2.fSortHashStr then
    Result := 1
  else if FileEntry1.fSortHashStr = FileEntry2.fSortHashStr then
    Result := 0;
end;

procedure TGDReader.HashTableSortRecords(Item: Pointer; const Key: string;
  var Continue: Boolean);
var
  FilesList: TFilesList;

begin
  FilesList := TFilesList(Item);
  FilesList.fList.Sort(@__Compare__FileEntry);
  Continue := True;
end;

{ This is a shortcut for commands generating output files (like IP.BIN and
  sorttxt.txt). }
function TGDReader.RunCommandFileOutput(const CmdSwitch: string;
  const OutputFileName: TFileName): Boolean;
var
  OutputDirectory, TargetFileName: TFileName;

begin
  OutputDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(OutputFileName));
  TargetFileName := ExtractFileName(OutputFileName);
  RunCommand(OutputDirectory, Format('%s %s -o .', [CmdSwitch, TargetFileName]));
  Result := FileExists(OutputDirectory + TargetFileName);
end;

{ Thanks to Marc Weustink and contributors
  http://wiki.freepascal.org/Executing_External_Programs }
function TGDReader.RunCommand(OutputDirectory: TFileName;
  CommandLine: string): string;
const
  READ_BYTES = 2048;

var
  OutputLines: TStringList;
  MemStream: TMemoryStream;
{$IFDEF Windows}
  OurProcess: TProcess;
{$ELSE}
  OurProcess: TProcessUTF8;
{$ENDIF}
  NumBytes: LongInt;
  BytesRead: LongInt;
  SavedDirectory: TFileName;

begin
  // Checking the presence of the gditools.py script
  if not FileExists(fPythonScriptFileName) then
    raise EGDReader.Create('The gditools.py script wasn''t found');

  // Saving the current directory
  SavedDirectory := GetCurrentDir;

  // Handling the output directory
  OutputDirectory := IncludeTrailingPathDelimiter(OutputDirectory);
  ForceDirectories(OutputDirectory);
  SetCurrentDir(OutputDirectory);

  // A temp Memorystream is used to buffer the output
  MemStream := TMemoryStream.Create;
  try
    BytesRead := 0;
{$IFDEF Windows}
    OurProcess := TProcess.Create(nil);
{$ELSE}
    OurProcess := TProcessUTF8.Create(nil);
{$ENDIF}
    try

{$IFDEF Windows}
      OurProcess.Executable := PythonExecutable;
      OurProcess.Parameters.Add(Format('"%s" -i "%s" %s', [fPythonScriptFileName, LoadedFileName, CommandLine]));
{$ELSE}
      { On Linux (at least), we can't use the Parameters method above for an unknow reason.
        I think I should add several parameters for every part of the command line but it's too complicated for no gain.
        So I use the old deprecated CommandLine property instead and it works like a charm! }
      OurProcess.CommandLine := Format('"%s" "%s" -i "%s" %s', [PythonExecutable, fPythonScriptFileName, LoadedFileName, CommandLine]);
{$ENDIF}

{$IFDEF DEBUG}
      ShowMessage(OurProcess.Parameters.Text);
{$ENDIF}

      { We cannot use poWaitOnExit here since we don't know the size of the output.
        On Linux the size of the output pipe is 2 kB; if the output data is more, we
        need to read the data. This isn't possible since we are waiting.
        So we get a deadlock here if we use poWaitOnExit. }
      OurProcess.Options := [poUsePipes, poStderrToOutput];
      OurProcess.ShowWindow := swoHide;
      OurProcess.Execute;

      while True do
      begin
        // Refresh the GUI
        Application.ProcessMessages;

        // make sure we have room
        MemStream.SetSize(BytesRead + READ_BYTES);

        // try reading it
        NumBytes := OurProcess.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
        if NumBytes > 0 then
        begin
          Inc(BytesRead, NumBytes);
        end
        else
          Break;
      end;

      MemStream.SetSize(BytesRead);

      OutputLines := TStringList.Create;
      try
         OutputLines.LoadFromStream(MemStream);
         Result := OutputLines.Text;
      finally
        OutputLines.Free;
      end;

    finally
      OurProcess.Free;
    end;

  finally
    MemStream.Free;
  end;

  // Restoring the right directory
  SetCurrentDir(SavedDirectory);
end;

constructor TGDReader.Create;
begin
  fFilesListHashTable := TFPDataHashTable.Create;
  fDirectoriesList := TStringList.Create;
  with fDirectoriesList do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
  end;
  fPythonScriptFileName:= GetApplicationPath + SFILE_GDITOOLS_PY;
end;

destructor TGDReader.Destroy;
begin
  Clear;
  fFilesListHashTable.Free;
  fDirectoriesList.Free;
  inherited Destroy;
end;

procedure TGDReader.LoadFromFile(const AFileName: TFileName);
const
  FILELIST_SIGN = 'Listing all files in the filesystem:';

var
  Index, i, StartIndex: Integer;
  OutputBuffer, RecordFullPath, RecordDirectoryPath, RecordFileName: string;
  FilesList: TStringList;
  Buffer: TFilesList;

begin
  Clear;
  fLoadedFileName := AFileName;

  // Executing gditools.py
  OutputBuffer := RunCommand(GetCurrentDir, '--list');

  // Extracting the files listing
  StartIndex := Pos(FILELIST_SIGN, OutputBuffer);
  if StartIndex = 0 then
    raise EGDReader.CreateFmt('Error when parsing the GD-ROM Image%s%s',
      [sLineBreak, Trim(OutputBuffer)]);
  Index := StartIndex + Length(FILELIST_SIGN) + 1;

  // Parsing the files list...
  FilesList := TStringList.Create;
  try
    // Load the files list buffer
    FilesList.Text := Trim(Copy(OutputBuffer, Index, Length(OutputBuffer) - Index));

    // Extracting the directory listing and building the files list
    for i := 0 to FilesList.Count - 1 do
    begin
      RecordFullPath := Trim(FilesList[i]);
      RecordDirectoryPath := ExtractFilePath(RecordFullPath);
      RecordFileName := ExtractFileName(RecordFullPath);

      // Building the directory listing...
      fDirectoriesList.Add(RecordDirectoryPath);

      // Handling the files list...
      if not Assigned(fFilesListHashTable.Find(RecordDirectoryPath)) then
        fFilesListHashTable.Add(RecordDirectoryPath, TFilesList.Create);
      Buffer := TFilesList(fFilesListHashTable[RecordDirectoryPath]);

      // Adding the record...
      if RecordFileName <> '' then
        Buffer.Add(RecordFileName, RecordFullPath);
    end;

    // Setting the Directory flag:
    // This can be optimized if the output of gditools.py indicates if the entry is a file/dir
    fFilesListHashTable.Iterate(@HashTableUpdateDirectoryRecord);

    // Sorting the directories and the files order
    fFilesListHashTable.Iterate(@HashTableSortRecords);
  finally
    FilesList.Free;
  end;
end;

procedure TGDReader.FillTreeView(TreeView: TTreeView);
var
  i, ImageIndex: Integer;
  Directory, DirectoryName, DirectoryPath: string;
  HashTable: TFPDataHashTable;
  Node: TTreeNode;

begin
  HashTable := TFPDataHashTable.Create;
  try
    TreeView.Items.Clear;
    ImageIndex := 0;
    for i := 0 to fDirectoriesList.Count - 1 do
    begin
      // Work with the current directory entry
      Directory := fDirectoriesList[i];

      // Get only the directory name
      DirectoryName := ExtractFileName(Copy(Directory, 1, Length(Directory) - 1));

      // Get the directory path (to handle parent nodes)
      DirectoryPath := Copy(Directory, 1, Pos(DirectoryName, Directory) - 1);

      // If the directory name is empty, then use the full path (the 'directory' it-self)
      if DirectoryName = '' then
        DirectoryName := Directory;

      // Get the parent node (returns 'nil' if the parent node doesn't exists
      Node := TTreeNode(HashTable.Items[DirectoryPath]);

      // Add the current directory under the found parent (or under 'nil')
      Node := TreeView.Items.AddChild(Node, DirectoryName);
      Node.ImageIndex := ImageIndex;
      Node.SelectedIndex := ImageIndex;

      // For the first node (the root) the image will be a 'CD', after 'folders'
      ImageIndex := 1;

      // Adding the current directory (may be a parent for further nodes)
      HashTable.Add(Directory, Node);
    end;
  finally
    HashTable.Free;
  end;

  // Open the first node
  if TreeView.Items.Count > 0 then
    TreeView.Items[0].Expand(False);
end;

procedure TGDReader.FillListView(ListView: TListView; Path: string);
var
  FilesList: TFilesList;
  i: Integer;
  FileEntry: TFileEntry;

begin
  // Get the selected files list from the GDI path
  FilesList := GetFilesList(Path);

  // Fill the ListView with the contents of the files list
  ListView.Clear;
  if Assigned(FilesList) then
  begin
    ListView.BeginUpdate;
    for i := 0 to FilesList.Count - 1 do
      with ListView.Items.Add do
      begin
        FileEntry := FilesList[i];
        Caption := FileEntry.FileName;

        // By default, every entries are files...
        ImageIndex := 0;

        // ... but mark directories with the right icon
        if FileEntry.Directory then
          ImageIndex := 1;
      end;
    ListView.EndUpdate;
  end;
end;

function TGDReader.GetFilesList(const Path: string): TFilesList;
begin
  Result := TFilesList(fFilesListHashTable.Items[Path]);
end;

function TGDReader.Extract(const AFullPath: string;
  const OutputFileName: TFileName): Boolean;
var
  SourceFileName, TargetFileName, OutputDir: TFileName;

begin
  Result := False;

  SourceFileName := ExtractFileName(AFullPath);
  TargetFileName := ExtractFileName(OutputFileName);

  // Handling the output directory
  OutputDir := IncludeTrailingPathDelimiter(ExtractFilePath(OutputFileName));

  // Calling the gditools.py script
  RunCommand(OutputDir, Format('-e %s -o .', [AFullPath]));

  // if the file need to be renamed, do it
  if (SourceFileName <> TargetFileName) then
    RenameFile(OutputDir + SourceFileName, OutputDir + TargetFileName);

  Result := FileExists(OutputFileName);
end;

procedure TGDReader.ExtractAll(const OutputDirectory: TFileName;
  const DataFolder: string);
begin
  RunCommand(OutputDirectory, Format('--extract-all -o . --data-folder %s', [DataFolder]));
end;

function TGDReader.ExtractBootstrap(const OutputFileName: TFileName): Boolean;
begin
  Result := RunCommandFileOutput('-b', OutputFileName);
end;

function TGDReader.GenerateSortFile(const OutputFileName: TFileName;
  const DataFolder: string): Boolean;
begin
  Result := RunCommandFileOutput(Format('--data-folder %s -s', [DataFolder]), OutputFileName);
end;

end.