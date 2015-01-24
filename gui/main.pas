{     
  gditools.py GUI are licensed under the GNU General Public License (version 3), 
  a copy of which is provided in the licences folder: GNU_GPL_v3.txt.
  
  SiZiOUS 2015 / www.sizious.com
}
unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Menus, ExtCtrls, Engine, Types, LCLIntf;

type
  { TfrmMain }

  TfrmMain = class(TForm)
    btnOpen: TButton;
    edtFileName: TEdit;
    gbxDiscImage: TGroupBox;
    gbxContents: TGroupBox;
    ilDirectories: TImageList;
    ilFiles: TImageList;
    lvwFiles: TListView;
    miSep5: TMenuItem;
    miOpenItem: TMenuItem;
    miGenerateSortFile: TMenuItem;
    miExtractBootstrap: TMenuItem;
    miSep4: TMenuItem;
    miExtract2: TMenuItem;
    miSettings: TMenuItem;
    miSep3: TMenuItem;
    miExtractAll: TMenuItem;
    miTools: TMenuItem;
    miExtract: TMenuItem;
    miFile: TMenuItem;
    miSep2: TMenuItem;
    miHelp: TMenuItem;
    miOpenFile: TMenuItem;
    miCloseFile: TMenuItem;
    miSep1: TMenuItem;
    miQuit: TMenuItem;
    miProjectPage: TMenuItem;
    miAbout: TMenuItem;
    miGetAssistance: TMenuItem;
    mmMain: TMainMenu;
    opdGDI: TOpenDialog;
    pmFiles: TPopupMenu;
    sbrMain: TStatusBar;
    svdExtract: TSaveDialog;
    Splitter1: TSplitter;
    svdExtractBootstrap: TSaveDialog;
    tmrResetStatusBar: TTimer;
    tvwDirectories: TTreeView;
    procedure btnOpenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvwFilesContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure lvwFilesDblClick(Sender: TObject);
    procedure lvwFilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure miAboutClick(Sender: TObject);
    procedure miCloseFileClick(Sender: TObject);
    procedure miGetAssistanceClick(Sender: TObject);
    procedure miProjectPageClick(Sender: TObject);
    procedure miQuitClick(Sender: TObject);
    procedure miExtractAllClick(Sender: TObject);
    procedure miExtractBootstrapClick(Sender: TObject);
    procedure miExtractClick(Sender: TObject);
    procedure miGenerateSortFileClick(Sender: TObject);
    procedure miOpenItemClick(Sender: TObject);
    procedure miSettingsClick(Sender: TObject);
    procedure tmrResetStatusBarTimer(Sender: TObject);
    procedure tvwDirectoriesClick(Sender: TObject);
  private
    { private declarations }
    fSelectedFile: string;
    procedure ChangeButtonsState(State: Boolean);
    function GetSelectedNodePath: string;
    function GetStatusText: string;
    procedure SetStatusText(AValue: string);
    procedure ResetStatusBar(Success: Boolean);
  public
    { public declarations }
    property SelectedFile: string read fSelectedFile;
    property StatusText: string read GetStatusText write SetStatusText;
  end;

var
  frmMain: TfrmMain;
  GDReader: TGDReader;

implementation

{$R *.lfm}

uses
{$IFDEF Darwin}
  LCLType,
{$ENDIF}
  Utils,
  Extract,
  SortFile,
  Settings,
  About;

{ TfrmMain }

procedure TfrmMain.btnOpenClick(Sender: TObject);
var
  Success: Boolean;

begin
  with opdGDI do
    if Execute then
    begin
      if FileExists(FileName) then
      begin
        StatusText := 'Opening the specified file... Please wait.';

        // Load the file then populate the TreeView
        GDReader.LoadFromFile(FileName);
        GDReader.FillTreeView(tvwDirectories);

        // Check if the file was successfully opened or not
        Success := tvwDirectories.Items.Count > 0;

        // Reset the status bar
        ResetStatusBar(Success);

        // If we can open the root folder, do it
        // This means that the opened file is valid.
        if Success then
        begin
          edtFileName.Text := FileName;
          ChangeButtonsState(True);
          tvwDirectories.Items[0].Selected := True;
          tvwDirectoriesClick(Self);
        end
        else
        begin
          MessageDlg('The specified file seems to be invalid.', mtWarning, [mbOK], 0);
          miCloseFileClick(Self);
        end;
      end
      else
        MessageDlg('The specified GDI image doesn''t exist.', mtWarning, [mbOK], 0);
    end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  ChangeButtonsState(False);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  LoadConfig;
end;

procedure TfrmMain.lvwFilesContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  // Show the context menu only on items
  if not Assigned(lvwFiles.Selected) then
    Handled := True;
  miExtract.Enabled := not Handled;
  miExtract2.Enabled := not Handled;
end;

procedure TfrmMain.lvwFilesDblClick(Sender: TObject);
var
  Node: TTreeNode;

begin
  if Assigned(lvwFiles.Selected) then
    if lvwFiles.Selected.ImageIndex = 1 then // it's a directory
    begin
      // get the directory on the left tree view
      Node := tvwDirectories.Items.FindNodeWithText(lvwFiles.Selected.Caption);
      if Assigned(Node) then
      begin
        // the current directory was found in the left tree view, open the contents
        Node.Selected := True;
        tvwDirectoriesClick(Self);
      end;
    end
    else
      MessageDlg('This action is only available on folders.', mtWarning, [mbOK], 0);
end;

procedure TfrmMain.lvwFilesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
    fSelectedFile := GetSelectedNodePath + Item.Caption;
  miExtract.Enabled := Selected;
  miExtract2.Enabled := Selected;
end;

procedure TfrmMain.miAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.miCloseFileClick(Sender: TObject);
begin
  edtFileName.Clear;
  lvwFiles.Clear;
  tvwDirectories.Items.Clear;
  fSelectedFile := '';
  ChangeButtonsState(False);
end;

procedure TfrmMain.miGetAssistanceClick(Sender: TObject);
begin
  OpenURL('http://www.assemblergames.com/forums/showthread.php?55022-gditools-py-a-Python-program-library-to-handle-GDI-files!');
end;

procedure TfrmMain.miProjectPageClick(Sender: TObject);
begin
  OpenURL('https://sourceforge.net/projects/dcisotools/');
end;

procedure TfrmMain.miQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.miExtractAllClick(Sender: TObject);
var
  Success: Boolean;

begin
  Success := True;
  with frmExtractAll do
    if ShowModal = mrOK then
    begin
      StatusText := 'Please wait while extracting the whole dump...';
      GDReader.ExtractAll(OutputDirectory, DataDirectory);
      Success := Success and DirectoryExists(OutputDirectory);
      if ExtractBootstrap then
        Success := Success and GDReader.ExtractBootstrap(OutputDirectory + SFILE_BOOTSTRAP);
      if GenerateSortFile then
        Success := Success and GDReader.GenerateSortFile(OutputDirectory + SFILE_SORT, DataDirectory);
      ResetStatusBar(Success);
    end;
end;

procedure TfrmMain.miExtractBootstrapClick(Sender: TObject);
var
  Success: Boolean;

begin
  with svdExtractBootstrap do
    if Execute then
    begin
      StatusText := 'Extracting Boostrap...';
      Success := GDReader.ExtractBootstrap(FileName);
      ResetStatusBar(Success);
    end;
end;

procedure TfrmMain.miExtractClick(Sender: TObject);
var
  Success: Boolean;

begin
  if Assigned(lvwFiles.Selected) then
  begin
    // An item is selected

    // If the item is a directory, the feature can't be used (for now)
    if lvwFiles.Selected.ImageIndex = 1 then
      MessageDlg('This feature was only designed to handle files.', mtWarning, [mbOK], 0)
    else
    begin
      // Extract the file
      with svdExtract do
      begin
        FileName := lvwFiles.Selected.Caption;
        if Execute then
        begin
          StatusText := Format('Extracting ''%s''...', [SelectedFile]);
          Success := GDReader.Extract(SelectedFile, FileName);
          ResetStatusBar(Success);
        end;
      end;
    end;
  end
  else
  begin
    MessageDlg('Please select a file in order to use this feature.', mtWarning, [mbOK], 0);
  end;
end;

procedure TfrmMain.miGenerateSortFileClick(Sender: TObject);
var
  Success: Boolean;

begin
  with frmSortFile do
    if ShowModal = mrOK then
    begin
      StatusText := 'Generating Sort File...';
      Success := GDReader.GenerateSortFile(OutputFileName, DataDirectory);
      ResetStatusBar(Success);
    end;
end;

procedure TfrmMain.miOpenItemClick(Sender: TObject);
begin
  if Assigned(lvwFiles.Selected) then
    lvwFilesDblClick(Self)
end;

procedure TfrmMain.miSettingsClick(Sender: TObject);
begin
  with frmSettings do
  begin
    PythonExecutable := GDReader.PythonExecutable;
    if ShowModal = mrOK then
      GDReader.PythonExecutable := PythonExecutable;
  end;
end;

procedure TfrmMain.tmrResetStatusBarTimer(Sender: TObject);
begin
  StatusText := '';
  tmrResetStatusBar.Enabled := False;
end;

procedure TfrmMain.tvwDirectoriesClick(Sender: TObject);
begin
  GDReader.FillListView(lvwFiles, GetSelectedNodePath);
end;

procedure TfrmMain.ChangeButtonsState(State: Boolean);
begin
  miCloseFile.Enabled := State;
  miExtractAll.Enabled := State;
  miExtractBootstrap.Enabled := State;
  miGenerateSortFile.Enabled := State;
  if not State then
  begin
    miExtract.Enabled := False;
    miExtract2.Enabled := False;
  end;
end;

function TfrmMain.GetSelectedNodePath: string;
begin
  Result := GetNodePath(tvwDirectories.Selected);
end;

function TfrmMain.GetStatusText: string;
begin
  Result := sbrMain.SimpleText;
end;

procedure TfrmMain.SetStatusText(AValue: string);
begin
  sbrMain.SimpleText := AValue;
end;

procedure TfrmMain.ResetStatusBar(Success: Boolean);
begin
  if Success then
    StatusText := 'Done!'
  else
    StatusText := 'Failed!';
  tmrResetStatusBar.Enabled := True;
end;

initialization
  GDReader := TGDReader.Create;

finalization
  GDReader.Free;

end.

