{     
  gditools.py GUI are licensed under the GNU General Public License (version 3), 
  a copy of which is provided in the licences folder: GNU_GPL_v3.txt.
  
  SiZiOUS 2015 / www.sizious.com
}
unit Extract;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type
  { TfrmExtractAll }

  TfrmExtractAll = class(TForm)
    btnOutputDir: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    cbxVolumeLabel: TCheckBox;
    ckgOptions: TCheckGroup;
    edtOutputDir: TEdit;
    edtDataDir: TEdit;
    gbxDataDir: TGroupBox;
    gbxOutputDir: TGroupBox;
    bfdExtractAll: TSelectDirectoryDialog;
    procedure btnOutputDirClick(Sender: TObject);
    procedure cbxVolumeLabelChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    function GetDataDirectory: string;
    function GetExtractBootstrap: Boolean;
    function GetGenerateSortFile: Boolean;
    function GetOutputDirectory: TFileName;
    procedure SetDataDirectory(AValue: string);
    procedure SetExtractBootstrap(AValue: Boolean);
    procedure SetGenerateSortFile(AValue: Boolean);
    procedure SetOutputDirectory(AValue: TFileName);
  public
    { public declarations }
    property DataDirectory: string read GetDataDirectory write SetDataDirectory;
    property OutputDirectory: TFileName
      read GetOutputDirectory write SetOutputDirectory;
    property ExtractBootstrap: Boolean read GetExtractBootstrap
      write SetExtractBootstrap;
    property GenerateSortFile: Boolean read GetGenerateSortFile
      write SetGenerateSortFile;
  end;

var
  frmExtractAll: TfrmExtractAll;

implementation

uses
  Engine;

{$R *.lfm}

{ TfrmExtractAll }

procedure TfrmExtractAll.btnOutputDirClick(Sender: TObject);
begin
  with bfdExtractAll do
  begin
    FileName := OutputDirectory;
    if Execute then
      OutputDirectory := FileName;
  end;
end;

procedure TfrmExtractAll.cbxVolumeLabelChange(Sender: TObject);
begin
  edtDataDir.Enabled := not cbxVolumeLabel.Checked;
end;

procedure TfrmExtractAll.FormCreate(Sender: TObject);
begin
  ExtractBootstrap := True;
  GenerateSortFile := True;
  OutputDirectory := GetCurrentDir;
end;

function TfrmExtractAll.GetDataDirectory: string;
begin
  Result := edtDataDir.Text;
  if cbxVolumeLabel.Checked then
    Result := SDIR_DATA_VOLUME_LABEL;
end;

function TfrmExtractAll.GetExtractBootstrap: Boolean;
begin
  Result := ckgOptions.Checked[0];
end;

function TfrmExtractAll.GetGenerateSortFile: Boolean;
begin
  Result := ckgOptions.Checked[1];
end;

function TfrmExtractAll.GetOutputDirectory: TFileName;
begin
  Result := IncludeTrailingPathDelimiter(edtOutputDir.Text);
end;

procedure TfrmExtractAll.SetDataDirectory(AValue: string);
begin
  edtDataDir.Text := AValue;
end;

procedure TfrmExtractAll.SetExtractBootstrap(AValue: Boolean);
begin
  ckgOptions.Checked[0] := AValue;
end;

procedure TfrmExtractAll.SetGenerateSortFile(AValue: Boolean);
begin
  ckgOptions.Checked[1] := AValue;
end;

procedure TfrmExtractAll.SetOutputDirectory(AValue: TFileName);
begin
  edtOutputDir.Text := IncludeTrailingPathDelimiter(AValue);
end;

end.

