{     
  gditools.py GUI are licensed under the GNU General Public License (version 3), 
  a copy of which is provided in the licences folder: GNU_GPL_v3.txt.
  
  SiZiOUS 2015 / www.sizious.com
}
unit SortFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfrmSortFile }

  TfrmSortFile = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    btnOutputFileName: TButton;
    cbxVolumeLabel: TCheckBox;
    edtDataDir: TEdit;
    edtOutputFileName: TEdit;
    gbxDataDir: TGroupBox;
    gbxOutputDir: TGroupBox;
    svdSortFile: TSaveDialog;
    procedure btnOutputFileNameClick(Sender: TObject);
    procedure cbxVolumeLabelChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetDataDirectory: string;
    function GetOutputFileName: TFileName;
    procedure SetDataDirectory(AValue: string);
    procedure SetOutputFileName(AValue: TFileName);
    { private declarations }
  public
    { public declarations }
    property DataDirectory: string
      read GetDataDirectory write SetDataDirectory;
    property OutputFileName: TFileName
      read GetOutputFileName write SetOutputFileName;
  end;

var
  frmSortFile: TfrmSortFile;

implementation

uses
  Engine;

{$R *.lfm}

{ TfrmSortFile }

procedure TfrmSortFile.btnOutputFileNameClick(Sender: TObject);
begin
  with svdSortFile do
  begin
    FileName := OutputFileName;
    if Execute then
      OutputFileName := FileName;
  end;
end;

procedure TfrmSortFile.cbxVolumeLabelChange(Sender: TObject);
begin
  edtDataDir.Enabled := not cbxVolumeLabel.Checked;
end;

procedure TfrmSortFile.FormCreate(Sender: TObject);
begin
  OutputFileName := IncludeTrailingPathDelimiter(GetCurrentDir) + SFILE_SORT;
end;

function TfrmSortFile.GetDataDirectory: string;
begin
  Result := edtDataDir.Text;
  if cbxVolumeLabel.Checked then
    Result := SDIR_DATA_VOLUME_LABEL;
end;

function TfrmSortFile.GetOutputFileName: TFileName;
begin
  Result := edtOutputFileName.Text;
end;

procedure TfrmSortFile.SetDataDirectory(AValue: string);
begin
  edtDataDir.Text := AValue;
end;

procedure TfrmSortFile.SetOutputFileName(AValue: TFileName);
begin
  edtOutputFileName.Text := AValue;
end;

end.

