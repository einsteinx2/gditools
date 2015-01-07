unit Settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type
  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    btnPythonFileName: TButton;
    edtPythonFileName: TEdit;
    gbxPython: TGroupBox;
    lblPythonHint: TLabel;
    opdPythonFileName: TOpenDialog;
    pnlPython: TPanel;
    pnlButtons: TPanel;
    procedure btnPythonFileNameClick(Sender: TObject);
  private
    function GetPythonExecutable: TFileName;
    procedure SetPythonExecutable(AValue: TFileName);
    { private declarations }
  public
    { public declarations }
    property PythonExecutable: TFileName
      read GetPythonExecutable write SetPythonExecutable;
  end;

var
  frmSettings: TfrmSettings;

implementation

{$R *.lfm}

{ TfrmSettings }

procedure TfrmSettings.btnPythonFileNameClick(Sender: TObject);
begin
  with opdPythonFileName do
  begin
    FileName := PythonExecutable;
    if Execute then
      PythonExecutable := FileName;
  end;
end;

function TfrmSettings.GetPythonExecutable: TFileName;
begin
  Result := edtPythonFileName.Text;
end;

procedure TfrmSettings.SetPythonExecutable(AValue: TFileName);
begin
  edtPythonFileName.Text := AValue;
end;

end.

