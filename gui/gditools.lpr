program gditools;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, utils, extract, sortfile, Settings, Engine, Version, about;

{$R *.res}

begin
  Application.Title:='gditools.py GUI';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmExtractAll, frmExtractAll);
  Application.CreateForm(TfrmSortFile, frmSortFile);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.

