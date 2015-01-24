{     
  gditools.py GUI are licensed under the GNU General Public License (version 3), 
  a copy of which is provided in the licences folder: GNU_GPL_v3.txt.
  
  SiZiOUS 2015 / www.sizious.com
}
unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls;

function GetNodePath(TreeNode: TTreeNode): string;
procedure LoadConfig;
procedure SaveConfig;
function GetApplicationPath: TFileName;

implementation

uses
  Forms, IniFiles, Main;

var
  sAppPath: TFileName;

{ Thanks to L.Saenz
  http://www.swissdelphicenter.ch/torry/showcode.php?id=859 }
function GetNodePath(TreeNode: TTreeNode; var Path: string): string; overload;
begin
  Result := '';
  if Assigned(TreeNode) then
  begin
    Path := TreeNode.Text + '/' + Path;
    if TreeNode.Level = 0 then
      Result := Path
    else
      Result := GetNodePath(TreeNode.Parent, Path);
  end;
end;

function GetNodePath(TreeNode: TTreeNode): string;
var
  Path: string;

begin
  Path := '';
  Result := StringReplace(GetNodePath(TreeNode, Path), '//', '/', []);
end;

// Get the application configuration file name.
function GetConfigFile: TFileName;
begin
  Result := GetApplicationPath +
    ChangeFileExt(ExtractFileName(Application.ExeName), '.conf');
end;

// Load the previous saved configuration.
procedure LoadConfig;
var
  IniFile: TIniFile;

begin
  IniFile := TIniFile.Create(GetConfigFile);
  try
    GDReader.PythonExecutable := IniFile.ReadString('General', 'PythonExecutable', 'python');
  finally
    IniFile.Free;
  end;
end;

// Save the current configuration.
procedure SaveConfig;
var
  IniFile: TIniFile;

begin
  IniFile := TIniFile.Create(GetConfigFile);
  try
    IniFile.WriteString('General', 'PythonExecutable', GDReader.PythonExecutable);
  finally
    IniFile.Free;
  end;
end;

// Get the application location
function GetApplicationPath: TFileName;
var
  Path: TFileName;
{$IFDEF Darwin}
  i: Integer;
{$ENDIF}

begin
  if (sAppPath = '') then
  begin
    Path := Application.Location;
{$IFDEF Darwin}
    i := Pos('.app', Path);
    if i > 0 then
    begin
      i := LastDelimiter('/', Copy(Path, 1, i));
      Path := Copy(Path, 1, i);
    end;
{$ENDIF}
    sAppPath := IncludeTrailingPathDelimiter(Path);
  end;
  Result := sAppPath;
end;

initialization
  sAppPath := '';

end.

