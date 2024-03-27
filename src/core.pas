
unit Core;

{$mode ObjFPC}{$H+}{$M+}

interface

uses Classes;

type
  TJdkVersionInfo = class(TPersistent)
  private
    fdistro: string;
    fversion: string;
  published
    property distro: string read fdistro write fdistro;
    property version: string read fversion write fversion;
  end;

  TJpvmConfig = class
  private
    fjdkPath: string;
    fcachePath: string;
    fjdkCachePath: string;
    fversionPath: string;
    fcurrentVersionPath: string;
    flogPath: string;
  public
    constructor Create(jpvmPath: string);
  published
    property jdkPath: string read fjdkPath write fjdkPath;
    property cachePath: string read fcachePath write fcachePath;
    property jdkCachePath: string read fjdkCachePath write fjdkCachePath;
    property versionPath: string read fversionPath write fversionPath;
    property currentVersionPath: string read fcurrentVersionPath
      write fcurrentVersionPath;
    property logPath: string read flogPath write flogPath;
  end;

function GetJpvmHome: string;
function Current: string;
function Clean(): boolean;
function Remove(distro, version: string): boolean;
//function Install(distro, version: string): boolean;
//function Use(distro, version: string): boolean;
function DistroList(): string;
//function VersionList(distro: string): string;
procedure CheckJpvmHome();

implementation

uses
  SysUtils, FileUtil, fpjson, jsonparser;

const
  VERSION_URL = 'https://gitee.com/monkeyNaive/jpvm/raw/master/versions.json';

constructor TJpvmConfig.Create(jpvmPath: string);
begin
  fjdkPath := jpvmPath + DirectorySeparator + 'jdks';
  fcachePath := jpvmPath + DirectorySeparator + 'cache';
  fjdkCachePath := cachePath + DirectorySeparator + 'jdks';
  fversionPath := jdkPath + DirectorySeparator + 'versions.json';
  fcurrentVersionPath := jpvmPath + DirectorySeparator + '.jdk_version';
  flogPath := jpvmPath + DirectorySeparator + 'jpvm.log';
end;

//------------------------- static function---------------------------------

function GetJpvmHome: string;
var
  EnvVal: string;
begin
  EnvVal := GetEnvironmentVariable('JPVM_HOME');
  if EnvVal <> '' then
    Result := EnvVal
  else
    {$IFDEF MSWINDOWS}
    Result := GetEnvironmentVariable('USERPROFILE') + DirectorySeparator + '.jpvm';
  {$ENDIF}
  {$IFDEF UNIX}
    Result:= GetEnvironmentVariable('HOME') + DirectorySeparator + '.jpvm';
  {$ENDIF}
end;

function Current: string;
var
  jpvmHome: string;
  config: TJpvmConfig;
begin
  jpvmHome := GetJpvmHome();
  CheckJpvmHome();
  config := TJpvmConfig.Create(jpvmHome);

  if FileExists(config.currentVersionPath) then
  begin
    Result := ReadFileToString(config.currentVersionPath);
  end
  else
    WriteLn('version file not exists');
end;

function Clean(): boolean;
var
  config: TJpvmConfig;
begin
  config := TJpvmConfig.Create(GetJpvmHome());
  if DirectoryExists(config.cachePath) then
    Result := DeleteDirectory(config.cachePath, True)
  else
  begin
    WriteLn('directory is not exists');
    Result := True;
  end;
end;

function Remove(distro, version: string): boolean;
var
  config: TJpvmConfig;
  distroPath: string;
begin
  config := TJpvmConfig.Create(GetJpvmHome());
  distroPath := config.jdkPath + DirectorySeparator + Trim(distro) +
    DirectorySeparator + Trim(version);
  if DirectoryExists(distroPath) then
    Result := DeleteDirectory(distroPath, False)
  else
  begin
    WriteLn('directory is not exists');
    Result := True;
  end;
end;

function DistroList(): string;
var
  config: TJpvmConfig;
  versionJsonStr, distroName: string;
  jData: TJSONData;
  i: integer;
  jArray: TJSONArray;
begin
  config := TJpvmConfig.Create(GetJpvmHome());
  if FileExists(config.versionPath) then
  begin
    versionJsonStr := ReadFileToString(config.versionPath);
    jData := GetJSON(versionJsonStr);
    jArray := TJSONArray.Create;

    for i := 0 to jData.Count - 1 do
    begin
      distroName := TJSONObject(jData).Names[i];
      jArray.Add(distroName);
    end;
    jData.Free;
    Result := jArray.FormatJSON;
  end
  else
    Result := jArray.FormatJSON;
end;

procedure CheckJpvmHome();
begin
  ForceDirectories(GetJpvmHome);
end;

end.
