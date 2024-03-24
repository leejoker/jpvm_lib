
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
procedure CheckJpvmHome();

implementation

uses
  SysUtils, FileUtil;

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

procedure CheckJpvmHome();
begin
  ForceDirectories(GetJpvmHome);
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

end.
