
unit Core;

{$mode ObjFPC}{$H+}{$M+}

interface

uses Classes, SysUtils, FileUtil, fpjson, jsonparser, JpvmUtils, process;

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
function Install(distro, version: string): boolean;
//function Use(distro, version: string): boolean;
function DistroList(): string;
function VersionList(distro: string): string;
procedure CheckJpvmHome();

implementation

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
    Result := MyDeleteDirectory(config.cachePath, True)
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
  CheckJpvmHome();
  config := TJpvmConfig.Create(GetJpvmHome());
  Downloadfile(VERSION_URL, config.versionPath);
  jArray := TJSONArray.Create;
  if FileExists(config.versionPath) then
  begin
    if FileSize(config.versionPath) <> 0 then
    begin
      versionJsonStr := ReadFileToString(config.versionPath);
      jData := GetJSON(versionJsonStr);

      for i := 0 to jData.Count - 1 do
      begin
        distroName := TJSONObject(jData).Names[i];
        jArray.Add(distroName);
      end;
      jData.Free;
    end;
  end;
  Result := jArray.FormatJSON;
end;

function VersionList(distro: string): string;
var
  config: TJpvmConfig;
  versionJsonStr, distroVersion: string;
  jData: TJSONData;
  i: integer;
  jArray: TJSONArray;
  distroObj: TJSONObject;
begin
  CheckJpvmHome();
  config := TJpvmConfig.Create(GetJpvmHome());
  Downloadfile(VERSION_URL, config.versionPath);
  jArray := TJSONArray.Create;
  if FileExists(config.versionPath) then
  begin
    if FileSize(config.versionPath) <> 0 then
    begin
      versionJsonStr := ReadFileToString(config.versionPath);
      jData := GetJSON(versionJsonStr);
      try
        distroObj := TJSONObject(jData).Get(distro, TJSONObject.Create());

        for i := 0 to distroObj.Count - 1 do
        begin
          distroVersion := distroObj.Names[i];
          jArray.Add(distroVersion);
        end;
        jData.Free;
      except
        WriteLn('read version file failed');
      end;
    end;
  end;
  Result := jArray.FormatJSON;
end;

function Install(distro, version: string): boolean;
var
  config: TJpvmConfig;
  url, zipFile, zipFileDir, zipResult, targetDir, output: string;
  a: TStringArray;
begin
  CheckJpvmHome();
  config := TJpvmConfig.Create(GetJpvmHome());
  Downloadfile(VERSION_URL, config.versionPath);
  if FileExists(config.versionPath) then
  begin
    if FileSize(config.versionPath) <> 0 then
    begin
      try
        url := GetVersionUrl(config.versionPath, distro, version);
        a := url.Split('/');

        zipFileDir := config.jdkCachePath + DirectorySeparator +
          distro + DirectorySeparator + version;

        ForceDirectories(zipFileDir);

        zipFile := zipFileDir + DirectorySeparator + a[Length(a) - 1];

        if not FileExists(zipFile) then  Downloadfile(url, zipFile);

        zipResult := Decompress(zipFile);

        if DirectoryExists(config.jdkPath + DirectorySeparator +
          distro + DirectorySeparator + version) then
          MyDeleteDirectory(config.jdkPath + DirectorySeparator +
            distro + DirectorySeparator + version, False);

        targetDir := config.jdkPath + DirectorySeparator + distro +
          DirectorySeparator + version + DirectorySeparator +
          GetSystemType() + DirectorySeparator + GetArchType();

        ForceDirectories(targetDir);

        {$IFDEF MSWINDOWS}
        if not RenameFile(zipResult, targetDir + DirectorySeparator +
          distro + '-' + version) then
          Writeln('Failed to move directory: ' + targetDir +
            DirectorySeparator + distro + '-' + version + ' from: ' + zipResult);
        {$ELSE}
        if RunCommand('mv', ['-rf', zipResult, targetDir + DirectorySeparator +
          distro + '-' + version], output) then
          Writeln('Failed to move directory');
        {$ENDIF}
      except
        on E: Exception do
          WriteLn(E.Message);
        else
          WriteLn('download version failed');
      end;
      Result := True;
    end;
  end
  else
    Result := False;
end;

procedure CheckJpvmHome();
begin
  ForceDirectories(GetJpvmHome);
end;

end.
