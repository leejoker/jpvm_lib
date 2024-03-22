
unit Core;

{$mode ObjFPC}{$H+}

interface

type
  JpvmConfig = class
  private
    jdkPath: string;
    cachePath: string;
    jdkCachePath: string;
    versionPath: string;
    currentVersionPath: string;
    logPath: string;
  public
    constructor Create(jpvmPath: string);
    //function CreateDirectory(dirPath: string): boolean;
    function GetJdkPath: string;
    function GetCachePath: string;
    function GetJdkCachePath: string;
    function GetVersionPath: string;
    function GetCurrentVersionPath: string;
    function GetLogPath: string;

    procedure SetJdkPath(_jdkPath: string);
    procedure SetCachePath(_cachePath: string);
    procedure SetJdkCachePath(_jdkCachePath: string);
    procedure SetVersionPath(_versionPath: string);
    procedure SetCurrentVersionPath(_currentVersionPath: string);
    procedure SetLogPath(_logPath: string);
  published
    property PJdkPath: string read GetJdkPath write SetJdkPath;
    property PCachePath: string read GetCachePath write SetCachePath;
    property PJdkCachePath: string read GetJdkCachePath write SetJdkCachePath;
    property PVersionPath: string read GetVersionPath write SetVersionPath;
    property PCurrentVersionPath: string read GetCurrentVersionPath
      write SetCurrentVersionPath;
    property PLogPath: string read GetLogPath write SetLogPath;
  end;

function GetJpvmHome: string;

implementation

uses
  SysUtils;

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

constructor JpvmConfig.Create(jpvmPath: string);
begin
  PJdkPath := jpvmPath + DirectorySeparator + 'jdks';
  PCachePath := jpvmPath + DirectorySeparator + 'cache';
  PJdkCachePath := PCachePath + DirectorySeparator + 'jdks';
  PVersionPath := PJdkPath + DirectorySeparator + 'versions.json';
  PCurrentVersionPath := jpvmPath + DirectorySeparator + '.jdk_version';
  PLogPath := jpvmPath + DirectorySeparator + 'jpvm.log';
end;

function JpvmConfig.GetJdkPath: string;
begin
  Result := jdkPath;
end;

function JpvmConfig.GetCachePath: string;
begin
  Result := cachePath;
end;

function JpvmConfig.GetJdkCachePath: string;
begin
  Result := jdkCachePath;
end;

function JpvmConfig.GetVersionPath: string;
begin
  Result := versionPath;
end;

function JpvmConfig.GetCurrentVersionPath: string;
begin
  Result := currentVersionPath;
end;

function JpvmConfig.GetLogPath: string;
begin
  Result := logPath;
end;

procedure JpvmConfig.SetJdkPath(_jdkPath: string);
begin
  jdkPath := _jdkPath;
end;

procedure JpvmConfig.SetCachePath(_cachePath: string);
begin
  cachePath := _cachePath;
end;

procedure JpvmConfig.SetJdkCachePath(_jdkCachePath: string);
begin
  jdkCachePath := _jdkCachePath;
end;

procedure JpvmConfig.SetVersionPath(_versionPath: string);
begin
  versionPath := _versionPath;
end;

procedure JpvmConfig.SetCurrentVersionPath(_currentVersionPath: string);
begin
  currentVersionPath := _currentVersionPath;
end;

procedure JpvmConfig.SetLogPath(_logPath: string);
begin
  logPath := _logPath;
end;

end.
