unit CoreTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Core,
  fpjsonrtti;

type

  CoreTestCase = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
  end;

implementation

procedure CoreTestCase.TestHookUp;
var
  config: TJpvmConfig;
  jpvmHome: string;
  jdkVersion: TJdkVersionInfo;
  jsonStreamer: TJSONStreamer;
  jsonStr: string;
begin
  config := TJpvmConfig.Create('D:\home\.jpvm');
  writeln(config.jdkPath);
  writeln(config.cachePath);
  writeln(config.jdkCachePath);
  writeln(config.versionPath);
  writeln(config.currentVersionPath);
  writeln(config.logPath);

  jpvmHome := GetJpvmHome;
  writeln(jpvmHome);

  CheckJpvmHome();

  jdkVersion := TJdkVersionInfo.Create;
  jdkVersion.distro := 'zulu';
  jdkVersion.version := '11';

  jsonStreamer := TJSONStreamer.Create(nil);
  try
    jsonStr := jsonStreamer.ObjectToJSONString(jdkVersion);
    writeln(jsonStr);
  finally
    jsonStreamer.Free;
  end;
end;

initialization

  RegisterTest(CoreTestCase);
end.
