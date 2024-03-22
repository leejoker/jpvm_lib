program jpvm;

{$mode objfpc}{$H+}

uses
  Classes,
  Core { you can add units after this };

var
  config: JpvmConfig;
  jpvmHome: string;
begin
  config := JpvmConfig.Create('D:\home\.jpvm');
  writeln(config.PJdkPath);
  writeln(config.PCachePath);
  writeln(config.PJdkCachePath);
  writeln(config.PVersionPath);
  writeln(config.PCurrentVersionPath);
  writeln(config.PLogPath);

  jpvmHome := GetJpvmHome;
  writeln(jpvmHome);
end.
