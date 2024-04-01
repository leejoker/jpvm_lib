unit JpvmUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, IdHTTP, IdSSL, IdSSLOpenSSL,
  IdSSLOpenSSLHeaders;

function GetArchType(): string;
function GetSystemType(): string;
function GetVersionUrl(filepath, distro, verison: string): string;
procedure Downloadfile(url, dest: string);

implementation

function GetArchType: string;
begin
  {$IFDEF CPUX86_64}
  Result:='amd64';
  {$ENDIF}
  {$IFDEF CPUARM}
  Result:='aarch64';
  {$ENDIF}
end;

function GetSystemType: string;
begin
  {$IFDEF MSWINDOWS}
  Result := 'windows';
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFDEF DARWIN}
    Result := 'macos';
    {$ELSE}
    Result := 'linux';
    {$ENDIF}
  {$ENDIF}
end;

function GetVersionUrl(filepath, distro, verison: string): string;
var
  versionJsonStr, distroVersion: string;
  jData: TJSONObject;
begin
  if FileExists(filepath) then
  begin
    if FileSize(filepath) <> 0 then
    begin
      versionJsonStr := ReadFileToString(filepath);
      jData := GetJSON(versionJsonStr);

      try
        Result := TJSONObject(jData).FindPath(distro + '.' + version +
          '.' + GetSystemType + '.' + GetArchType);
      except
        WriteLn('read version file failed');
      end;
    end;
  end;
end;

procedure Downloadfile(url, dest: string);
var
  HTTP: TIdHTTP;
  FileStream: TFileStream;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  IdOpenSSLSetLibPath('E:\projects\pascal\jpvm');
  HTTP := TIdHTTP.Create(nil);
  HTTP.HandleRedirects := True;
  HTTP.Request.BasicAuthentication := True;
  HTTP.Request.Accept :=
    'text/html, image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*';
  HTTP.Request.UserAgent := 'Mozilla/4.0';
  FileStream := TFileStream.Create(dest, fmCreate);
  try
    SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    SSLHandler.SSLOptions.Method := sslvTLSv1_2;
    HTTP.IOHandler := SSLHandler;
    try
      HTTP.Get(url, FileStream);
    except
      on E: Exception do WriteLn(E.Message);
      else
        WriteLn('Download file failed');
    end;
  finally
    FileStream.Free;
    HTTP.Free;
  end;
end;

end.
