unit JpvmUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fpjson, jsonparser, IdHTTP, IdSSL, IdSSLOpenSSL,
  IdSSLOpenSSLHeaders, Zipper, LazFileUtils;

function MyDeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
function GetArchType(): string;
function GetSystemType(): string;
function GetVersionUrl(filepath, distro, version: string): string;
function SliceStringArray(const AArray: TStringArray;
  AStartIndex, AEndIndex: integer): TStringArray;
function Decompress(src: string): string;
function GetEnv(key: string): string;
function PathJoin(paths: array of string): string;
procedure Downloadfile(url, dest: string);

implementation

function MyDeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
const
  //Don't follow symlinks on *nix, just delete them
  DeleteMask = faAnyFile
  {$ifdef unix}
 or faSymLink{%H-}
  {$endif unix}
  ;
var
  FileInfo: TSearchRec;
  CurSrcDir: string;
  CurFilename: string;
  FileAttr: integer;
begin
  Result := False;
  CurSrcDir := CleanAndExpandDirectory(DirectoryName);
  if FindFirstUTF8(CurSrcDir + GetAllFilesMask, DeleteMask, FileInfo) = 0 then
  begin
    repeat
      // check if special file
      if (FileInfo.Name = '.') or (FileInfo.Name = '..') or (FileInfo.Name = '') then
        continue;
      CurFilename := CurSrcDir + FileInfo.Name;
      if ((FileInfo.Attr and faDirectory) > 0)
      {$ifdef unix}
 and ((FileInfo.Attr and faSymLink{%H-})=0)
      {$endif unix}
      then
      begin
        if not MyDeleteDirectory(CurFilename, False) then exit;
      end
      else
      begin
        FileAttr := FileInfo.Attr;
        FileAttr := FileAttr and not faReadOnly;
        FileSetAttr(CurFilename, FileAttr);
        if not DeleteFileUTF8(CurFilename) then exit;
      end;
    until FindNextUTF8(FileInfo) <> 0;
  end;
  FindCloseUTF8(FileInfo);
  if (not OnlyChildren) and (not RemoveDirUTF8(CurSrcDir)) then exit;
  Result := True;
end;

function SliceStringArray(const AArray: TStringArray;
  AStartIndex, AEndIndex: integer): TStringArray;
var
  I, NewArraySize: integer;
  a: TStringArray;
begin
  NewArraySize := AEndIndex - AStartIndex + 1;
  SetLength(a, NewArraySize);
  for I := 0 to NewArraySize - 1 do
    a[I] := AArray[AStartIndex + I];
  Result := a;
end;

function Decompress(src: string): string;
var
  UnZipper: TUnZipper;
  dirName: string;
begin
  UnZipper := TUnZipper.Create;
  try
    UnZipper.FileName := src;
    UnZipper.OutputPath := src.Substring(0, src.LastIndexOf(DirectorySeparator));
    UnZipper.Examine;

    dirName := src.Substring(0, src.LastIndexOf(DirectorySeparator)) +
      DirectorySeparator + UnZipper.Entries.Entries[0].ArchiveFileName;

    if DirectoryExists(dirName.Substring(0, Length(dirName) - 1)) then
      MyDeleteDirectory(dirName.Substring(0, Length(dirName) - 1), False);

    UnZipper.UnZipAllFiles;
    Result := dirName;
  finally
    UnZipper.Free;
  end;
end;

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

function GetVersionUrl(filepath, distro, version: string): string;
var
  versionJsonStr: string;
  jData: TJSONData;
begin
  if FileExists(filepath) then
  begin
    if FileSize(filepath) <> 0 then
    begin
      versionJsonStr := ReadFileToString(filepath);
      jData := GetJSON(versionJsonStr);
      try
        Result := TJSONObject(jData).FindPath(distro + '.' + version +
          '.' + GetSystemType + '.' + GetArchType).AsString;
      except
        WriteLn('read version file failed');
      end;
    end;
  end;
end;

function PathJoin(paths: array of string): string;
var
  targetPath: string;
  idx: integer;
begin
  targetPath := '';
  for idx := 0 to Length(paths) do
  begin
    targetPath := targetPath + paths[idx] + DirectorySeparator;
  end;
  Result := targetPath.Substring(0, Length(targetPath) - 1);
end;

function GetEnv(key: string): string;
var
  EnvVal: string;
begin
  EnvVal := GetEnvironmentVariable('key');
  if EnvVal <> '' then
    Result := EnvVal
  else
    Result := '';
end;

procedure Downloadfile(url, dest: string);
var
  HTTP: TIdHTTP;
  FileStream: TFileStream;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
  parentDir, sslPath: string;
begin
  sslPath := GetEnv('SSL_PATH');
  if sslPath <> '' then IdOpenSSLSetLibPath(sslPath);

  parentDir := dest.Substring(0, dest.LastIndexOf(DirectorySeparator));
  if not DirectoryExists(parentDir) then ForceDirectories(parentDir);

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
