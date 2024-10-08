library jpvm;

{$mode objfpc}{$H+}

uses
  Core;

  function CurrentDistro(): pchar; cdecl;
  begin
    Result := PChar(Utf8ToAnsi(Current()));
  end;


  function CleanCache(): boolean; cdecl;
  begin
    Result := Clean();
  end;

  function RemoveDistro(distro, version: pchar): boolean; cdecl;
  begin
    Result := Remove(AnsiToUtf8(distro), AnsiToUtf8(version));
  end;

  function Distros(): pchar; cdecl;
  begin
    Result := PChar(Utf8ToAnsi(DistroList()));
  end;

  function Versions(distro: pchar): pchar; cdecl;
  begin
    Result := PChar(Utf8ToAnsi(VersionList(AnsiToUtf8(distro))));
  end;

  function InstallVersion(distro, version: pchar): boolean; cdecl;
  begin
    Result := Install(AnsiToUtf8(distro), AnsiToUtf8(version));
  end;

exports
  CurrentDistro,
  CleanCache,
  RemoveDistro,
  Distros,
  Versions,
  InstallVersion;
end.
