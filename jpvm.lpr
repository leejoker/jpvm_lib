library jpvm;

{$mode objfpc}{$H+}

uses
  Classes,
  Core;

  function CurrentDistro(): PChar; cdecl;
  begin
    Result := PChar(Utf8ToAnsi(Current()));
  end;


  function CleanCache(): boolean; cdecl;
  begin
    Result := Clean();
  end;

  function RemoveDistro(distro, version: PChar): boolean; cdecl;
  begin
    Result := Remove(AnsiToUtf8(distro), AnsiToUtf8(version));
  end;

  function Distros(): PChar; cdecl;
  begin
    Result := PChar(Utf8ToAnsi(DistroList()));
  end;

  function Versions(distro: PChar): PChar; cdecl;
  begin
    Result := PChar(Utf8ToAnsi(VersionList(AnsiToUtf8(distro))));
  end;

exports
  CurrentDistro,
  CleanCache,
  RemoveDistro,
  Distros,
  Versions;
end.
