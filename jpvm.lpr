program jpvm;

{$mode objfpc}{$H+}

uses
  Classes,
  Core;

var
  configStr: string;
begin
  configStr := Current;
  WriteLn(configStr);
end.
