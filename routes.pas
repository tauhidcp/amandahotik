unit routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses info_controller, amandahotik_controller;

initialization
  Route[ 'info'] := TInfoModule;
  Route[ '/'] := TAmandahotikController; // Main Controller

end.

