unit routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses amandahotik_controller, admin_controller, hotspotuser_controller;

initialization
  Route[ '/admin/hotspotuser'] := THotspotuserController;
  Route[ '/admin'] := TAdminController;
  Route[ '/'] := TAmandahotikController; // Main Controller

end.

