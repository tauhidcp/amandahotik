unit amandahotik_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, html_lib, fpcgi, fpjson, json_lib, HTTPDefs, 
    fastplaz_handler, database_lib, dateutils, string_helpers, 
    datetime_helpers, array_helpers, json_helpers, Koneksi;

type

  { TAmandahotikController }

  TAmandahotikController = class(TMyCustomController)
  private
    function Tag_MainContent_Handler(const TagName: string; Params: TStringList
      ): string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
    procedure Get; override;
    procedure Post; override;

  end;


implementation

uses theme_controller, common;

constructor TAmandahotikController.CreateNew(AOwner: TComponent; 
  CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TAmandahotikController.Destroy;
begin
  inherited Destroy;
end;



// Init First
procedure TAmandahotikController.BeforeRequestHandler(Sender: TObject; 
  ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TAmandahotikController.Get;
begin
  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  ThemeUtil.Assign('$error','');
  Response.Content := ThemeUtil.RenderFromContent(nil, '','themes/sbadmin/templates/login.html');
end;

// POST Method Handler
procedure TAmandahotikController.Post;
var
  login       : Boolean;
  con         : TKoneksi;
begin
   con.setKoneksi(_POST['host'], _POST['username'], _POST['password'], _POST['port']);
   login:=con.getLogin;
   if login then
      begin
        _SESSION['userlogin'] := _POST['username'];
        _SESSION['host'] := _POST['host'];
        _SESSION['user'] := _POST['username'];
        _SESSION['pass'] := _POST['password'];
        _SESSION['port'] := _POST['port'];
        Redirect('./admin'); end else
     begin
       ThemeUtil.Assign('$error', '<div style="font-size: 13px;" class="alert alert-danger alert-dismissible">'+
                                  '<button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;</button>'+
                                  'Login Failed!'+
                                  '</div>');
       Response.Content := ThemeUtil.RenderFromContent(nil, '','themes/sbadmin/templates/login.html');
     end;
end;

function TAmandahotikController.Tag_MainContent_Handler(const TagName: string; 
  Params: TStringList): string;
begin

end;


end.

