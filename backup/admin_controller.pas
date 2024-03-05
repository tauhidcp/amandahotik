unit admin_controller;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, html_lib, fpcgi, fpjson, json_lib, HTTPDefs, 
    fastplaz_handler, database_lib, dateutils, string_helpers, 
    datetime_helpers, array_helpers, json_helpers, RouterOSAPI;

type
  TAdminController = class(TMyCustomController)
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

constructor TAdminController.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TAdminController.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TAdminController.BeforeRequestHandler(Sender: TObject; 
  ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TAdminController.Get;
begin
  if not (_SESSION['userlogin'] = '') then
    begin

      Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
      ThemeUtil.Assign('$userlogin', _SESSION['userlogin']);
      ThemeUtil.Assign('$tahun', FormatDateTime('Y',Now));
      ThemeUtil.Assign('$halaman', ThemeUtil.RenderFromContent(nil, '', 'themes/sbadmin/templates/pages/home.html'));
      ThemeUtil.Layout := 'admin';
      Response.Content := ThemeUtil.Render();

      if (_GET['logout'] = 'yes') then
      begin
        _SESSION['userlogin'] := '';
        _SESSION['host'] := '';
        _SESSION['username'] := '';
        _SESSION['password'] := '';
        _SESSION['port'] := '';
        SessionController.EndSession;
        Redirect(BaseURL);
      end;

    end else Redirect(BaseURL);

end;

// POST Method Handler
procedure TAdminController.Post;
begin
  Response.Content := 'This is POST Method';
end;

function TAdminController.Tag_MainContent_Handler(const TagName: string; 
  Params: TStringList): string;
begin

  // your code here
  Result:=ThemeUtil.RenderFromContent(@TagController, '','themes/sbadmin/templates/admin.html');

end;


end.

