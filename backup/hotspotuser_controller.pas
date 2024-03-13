unit hotspotuser_controller;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, html_lib, fpcgi, fpjson, json_lib, HTTPDefs, 
    fastplaz_handler, database_lib, dateutils, string_helpers, 
    datetime_helpers, array_helpers, json_helpers, Koneksi, RouterOSAPI;

type

  { THotspotuserController }

  THotspotuserController = class(TMyCustomController)
  private
    function Tag_MainContent_Handler(const TagName: string; Params: TStringList
      ): string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
    function getAllData: String;
    procedure Get; override;
    procedure Post; override;
  end;

  var
    con         : TKoneksi;


implementation

uses theme_controller, common;

constructor THotspotuserController.CreateNew(AOwner: TComponent; 
  CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor THotspotuserController.Destroy;
begin
  inherited Destroy;
end;

function THotspotuserController.getAllData: String;
var
  user    : TRosApiResult;
  i       : integer;
  userhot : string;
begin
   con.setKoneksi(_SESSION['host'], _SESSION['user'], _SESSION['pass'], _SESSION['port']);
   user := con.getResult('/ip/hotspot/user/print');
   for i := 0 to (user.RowsCount-1) do begin
       userhot += '{'+
                    '"id"     : "'+user.ValueByName['.id']+'",'+
                    '"group"   : "'+user.ValueByName['profile']+'",'+
                    '"name"   : "'+user.ValueByName['name']+'",'+
                    '"pass"   : "'+user.ValueByName['password']+'",'+
                    '"uptime"   : "'+user.ValueByName['limit-uptime']+'",'+
                    '"status" : "'+user.ValueByName['disabled']+'"'+
                    '},';
       user.Next;
    end;

   Result:='{ "hotspotuser" : [ '+Copy (userhot,0,(length(userhot)-1))+' ] }';

end;

// Init First
procedure THotspotuserController.BeforeRequestHandler(Sender: TObject; 
  ARequest: TRequest);
begin
end;

// GET Method Handler
procedure THotspotuserController.Get;
var
jData : TJSONData;
datauser : TJSONArray;
begin
  if not (_SESSION['userlogin'] = '') then
  begin
    jData := GetJSON(getAllData).GetData('hotspotuser');
    datauser := jData as TJSONArray;
    Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
    ThemeUtil.Assign('$userlogin', _SESSION['userlogin']);
    ThemeUtil.Assign('$tahun', FormatDateTime('yyyy',Now));
    ThemeUtil.AssignVar['$uhotspot']:=@datauser;
    ThemeUtil.Assign('$halaman', ThemeUtil.RenderFromContent(nil, '', 'themes/sbadmin/templates/pages/hotspotuser.html'));
    ThemeUtil.Layout := 'admin';
    Response.Content := ThemeUtil.Render();

    if not (_GET['deleteuser'].IsEmpty) then
      begin
        if con.Exec('/ip/hotspot/user/remove .id='+_GET['deleteuser'])='Done!' then
           Redirect(BaseURL+'admin/hotspotuser');
      end;

  end else Redirect(BaseURL);
end;

// POST Method Handler
procedure THotspotuserController.Post;
begin
  Response.Content := 'This is POST Method';
end;

function THotspotuserController.Tag_MainContent_Handler(const TagName: string; 
  Params: TStringList): string;
begin

  // your code here
  Result:=ThemeUtil.RenderFromContent(@TagController, '','themes/sbadmin/templates/admin.html');

end;


end.

