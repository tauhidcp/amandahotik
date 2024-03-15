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
    function getUserData: String;
    function getProfile: String;
    function getServer: String;
    procedure Get; override;
    procedure Post; override;
  end;

  var
    con  : TKoneksi;

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

function THotspotuserController.getUserData: String;
var
  user    : TRosApiResult;
  i       : integer;
  userhot : string;
begin
   user := con.getResult('/ip/hotspot/user/print');
   for i := 0 to (user.RowsCount-1) do begin
       userhot += '{'+
                    '"id"     : "'+user.ValueByName['.id']+'",'+
                    '"group"   : "'+user.ValueByName['profile']+'",'+
                    '"name"   : "'+user.ValueByName['name']+'",'+
                    '"pass"   : "'+user.ValueByName['password']+'",'+
                    '"uptime"   : "'+user.ValueByName['limit-uptime']+'",'+
                    '"comment" : "'+user.ValueByName['comment']+'",'+
                    '"status" : "'+user.ValueByName['disabled']+'"'+
                    '},';
       user.Next;
    end;

   Result:='{ "hotspotuser" : [ '+Copy (userhot,0,(length(userhot)-1))+' ] }';

end;

function THotspotuserController.getProfile: String;
var
  group    : TRosApiResult;
  i       : integer;
  userprofile : string;
begin
   group := con.getResult('/ip/hotspot/profile/print');
   for i := 0 to (group.RowsCount-1) do begin
       userprofile += '{'+
                    '"id"     : "'+group.ValueByName['.id']+'",'+
                    '"name"   : "'+group.ValueByName['name']+'"'+
                    '},';
       group.Next;
    end;

   Result:='{ "userprofile" : [ '+Copy (userprofile,0,(length(userprofile)-1))+' ] }';

end;

function THotspotuserController.getServer: String;
var
  server  : TRosApiResult;
  i       : integer;
  dataserver : string;
begin
   server := con.getResult('/ip/hotspot/print');
   for i := 0 to (server.RowsCount-1) do begin
       dataserver += '{'+
                    '"id"     : "'+server.ValueByName['.id']+'",'+
                    '"name"   : "'+server.ValueByName['name']+'"'+
                    '},';
       server.Next;
    end;

   Result:='{ "serverdata" : [ '+Copy (dataserver,0,(length(dataserver)-1))+' ] }';

end;

// Init First
procedure THotspotuserController.BeforeRequestHandler(Sender: TObject; 
  ARequest: TRequest);
begin
   con.setKoneksi(_SESSION['host'], _SESSION['user'], _SESSION['pass'], _SESSION['port']);
end;

// GET Method Handler
procedure THotspotuserController.Get;
var
UData,PData,SData : TJSONData;
datauser,dataprofile,dataserver : TJSONArray;
begin
  if not (_SESSION['userlogin'] = '') then
  begin
    UData := GetJSON(getUserData).GetData('hotspotuser');
    PData := GetJSON(getProfile).GetData('userprofile');
    SData := GetJSON(getServer).GetData('serverdata');
    datauser := UData as TJSONArray;
    dataprofile := PData as TJSONArray;
    dataserver := SData as TJSONArray;
    Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
    ThemeUtil.Assign('$userlogin', _SESSION['userlogin']);
    ThemeUtil.Assign('$tahun', FormatDateTime('yyyy',Now));
    ThemeUtil.AssignVar['$uhotspot']:=@datauser;
    ThemeUtil.AssignVar['$profile']:=@dataprofile;
    ThemeUtil.AssignVar['$server']:=@dataserver;
    ThemeUtil.Assign('$halaman', ThemeUtil.RenderFromContent(nil, '', 'themes/sbadmin/templates/pages/hotspotuser.html'));
    ThemeUtil.Layout := 'admin';
    Response.Content := ThemeUtil.Render();
  end else Redirect(BaseURL);
end;

// POST Method Handler
procedure THotspotuserController.Post;
begin

  if not (_SESSION['userlogin'] = '') then
  begin
    Response.ContentType := 'application/json';

    if (_POST['aksi']='hapus') then
       begin
          if con.Exec('/ip/hotspot/user/remove .id='+_POST['idh'])='Done!' then
                echo('{ "code" : 1, "response" : { "msg" : "OK"} }')
               else
                echo('{ "code" : 0, "response" : { "msg" : "ERROR"} }');
        end;

    if (_POST['aksi']='simpan') then
       begin
          if con.Exec('/ip/hotspot/user/add name='+_POST['user']+' profile='+_POST['group']+' password='+_POST['pass']+' limit-uptime='+_POST['limit']+' comment='+_POST['comment']+' server='+_POST['server']+' disabled=no')='Done!' then
         echo('{ "code" : 1, "response" : { "msg" : "OK"} }')
               else
                echo('{ "code" : 0, "response" : { "msg" : "ERROR"} }');
        end;

    if (_POST['aksi']='update') then
       begin
          if con.Exec('/ip/hotspot/user/set name='+_POST['user']+' profile='+_POST['group']+' password='+_POST['pass']+' limit-uptime='+_POST['limit']+' comment='+_POST['comment']+' server='+_POST['server']+' .id='+_POST['id'])='Done!' then
         echo('{ "code" : 1, "response" : { "msg" : "OK"} }')
               else
                echo('{ "code" : 0, "response" : { "msg" : "ERROR"} }');
        end;

    die;

  end else Redirect(BaseURL);
end;

function THotspotuserController.Tag_MainContent_Handler(const TagName: string; 
  Params: TStringList): string;
begin

  // your code here
  Result:=ThemeUtil.RenderFromContent(@TagController, '','themes/sbadmin/templates/admin.html');

end;


end.

