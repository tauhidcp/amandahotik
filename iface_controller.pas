unit iface_controller;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, html_lib, fpcgi, fpjson, json_lib, HTTPDefs, 
    fastplaz_handler, database_lib, dateutils, string_helpers, 
    datetime_helpers, array_helpers, json_helpers, RouterOSAPI, amandahotik_controller;

type

  { TIfaceController }

  TIfaceController = class(TMyCustomController)
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

implementation

uses theme_controller, common;

constructor TIfaceController.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TIfaceController.Destroy;
begin
  inherited Destroy;
end;

function TIfaceController.getAllData: String;
var
  Res         : TRosApiResult;
  i           : integer;
  iface, koma : string;
  ROS : TRosApiClient;
begin
   ROS := TRosApiClient.Create;
   ROS.Connect(_SESSION['host'], _SESSION['username'], _SESSION['password'], _SESSION['port']);
   Res := ROS.Query(['/interface/print'], True);
   for i := 0 to Res.RowsCount-1 do begin
     if (i=Res.RowsCount+2) then koma :='' else koma := ',';
     iface += '{'+
                    '"id"     : "'+Res.ValueByName['.id']+'",'+
                    '"nama"   : "'+Res.ValueByName['name']+'",'+
                    '"tipe"   : "'+Res.ValueByName['type']+'",'+
                    '"status" : "'+Res.ValueByName['disabled']+'"'+
               '}'+koma;
      Res.Next;

    end;

   Result:='{"interface": [ '+iface+' ] }';

end;

// Init First
procedure TIfaceController.BeforeRequestHandler(Sender: TObject; 
  ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TIfaceController.Get;
var
jData : TJSONData;
dataiface : TJSONArray;
begin
  if not (_SESSION['userlogin'] = '') then
  begin
    jData := GetJSON(getAllData).GetData('interface');
    dataiface := jData as TJSONArray;
    Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
    ThemeUtil.AssignVar['$inface']:=@dataiface;
    ThemeUtil.Assign('$halaman', ThemeUtil.RenderFromContent(nil, '', 'themes/sbadmin/templates/pages/iface.html'));
    ThemeUtil.Layout := 'admin';
    Response.Content := ThemeUtil.Render();
  end else Redirect(BaseURL);
end;

// POST Method Handler
procedure TIfaceController.Post;
begin
  Response.Content := 'This is POST Method';
end;

function TIfaceController.Tag_MainContent_Handler(const TagName: string; 
  Params: TStringList): string;
begin

  // your code here
  Result:=ThemeUtil.RenderFromContent(@TagController, '','themes/sbadmin/templates/admin.html');

end;

end.

