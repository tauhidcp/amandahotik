unit Koneksi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RouterOSAPI;

type

    { TKoneksi }

    TKoneksi = class
    public
      procedure setKoneksi(host, user, pass, port : String);
      function getLogin:Boolean;
      function getResult(cmd:String):TRosApiResult;
      function Exec(Command : String) : String;
    end;

var
  ROS : TRosApiClient;
  Res : TRosApiResult;
  hostx, userx, passx, portx : String;

implementation

{ TKoneksi }

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter       := Delimiter;
   ListOfStrings.StrictDelimiter := True;
   ListOfStrings.DelimitedText   := Str;
end;

procedure TKoneksi.setKoneksi(host, user, pass, port: String);
begin
  ROS := TRosApiClient.Create;
  hostx := host;
  userx := user;
  passx := pass;
  portx := port;
  ROS.Connect(hostx, userx, passx, portx);
end;

function TKoneksi.getLogin: Boolean;
begin
  Result:=ROS.Connect(hostx, userx, passx, portx);
end;

function TKoneksi.getResult(cmd: String): TRosApiResult;
begin
  Result:=ROS.Query([cmd], True);
end;

function TKoneksi.Exec(Command: String): String;
var
  Pecah : TStringList;
  Perintah : array of String;
  i,j : integer;
begin
   try
     Pecah := TStringList.Create;
     Split(' ', Command, Pecah);
       if (Pecah.Count >= 1) then
       begin
          i := 1;
          SetLength(Perintah, Pecah.Count);
          Perintah[0] := Trim(Pecah[0]);
         for j := 1 to (Pecah.Count-1) do begin
          Perintah[j] := '='+Trim(Pecah[j]);
          end;
       end;
       if High(Perintah) >= 1 then ROS.Execute(Perintah) else Result := ROS.LastError;
       if ROS.LastError <> '' then Result := ROS.LastError;
       Pecah.Free;
       SetLength(Perintah, 0);
       Result := 'Done!';
   except
   Result := ROS.LastError;
   end;
end;

end.

