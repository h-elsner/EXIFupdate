unit geoideval1;                                       {GUI for web service GeoidEval}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, lclintf, Iphttpbroker, opensslsockets, clipbrd, Menus;

type

  { TfrmGeoidEval }

  TfrmGeoidEval = class(TForm)
    btnObtain: TBitBtn;
    btnCopy: TBitBtn;
    btnClose: TBitBtn;
    iproHTMLine: TIpHttpDataProvider;
    leLat: TLabeledEdit;
    leLon: TLabeledEdit;
    lblURL: TLabel;
    memoOut: TMemo;
    mnFile: TMenuItem;
    mnClipboard: TMenuItem;
    PopupMenu1: TPopupMenu;
    FileDialog: TSaveDialog;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnObtainClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblURLClick(Sender: TObject);
    procedure GetHTML(const url: string; var list: TStringList);
    procedure lblURLMouseEnter(Sender: TObject);
    procedure lblURLMouseLeave(Sender: TObject);
    procedure leLatDblClick(Sender: TObject);
    procedure mnClipboardClick(Sender: TObject);
    procedure mnFileClick(Sender: TObject);
  end;

var
  frmGeoidEval: TfrmGeoidEval;
  mode: integer;
  output, op: string;                                  {output: Result, op: Transfer to Main form at Copy}

const
  urlGeoid='https://geographiclib.sourceforge.io/cgi-bin/GeoidEval';
  gMapURL='https://www.google.de/maps/';
  EGMs: array[0..2] of string =('EGM2008', 'EGM96', 'EGM84');
  altfrm='0.00';                                       {Default altitude}
  ziff=['0'..'9'];                                     {Valid digits}
  gleich=' = ';
  gzoom='16';                                          {Zoom value for GoogleMaps}

{Published functions}
  function URLGMap(lati, long: string): string;        {URL für Koordinate in Google Maps}
  function GeoidCgiURL(const pos: string): string;     {pos: lat+lon}
  function FilterValue(s: string; p: integer=1): string; {Copy the first float string part}
  function ExtractHeight(s: string): string;           {Extract delta heigth from geoid}
  function FindLineHTTP(const substr: string; list: TStringList): string;

implementation

{$R *.lfm}

{$I exifupd_en.inc}                                    {English GUI}
{.$I exifupd_bg.inc}                                   {Bulgarian GUI}

procedure TfrmGeoidEval.FormShow(Sender: TObject);     {Initialization on restart}
begin
  lblURL.Font.Color:=clNavy;
  memoOut.Text:=capResult;
  output:='';
  op:='';
end;

procedure TfrmGeoidEval.FormCreate(Sender: TObject);   {Initialization at first creation}
begin
  Caption:=capCorrAlt;
  lblURL.Caption:=urlGeoid;
  btnClose.Caption:=capCancel;
  btnCopy.Caption:=capCopy;
  btnObtain.Caption:=capObtain;
  btnObtain.Hint:=hntObtain;
  mnFile.Caption:=capResFile;
  mnClipboard.Caption:=capClip;
end;

function URLGMap(lati, long: string): string;          {URL für Koordinate in Google Maps}
begin
  result:=gmapURL+'?q='+lati+','+long+'&z='+
                        gzoom+'&t=h&om=0';             {&t=k: Sat, &t=h: hybrid}
end;

function GeoidCgiURL(const pos: string): string;       {pos: lat+lon}
begin
  result:=urlGeoid+'?input='+pos+'&option=Submit';
end;

function FilterValue(s: string; p: integer=1): string; {Copy the first float string part}
var i: integer;
    dot: boolean;
begin
  result:='';
  dot:=true;
  if length(s)>p then begin
    for i:=p to length(s) do begin
      if (s[i] in ziff) or (s[i]='-') then             {Collect digits}
        result:=result+s[i];
      if dot and                                       {Take only first dot}
         ((s[i]='.') or (s[i]=',')) then begin
        result:=result+DefaultFormatSettings.DecimalSeparator;
        dot:=false;
      end;
    end;
  end else
    result:=altfrm;                                    {If nothing found default altitude}
end;

{Output GeoidEval (https://geographiclib.sourceforge.io/cgi-bin/GeoidEval):
  <a href="http://earth-info.nga.mil/GandG/wgs84/gravitymod/egm2008">EGM2008</a> = <font color="blue">42.5261</font>
  <a href="http://earth-info.nga.mil/GandG/wgs84/gravitymod/egm96/egm96.html">EGM96</a>   = <font color="blue">41.4735</font>
  <a href="http://earth-info.nga.mil/GandG/wgs84gravitymod/wgs84_180/wgs84_180.html">EGM84</a>   = <font color="blue">41.8857</font></pre></font>
}
function ExtractHeight(s: string): string;             {Extract delta heigth from geoid}
begin
  result:=altfrm;                                      {Default: 0.00}
  if length(s)>100 then                                {Correction value is somewhere at the end of the line}
    result:=FilterValue(s, 100);
end;

function FindLineHTTP(const substr: string; list: TStringList): string;
var i: integer;
begin
  result:='';                                          {Default: Empty string}
  if list.Count>0 then begin
    for i:=0 to list.count-1 do begin                  {Find keyword in list}
      if pos('>'+substr+'<', list[i])>0 then begin
        result:=list[i];                               {Result is string where the keywors is in}
        break;                                         {Finish when first hit was found}
      end;
    end;
  end;
end;

procedure TfrmGeoidEval.GetHTML(const url: string; var list: TStringList);
var strm: TStream;                                     {Find a line in HTML file from Internet}
    ct: string;
begin
  list.Clear;
  ct:='';
  if length(url)>8 then begin
    try
      if iproHTMLine.CheckURL(url, ct) then            {Test the URL if connection is possible}
        strm:=iproHTMLine.DoGetStream(url);            {Download file to stream}
    except
      on e: Exception do begin
        list.Text:=e.Message;                          {Error message as result}
        exit;
      end;
    end;
    if strm.Size>100 then                              {Check if download was successful}
      list.LoadFromStream(strm);
  end;
end;

procedure TfrmGeoidEval.lblURLMouseEnter(Sender: TObject); {Animate link}
begin
  lblURL.Font.Style:=lblURL.Font.Style+[fsBold];
end;

procedure TfrmGeoidEval.lblURLMouseLeave(Sender: TObject);
begin
  lblURL.Font.Style:=lblURL.Font.Style-[fsBold];
end;

procedure TfrmGeoidEval.leLatDblClick(Sender: TObject); {Show in GoogleMaps}
begin
  if (leLat.Text<>'') and (leLon.Text<>'') then
    OpenURL(URLGMap(leLat.Text, leLon.Text));
end;

procedure TfrmGeoidEval.mnClipboardClick(Sender: TObject);
begin                                                  {Menu: Copy result to clipboard}
  Clipboard.AsText:=memoOut.Text;
end;

procedure TfrmGeoidEval.mnFileClick(Sender: TObject);  {Menu: Save result as file}
begin
  FileDialog.Title:=capResFile;
  FileDialog.FileName:=capGravity;
  if FileDialog.Execute then
    memoOut.Lines.SaveToFile(FileDialog.FileName);
end;

procedure TfrmGeoidEval.lblURLClick(Sender: TObject);
begin
  if OpenURL(urlGeoid)then
    lblURL.Font.Color:=clPurple;
end;

procedure TfrmGeoidEval.btnCloseClick(Sender: TObject);
begin
  output:='';
  Close;
end;

procedure TfrmGeoidEval.btnCopyClick(Sender: TObject);
begin
  if op<>'' then begin
    ClipBoard.AsText:=memoOut.Text;
    output:=op;
  end;
  Close;
end;

{https://geographiclib.sourceforge.io/cgi-bin/GeoidEval?input=43.44+23.43&option=Submit}
procedure TfrmGeoidEval.btnObtainClick(Sender: TObject); {Call web service GeoidEval}
var w: double;
    lne: string;
    dsepdef: char;
    i: integer;
    inlist: TStringList;
begin
  dsepdef:=DefaultFormatSettings.DecimalSeparator;     {Preparations}
  DefaultFormatSettings.DecimalSeparator:='.';
  memoOut.Lines.Clear;
  if (leLat.Text<>'') and (leLon.Text<>'') and         {if coordinates available}
      TryStrToFloat(leLat.Text, w) and TryStrToFloat(leLon.Text, w) then begin
    inlist:=TstringList.Create;
    Screen.Cursor:=crHourGlass;
    GetHTML(GeoidCgiURL(leLat.Text+'+'+leLon.Text), inlist);  {get result page}

    if inlist.Count>0 then begin                       {If something was downloaded}
      memoOut.Lines.Add(leLat.EditLabel.Caption+gleich+leLat.Text);
      memoOut.Lines.Add(leLon.EditLabel.Caption+gleich+leLon.Text);
      memoOut.Lines.Add('');
      try
        for i:=Low(EGMs) to High(EGMs) do begin
          lne:=FindLineHTTP(EGMs[i], inlist);
          lne:=ExtractHeight(lne);
          memoOut.Lines.Add(Format('%0:-7s', [EGMs[i]])+gleich+lne+'m');
          if i=mode then
            op:=lne;
        end;
        btnCopy.Enabled:=(op<>'');
      finally
        inlist.Free;
        screen.Cursor:=crDefault;
      end;
    end else
      memoOut.Lines.Add(rsNoResponse);                 {No download was available}

  end else begin
    memoOut.Lines.Add(rsInvalidCrd);                   {No or invalid float for coordinates}
//    OpenURL(urlGeoid);
  end;
  DefaultFormatSettings.DecimalSeparator:=dsepdef;     {Get back original decimal separator}
end;

end.

