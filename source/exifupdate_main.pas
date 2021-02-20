(*Update EXIF data of JPG pictures from Yuneec Typhoon H (CGO3+)
  Discussion:
  https://yuneecpilots.com/conversations/to-use-a-part-your-software-as-a-platform-for-image-handling.7828

 1. Correlation by timestamp. The whole row from all sources with a simple
    crosscheck to GPS position, to be near (possible to set how near is OK);
 2. All from remote;
 3. Model without the icon.

 This all in UserComment as JSON. Especially for me, all the rest fields in
 the EXIF, except the GPS position for secondary crosscheck are unnecessary.

 Please, add in the EXIF, as a string, the path to the log file directory.
 For example "C:\Users\VL\Documents\FlightLog - A" (flpath)

 Compiled with Lazarus 2.0.10/FPC 3.2.0  + fpEXIF
                   https://www.lazarus-ide.org/

 Needed component: https://sourceforge.net/p/lazarus-ccr/svn/HEAD/tree/components/fpexif/

 EXIF tags:        https://exiftool.org/TagNames/EXIF.html
 JSON schema:      http://json-schema.org/

 Created: 2020-11-21 to 2021-02-19

 =======================================================================================

 Ellipsoid vs. geoid:
 There are a lot of reference models used in the word to define null for the altitude.
 GPS uses WGS84 ellipsiod while geodesy uses geoid reference models
 (in Germany NHN is used with reference model DHHN92; other countries use other models).
 In my area WSG84 is ~47m above geoid. ST16 shows 561m and the geoid related
 altitude (the NHN altitude) is 513m.
 https://support.virtual-surveyor.com/en/support/solutions/articles/1000261351
 https://support.virtual-surveyor.com/en/support/solutions/articles/1000261349

 Correction value: https://geographiclib.sourceforge.io/cgi-bin/GeoidEval
 *)

unit EXIFupdate_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, ComCtrls, XMLPropStorage, Grids, fileutil, dateutils, math,
  lclintf, Menus, fpeMetaData, fpeExifData, exifstuff, clipbrd, MaskEdit,
  geoideval1, Iphttpbroker, opensslsockets;

type                                                   {Data record for FlightLog data}
  TEXdata = record
    zeit:  TDateTime;                                  {Time stamp from Telemetry}
    telem: string;                                     {Telemetry data line}
    regps: string;                                     {RemoteGPS data line}
    remot: string;                                     {Remote data line}
    lat:   double;                                     {Coordinates from Telemetry}
    lon:   double;
    alt:   double;
  end;

  TXMPdat = record                                     {XMP data record from gimbal}
    alt:   string[32];
    altr:  string[32];
    yaw:   string[32];
    pitch: string[32];
    roll:  string[32];
  end;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnClose: TBitBtn;
    btnScan: TBitBtn;
    cbBackup: TCheckBox;
    cbKeepComment: TCheckBox;
    cbLog: TCheckBox;
    cbxLogs: TComboBox;
    cbxPics: TComboBox;
    cbUpdateAlt: TCheckBox;
    cbJSON: TCheckBox;
    cbEXIFwrite: TCheckBox;
    cbAddText: TCheckBox;
    cbAutoGeoid: TCheckBox;
    edController: TEdit;
    gridPictures: TStringGrid;
    gbEXIF: TGroupBox;
    gbFiles: TGroupBox;
    gbCorrAlt: TGroupBox;
    ImageList: TImageList;
    iproHTTPin: TIpHttpDataProvider;
    lblMeter: TLabel;
    lblGitHub: TLabel;
    lblManual: TLabel;
    lbeGeoid: TLabeledEdit;
    lblDelta: TLabel;
    lblLogs: TLabel;
    lblPics: TLabel;
    lblVariance: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    mnGeoEval: TMenuItem;
    mnSave: TMenuItem;
    mnSettings: TMenuItem;
    mnTools: TMenuItem;
    mnAbout: TMenuItem;
    N5: TMenuItem;
    mnGitHub: TMenuItem;
    mnManual: TMenuItem;
    mnHelp: TMenuItem;
    N4: TMenuItem;
    mnScan1: TMenuItem;
    mnClose: TMenuItem;
    N2: TMenuItem;
    mnLogs: TMenuItem;
    mnFile: TMenuItem;
    mnPics: TMenuItem;
    N3: TMenuItem;
    N1: TMenuItem;
    mnScan: TMenuItem;
    mnSaveCSV: TMenuItem;
    mnPaste: TMenuItem;
    mnSetInfo: TMenuItem;
    mnLoad: TMenuItem;
    mnClear: TMenuItem;
    OpenDialog: TOpenDialog;
    pcTabs: TPageControl;
    Panel1: TPanel;
    btnLogs: TSpeedButton;
    btnPics: TSpeedButton;
    DirDialog: TSelectDirectoryDialog;
    MenuMemo: TPopupMenu;
    MenuGrid: TPopupMenu;
    rgGravity: TRadioGroup;
    rgController: TRadioGroup;
    SaveDialog: TSaveDialog;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    tabMain: TTabSheet;
    tabSettings: TTabSheet;
    tbDelta: TTrackBar;
    Timer1: TTimer;
    XMLPropStorage1: TXMLPropStorage;
    procedure btnCloseClick(Sender: TObject);
    procedure btnLogsClick(Sender: TObject);
    procedure btnPicsClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure cbAddTextChange(Sender: TObject);
    procedure cbUpdateAltChange(Sender: TObject);
    procedure cbxLogsChange(Sender: TObject);
    procedure cbxLogsDblClick(Sender: TObject);
    procedure cbxPicsChange(Sender: TObject);
    procedure cbxPicsDblClick(Sender: TObject);
    procedure edControllerDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblGitHubClick(Sender: TObject);
    procedure lblGitHubMouseEnter(Sender: TObject);
    procedure lblGitHubMouseLeave(Sender: TObject);
    procedure lblManualClick(Sender: TObject);
    procedure lblManualMouseEnter(Sender: TObject);
    procedure lblManualMouseLeave(Sender: TObject);
    procedure mnAboutClick(Sender: TObject);
    procedure mnClearClick(Sender: TObject);
    procedure mnCloseClick(Sender: TObject);
    procedure mnGeoEvalClick(Sender: TObject);
    procedure mnLoadClick(Sender: TObject);
    procedure mnPasteClick(Sender: TObject);
    procedure mnSaveCSVClick(Sender: TObject);
    procedure mnScan1Click(Sender: TObject);
    procedure mnScanClick(Sender: TObject);
    procedure mnSetInfoClick(Sender: TObject);
    procedure mnSettingsClick(Sender: TObject);
    procedure tbDeltaChange(Sender: TObject);
    procedure GetInetList(const url: string; var list: TStringList);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure ScanPics;                                {Scan picture directory}
    procedure ShowSliderPos;                           {Show position of the slider}
    procedure ScanEnable;                              {Check if scanning is possible}
   public

  end;

var
  Form1: TForm1;

const
  appversion='V1.0';
  builddt='2021-02-19';

  makefilter='yuneec';                                 {Proper works only for Yuneec Typhoon H}
  altfrm='0.00';
  coordfrm='0.000000';
  defalt=-0.001;                                       {-1mm als indicator for default}
  maxalt=20000;                                        {Unrealistic high altitude}
  timeformat='yyyy-mm-dd hh:nn:ss';                    {Date/time format}
  fltime='yyyymmdd hh:nn:ss';                          {Time format FlightLog}
  fltimez=fltime+':zzz';
  numitems=10;
  addtxtfn='addtext.txt';
  csvfile='camerashoots.csv';
  logtxtfn='log.csv';
  updfolder='pics_upd';
  tab1=' ';
  tab6='      ';
  usID='_';
  sep=',';                                             {CSV data separator}
  cext='.csv';
  fmode='f_mode';
  idleID='16';                                         {f-mode 16, Initialization}
  txtpart=12;                                          {Length partially displayed text}
  le=LineEnding;                                       {Better readable for tests}
//  le='';                                               {No line breaks}
  hotky='&';
  githublink='https://github.com/h-elsner/EXIFupdate';
  manual='Manual.pdf';

{JSON IDs}
  strID='"';
  startID='{';
  endID='}';
  dpID=':';
  arbID='[';
  areID=']';
  rsDT='Date/time';                                    {Add to headers}

{XMP tags CGO3+}
  xmpCamYaw='CameraYaw';
  xmpCamPitch='CameraPitch';
  xmpCamRoll='CameraRoll';
  xmpGPSalt='GPSAltitude';

{C23 / E90}
  xmpAltRel='AboveHomeAltitude';
  xmpGimbalYaw='GimbalYaw';
  xmpGimbalPitch='GimbalPitch';
  xmpGimbalRoll='GimbalRoll';
//  xmpDroneYaw='DroneYaw';
//  xmpDronePitch='DronePitch';
//  xmpDronelRoll='DroneRoll';
//  xmpGPSacch='GPSXYAccuracy';
//  xmpGPSaccv='GPSZAccuracy';

{File names}
  flog='FlightLog';
  trem='Remote';
  tgps='RemoteGPS';
  tele='Telemetry';
  moda='analog';
  modv='curves';
  modm='manifest';
  mods='switches';

implementation

{$R *.lfm}

{$I exifupd_en.inc}                                    {English GUI}
{.$I exifupd_bg.inc}                                   {Bulgarian GUI}

{ TForm1 }

function GetExePath: string;                           {Path to exe file with / at the end}
begin
  result:=IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
end;

procedure TForm1.FormCreate(Sender: TObject);          {Initialize GUI captions}
var i: integer;
begin
  Caption:=capForm;
  btnScan.Caption:=hotky+capScan;
  btnScan.Hint:=hntScan;
  btnScan.Enabled:=false;
  mnScan.Enabled:=false;
  mnScan1.Enabled:=false;
  btnClose.Caption:=hotky+capClose;
  btnClose.Hint:=capClose;
  btnLogs.Hint:=hntLogs;
  btnPics.Hint:=hntPics;
  cbBackup.Caption:=capBackup;
  cbBackup.Hint:=hntBackup;
  cbKeepComment.Caption:=capKeepComment;
  cbKeepComment.Hint:=hntKeepComment;
  cbLog.Caption:=capLog;
  cbLog.Hint:=hntLog;
  lblVariance.Caption:=capVariance;
  lblVariance.Hint:=hntDelta;
  lblDelta.Hint:=hntDelta;
  ShowSliderPos;
  lblLogs.Caption:=capLogs;
  cbxLogs.Hint:=hntLogs;
  lblPics.Caption:=capPics;
  cbxPics.Hint:=hntPics;
  lbeGeoid.EditLabel.Caption:=capGeoid;
  lbeGeoid.Hint:=hntGeoid;
  pcTabs.ActivePage:=tabMain;
  tabSettings.Caption:=capSettings;
  tabMain.Caption:=capMain;
  gbEXIF.Caption:=capEXIF;
  gbFiles.Caption:=capFiles;
  cbUpdateAlt.Caption:=capUpdateAlt;
  cbUpdateAlt.Hint:=hntUpdateAlt;
  cbJSON.Caption:=capJSON;
  cbJSON.Hint:=hntJSON;
  cbEXIFwrite.Caption:=capEXIFwrite;
  cbEXIFwrite.Hint:=hntEXIFwrite;
  cbAddText.Caption:=capSetInfo;
  cbAddtext.Hint:=hntSetInfo;
  gbCorrAlt.Caption:=capCorrAlt;
  cbAutoGeoid.Caption:=capAutoGeoid;
  cbAutoGeoid.Hint:=hntAutoGeoid;
  rgGravity.Caption:=capGravity;
  rgGravity.Hint:=hntGravity;
  lblGitHub.Caption:=capGitHub;
  lblGitHub.Hint:=githublink;
  lblManual.Caption:=capManual;
  lblManual.Hint:=GetExePath+manual;

  Memo1.Lines.Clear;
  Memo1.Hint:=hntMemo;
  mnSetInfo.Caption:=capSetInfo;
  mnLoad.Caption:=capLoad;
  mnPaste.Caption:=capPaste;
  mnClear.Caption:=capClear;
  mnSaveCSV.Caption:=capSaveCSV;
  mnSaveCSV.Enabled:=false;
  mnScan.Caption:=capScan;
  mnScan1.Caption:=capScan;
  mnFile.Caption:=hotky+capFile;
  mnTools.Caption:=hotky+capTools;
  mnHelp.Caption:=hotky+capHelp;
  mnScan.Caption:=capScan;
  mnManual.Caption:=capManual;
  mnGitHub.Caption:=capGitHub;
  mnAbout.Caption:=capAbout;
  mnClose.Caption:=capClose;
  mnSave.Caption:=capSaveTab;
  mnPics.Caption:=capDirDialog;
  mnLogs.Caption:=capOpenDialog;
  mnSettings.Caption:=capSettings;

  rgController.Caption:=capController;
  rgController.Hint:=capController;
  rgController.Items[0]:=rgitem0;
  rgController.Items[1]:=rgitem1;
  rgController.Items[2]:=rgitem2;
  rgController.Items[3]:=rgitem3;
  rgController.Items[4]:=rgitem4;
  edController.TextHint:=capController;
  edController.Hint:=rgitem4+tab1+capController;
  for i:=Low(EGMs) to High(EGMs) do                   {Fill Gravity models}
    rgGravity.Items.Add(EGMs[i]);

  OpenDialog.Title:=capOpenDialog;
  OpenDialog.Filter:=rsExtFilter;
  OpenDialog.FilterIndex:=1;
  DirDialog.Title:=capDirDialog;

  gridPictures.Rows[0].Delimiter:=sep;
  gridPictures.Rows[0].StrictDelimiter:=true;
  gridPictures.Rows[0].DelimitedText:=rsHeader;
  gridPictures.AutoSizeColumns;
  {$IFDEF LINUX}                                       {Ubuntu Linux}
    btnLogs.Height:=30;                                {Needs larger Speed buttons}
    btnLogs.Width:= 30;
    btnPics.Height:=30;
    btnPics.Width:= 30;
  {$ENDIF}
end;

procedure TForm1.ScanEnable;                           {Check if scanning is possible}
begin
  btnScan.Enabled:=FileExists(cbxLogs.Text) and
                   DirectoryExists(cbxPics.Text);
  mnScan.Enabled:=btnScan.Enabled;
  mnScan1.Enabled:=mnScan.Enabled;
end;

procedure TForm1.FormShow(Sender: TObject);            {At start after loading session properties}
begin
  ScanEnable;
  Splitter1.Update;
  mnSetInfo.Checked:=cbAddText.Checked;
  gbCorrAlt.Enabled:=cbUpdateAlt.Checked;
end;

procedure TForm1.lblGitHubClick(Sender: TObject);      {Open GitHub repo}
begin
  if OpenURL(lblGitHub.Hint) then
    lblGitHub.Font.Color:=clPurple;
end;

procedure TForm1.lblGitHubMouseEnter(Sender: TObject); {Animate link}
begin
  lblGitHub.Font.Style:=lblGitHub.Font.Style+[fsBold];
end;

procedure TForm1.lblGitHubMouseLeave(Sender: TObject);
begin
  lblGitHub.Font.Style:=lblGitHub.Font.Style-[fsBold];
end;

procedure TForm1.lblManualClick(Sender: TObject);      {Open user manual}
begin
  if OpenDocument(lblManual.Hint) then
    lblManual.Font.Color:=clPurple;
end;

procedure TForm1.lblManualMouseEnter(Sender: TObject); {Animate link}
begin
  lblManual.Font.Style:=lblManual.Font.Style+[fsBold];
end;

procedure TForm1.lblManualMouseLeave(Sender: TObject);
begin
  lblManual.Font.Style:=lblManual.Font.Style-[fsBold];
end;

procedure TForm1.mnAboutClick(Sender: TObject);        {About box}
begin
  if MessageDlg(ChangeFileExt(ExtractFileName(Application.ExeName), '')+
                tab1+appversion+sLineBreak+
                'Build '+builddt+sLineBreak+sLineBreak+capForm,
                mtInformation,[mbHelp, mbOK],0)=mrNone then
    OpenDocument(lblManual.Hint);
end;

procedure TForm1.mnClearClick(Sender: TObject);        {Clear text}
begin
  Memo1.Lines.Clear;
end;

procedure TForm1.mnCloseClick(Sender: TObject);        {Menu: Close}
begin
  Close;
end;

procedure TForm1.mnGeoEvalClick(Sender: TObject);      {Menu: GUI for GeoidEval}
begin
  GeoidEvalMode:=rgGravity.ItemIndex;
  Timer1.Enabled:=true;
  frmGeoidEval.Show;                                   {Opens tool window}
end;

procedure TForm1.mnLoadClick(Sender: TObject);         {Menu: Text load from File}
begin
  OpenDialog.Title:=capTextDialog;
  OpenDialog.FilterIndex:=2;                           {Switch to *.txt}
  if OpenDialog.Execute then
    Memo1.Lines.LoadFromFile(OpenDialog.FileName);
end;

procedure TForm1.mnPasteClick(Sender: TObject);        {Menu: Paste to Memo}
begin
  Memo1.text:=Clipboard.AsText;
end;

procedure TForm1.mnSaveCSVClick(Sender: TObject);
begin
  SaveDialog.Title:=capSaveTab;
  SaveDialog.InitialDir:=IncludeTrailingPathdelimiter(cbxPics.Text);
  SaveDialog.FileName:=csvfile;                        {Propose file name}
  SaveDialog.Filter:=rsExtFilter;
  SaveDialog.FilterIndex:=1;
  if SaveDialog.Execute then begin
    gridPictures.SaveToCSVFile(SaveDialog.FileName, sep, true, false);
    StatusBar.Panels[3].Text:=rsSavedTo+tab1+SaveDialog.FileName;
  end;
end;

procedure TForm1.mnScan1Click(Sender: TObject);
begin
  ScanPics;
end;

procedure TForm1.mnScanClick(Sender: TObject);         {Menu: Same as Scan button}
begin
  ScanPics;
end;

procedure TForm1.mnSetInfoClick(Sender: TObject);      {Menu: Allow/disallow AdditionalInfo in JSON}
begin
  mnSetInfo.Checked:=not mnSetInfo.Checked;
  cbAddText.Checked:=mnSetInfo.Checked;
end;

procedure TForm1.mnSettingsClick(Sender: TObject);     {Menu: Switch to Settings}
begin
  pcTabs.ActivePage:=tabSettings;
end;

procedure TForm1.ShowSliderPos;                        {Show position of the slider}
begin
  lblDelta.Caption:=IntToStr(tbDelta.Position)+'cm';
  tbDelta.Hint:=hntDelta+tab1+lblDelta.Caption;
  StatusBar.Panels[2].Text:=lblDelta.Caption;
end;

procedure TForm1.tbDeltaChange(Sender: TObject);       {Change position}
begin                                                  {Update hint for slider}
  ShowSliderPos;
  StatusBar.Panels[3].Text:=hntDelta+tab1+lblDelta.Caption;
  StatusBar.Panels[2].Text:=lblDelta.Caption;
end;

procedure Merkliste(ml: TComboBox);
begin                                                  {Fill Drop Down list}
  if (ml.Text<>'') and
     (ml.Items.IndexOf(ml.Text)<0) then                {New item}
    ml.Items.Insert(0, ml.Text);
  if ml.Items.Count>numitems then                      {Limited number items}
    ml.Items.Delete(numitems);
end;

{Distance between two coordinates in m
 Haversine formula, Earth radius: 6,371km depending on latitude
 https://rechneronline.de/erdradius/
 6365.692 optimized for 50° lat and 60m altitude over ellipsoid}
function DeltaCoord(lat1, lon1, lat2, lon2: double): double;
begin
  result:=100;                                         {No match at all, exclude}
  try
    result:=6365692*arccos(sin(lat1*pi/180)*sin(lat2*pi/180)+
            cos(lat1*pi/180)*cos(lat2*pi/180)*cos((lon1-lon2)*pi/180));
    if IsNan(result) then
      result:=100;                                     {'nan' shall not match, dist>=100m}
  except
  end;
end;

function RadToGrad180(const r: double): double;        {Radiant to +/-180°}
begin
  result:=r*180/pi;
end;

function RadToGrad360(const r: double): double;        {Radiant to 0..360°}
begin
  result:=(RadToGrad180(r)+360) mod 360;
end;

{https://www.movable-type.co.uk/scripts/latlong.html
 https://towardsdatascience.com/calculating-the-bearing-between-two-geospatial-coordinates-66203f57e4b4}
function Bearing(lat1, lon1, lat2, lon2: double): double; {From lat/lon1 to lat/lon2 [°]}
var delta, cosl2: double;
begin
  result:=0;
  try
    delta:=lon2-lon1;
    cosl2:=cos(lat2);
    result:=-arctan2(cosl2*sin(delta), cos(lat1)*sin(lat2)-sin(lat1)*cosl2*cos(delta));
    result:=RadToGrad360(result);                      {+/-180° in rad --> 0..360°}
  except                                               {Ignore exceptions, result remains 0}
  end;
end;

function GetNumFlight(s: string): string;              {Filter flight number from file name}
var i: integer;
    gef: boolean;
begin
  result:='';
  gef:=false;
  for i:=length(s) downto 1 do begin
    if s[i] in ziff then begin
      gef:=true;
      result:=s[i]+result;
    end else                                           {Not a digit any more}
      if gef then
        exit;
  end;
end;

function FindDataLine(list: TStringList; tp: TDateTime; var ln: integer): string;
var i: integer;                                        {Find a time stamp correlation}
begin
  result:='';
  ln:=0;
  if list.Count>10 then begin                          {Less data lines makes no sense}
    for i:=2 to list.Count-1 do begin
      if ScanDateTime(fltimez, list[i])>tp then begin
        result:=list[i];
        ln:=i;                                         {Line number of match}
        break;
      end;
    end;
  end;
end;

function GetAltFromST(list: TStringList; tp: integer): double;  {Altitude WGS84}
var splist: TStringList;                               {list: RemoteGPS}
    i, n: integer;
    hw, hx: double;
begin
  splist:=TStringList.Create;
  splist.Delimiter:=sep;
  splist.StrictDelimiter:=True;
  result:=0;
  hw:=0;
  hx:=0;
  n:=0;                                                {Counter how many datasets for avarage}
  try
    for i:=tp to list.Count-1 do begin                 {From take-off on}
      splist.DelimitedText:=list[i];
      if TryStrToFloat(splist[3], hx) then begin
        hw:=hw+hx;
        inc(n);
      end;
      if n>40 then
        break;                                         {Limit number of data for avarage}
    end;
    result:=hw/n-1; {Average value from 41 data sets, minus alt of the RC controller to ground (1m)}
  finally
    splist.Free;
  end;
end;

function ClearXMP: TXMPdat;
begin
  result.pitch:='';                                    {Clear data set first}
  result.roll:='';
  result.yaw:='';
  result.alt:='';
  result.altr:='';
end;

function ClearTXD: TEXdata;                            {Delete Telemetry data for EXIF}
begin
  result.telem:='';
  result.remot:='';
  result.regps:='';
  result.lat:=0;
  result.lon:=0;
  result.alt:=defalt;
end;

{XMP data in CGO3+ picture files:

http://ns.adobe.com/xap/1.0/
<?xpacket begin="?" id="W5M0MpCehiHzreSzNTczkc9d"?>
  <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
    <xmp:GPSAltitude>6</xmp:GPSAltitude>
    <xmp:CameraYaw>-85.580353</xmp:CameraYaw>
    <xmp:CameraPitch>-13.905984</xmp:CameraPitch>
    <xmp:CameraRoll>0.002856</xmp:CameraRoll>
  </rdf:RDF>
<?xpacket end="w"?>

------------------------------------------------------

C23

<x:xmpmeta xmlns:x="adobe:ns:meta/">
 <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
  <rdf:Description rdf:about="YUNEEC Meta Data"
    xmlns:tiff="http://ns.adobe.com/tiff/1.0/"
    xmlns:exif="http://ns.adobe.com/exif/1.0/"
    xmlns:xmp="http://ns.adobe.com/xap/1.0/"
    xmlns:xmpMM="http://ns.adobe.com/xap/1.0/mm/"
    xmlns:dc="http://purl.org/dc/elements/1.1/"
    xmlns:crs="http://ns.adobe.com/camera-raw-settings/1.0/"
    xmlns:drone-yuneec="http://www.yuneec.com/drone-yuneec/1.0/"
   xmp:ModifyDate="2013-12-01"
   xmp:CreateDate="2013-12-01"
   tiff:Make="Yuneec"
   tiff:Model="C23"
   dc:format="image/jpg"
   drone-yuneec:GPSAltitude="201.02"
   drone-yuneec:AboveHomeAltitude="69.84"
   drone-yuneec:DroneYaw="93.940002"
   drone-yuneec:DronePitch="3.480000"
   drone-yuneec:DroneRoll="4.740000"
   drone-yuneec:GimbalYaw="92.680000"
   drone-yuneec:GimbalPitch="-13.74000"
   drone-yuneec:GimbalRoll="0.000000"
   crs:Version="7.0"
   crs:HasSettings="False"
   crs:HasCrop="False"
   crs:AlreadyApplied="False">
  </rdf:Description>
 </rdf:RDF>
</x:xmpmeta>

--------------------------------------------------------------
E90
+   Camera:GPSXYAccuracy="5.000000"
+   Camera:GPSZAccuracy="10.000000"
}

function GetXMPvalue(id, s: string): string;           {Read data between > and < }
var i, n: integer;
    gef: boolean;
begin
  result:='';
  n:=pos('<xmp:'+id, s);                               {It must be a XMP tag}
  if n>0 then begin
    gef:=false;
    for i:=n+length(id)+4 to length(s) do begin
      if s[i]='<' then break;
      if gef then
        result:=result+s[i];
      if s[i]='>' then
        gef:=true;
    end;
  end;
end;

function GetDRYvalue(id, s: string): string;           {Read data between > and < }
var i, n: integer;
    gef: boolean;
begin
  result:='';
  n:=pos('drone-yuneec:'+id, s);                       {It must be a yuneec tag}
  if n>0 then begin
    gef:=false;
    for i:=n+length(id)+12 to length(s) do begin
      if s[i]='<' then break;
      if gef then
        result:=result+s[i];
      if s[i]='>' then
        gef:=true;
    end;
  end;
end;

function CleanGeoid(txt: string; var geo: double): string;
begin
  geo:=0;
  result:=FilterValue(txt, 1);
  if (result='') or (not TryStrToFloat(result, geo)) then
    result:=altfrm;
end;

function GetXMPdata(const fn: string): TXMPdat;        {Find XMP data in JPG file}
var indat: TFileStream;
    i, nb: integer;
    buf: array[0..65535] of byte;                      {Search in the first 64 kByte}
    rawstr: string;
begin
  result:=ClearXMP;
  indat:=TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
    nb:=indat.Read(buf, SizeOf(buf));
    rawstr:='';
    if nb>100 then begin
      for i:=0 to nb do                                {Filter string from Byte stream}
        if (buf[i]>41) and (buf[i]<123) then           {Remove spaces and some unneeded chars}
          rawstr:=rawstr+chr(buf[i]);
      if pos('/rdf:', rawstr)>0 then begin
        result.roll:=GetXMPvalue(xmpCamRoll, rawstr);
        result.pitch:=GetXMPvalue(xmpCamPitch, rawstr);
        result.yaw:=GetXMPvalue(xmpCamYaw, rawstr);
        result.alt:=GetXMPvalue(xmpGPSalt, rawstr);
        if result.Roll='' then                         {For C23 and better}
          result.roll:=GetDRYvalue(xmpGimbalRoll, rawstr);
        if result.pitch='' then
          result.pitch:=GetDRYvalue(xmpGimbalPitch, rawstr);
        if result.yaw='' then
          result.yaw:=GetDRYvalue(xmpGimbalYaw, rawstr);
        if result.alt='' then begin
          result.alt:=GetDRYvalue(xmpGPSalt, rawstr);
          result.altr:=GetDRYvalue(xmpAltRel, rawstr);
        end;
      end;
    end;
  finally
    indat.Free;
  end;
end;

function AusgData(id, dat: string; ln: integer): string; {Linenumber and Data}
begin
  result:=strID+id+strid+dpID+startID+
          '"LineNumber"'+dpID+IntToStr(ln)+sep+
          '"Data"'+dpID+strID+dat+strID+endID;
end;

function AusgZeile(ln: integer): string;               {Format output matching line number}
begin
  result:='#'+IntToStr(ln);
end;

procedure CleanStrings(list: TStrings);                {Removes line endings and empty lines}
var i: integer;
begin
  if list.Count>0 then
    for i:=list.Count-1 downto 0 do
      if trim(list[i])='' then
        list.Delete(i);
end;

function FindFMode(const s: string): integer;          {Index fligth mode column}
var i: integer;
    splist: TStringList;
begin
  result:=17;                                          {default column for H480}
  splist:=TStringList.Create;
  splist.Delimiter:=sep;
  splist.StrictDelimiter:=True;
  try
    splist.Delimitedtext:=s;
    for i:=1 to splist.Count-1 do
      if splist[i]=FMode then begin                    {Find index of column with f_mode}
        result:=i;
        break;
      end;
  finally
    splist.Free;
  end;
end;

function vTypeToStr(const v: integer): string;         {Convert vehicle type}
begin
  result:='';
  case v of
    1: result:='H920';
    2: result:='Q500';
    3: result:='Blade 350QX';
    4: result:='Blade Chroma (380QX)';
    5: result:='Typhoon H';
  end;
end;

function vTypeToRC(const v: integer): string;          {RC type depending on vehicle type}
begin
  result:='';
  case v of
    1: result:='ST24/ST16';
    2, 3, 4: result:='ST10';
    5, 6: result:='ST16';
  end;
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.btnLogsClick(Sender: TObject);        {Select telemetry file}
begin
   OpenDialog.Title:=capOpenDialog;
   OpenDialog.FilterIndex:=1;                          {Switch to *.csv}
  if OpenDialog.Execute then begin
    cbxLogs.Text:=OpenDialog.FileName;
    Merkliste(cbxLogs);
    ScanEnable;
  end;
end;

procedure TForm1.btnPicsClick(Sender: TObject);        {Select picture folder}
begin
  if DirDialog.Execute then begin
    cbxPics.Text:=DirDialog.FileName;
    merkliste(cbxPics);
    ScanEnable;
  end;
end;

procedure TForm1.btnScanClick(Sender: TObject);        {Scan picture directory}
begin
  ScanPics;
end;

procedure TForm1.cbAddTextChange(Sender: TObject);
begin
  mnSetInfo.Checked:=cbAddText.Checked;
end;

procedure TForm1.cbUpdateAltChange(Sender: TObject);
begin
  gbCorrAlt.Enabled:=cbUpdateAlt.Checked;
end;

procedure TForm1.cbxLogsChange(Sender: TObject);
begin
  ScanEnable;
end;

procedure TForm1.GetInetList(const url: string; var list: TStringList);
var strm: TStream;                                     {Find a line in HTML file from Internet}
    ct: string;
begin
  list.Clear;
  ct:='';
  if length(url)>8 then begin
    try
      if iproHTTPin.CheckURL(url, ct) then             {Test the URL if connection is possible}
        strm:=iproHTTPin.DoGetStream(url);             {Download file to stream}
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

procedure TForm1.Timer1Timer(Sender: TObject);         {Abfrage GeoidEval}
begin
  if GeoidEvalValue<>''  then begin                    {Result as string from Tool}
    lbeGeoid.Text:=GeoidEvalValue;
  end;
  if not frmGeoidEval.Visible then
    Timer1.Enabled:=false;
end;

procedure TForm1.ScanPics;                             {Scan picture directory}
var
  dsepdef: char;
  i, p, zhl, zl1, zl2, zl3, drone: integer;
  lat, lon, diffalt, dist, bear, geoalt, vari: double;
  filelist, tlmlist, splitlist, remlist, remgpslist, loglist: TStringList;
  aImgInfo: TImgInfo;
  stdat, picdat, ldat: TEXdata;
  flnum, flpath, mainpath, fn, mname, addtxt, RCrole, cam, startpos: string;
  jsonstr, outstr: string;
  Gimbal: TXMPdat;
  fdat: TDateTime;

  {Get filesystem environment: mainpath, flpath, flnum}
  procedure CheckFileSystem;
  var i, k: integer;
  begin
    splitlist.Delimiter:=PathDelim;
    splitlist.StrictDelimiter:=true;
    flnum:=GetNumFlight(cbxLogs.Text);                 {FlightLog number}
    splitlist.DelimitedText:=cbxLogs.Text;             {Find FlightLog path}
    if splitlist.Count>1 then begin
      for i:=splitlist.Count-1 downto 0 do begin
        if pos(flog, splitlist[i])>0 then begin        {FlightLog found}
          mainpath:='';
          for k:=0 to i-1 do
            mainpath:=mainpath+splitlist[k]+PathDelim;
          Caption:=capForm+tab6+splitlist[k];          {Project name}
          flpath:=mainpath+splitlist[i]+PathDelim;
          break;
        end;
      end;
    end;
  end;

  procedure AdditionalTextFromFile;
  begin
    FindAllFiles(filelist, mainpath, addtxtfn, true);  {Find and load add text file}
    if (filelist.Count>0) and FileExists(filelist[0]) then begin
      memo1.Lines.LoadFromFile(filelist[0]);           {If file available allow add text}
      if Memo1.Lines.Count>0 then begin
        if Splitter1.Top<25 then begin
          Splitter1.Top:=40;
          Splitter1.Update;
        end;
        mnSetInfo.Checked:=true;
      end;
    end;
    filelist.Clear;
  end;

  {If additional text is allowed add additional text}
  procedure AdditionalTextToJSON;
  var k: integer;
  begin
    if mnSetInfo.Checked then begin
      CleanStrings(Memo1.Lines);                       {Remove line Endings and "}
      if Memo1.Lines.Count>0 then begin                {Add text only if availble}
        addtxt:=Memo1.Lines[0];
        for k:=1 to Memo1.Lines.Count-1 do
          addtxt:=addtxt+' '+Memo1.Lines[k];
        addtxt:=StringReplace(addtxt, '"', '''', [rfReplaceAll]);
        jsonstr:=jsonstr+'"AdditionalText"'+dpID+                               {JSON: Additional info from memo}
                 strID+addtxt+strID+sep+le;
        if length(addtxt)>txtpart then
          addtxt:=copy(addtxt, 1, txtpart)+'...';      {Shorten AddText for protocol table}
//        addtxt:=addtxt.Split([' '])[0]+'...';        {Optional only the first word}
      end;
    end;
  end;

  procedure Modeldata(const s: string);                {Read the files from Exported model}
  var i: integer;
      f: string;
  begin
    f:=fn+s+cext;                                      {Model: analog, curves, switches CSV}
    remlist.Clear;
    if FileExists(f) then
      remlist.LoadFromFile(f);
    if remlist.Count>0 then begin
      jsonstr:=jsonstr+sep+le+strID+s+strID+dpID+arbID+
      strID+remlist[0]+strID;
      for i:=1 to remlist.Count-1 do
        jsonstr:=jsonstr+sep+strID+remlist[i]+strID;
      jsonstr:=jsonstr+areID;
    end;
  end;

{Optional: Load exported model data, common for all pictures}
  procedure LoadExpModel;
  var i: integer;
  begin
    remgpslist:=FindAllFiles(mainpath, modm+cext, true);
    if remgpslist.Count>0 then begin                   {Exported Model found}
      fn:=ExtractFilePath(remgpslist[0]);              {Path to model data; filename Manifest}
      StatusBar.Panels[3].Text:=fn;

      jsonstr:=jsonstr+'"Model"'+dpid+startID;                                  {JSON: Start Model}

      remlist.LoadFromFile(remgpslist[0]);             {Model: Manifest}
      if remlist.Count>0 then begin
        splitlist.Delimitedtext:=remlist[0];
        jsonstr:=jsonstr+strID+modm+strID+dpID+startID+                         {JSON: Begin Manifest}
                 strID+splitlist[0]+strID+dpID+
                 strID+splitlist[1]+strID;
        for i:=1 to remlist.Count-1 do begin
          splitlist.DelimitedText:=remlist[i];
          if splitlist[0]='name' then
            mname:=splitlist[1];                       {Modelname}
          jsonstr:=jsonstr+sep+strID+splitlist[0]+strID+dpID+
                   strID+splitlist[1]+strID;
        end;
        jsonstr:=jsonstr+endID;                                                 {JSON: end Manifest}
      end;
      Modeldata(moda);                                 {Model: analog.csv}
      Modeldata(modv);                                 {Model: curves.csv}
      Modeldata(mods);                                 {Model: switches.csv}
      jsonstr:=jsonstr+endID+sep+le;                                            {JSON end Model}
    end;
  end;

{If lat/lon is not available in EXIF then use it from telemetry + altitude}
  procedure TakePosFromTelemetry;
  begin
    if ((picdat.lat=0) and (picdat.lon=0)) and         {No coordinates in picture}
       ((lat<>0) or (lon<>0)) then begin               {Valid coordinates from telemetry}
      picdat.lat:=lat;
      picdat.lon:=lon;
      if cbUpdateAlt.Checked then begin
        WriteCoordinates(aImgInfo, lat, lon, true);
        gridPictures.Cells[3, i+1]:=capLogs;           {Overwritten by Telemetry}
        WriteAltitude(aImgInfo, picdat.alt, true);
        gridPictures.Cells[5, i+1]:=FormatFloat(altfrm, picdat.alt);
      end;
    end;
  end;

{Add XMP package to JSON if XMP was available in EXIF}
  procedure WriteXMPdata;
  begin
    if Gimbal.roll<>'' then begin                                               {JSON: XMPdata}
      outstr:=outstr+sep+le+'"XMPdata"'+dpID+startID+
        strID+xmpGPSalt+strID+dpID+strID+Gimbal.alt+strID+sep;
      if Gimbal.altr<>'' then
        outstr:=outstr+
        strID+xmpAltRel+strID+dpID+strID+Gimbal.altr+strID+sep;
      outstr:=outstr+
        strID+xmpCamYaw+strID+dpID+strID+Gimbal.yaw+strID+sep+
        strID+xmpCamPitch+strID+dpID+strID+Gimbal.pitch+strID+sep+
        strID+xmpCamRoll+strID+dpID+strID+Gimbal.roll+strID+endID;
      gridPictures.Cells[10, i+1]:=Gimbal.alt+'m';     {Indicates XMP data}
    end;
  end;

 {Save EXIF to picture, keep original as backup file if needed (see settings)}
  procedure SaveEXIFdata;
  begin
    if cbEXIFwrite.Checked then begin
      try
        if cbBackup.Checked then begin
          fn:=IncludeTrailingPathDelimiter(cbxPics.Text)+
              updfolder+PathDelim;                     {Name of extra folder}
          if not DirectoryExists(fn) then
            if not CreateDir(fn)then                   {Create extra folder}
              gridPictures.Cells[12, i+1]:=rsFailDir+tab1+updfolder;
          fn:=fn+ExtractFileName(filelist[i]);
          CopyFile(filelist[i], fn, true);
          aImgInfo.SaveToFile(fn);                     {Updated files in extra folder}
          gridPictures.Cells[12, i+1]:=rsYes;
        end else begin                                 {Update EXIF in original JPG file}
          aImgInfo.SaveToFile(filelist[i]);
          gridPictures.Cells[12, i+1]:=rsYes;
        end;
      except
        on e: Exception do
          gridPictures.Cells[12, i+1]:=e.Message;      {Print error messages}
      end;
    end;
  end;

{Create JSON files that contain the same as the UserComment in EXIF would have
 per picture file. For test or other usage.}
  procedure SaveJSONfiles;                             {Create JSON data as file}
  begin
    if cbJSON.Checked then begin
      splitlist.Text:=outstr;
      splitlist.SaveToFile(ChangeFileExt(filelist[i], '.json'));
    end;
  end;

{Create protocol data with the available data as CSV dataset per picture.
   Available data:

     flnum:    Flight number in FlightLog
     mainpath: Project path
     flpath:   FlightLog path
     RCrole:   Controller role
     mname:    Model name
     drone:    Drone type
     stdat:    All take-off data (Time, lat, lon, alt, telemetry, Remote, RemoteGPS)
     geoalt:   GPS altitude at take-off (possibly corrected to geoid)
     Program settings and additional text (memo1) from GUI

   Per picture file
     filelist[i]: File name picture
     fdat:        File date/time
     i:           Number picture file (0..n)
     zhl:         Sequence number updated picture 1..n
     cam:         Camera type           ('' if no EXIF)
     dist:        Distance to previous or take-off
     bear:        Bering to current pic
     diffalt:     Ascent/descent
     vari:        Delta coordinates
     picdat:      All data from picture (Time, lat, lon, alt, telemetry, Remote, RemoteGPS)
     gimbal:      Gimbal data from XMP
     zl1:         Line number Telemetry (0: not found)
     zl2:         Line number Remote
     zl3:         Line number RemoteGPS

   Output format:
     File name  : YUN00037.jpg
     File date  : 2020-11-16 15:04:54
     EXIF date  : 2020-11-16 14:04:53
     Variance   : 30
     Camera type: CGO3+ 3.2.34(A)
     Sequence No: 12
     Latitude   : 43.780863
     Longitude  : 23.743606
     Altitude   : 33
     Ascent     : -6.44
     Distance   : 2.03
     Bearing    : 150.90
  }
  procedure CreateLogData;                             {One dataset log per picture}
  var k: integer;
  begin
    if cbLog.Checked then begin                        {Allowed to create log file}
      fn:=ExtractFileName(filelist[i])+sep+
          FormatDateTime(timeformat, fdat)+sep;
      if cam<>'' then begin                            {File has EXIF}
        cam:=StringReplace(cam, sep, ' ', [rfreplaceAll]);
        fn:=fn+FormatDateTime(timeformat, picdat.zeit)+sep+cam+sep;
        if (gridPictures.Cells[6, i+1]<>'') and
           (gridPictures.Cells[6, i+1]<>rsNo) and
           (zl1>0) then begin                          {Match found}
          fn:=fn+IntToStr(zhl)+sep+
              FormatFloat(altfrm, vari*100)+sep+
              FormatFloat(coordfrm, picdat.lat)+sep+
              FormatFloat(coordfrm, picdat.lon)+sep+
              FormatFloat(altfrm, picdat.alt)+sep+
              FormatFloat(altfrm, diffalt)+sep+
              FormatFloat(altfrm, dist)+sep+
              FormatFloat(altfrm, bear);
        end;
      end;
      p:=0;
      for k:=1 to length(fn) do                        {Fill with empty columns}
        if fn[k]=sep then inc(p);
      if p<11 then
        for k:=p to 10 do
          fn:=fn+sep;
      loglist.Add(fn);
    end;
  end;

{Check if there was already a UserComment in the EXIF data and add to the
 JSON type UserComment at the end if allowed in Settings}
  function KeepOrigUserComment: string;                {Read and preserve original UserComment}
  begin
    result:='';
    if cbKeepComment.Checked then begin
      result:=ReadString(aImgInfo, exUser, '');
      if trim(result)<>'' then
        result:=LineEnding+result;
    end;
  end;

{Write altitude changed from relative to absolute in the EXIF meta data}
  procedure WriteEXIFdata;                             {Write EXIF data}
  begin
    WriteTagAsString(aImgInfo, exUser, outstr+KeepOrigUserComment, true);
    if cbUpdateAlt.Checked and
       (geoalt<>0) then begin
      WriteAltitude(aImgInfo, picdat.alt, true);
      gridPictures.Cells[5, i+1]:=FormatFloat(altfrm, picdat.alt);
    end;
  end;

{Find take-off in telemetry; Take-off means the first change from
 f_mode 16 to another f_mode. }
  procedure FindTakeOff;
  var found16: boolean;
      colidx, i: integer;
  begin
    found16:=false;
    colidx:=FindFMode(tlmlist[0]);
    for i:=3 to tlmlist.count-1 do begin               {Find take off}
      splitlist.DelimitedText:=tlmlist[i];
      if found16 and (splitlist[colidx]<>idleID) then begin   {Start detected}
        stdat.zeit:=ScanDateTime(fltimez, splitlist[0]);  {Start time}
        stdat.telem:=tlmlist[i];
        if not TryStrToFloat(splitlist[4], stdat.alt) or
           (stdat.alt>maxalt) then
          stdat.alt:=defalt;
        if not TryStrToFloat(splitlist[5], stdat.lat) then
          stdat.lat:=0;
        if not TryStrToFloat(splitlist[6], stdat.lon) then
          stdat.lon:=0;
        zl1:=i;
        break;
      end;
      if splitlist[colidx]=idleID then
        found16:=true;
    end;
    drone:=StrToIntDef(splitlist[colidx+2], 5);        {Vehicle type}
  end;

begin
  dsepdef:=DefaultFormatSettings.DecimalSeparator;     {Preparations}
  DefaultFormatSettings.DecimalSeparator:='.';
  filelist:=TStringList.Create;
  loglist:=TStringList.Create;
  tlmlist:=TStringList.Create;
  remlist:=TStringList.Create;
  remgpslist:=TStringList.Create;
  splitlist:=TStringList.Create;
  aImgInfo:=TImgInfo.Create;
  screen.Cursor:=crHourGlass;
  mnSaveCSV.Enabled:=false;
  gridPictures.RowCount:=1;
  stdat:=ClearTXD;
  ldat:=ClearTXD;
  mainpath:='';                                        {Path above FlightLog path}
  flpath:='';                                          {FlightLog path}
  mname:='';                                           {Modelname, ID if exported model was found}
  flnum:='';                                           {Flight number}
  RCrole:='';                                          {Contriller role}
  zhl:=0;
  zl2:=0;
  zl3:=0;
  geoalt:=0;                                           {Default: Use relative altitude}
  loglist.Add(rsLogHeader);                            {Header CSV log file}

  try
    CheckFileSystem;
    AdditionalTextFromFile;                            {Find and load add text file}

{Add controller role to JSON, common for all pictures}
    if rgController.ItemIndex=rgController.Items.Count-1 then
      RCrole:=edController.Text                            {Self-defined item}
    else
      RCrole:=rgController.Items[rgController.ItemIndex];
    StatusBar.Panels[3].Text:=flpath;

    jsonstr:=startID+strID+flog+strID+dpID+startID+                             {JSON Start, telemetry}
             '"Path"'+dpID+strID+                                               {JSON FlightLog path }
             StringReplace(flpath, PathDelim, '/', [rfReplaceAll])+strID+sep+
             '"Number"'+dpID+strID+flnum+strID+sep+                             {JSON Flight number}
             '"ControllerRole"'+dpID+strID+RCrole+strID+endID+sep+le;           {JSON Controller role}

    splitlist.Clear;
    splitlist.Delimiter:=sep;
    splitlist.StrictDelimiter:=true;

    LoadExpModel;                                      {Find exported model}
    AdditionalTextToJSON;                              {Add additional text}

{Find all JPG/JEPG pictures in the Picture folder --> filelist}
    FindAllFiles(filelist, cbxPics.Text, '*.jpg;*.jpeg', false);
    if filelist.Count>0 then begin                     {Only if pictures where found}
      mnSaveCSV.Enabled:=true;
      StatusBar.Panels[0].Text:=IntToStr(filelist.Count);
      gridPictures.RowCount:=filelist.Count+1;
      for i:=0 to filelist.Count-1 do                  {List picture files}
        gridPictures.Cells[0, i+1]:=ChangeFileExt(ExtractFileName(filelist[i]), '');

{Find take-off in Telemetry file, common for all pictures}
      if (flpath<>'') and (flnum<>'') then begin       {FlightLog found}
        fdat:=FileDateToDateTime(FileAge(cbxLogs.Text));

        tlmlist.LoadFromFile(cbxLogs.Text);            {Telemetry}
        FindTakeOff;                                   {Check telemetry}

{Load Remote and RemoteGPS from take-off, common for all pictures}
        if stdat.telem<>'' then begin                  {if start data available}

          startpos:=FormatFloat(coordfrm, stdat.lat)+'+'+
                    FormatFloat(coordfrm, stdat.lon);  {for URL with cgi}

          if cbUpdateAlt.Checked and cbAutoGeoid.Checked then begin  {Find correction value for geoid from internet}
            GetInetList(GeoidCgiURL(startpos), remlist);
            fn:=FindLineHTTP(rgGravity.Items[rgGravity.ItemIndex], remlist);
            lbeGeoid.Text:=ExtractHeight(fn);          {Update settings}
            remlist.Clear;
          end;

          fn:=flpath+trem+PathDelim+trem+usID+flnum+cext;
          if FileExists(fn) then begin                 {load Remote.csv}
            remlist.LoadFromFile(fn);
            stdat.remot:=FindDataLine(remlist, stdat.zeit, zl2);
          end;

          fn:=flpath+tgps+PathDelim+tgps+usID+flnum+cext;
          if FileExists(fn) then begin                 {load RemoteGPS.csv}
            remgpslist.LoadFromFile(fn);
            stdat.regps:=FindDataLine(remgpslist, stdat.zeit, zl3);
            lbeGeoid.Text:=CleanGeoid(lbeGeoid.Text, geoalt);
{Correction value ellipsoid above geoid, only used if RemoteGPS is available! default=0;}
            geoalt:=GetAltFromST(remgpslist, zl3)-geoalt;
            if cbUpdateAlt.Checked then
              stdat.alt:=stdat.alt+geoalt;
          end;  {geoalt now contains abs alt of ground geoid}

          jsonstr:=jsonstr+'"DataHeaders"'+dpID+startID+                        {JSON: DataHeaders}
                   strID+tele+strid+dpID+strID+rsDT+tlmlist[0]+strID;           {JSON Header Telemetry}
          if stdat.remot<>'' then
            jsonstr:=jsonstr+sep+le+strID+trem+strID+dpID+
                     strID+rsDT+remlist[0]+strID;                               {JSON: Header Remote}
          if stdat.regps<>'' then
            jsonstr:=jsonstr+sep+le+strID+tgps+strID+dpID+
                     strID+rsDT+remGPSlist[0]+strID;                            {JSON: Header RemoteGPS}
          jsonstr:=jsonstr+endID+sep+le;

          jsonstr:=jsonstr+'"TakeOff"'+dpID+startID;                            {JSON: TakeOff start}
          jsonstr:=jsonstr+AusgData(tele, stdat.telem, zl1);
          if stdat.remot<>'' then
            jsonstr:=jsonstr+sep+le+AusgData(trem, stdat.remot, zl2);           {JSON: TO Remote}
          if stdat.regps<>'' then
            jsonstr:=jsonstr+sep+le+AusgData(tgps, stdat.regps, zl3);           {JSON: TO RemoteGPS}
          jsonstr:=jsonstr+endID;                                               {JSON: End TakeOff}
          ldat:=stdat;                                 {Save as previous dataset}

{Save take-off data in first line of log file}
          startpos:=FormatFloat(coordfrm, stdat.lat);
          loglist.Add(ExtractFileName(cbxLogs.Text)+sep+           {Filename telemetry}
                      FormatDateTime(timeformat, fdat)+sep+        {File date/time}
                      FormatDateTime(timeformat, stdat.zeit)+sep+  {Time take-off telemetry}
                      VtypeToStr(drone)+sep+                       {Vehicle type}
                      IntToStr(zl1)+sep+                           {Line number take-off}
                      IntToStr(tbDelta.Position)+sep+              {Variance from settings}
                      FormatFloat(coordfrm, stdat.lat)+sep+        {lat/lon take-off}
                      FormatFloat(coordfrm, stdat.lon)+sep+
                      FormatFloat(altfrm, stdat.alt)+sep+          {Telemetry altitude}
                      FormatFloat(altfrm, geoalt)+sep+             {Abs altitude ST16 smoothed}
                      '0'+sep+                                     {Null point for distance}
                      splitlist[12]);                              {Yaw}

{Get log data for RemoteGPS}
          picdat:=ClearTXD;                                        {picdat reused for RC data}
          try                                                      {Try to find Distance to RC}
            picdat.lat:=StrToFloat(stdat.regps.Split([sep])[2]);
            picdat.lon:=StrToFloat(stdat.regps.Split([sep])[1]);
            picdat.alt:=DeltaCoord(stdat.lat, stdat.lon, picdat.lat, picdat.lon);
            picdat.regps:=FormatFloat(altfrm, picdat.alt);
          except
            picdat.regps:='';
          end;
          picdat.zeit:=FileDateToDateTime(FileAge(fn));

          loglist.Add(ExtractFileName(fn)+sep+                     {Filename RemoteGPS}
                      FormatDateTime(timeformat, picdat.zeit)+sep+ {File date/time}
                      FormatDateTime(timeformat, stdat.zeit)+sep+  {Time take-off telemetry}
                      vTypeToRC(drone)+sep+                        {ST10 or ST16}
                      IntToStr(zl3)+sep+                           {Line number take-off}
                      stdat.regps.Split([sep])[4]+sep+             {Accuracy}
                      stdat.regps.Split([sep])[2]+sep+             {lat}
                      stdat.regps.Split([sep])[1]+sep+             {lon}
                      stdat.regps.Split([sep])[3]+sep+             {GPS alt, data point}
                      FormatFloat(altfrm, geoalt)+sep+             {GPS alt, smoothed: average from 41 values}
                      picdat.regps+sep+                            {Distance RC to start}
                      stdat.regps.Split([sep])[6]);                {Angle}

{Find components and read / write EXIF per picture file}
          StatusBar.Panels[0].Text:=IntToStr(filelist.Count);
          gridPictures.RowCount:=filelist.Count+1;
          gridPictures.BeginUpdate;

          if not cbUpdateAlt.Checked then              {Use relative alt instead of absolute}
            geoalt:=0;                                 {Reset altitude ground}

{Preparations done, now all picture files will be checked one by one from filelist}
          for i:=0 to filelist.Count-1 do begin        {Actions per pictue}
            picdat:=ClearTXD;
            Gimbal:=GetXMPdata(filelist[i]);           {Find XMP data in the pic file}
            dist:=0;
            bear:=0;
            diffalt:=0;
            zl1:=0;
            zl2:=0;
            zl3:=0;
            fdat:=0;                                   {File date time}
            cam:='';
            outstr:=jsonstr;                           {Take over common data}
            fdat:=FileDateToDateTime(FileAge(filelist[i]));
            picdat.zeit:=fdat;                         {Will be overwritten by EXIF time}
            gridPictures.Cells[1, i+1]:=FormatDateTime(timeformat, picdat.zeit);
            try
              aImgInfo.LoadFromFile(filelist[i]);
            except                                     {Error message data structure EXIF}
              on e: Exception do begin
                gridPictures.Cells[12, i+1]:=e.Message;
                break;
              end;
            end;
            splitlist.Clear;
            splitlist.Delimiter:=sep;
            splitlist.StrictDelimiter:=true;
            if aImgInfo.HasEXIF then begin
              try
                picdat.zeit:=GetEXIFtime(aImgInfo);
                if picdat.zeit<1 then                  {EXIF time not available (i.e. CGO)}
                  picdat.zeit:=fdat;                   {Take file date time}
                cam:=ReadString(aImgInfo, exModel, '');

{List all picture files in info table}
                gridPictures.Cells[1, i+1]:=FormatDateTime(timeformat, picdat.zeit);
                gridPictures.Cells[2, i+1]:=ReadString(aImgInfo, exVersn, '0000');
                try
                  if ReadCoordinates(aImgInfo, picdat.lat, picdat.lon) then
                    gridPictures.Cells[3, i+1]:='EXIF';   {Valid coordinates in EXIF}
                except
                  picdat.lat:=0;
                  picdat.lon:=0;
                end;
{Handle only Yuneec picture files}
                if lowercase(ReadString(aImgInfo, exMake, ''))=makefilter then begin
                  picdat.telem:=FindDataLine(tlmlist, picdat.zeit, zl1);
                  if picdat.telem<>'' then begin         {Found time match}

{Read lat/lon from EXIF and from telemetry to compare}
                    splitlist.DelimitedText:=picdat.telem;
                    if not TryStrToFloat(splitlist[4], picdat.alt) or
                       (picdat.alt>maxalt) then          {Read altitude from telemetry}
                      picdat.alt:=defalt;
                    picdat.alt:=picdat.alt+geoalt;
                    if not TryStrToFloat(splitlist[5], lat) then
                      lat:=0;
                    if not TryStrToFloat(splitlist[6], lon) then
                      lon:=0;

                    TakePosFromTelemetry;                {Missing coordinates in EXIF}

{Handle matching pictures, edit and save it}
                    vari:=DeltaCoord(lat, lon, picdat.lat, picdat.lon);
                    if vari<=tbDelta.Max then            {Show variance only if in allowed area}
                      gridPictures.Cells[4, i+1]:=FormatFloat(altfrm, vari*100);
                    if vari<=tbDelta.Position/100 then begin
                      gridPictures.Cells[6, i+1]:=AusgZeile(zl1);
                      inc(zhl);                          {Count matches}

                      outstr:=outstr+sep+le+'"CameraShoot"'+dpID+startID;       {JSON: Picture start}
                      outstr:=outstr+le+'"SequenceNo"'+dpID+IntToStr(zhl);      {JSON: Number of the file}
                      outstr:=outstr+le+sep+
                              AusgData(tele, picdat.telem, zl1);                {JSON: Pic Telemetry}

                      picdat.remot:=FindDataLine(remlist, picdat.zeit, zl2);
                      if picdat.remot<>'' then begin
                        outstr:=outstr+sep+le+AusgData(trem, picdat.remot, zl2); {JSON: Pic Remote}
                        gridPictures.Cells[7, i+1]:=AusgZeile(zl2);
                      end;

                      picdat.regps:=FindDataLine(remGPSlist, picdat.zeit, zl3);
                      if picdat.regps<>'' then begin
                        outstr:=outstr+sep+le+AusgData(tgps, picdat.regps, zl3); {JSON: Pic RemoteGPS}
                        gridPictures.Cells[8, i+1]:=AusgZeile(zl3);
                      end;

{Delta values between the pictures}
                      dist:=DeltaCoord(ldat.lat, ldat.lon, picdat.lat, picdat.lon);
                      bear:=Bearing(ldat.lat, ldat.lon, picdat.lat, picdat.lon);
                      diffalt:=picdat.alt-ldat.alt;
                      outstr:=outstr+sep+le+'"DistanceToPreviousPosition"'+
                              dpID+FormatFloat(altfrm, dist);                   {JSON: Distance}
                      outstr:=outstr+sep+le+'"Bearing"'+
                              dpID+FormatFloat(altfrm, bear);                   {JSON: Bearing}
                      outstr:=outstr+sep+le+'"Ascent"'+
                              dpID+FormatFloat(altfrm, diffalt);                {JSON: Ascent}
                      ldat:=picdat;                    {Save current as previous dataset}

                      WriteXMPdata;
                      outstr:=outstr+endID+endID;                               {JSON: End Picture, end all}

                      WriteEXIFdata;                   {Write EXIF data}
                      SaveJSONfiles;                   {Create JSON data as file}
                      SaveEXIFdata;                    {Save EXIF to file, possibly backup}

                      if mName<>'' then                {Model name}
                        gridPictures.Cells[9, i+1]:=mName;
                      if addtxt<>'' then
                        gridPictures.Cells[11, i+1]:=addtxt;
                    end else
                      gridPictures.Cells[6, i+1]:=rsNo; {No correlation in position}

                  end else
                    gridPictures.Cells[6, i+1]:=rsNo;  {No time correlation found}

                end;                                   {End camera filter}

              except                                   {Error message EXIF}
                on e: Exception do
                  gridPictures.Cells[12, i+1]:=e.Message;
              end;

            end else
              gridPictures.Cells[2, i+1]:=rsNo;        {No EXIF data}

            CreateLogData;                             {One dataset log per picture}
          end;                                         {Actions per picture ended}

          gridPictures.EndUpdate;
          StatusBar.Panels[1].Text:=IntToStr(zhl);     {Number updated pictures}

        end else
          StatusBar.Panels[3].Text:=rsNoFlight;

      end else
        StatusBar.Panels[3].Text:=rsMissLog;

    end else
      StatusBar.Panels[3].Text:=rsNoFiles;
    gridPictures.AutoSizeColumns;

    if cbLog.Checked and
       (loglist.Count>1) then begin                    {Save log file}
      fn:=ExtractFilePath(cbxLogs.Text)+flnum+usID+logtxtfn;
      loglist.SaveToFile(fn);
    end;
    pcTabs.ActivePage:=tabMain;

  finally                                              {Clean up after work}
    DefaultFormatSettings.DecimalSeparator:=dsepdef;
    filelist.Free;
    loglist.Free;
    tlmlist.Free;
    remlist.Free;
    remgpslist.Free;
    splitlist.Free;
    aImgInfo.Free;
    Screen.Cursor:=crDefault;
  end;

end;

procedure TForm1.cbxLogsDblClick(Sender: TObject);     {Open FlightLog folder}
begin
  OpenDocument(ExtractFilePath(cbxLogs.Text));
end;

procedure TForm1.cbxPicsChange(Sender: TObject);
begin
  ScanEnable;
end;

procedure TForm1.cbxPicsDblClick(Sender: TObject);     {Open picture folder}
begin
  OpenDocument(IncludeTrailingPathDelimiter(cbxPics.Text));
end;

procedure TForm1.edControllerDblClick(Sender: TObject); {Clear Role controller}
begin
  edController.Text:='';
end;

end.

