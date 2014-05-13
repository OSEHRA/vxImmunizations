unit uImmunizations;

//*****************************************************************************
//gkp added 2011-03
//*****************************************************************************

//{$define DEBUG}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, stdCtrls,
  strUtils,
  ORFn, OrNet, ORCtrls, fODBase, fODMedOIFA, uCore, fOrders, Trpcb, uPCE;

type

  TLaunchDLLImmunization = function(ThisPatient: TPatient; ThisUser: TUser;
                            ThisEncounter: TEncounter; ThisfOrder: TfrmOrders;
                            ThisBroker: TRPCBroker): boolean; stdcall;

  TDLLImmunizationInfo = class
  protected
    fDLLHinst   : HInst;
    fDLLExecute : TFarProc;
  public
    constructor Create;
    procedure Free;
    property DLLHinst   : HInst read fDLLHinst write fDLLHinst;
    property DLLExecute : TFarProc read fDLLExecute write fDLLExecute;
  end;

  TDLLImmunizations = class  //group the DLL functions for Immunizations
    class function CreateslLaunchedDLLImmunizations():boolean;
    class function ActivateDLLImmunization(DLLPath: string): Boolean;
    class function LoadDLLImmunization(DLLPath: string): TDLLImmunizationInfo; //gkp split ActivateDLLOrder()
    class function iLaunchedDLLImmunizations(DLLPath: string): integer;  //gkp added
    class procedure UnloadDLLImmunization(CurDLLImmunizationInfo: TDLLImmunizationInfo); //gkp created to unload from memory
    class procedure FreeslLaunchedDLLImmunizations();
    class function GetVersion(sFileName:string): string;
  end;

////IEN # ^ LOT NUMBER ^ MANUFACTURER ^ PRODUCT NAME ^ CVX CODE ^ MVX CODE ^ START DATE ^ EXPIRATION DATE ^ OVERRIDE EXPIRATION DATE ^ OUT OF STOCK? ^ QUANTITY ^ UNIT ^ IMMUNIZATION
  TImmAdmLot = class(TPCEItem) //adapted from TPCEImm (consider base TPCEProc ?)
    CVX           : String; //immunization identifier CVX code
    MVX           : String; //Manufacturer identifier MVX code
    Manufacturer  : String; //manufacturer name
    ProductName   : String;
    Lot           : String; //lot number (alphanumeric)
    DosageQty     : String; //float number. a.k.a. Amount
    DosageUnit    : String; //{mg,ml, etc.}
    procedure SetFromString(const x: string); override;
  end;

  TImmAdmin = class(TImmAdmLot) //adapted from TPCEImm
    Who           : String; //person who administered the immunization
    StartDateTime : String; //M datetime format
    EndDateTime   : String; //M datetime format
    //pointers to other M files
    LocationIEN   : String; //integer  per Lee: Encounter.Location
    OrderIEN      : String; //integer
    VFDStockIEN   : String; //integer
    VImmIEN       : String; //integer
    function GetSaveString() : string;
    function SaveToDB() : boolean;
  end;

  TStoreImm = (tsiAdd, tsiEdit);

  TImmLot = class(TImmAdmLot) //adapted from TPCEImm
  {class for immunization inventory}
  public
    OrderableItem : String; //
    StartDate     : String; //Mumps date
    ExpirationDate: String; //Mumps date
    OverrideExp   : String; //boolean (YES/NO)
    OutOfStock    : String; //boolean (YES/NO)
    procedure SetFromString(const x: string); override;
    function SaveToDB(tsiAddEdit: TStoreImm) : boolean;
  end;

  TImmAdministration = class //interface with rImmunizations.pas portion which deals with administration of immunizations
    class function GetDLLFilename(): TFilename;
    //
    class function bAnyPatientActiveImmOrders(DFN: Integer): boolean;
    class function GetPatientImmsOrderedToBeAdmin(DFN: Integer): TStrings;
    class function ProcessList(ScriptList : TStringList; SendForm: TForm): boolean;
    class procedure GetImmLots(var slAdmLot:TStringList; IEN: Integer);
    class function GetUserList(sLookUp : string): TStrings;
    class function SaveImmAdministeredInfo(ImmAdmin: TImmAdmin): boolean;
  end;

  TImmInventory = class //interface with rImmunizations.pas portion which deals with immunizations inventory
    class function GetDLLFilename(): TFilename;
    //
    class function GetOrderableItems(): TStrings;
    class procedure GetImmLots(var slInvLot:TStringList; IEN: Integer);
    class function SaveImmLot(ImmLot: TImmLot; tsiAddEdit: TStoreImm): boolean;
  end;

procedure ShowDebug(sText: string);
function DateTimeDropSeconds(DateTime: TDateTime): TDateTime;
function sDateTimeToFMDateTime(DateTime: TDateTime): string;
function BoolToYESorNO(bIn: boolean): string;
procedure FreeStringList(mySL : TStringList);



function EnterEditImmunization(ImmunizationIEN: integer; AddNew, MarkAsEnteredInError: boolean): boolean;

implementation

uses rImmunizations, Math;

var
  slLaunchedDLLImmunizations: TStringList;

function EnterEditImmunization(ImmunizationIEN: integer; AddNew, MarkAsEnteredInError: boolean): boolean;
//returns true if any data changed
var
  uAddingNew      : boolean;
  sBaseContext    : string;
begin
  try
    sBaseContext := RPCBrokerV.CurrentContext;
    uAddingNew := AddNew;
    if uAddingNew then
      Result := TDLLImmunizations.ActivateDLLImmunization(TImmAdministration.GetDLLFilename())
    else
    begin
      Result := TDLLImmunizations.ActivateDLLImmunization(TImmInventory{TImmAdministration}.GetDLLFilename())
    end;
  finally
    //just called a DLL, so make sure the menu context is refreshed
    UpdateContext(sBaseContext);
  end;
end;


//*****************************************************************************
{* TDLLImmunizations *}

class function TDLLImmunizations.LoadDLLImmunization(DLLPath: string): TDLLImmunizationInfo; //gkp split ActivateDLLOrder()
//just try to Load the DLL, not execute it yet
var
  ptr: integer;
  CurDLLImmunizationInfo: TDLLImmunizationInfo;
begin
  if not Assigned(slLaunchedDLLImmunizations) then
    TDLLImmunizations.CreateslLaunchedDLLImmunizations();
  ptr := TDLLImmunizations.iLaunchedDLLImmunizations(DLLPath);
  if ptr <> -1 then
  begin
    CurDLLImmunizationInfo := TDLLImmunizationInfo(slLaunchedDLLImmunizations.Objects[ptr]);
  end
  else
  begin
    CurDLLImmunizationInfo := TDLLImmunizationInfo.Create;
    try
      CurDLLImmunizationInfo.DLLHinst := SafeLoadLibrary(DLLPath);
      if CurDLLImmunizationInfo.DLLHinst > 0 then
      begin
        CurDLLImmunizationInfo.DLLExecute := GetProcAddress(CurDLLImmunizationInfo.DLLHinst, 'ImmunizationDialogLauncher');
        if CurDLLImmunizationInfo.DLLExecute <> nil then
        begin
          slLaunchedDLLImmunizations.AddObject(DLLPath, CurDLLImmunizationInfo);
        end
        else
        begin
          Result := nil;
          Exit;
        end;
      end
      else
      begin
        Result := nil;
        Exit;
      end;
    except on exception do
      Result := nil;
    end;
  end;
  Result := CurDLLImmunizationInfo;
end;

class function TDLLImmunizations.ActivateDLLImmunization(DLLPath: string): Boolean;
//load DLL if not already loaded, then execute it
var
  CurDLLImmunizationInfo: TDLLImmunizationInfo;
  LaunchDLLImmunization: TLaunchDLLImmunization;
begin
  Result := false;
  CurDLLImmunizationInfo := LoadDLLImmunization(DLLPath);
  if (CurDLLImmunizationInfo <> nil) then
  begin
    LaunchDLLImmunization := TLaunchDLLImmunization(CurDLLImmunizationInfo.DLLExecute);
    Result := LaunchDLLImmunization(Patient,User,Encounter,frmOrders,RPCBrokerV);
  end;
end;

class procedure TDLLImmunizations.UnloadDLLImmunization(CurDLLImmunizationInfo: TDLLImmunizationInfo); //gkp created to unload from memory
//try to unload the DLL if the object exists and we still have DLLHinst pointer
begin
  if Assigned(CurDLLImmunizationInfo) then
    if CurDLLImmunizationInfo.DLLHinst <> 0 then
      try
        FreeLibrary(CurDLLImmunizationInfo.DLLHinst);
      except
        //eat error (if any)
      end;
end;

class function TDLLImmunizations.CreateslLaunchedDLLImmunizations():boolean;
begin
  Result := true;
  try
    slLaunchedDLLImmunizations := TStringList.Create;
  except on exception do
    result := false;
  end;
end;

class procedure TDLLImmunizations.FreeslLaunchedDLLImmunizations();
var
  CurDLLImmunizationInfo: TDLLImmunizationInfo;
begin
  if not Assigned(slLaunchedDLLImmunizations) then
    exit;
  try
    while slLaunchedDLLImmunizations.Count > 0 do
    begin
      CurDLLImmunizationInfo := TDLLImmunizationInfo(slLaunchedDLLImmunizations.Objects[0]);
      //unload DLL if still loaded  (gkp added)
      UnloadDLLImmunization(CurDLLImmunizationInfo);
      //free the object and delete it from the stringlist
      CurDLLImmunizationInfo.Free;
      slLaunchedDLLImmunizations.Delete(0);
    end;
  finally
    slLaunchedDLLImmunizations.Free;
    slLaunchedDLLImmunizations := nil;
  end;
end;

class function TDLLImmunizations.iLaunchedDLLImmunizations(DLLPath: string): integer;  //gkp added
var
  iIndex : integer;
  iLoop : integer;
  sDLLPathLowerCase : string;
begin
  Assert(Assigned(slLaunchedDLLImmunizations), 'slLaunchedDLLImmunizations has not been instantiated.');
  Assert(DLLPath <> '', 'DLLPath has not been set');
  //look for DLLPath in loaded list via case-sensitive match
  iIndex := slLaunchedDLLImmunizations.IndexOf(DLLPath);
  result := iIndex;
  if result > -1 then //if found from case-sensitive search
    exit;
  //perform case IN-sensitive search
  sDLLPathLowerCase := LowerCase(DLLPath);
  iLoop := 0;
  while (iLoop < slLaunchedDLLImmunizations.Count) and (result = -1) do
  begin
    if sDLLPathLowerCase = LowerCase(slLaunchedDLLImmunizations[iLoop]) then
      result := iLoop;
    inc(iLoop);
  end;
end;

class function TDLLImmunizations.GetVersion(sFileName:string): string;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(sFileName), Dummy);
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(sFileName), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  with VerValue^ do
  begin
    Result := IntToStr(dwFileVersionMS shr 16);
    Result := Result + '.' + IntToStr(dwFileVersionMS and $FFFF);
    Result := Result + '.' + IntToStr(dwFileVersionLS shr 16);
    Result := Result + '.' + IntToStr(dwFileVersionLS and $FFFF);
  end;
  FreeMem(VerInfo, VerInfoSize);
end;


//*****************************************************************************
{* TDLLImmunizationInfo *}

constructor TDLLImmunizationInfo.Create;
begin
  inherited;
  fDLLHinst := 0;
  fDLLExecute := nil;
end;

procedure TDLLImmunizationInfo.Free;
begin
  inherited;
end;


//*****************************************************************************
{* TImmAdministration *}

class function TImmAdministration.GetDLLFilename(): TFilename;
var
  sDir : string;
begin
  sDir := ExtractFilePath(Application.ExeName);
{$ifdef DEBUG}
  result := sDir + 'Immunizations\ImmunizationAdministration.dll';
{$else}
  result := sDir + 'ImmunizationAdministration.dll';
{$endif}
  if not FileExists(result) then
    ShowMessage(result + ' DLL file does not exist.');
end;

class function TImmAdministration.ProcessList(ScriptList : TStringList; SendForm: TForm): boolean;
{process series of immunization administrations from the active signed
 immunization orders selected (from frmOrders)}
//adapted from rOrders.ReprintOrders()
var
  sImmunizationsDLLFilename : string;
  iOrder : integer;
  ptr : integer;
  sOrder : string;
begin
  Result := false;
  if User.vxCPRS_IEN <> '-1' then
  begin
    sImmunizationsDLLFilename := TImmAdministration.GetDLLFilename();
    for ptr := 0 to ScriptList.Count - 1 do
    begin
      sOrder := Piece(ScriptList[ptr], ';', 1);
      iOrder := StrToIntDef(sOrder, 0);
      try
        result := EnterEditImmunization(iOrder, TRUE, FALSE);
      except on exception do;
        //eat exception
      end;
    end;
  end;
end;

class function TImmAdministration.bAnyPatientActiveImmOrders(DFN: Integer): boolean;
var
  slImmOrders : TStringList;
begin
  slImmOrders := TStringList.Create;
  try
    slImmOrders.AddStrings(TImmAdministration.GetPatientImmsOrderedToBeAdmin(DFN));
    result := (slImmOrders.Count > 0);
    if not result then
      ShowMessage('There are no active immunization orders for the patient.');
  finally
    slImmOrders.Free;
  end;
end;

class function TImmAdministration.GetPatientImmsOrderedToBeAdmin(DFN: Integer): TStrings;
begin
  //passthru
  result := rGetPatientImmsOrderedToBeAdmin(DFN);
  if (result.Count > 1) then
    result.Delete(0); //remove the record-count row
//  ShowDebug(result.Text);
end;

class procedure TImmAdministration.GetImmLots(var slAdmLot:TStringList; IEN: Integer);
//function returns: TStrings of string Lot, object TImmAdmLot
//RPC returns:
//   LIST(0) = Count of Active Lots
//   LIST(n) = Lot # ^ Manufacturer Name ^ Amount ^ Units ^ Product Name??
//IEN # ^ LOT NUMBER ^ MANUFACTURER ^ PRODUCT NAME ^ CVX CODE ^ MVX CODE ^ START DATE ^ EXPIRATION DATE ^ OVERRIDE EXPIRATION DATE ^ OUT OF STOCK? ^ QUANTITY ^ UNIT ^ IMMUNIZATION
//or LIST(0) = -1 ^ Error Text
var
  slRPCResult : TStringList;
  ImmAdmLot : TImmAdmLot;
  iIndex  : integer;
begin
  slRPCResult := TStringList.Create;
  if not Assigned(slAdmLot) then
    slAdmLot := TStringList.Create;
  slAdmLot.Clear;
  try
    slRPCResult.AddStrings(rGetImmLots(IEN));
    if slRPCResult.Count > 1 then
    begin
      slRPCResult.Delete(0); //remove the record-count row
      //fill results with TImmAdmLot objects
      for iIndex := 0 to slRPCResult.Count - 1 do
      begin
        ImmAdmLot := TImmAdmLot.Create;
        ImmAdmLot.SetFromString(slRPCResult.Strings[iIndex]);
        slAdmLot.AddObject(ImmAdmLot.Lot, ImmAdmLot);
      end;
    end
    else
    begin
      MessageDlg('Selected immunization cannot be administered'+#13#10+
                 'until it is entered into VFD Immunizations file.'+#13#10#13#10+
                 'Please select a different immunization.',
                 mtError, [mbOK], 0);
    end;
  finally
    slRPCResult.Free;
  end;
//  ShowDebug(slAdmLot.Text);
end;

class function TImmAdministration.GetUserList(sLookUp : string): TStrings;
begin
  result := rGetUserList(sLookup);
end;

class function TImmAdministration.SaveImmAdministeredInfo(ImmAdmin: TImmAdmin): boolean;
begin
  result := ImmAdmin.SaveToDB();
end;


//*****************************************************************************
{* TImmInventory *}

class function TImmInventory.GetDLLFilename(): TFilename;
var
  sDir : string;
begin
  { TODO : change to finding DLL name }
  sDir := ExtractFilePath(Application.ExeName);
  result := sDir + 'ImmunizationInventory.dll';
  if not FileExists(result) then
    ShowMessage(result + ' DLL file does not exist.');
end;

class function TImmInventory.GetOrderableItems(): TStrings;
begin
  result := rGetOrderableItems();
  if result.Count > 1 then
    result.Delete(0); //remove the count of the items returned
end;

class procedure TImmInventory.GetImmLots(var slInvLot:TStringList; IEN: Integer);
//adapted from TImmAdministration.GetImmLots()
//function returns: TStrings of string Lot, object TImmAdmLot
//RPC returns:
//   LIST(0) = Count of Active Lots
//   LIST(n) = Lot # ^ Manufacturer Name ^ Amount ^ Units ^ Product Name??
//IEN # ^ LOT NUMBER ^ MANUFACTURER ^ PRODUCT NAME ^ CVX CODE ^ MVX CODE ^ START DATE ^ EXPIRATION DATE ^ OVERRIDE EXPIRATION DATE ^ OUT OF STOCK? ^ QUANTITY ^ UNIT ^ IMMUNIZATION
//or LIST(0) = -1 ^ Error Text
var
  slRPCResult : TStringList;
  ImmLot : TImmLot;
  iIndex  : integer;
begin
  slRPCResult := TStringList.Create;
  if not Assigned(slInvLot) then
    slInvLot := TStringList.Create;
  try
    slRPCResult.AddStrings(rGetImmLots(IEN));
//    ShowDebug(slRPCResult.Text);
    if slRPCResult.Count > 1 then
      slRPCResult.Delete(0); //remove the record-count row
    //fill results with TImmLot objects
    for iIndex := 0 to slRPCResult.Count - 1 do
    begin
      ImmLot := TImmLot.Create;
      ImmLot.SetFromString(slRPCResult.Strings[iIndex]);
      slInvLot.AddObject(ImmLot.Lot, ImmLot);
    end;
  finally
    slRPCResult.Free;
  end;
//  ShowDebug(slInvLot.Text);
end;

class function TImmInventory.SaveImmLot(ImmLot: TImmLot; tsiAddEdit: TStoreImm): boolean;
begin
  result := ImmLot.SaveToDB(tsiAddEdit);
end;


//*****************************************************************************
{* TImmAdmLot *}

procedure TImmAdmLot.SetFromString(const x: string);
//IEN # ^ LOT NUMBER ^ MANUFACTURER ^ PRODUCT NAME ^ CVX CODE ^ MVX CODE ^
//START DATE ^ EXPIRATION DATE ^ OVERRIDE EXPIRATION DATE ^ OUT OF STOCK? ^
//QUANTITY ^ UNIT ^ IMMUNIZATION ^
{  TImmAdmLot = class(TPCEItem) //adapted from TPCEImm (consider base TPCEProc ?)
    CVX           : String; //immunization identifier CVX code
    MVX           : String; //Manufacturer identifier MVX code
    Manufacturer  : String; //manufacturer name
    ProductName   : String;
    Lot           : String; //lot number (alphanumeric)
    DosageQty     : String; //float number. a.k.a. Amount
    DosageUnit    : String; //[mg,ml, etc.]
    procedure SetFromString(const x: string); override;
  end; }
var
  iPiece : integer;
  sSkip  : string;
  function ImmGetNextPiece: string;
  begin
    inc(iPiece);
    result := Piece(x, U, iPiece);
  end;
begin
//  inherited(x);
  iPiece := 0;
  Code         := ImmGetNextPiece();
  Lot          := ImmGetNextPiece();
  Manufacturer := ImmGetNextPiece();
  ProductName  := ImmGetNextPiece();
  CVX          := ImmGetNextPiece();
  MVX          := ImmGetNextPiece();
  sSkip        := ImmGetNextPiece();
  sSkip        := ImmGetNextPiece();
  sSkip        := ImmGetNextPiece();
  sSkip        := ImmGetNextPiece();
  DosageQty    := ImmGetNextPiece();
  DosageUnit   := ImmGetNextPiece();
//  sSkip        := ImmGetNextPiece();
end;


//*****************************************************************************
{* TImmAdmin *}

function TImmAdmin.GetSaveString() : string;
//returns: DOSE^UNIT^WHO^START^END^ORC^LOC^VFD IMM^
begin
  result := DosageQty + U;
  result := result + DosageUnit + U;
  result := result + Who + U;
  result := result + StartDateTime + U;
  result := result + EndDateTime + U;
  result := result + OrderIEN + U;
  result := result + LocationIEN + U;
  result := result + VFDStockIEN;
end;

function TImmAdmin.SaveToDB() : boolean;
var
  sSaveArray : string;
begin
  sSaveArray := GetSaveString();
//  ShowDebug(sSaveArray);
  result := rSaveImmAdministeredInfo(sSaveArray);
end;


//*****************************************************************************
{* TImmLot *}

procedure TImmLot.SetFromString(const x: string);
//adapted from TImmAdmLot.SetFromString()
//IEN # ^ LOT NUMBER ^ MANUFACTURER ^ PRODUCT NAME ^ CVX CODE ^ MVX CODE ^
//START DATE ^ EXPIRATION DATE ^ OVERRIDE EXPIRATION DATE ^ OUT OF STOCK? ^
//QUANTITY ^ UNIT ^ IMMUNIZATION ^
{  TImmLot = class(TImmAdmLot) //adapted from TPCEImm
  public
    OrderableItem : String; //
    StartDate     : String; //Mumps date
    ExpirationDate: String; //Mumps date
    OverrideExp   : String; //boolean (YES/NO)
    OutOfStock    : String; //boolean (YES/NO)
    procedure SetFromString(const x: string); override;
    function SaveToDB(tsiAddEdit: TStoreImm) : boolean;
  end;  }
var
  iPiece : integer;
  function ImmGetNextPiece: string;
  begin
    inc(iPiece);
    result := Piece(x, U, iPiece);
  end;
begin
  iPiece := 0;
  Code           := ImmGetNextPiece();
  Lot            := ImmGetNextPiece();
  Manufacturer   := ImmGetNextPiece();
  ProductName    := ImmGetNextPiece();
  CVX            := ImmGetNextPiece();
  MVX            := ImmGetNextPiece();
  StartDate      := ImmGetNextPiece();
  ExpirationDate := ImmGetNextPiece();
  OverrideExp    := ImmGetNextPiece();
  OutOfStock     := ImmGetNextPiece();
  DosageQty      := ImmGetNextPiece();
  DosageUnit     := ImmGetNextPiece();
  OrderableItem  := ImmGetNextPiece();
end;

function TImmLot.SaveToDB(tsiAddEdit: TStoreImm) : boolean;
begin
  result := rSaveImmLotInfo(Self, tsiAddEdit);
end;


//*****************************************************************************
{* GENERAL *}

procedure ShowDebug(sText: string);
begin
  {$ifdef DEBUG}
  ShowMessage(sText);
  {$endif}
end;

function DateTimeDropSeconds(DateTime: TDateTime): TDateTime;
var
  iDate : integer;
  iHour, iMin, iSec, iMSec: word;
begin
  iDate := Trunc(DateTime);
  DecodeTime(DateTime, iHour, iMin, iSec, iMSec);
  result := iDate + EncodeTime(iHour, iMin, 0, 0);
end;

function sDateTimeToFMDateTime(DateTime: TDateTime): string;
var
  FMDateTime : TFMDateTime;
begin
  FMDateTime := DateTimeToFMDateTime(DateTime);
//  ShowDebug(FormatFMDateTime('mmm dd,yyyy@hh:nn', FMDateTime));
  result := FloatToStr(FMDateTime);
end;

function BoolToYESorNO(bIn: boolean): string;
begin
  if bIn then
    result := 'YES'
  else
    result := 'NO';
end;

procedure FreeStringList(mySL : TStringList);
var
  iIndex : integer;
begin
  try
    if Assigned(mySL) then
    begin
      try
        for iIndex := 0 to mySL.Count - 1 do
          if Assigned(mySL.Objects[iIndex]) then
            mySL.Objects[iIndex].Free;
      finally
        mySL.Free;
      end;
    end;
  except
    //eat error
  end;
end;


initialization

finalization
  TDLLImmunizations.FreeslLaunchedDLLImmunizations();

end.
