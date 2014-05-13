unit rImmunizations;

interface

uses SysUtils, Classes, Dialogs, Controls
    , ORFn, ORNet, uCore
    , rPCE, uPCE, uImmunizations
    ;

//general immunization test
procedure rTestThemAll(DFN: Integer);

//immunization stock management (vxDesktop?)
function rGetOrderableItems(): TStrings;
function rSaveImmLotInfo(ImmLot: TImmLot; tsiAddEdit: TStoreImm): boolean;

//immunization administration (vxCPRSChart)
function rGetPatientImmsOrderedToBeAdmin(DFN: Integer): TStrings;
function rGetImmLots(ORN: Integer): TStrings;
function rGetUserList(sFMEncounterDateTime : string): TStrings;
function rSaveImmAdministeredInfo(const sSaveArray: string): boolean;


implementation

uses ORClasses, Trpcb, rCore;

const
  RPC_GET_ORDERS     = 'VFD PXIM GET ORDERS';   //(DFN) list of active vaccinations for a patient.
  RPC_GET_LOTS       = 'VFD PXIM GET LOTS';     //(ORN) all active Lot #'s for a vaccine. Lot # ^ Manufacturer Name ^ Amount ^ Units
  RPC_GET_IMM_ORDERS = 'VFD PXIM GET IMM ORDERS';
  RPC_GET_ORDERABLE  = 'VFD PXIM GET ORDERABLE';//() list of valid Orderable Items for Vaccines (TYPE = IM100).
  RPC_STORE_LOT      = 'VFD PXIM STORE LOT';    //store/update a Vaccine Lot # stored inside the VFD IMMNIZATIONS (#21630.01) file.
  RPC_SET_IMM_ADMINISTERED='VFD PXIM ADMIN'; //This RPC will set a drug order as administered by setting the STOP or EXPIRATION DATE to NOW.  This will in effect expire the order when Outpatient or Inpatient Pharmacy normally does so.


//general immunization test

procedure rTestThemAll(DFN: Integer);
var
  slResults : TStringList;
  ORN : integer;
  procedure TestOneRPC(RPC_Name: string; RPC_Params: array of const);
  begin
//    ShowDebug('Testing: '+RPC_Name);
    CallV(RPC_Name, RPC_Params);
    slResults.AddStrings(RPCBrokerV.Results);
//    ShowDebug(RPC_Name +#13#10+ slResults.Text);
  end;
begin
  slResults := TStringList.Create;
  try
    //get all active immunization orders for a patient (DFN)
    slResults.Clear;
    TestOneRPC(RPC_GET_ORDERS, [DFN]);
    //get all available lots/manufacturers for the 1st immunization order
    if slResults.Count > 1 then
    begin
      ORN := StrToInt(piece(slResults[1], U, 1)); //index 0 is the count returned
      slResults.Clear;
      TestOneRPC(RPC_GET_LOTS, [ORN]);
    end;
    //get all orderables
    slResults.Clear;
    TestOneRPC(RPC_GET_ORDERABLE, []);
  finally
    slResults.Free;
  end;
end;


//immunization stock management (vxDesktop?)

function rGetOrderableItems(): TStrings;
{This RPC returns a list of valid Orderable Items for Vaccines (TYPE = IM100).}
//send: DFN ( Pointer to the PATIENT (#2) file.)
//returns:
//  LIST(0)=Total number of vaccination orders.
//  LIST(n)=Order Number IFN ^ Orderable Item Description
// or,
//  LIST(0)=-1^Error Text
begin
  CallV(RPC_GET_ORDERABLE, []);
  result := RPCBrokerV.Results;
//  ShowDebug(RPC_GET_ORDERABLE +#13#10+ result.Text);
end;

function rSaveImmLotInfo(ImmLot: TImmLot; tsiAddEdit: TStoreImm): boolean;
{This RPC will store/update a Vaccine Lot # stored inside the VFD
IMMNIZATIONS (#21630.01) file.}
//send order IEN,manufacturer,lot,etc to save the immunization lot/stock info
{send: Name: ARY
Sequence #: 1
IEN : 1
Type: LIST
MaxLength: 30
Mandatory: Yes
Description: Array of values and fields to update.
LIST(0) = "A" for Add, "E" for Edit ^ IEN to edit (blank for Add)
LIST(n) = Field # ^ Value where n = 1 or higher.
---
returns: Single value: 1^Success or -1^Error Text}
var
  iRecord : integer;
  procedure ImmLotAddParamMult(const sValue, {sFieldNum,} sFieldName: string);
  var
    sRecord : string;
  begin
    if sValue > '' then
    begin
      inc(iRecord);
      sRecord := IntToStr(iRecord);
      RPCBrokerV.Param[0].Mult['"'+sRecord+'"'] := sFieldName + '^' + sValue;
    end;
  end;
begin
  try
    RPCBrokerV.Param.Clear;
    RPCBrokerV.RemoteProcedure := RPC_STORE_LOT;
    RPCBrokerV.Param[0].PType := list;
    RPCBrokerV.Param[0].Value := '.X';
{VFD PXIM STORE LOT RPC in order to allow Adds and Updates to LOT # data in the
VFD IMMUNIZATIONS (#21630.01) file.  The first array subscript sent in to the
PC should be valued at zero (0) and contains two “^” delimited pieces:
(1) Literal “A” or “E” (‘A’ for Add, ‘E’ for Edit).
The second piece should be populated if the first piece is “E” only, and
contains the IEN of the Lot # to update.  You will get the IEN for the lot by
using the VFD PXIM GET LOTS RPC and looking at the first ^ piece of the returned
array for each lot retrieved.  You will have to keep track of this for editing
purposes once a lot # is selected to be updated. }
    iRecord := 0;
    if tsiAddEdit = tsiAdd then
      RPCBrokerV.Param[0].Mult['"0"'] := 'A'
    else
    if tsiAddEdit = tsiEdit then
      RPCBrokerV.Param[0].Mult['"0"'] := 'E^'+ImmLot.Lot;
    ImmLotAddParamMult(ImmLot.OrderableItem,{'.01',}'ORDERABLE ITEM');//RP101.43 0;1
    ImmLotAddParamMult(ImmLot.Lot,          {'.02',}'LOT NUMBER');    //RF       0;2
    ImmLotAddParamMult(ImmLot.Manufacturer, {'.03',}'MANUFACTURER');  //F        0;3
    ImmLotAddParamMult(ImmLot.ProductName,  {'.04',}'PRODUCT NAME');  //F        0;4
    ImmLotAddParamMult(ImmLot.CVX,          {'1',}  'CVX CODE');      //F        1;1
    ImmLotAddParamMult(ImmLot.MVX,          {'1.1',}'MVX CODE');      //F        1;2
    ImmLotAddParamMult(ImmLot.StartDate,    {'2',}  'START DATE');    //D        2;1
    ImmLotAddParamMult(ImmLot.ExpirationDate,{'2.1',}'EXPIRATION DATE');//D      2;2
    ImmLotAddParamMult(ImmLot.OverrideExp,  {'2.2',}'OVERRIDE EXPIRATION DATE');//S 2;3
    ImmLotAddParamMult(ImmLot.OutOfStock,   {'2.3',}'OUT OF STOCK?'); //S        2;4
    ImmLotAddParamMult(ImmLot.DosageQty,    {'3',}  'QUANTITY');      //F        3;1
    ImmLotAddParamMult(ImmLot.DosageUnit,   {'3.1',}'UNIT');          //F        3;2

    RPCBrokerV.call;
    Result := (RPCBrokerV.Results.Text = '1^Success');
//    ShowDebug(RPC_STORE_LOT + #13#10 + RPCBrokerV.Results.Text);
  except
    Result := FALSE;
  end;
end;


//immunization administration (vxCPRSChart)

function rGetPatientImmsOrderedToBeAdmin(DFN: Integer): TStrings;
{This RPC will return a list of acive vaccinations for a patient.}
//get patient's immunizations that have been ordered but not yet administered
//send patient DFN (Pointer to the PATIENT (#2) file.)
//returns:
//  LIST(0)=Total number of vaccination orders.
//  LIST(n)=Order Number IFN ^ Orderable Item Description
// or LIST(0)=-1^Error Text
begin
  CallV(RPC_GET_ORDERS, [DFN]);
  Result := RPCBrokerV.Results;
//  ShowDebug(RPC_GET_ORDERS +#13#10+ Result.Text);
end;

function rGetImmLots(ORN: Integer): TStrings;
//get the possible [pharmacy] immunization [lot] to be administered
//send order IEN (ORN) (The order number to select the Lot #'s from.)
//returns:
//   LIST(0) = Count of Active Lots
//   LIST(n) = Lot IEN ^ Lot # ^ Manufacturer Name ^ Amount ^ Units
//or LIST(0) = -1 ^ Error Text or 1 ^ Order Succesfully updated.
begin
  CallV(RPC_GET_LOTS, [ORN]);
  Result := RPCBrokerV.Results;
//  ShowDebug(RPC_GET_LOTS +#13#10+ Result.Text);
end;

function rGetUserList(sFMEncounterDateTime : string): TStrings;
begin
  result := SubSetOfUsersWithClass('', 1, sFMEncounterDateTime);
//  ShowDebug('ORWU NEWPERS'+#13#10+ RPCBrokerV.Results.Text);
end;

function rSaveImmAdministeredInfo(const sSaveArray: string): boolean;
var
  sResult : string;
begin
  CallV(RPC_SET_IMM_ADMINISTERED, [sSaveArray]);
  sResult := RPCBrokerV.Results.Text;
  Result := (Piece(sResult, U, 1) = '1');
  if Result then
//    ShowDebug(RPC_SET_IMM_ADMINISTERED +#13#10+ RPCBrokerV.Results.Text)
  else
    ShowMessage('Saving failed.  See the results below:'+#13#10+
                RPC_SET_IMM_ADMINISTERED +#13#10+
                RPCBrokerV.Results.Text  +#13#10+
                'We were looking for result of "1", but got '+Piece(sResult, U, 1));
end;


end.
