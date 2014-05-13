unit fImmunizationAdministration;

//*****************************************************************************
//gkp added 2011-03
//*****************************************************************************

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VA508AccessibilityManager, Buttons, StdCtrls, ExtCtrls, JvgLabel,
  JvExControls, JvGradient, ORCtrls, ComCtrls, Mask, JvExMask, JvSpin,
  JvExComCtrls, JvDateTimePicker,
  uPCE, uCore, rPCE, fOrders, Trpcb, uImmunizations;

type
  TfrmImmunizationAdministration = class(TForm)
    cboImmManufacturer: TORComboBox;
    cboImmLot: TORComboBox;
    jlblSeries: TJvgLabel;
    jlblLot: TJvgLabel;
    cboImmSeries: TORComboBox;
    jlblManufacturer: TJvgLabel;
    Panel1: TPanel;
    btnSave: TBitBtn;
    btnCancel: TBitBtn;
    amgrMain: TVA508AccessibilityManager;
    JvGradient2: TJvGradient;
    JvgLabel4: TJvgLabel;
    cboImmName: TORComboBox;
    jlblUnit: TJvgLabel;
    cboImmDosageUnit: TORComboBox;
    jlblAmount: TJvgLabel;
    spnedtDosageAmount: TJvSpinEdit;
    jlblWho: TJvgLabel;
    cboImmAdminByWhom: TORComboBox;
    jlblStartDate: TJvgLabel;
    jdtAdminStartDate: TJvDateTimePicker;
    jlblStartTime: TJvgLabel;
    jdtAdminStartTime: TJvDateTimePicker;
    jlblProductName: TJvgLabel;
    cboImmProductName: TORComboBox;
    jlblEndDate: TJvgLabel;
    jdtAdminEndDate: TJvDateTimePicker;
    jlblEndTime: TJvgLabel;
    jdtAdminEndTime: TJvDateTimePicker;
    JvGradient1: TJvGradient;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cboImmNameChange(Sender: TObject);
    procedure cboImmManufacturerChange(Sender: TObject);
    procedure cboImmProductNameChange(Sender: TObject);
    procedure cboImmLotChange(Sender: TObject);
    procedure jdtAdminStartDateChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    ImmAdmin: TImmAdmin;
    slLots : TStringList;
    DefaultDosageQty : real;
    DefaultDosageUnit : string;
    procedure ClearAndDefaultForm();
    procedure ClearAndDefaultLotFields();
    procedure LoadActiveImmunizations;
    procedure LoadByWhoms();
    procedure GetImmLots(iOrderIEN: integer);
    function FillLots(iOrderIEN: integer) : boolean;
    procedure FilterNext(cboFilter, cboDestToBeFiltered: TORComboBox);
    function sGetVFDImmStockIEN: string;
    function FinalChecks: boolean;
    procedure CleanClose;
  protected
  public
    Patient: TPatient;
    User: TUser;
    Encounter: TEncounter;
    Order: TfrmOrders;
  end;

var
  frmImmunizationAdministration: TfrmImmunizationAdministration;


implementation

{$R *.dfm}

uses fPCEOther, ORFn, fEncounterFrame, ORNet, fCover;


procedure TfrmImmunizationAdministration.FormCreate(Sender: TObject);
begin
//  ShowDebug('I am in FormCreate');
end;

procedure TfrmImmunizationAdministration.FormShow(Sender: TObject);
begin
  ClearAndDefaultForm();
  LoadActiveImmunizations();
  LoadByWhoms();
  StatusBar1.SimpleText := 'version: ' +
              TDLLImmunizations.GetVersion(TImmAdministration.GetDLLFilename());
end;

procedure TfrmImmunizationAdministration.ClearAndDefaultForm();
begin
  DefaultDosageQty := 0;
  DefaultDosageUnit := '';
  cboImmName.Clear;

  ClearAndDefaultLotFields();

  cboImmAdminByWhom.Clear;
  jdtAdminStartDate.Date := Trunc(Now());
  jdtAdminStartTime.Time := Now();
  jdtAdminEndDate.Date := Trunc(Now());
  jdtAdminEndTime.Time := Now();
end;

procedure TfrmImmunizationAdministration.ClearAndDefaultLotFields();
begin
  DefaultDosageQty := 0;
  DefaultDosageUnit := '';
  cboImmManufacturer.Clear;
  cboImmProductName.Clear;
  cboImmLot.Clear;
  spnedtDosageAmount.Value := spnedtDosageAmount.MinValue;
  cboImmDosageUnit.Clear;
end;

procedure TfrmImmunizationAdministration.LoadActiveImmunizations;
//adapted from fPCEBaseMain.btnOtherClick()
var
  iPatientDFN : integer;
begin
  iPatientDFN := StrToIntDef(Patient.DFN,0);
  cboImmName.Items.AddStrings(TImmAdministration.GetPatientImmsOrderedToBeAdmin(
                              iPatientDFN));
  if cboImmName.Items.Count = 1 then
  begin
    cboImmName.ItemIndex := 0;
    cboImmNameChange(Self);
  end;
end;

procedure TfrmImmunizationAdministration.LoadByWhoms();
//adapted from fPCEBaseMain.btnOtherClick()
var
  sFMDateTime : string;
  sUserName : string;
  iIndex : integer;
begin
  sFMDateTime := '';
  cboImmAdminByWhom.Items.AddStrings(TImmAdministration.GetUserList(sFMDateTime));
  if cboImmAdminByWhom.Items.Count = 1 then
    cboImmAdminByWhom.ItemIndex := 0
  else
  begin
    sUserName := MixedCase(User.Name);
    iIndex := cboImmAdminByWhom.Items.IndexOf(sUserName);
    if iIndex >= 0 then
      cboImmAdminByWhom.ItemIndex := iIndex;
  end;
end;

procedure TfrmImmunizationAdministration.cboImmNameChange(Sender: TObject);
var
  bEntered : boolean;
  IEN : integer;
begin
  try
    FreeStringList(slLots);
  finally
    slLots := nil;
    bEntered := (cboImmName.Text <> ''); //if an immunization order was selected
    ClearAndDefaultLotFields();
    //enable other controls
    jlblManufacturer.Enabled := bEntered;
    cboImmManufacturer.Enabled := bEntered;
    if bEntered then
    begin
      //get order IEN from selected immunization name
      IEN := cboImmName.ItemIEN; //Order.IEN
      //fill other comboboxes (manufacturer, lot, etc) for the selected immunization order
      FillLots(IEN);
      if jdtAdminStartDate.Date = 0 then
        jdtAdminStartDate.Date := Date;
    end;
  end;
end;

procedure TfrmImmunizationAdministration.GetImmLots(iOrderIEN: integer);
begin
  if not Assigned(slLots) then
  begin
    slLots := TStringList.Create;
    TImmAdministration.GetImmLots(slLots, iOrderIEN);
  end;
end;

function TfrmImmunizationAdministration.FillLots(iOrderIEN: integer) : boolean;
//returns True if any dropdowns changed
var
  iIndex : integer;
  ImmAdmLot : TImmAdmLot;
  sLastPiece : string;
  sAdd : string;
begin
  GetImmLots(iOrderIEN);
  try
    result := False;
    ImmAdmLot := nil;
    for iIndex := 0 to slLots.Count - 1 do
    begin
      ImmAdmLot := TImmAdmLot(slLots.Objects[iIndex]);
      //
      sLastPiece := ImmAdmLot.Manufacturer;
      sAdd := ImmAdmLot.Code + cboImmManufacturer.Delimiter + ImmAdmLot.Manufacturer;
      if cboImmManufacturer.Items.IndexOf(sLastPiece) < 0 then
      begin
        cboImmManufacturer.Items.Add(sAdd);
        result := True;
      end;
      //
      sLastPiece := ImmAdmLot.ProductName;
      sAdd := ImmAdmLot.Code +cboImmProductName.Delimiter+ImmAdmLot.Manufacturer+cboImmProductName.Delimiter+ImmAdmLot.ProductName;
      if cboImmProductName.Items.IndexOf(sLastPiece) < 0 then
      begin
        cboImmProductName.Items.Add(sAdd);
        result := True;
      end;
      //
      sLastPiece := ImmAdmLot.Lot;
      sAdd := ImmAdmLot.Code +cboImmLot.Delimiter+ImmAdmLot.Manufacturer+cboImmLot.Delimiter+ImmAdmLot.ProductName+cboImmLot.Delimiter+ImmAdmLot.Lot;
      if cboImmLot.Items.IndexOf(sLastPiece) < 0 then
      begin
        cboImmLot.Items.Add(sAdd);
        result := True;
      end;
      //
      spnedtDosageAmount.Value := StrToFloatDef(ImmAdmLot.DosageQty, 0);
      //
      sLastPiece := ImmAdmLot.DosageUnit;
      if cboImmDosageUnit.Items.IndexOf(sLastPiece) < 0 then
      begin
        cboImmDosageUnit.Items.Add(sLastPiece);
        result := True;
      end;
    end;
    if Assigned(ImmAdmLot) then
      cboImmDosageUnit.Text := ImmAdmLot.DosageUnit
    else
      cboImmDosageUnit.Text := '';
    DefaultDosageQty := spnedtDosageAmount.Value;
    DefaultDosageUnit := cboImmDosageUnit.Text;
    //default cboImmManufacturer if only 1 option (because we dont allow free-text entry for this field)
    if result and (cboImmManufacturer.Items.Count = 1) then
    begin
      cboImmManufacturer.ItemIndex := 0;
      cboImmManufacturerChange(cboImmLot);
    end;
  finally
  end;
end;

procedure TfrmImmunizationAdministration.FilterNext(cboFilter, cboDestToBeFiltered: TORComboBox);
var
  sFilter : string;
  sDestPiece : string;
  iPiecesFilter : integer;
  iIndex : integer;
begin
  sFilter := cboFilter.Text;
  iPiecesFilter := StrToIntDef(cboFilter.Pieces,2);
  iIndex := cboDestToBeFiltered.Items.Count - 1;
  while iIndex >= 0 do
  begin
    sDestPiece := Piece(cboDestToBeFiltered.Items[iIndex], U, iPiecesFilter);
    if sFilter <> sDestPiece then
      cboDestToBeFiltered.Items.Delete(iIndex);
    dec(iIndex);
  end;
end;

procedure TfrmImmunizationAdministration.cboImmManufacturerChange(Sender: TObject);
var
  bEntered : boolean;
begin
  bEntered := (cboImmManufacturer.Text <> '');
  //enable other controls
  jlblProductName.Enabled := bEntered;
  cboImmProductName.Enabled := bEntered;
  //filter rest of dropdowns based upon manufacturer selection
  if bEntered then
  begin
    FillLots(cboImmName.ItemIEN); //reset all lot possibilities in case manufacturer was changed
    FilterNext(cboImmManufacturer, cboImmProductName);
    FilterNext(cboImmManufacturer, cboImmLot);
  end;
  //default cboImmProductName if only 1 option (because we dont allow free-text entry for this field)
  if bEntered and (cboImmProductName.Items.Count = 1) then
  begin
    cboImmProductName.ItemIndex := 0;
    cboImmProductNameChange(cboImmManufacturer);
  end;
end;

procedure TfrmImmunizationAdministration.cboImmProductNameChange(Sender: TObject);
var
  bEntered : boolean;
begin
  bEntered := (cboImmProductName.Text <> '');
  //enable other controls
  jlblLot.Enabled := bEntered;
  cboImmLot.Enabled := bEntered;
  //filter rest of dropdowns based upon manufacturer selection
  if bEntered then
  begin
    FillLots(cboImmName.ItemIEN); //reset all lot possibilities in case productname was changed
    FilterNext(cboImmManufacturer, cboImmProductName);
    FilterNext(cboImmManufacturer, cboImmLot);
    FilterNext(cboImmProductName, cboImmLot);
  end;
  //default cboImmLot if only 1 option (because we dont allow free-text entry for this field)
  if bEntered and (cboImmLot.Items.Count = 1) then
  begin
    cboImmLot.ItemIndex := 0;
    cboImmLotChange(cboImmProductName);
  end;
end;

procedure TfrmImmunizationAdministration.cboImmLotChange(Sender: TObject);
var
  bEntered : boolean;
begin
  bEntered := (cboImmLot.Text <> '');
  //enable other controls
  jlblAmount.Enabled := bEntered;
  spnedtDosageAmount.Enabled := bEntered;
  jlblUnit.Enabled := bEntered;
  cboImmDosageUnit.Enabled := bEntered;
  jlblWho.Enabled := bEntered;
  cboImmAdminByWhom.Enabled := bEntered;
  jlblStartDate.Enabled := bEntered;
  jdtAdminStartDate.Enabled := bEntered;
  jlblStartTime.Enabled := bEntered;
  jdtAdminStartTime.Enabled := bEntered;
  jlblEndDate.Enabled := bEntered;
  jdtAdminEndDate.Enabled := bEntered;
  jlblEndTime.Enabled := bEntered;
  jdtAdminEndTime.Enabled := bEntered;
  btnSave.Enabled := bEntered;

  if bEntered then
  begin
    sGetVFDImmStockIEN();
    if (DefaultDosageQty > 0) and (DefaultDosageUnit <> '') then
    begin
      spnedtDosageAmount.Value := DefaultDosageQty;
      cboImmDosageUnit.Text := DefaultDosageUnit;
    end;
  end;
end;

procedure TfrmImmunizationAdministration.jdtAdminStartDateChange(Sender: TObject);
begin
  if jdtAdminStartDate.Date > jdtAdminEndDate.Date then
    jdtAdminEndDate.Date := jdtAdminStartDate.Date;
end;

function TfrmImmunizationAdministration.sGetVFDImmStockIEN: string;
//get stock ien from manufacturer, product name, and lot #
var
  iIndex : integer;
  ImmAdmLot : TImmAdmLot;
begin
  Assert(cboImmManufacturer.Text <> '', 'Manufacturer is empty.');
  Assert(cboImmProductName.Text <> '', 'Product Name is empty.');
  Assert(cboImmLot.Text <> '', 'Lot is empty.');
  GetImmLots(cboImmName.ItemID);
//  ShowDebug(slLots.Text);
  result := '';
  iIndex := 0;
  while (result = '') and (iIndex < slLots.Count) do
  begin
    ImmAdmLot := TImmAdmLot(slLots.Objects[iIndex]);
    if (ImmAdmLot.Manufacturer = cboImmManufacturer.Text) then
      if (ImmAdmLot.ProductName = cboImmProductName.Text) then
        if (ImmAdmLot.Lot = cboImmLot.Text) then
        begin
          result := ImmAdmLot.Code;
          DefaultDosageQty := StrToFloat(ImmAdmLot.DosageQty);
          DefaultDosageUnit := ImmAdmLot.DosageUnit;
        end;
    inc(iIndex);
  end;
  if (result = '') then
    MessageDlg('There was an error finding immunization stock.'+#13#10+
               'Please select manufacturer, product name, and lot from their dropdowns.',
               mtError, [mbOK], 0);
end;

function TfrmImmunizationAdministration.FinalChecks: boolean;
//returns True if passes validation checks and fills the object to save
var
  dtStart, dtEnd : TDateTime;
  VFDLotIEN  : string;
begin
  //start with fresh object
  if Assigned(ImmAdmin) then
    ImmAdmin.Free;
  ImmAdmin := TImmAdmin.Create;
  //
  try
    if cboImmName.ItemIndex < 0 then
    begin
      result := False;
      MessageDlg('Ordered immunization must be selected. Please adjust.', mtError, [mbOK], 0);
      cboImmName.SetFocus;
      exit;
    end;
    ImmAdmin.OrderIEN := cboImmName.ItemID;

    if cboImmManufacturer.ItemIndex < 0 then
    begin
      result := False;
      MessageDlg('Manufacturer must be selected. Please adjust.', mtError, [mbOK], 0);
      cboImmManufacturer.SetFocus;
      exit;
    end;
    ImmAdmin.Manufacturer := cboImmManufacturer.Text;

    if cboImmProductName.ItemIndex < 0 then
    begin
      result := False;
      MessageDlg('Product name must be selected. Please adjust.', mtError, [mbOK], 0);
      cboImmProductName.SetFocus;
      exit;
    end;
    ImmAdmin.ProductName := cboImmProductName.Text;

    if cboImmLot.ItemIndex < 0 then
    begin
      result := False;
      MessageDlg('Immunization lot [number] must be selected. Please adjust.', mtError, [mbOK], 0);
      cboImmLot.SetFocus;
      exit;
    end;
    ImmAdmin.Lot := cboImmLot.Text;

    VFDLotIEN  := sGetVFDImmStockIEN();

    if cboImmAdminByWhom.ItemIndex < 0 then
    begin
      result := False;
      MessageDlg('The person who administered the immunization must be selected. Please adjust.', mtError, [mbOK], 0);
      cboImmAdminByWhom.SetFocus;
      exit;
    end;
    ImmAdmin.Who := cboImmAdminByWhom.ItemID;

    ImmAdmin.DosageQty := FloatToStr(spnedtDosageAmount.Value);
    ImmAdmin.DosageUnit := cboImmDosageUnit.Text;
    if (DefaultDosageQty > 0) and (DefaultDosageUnit <> '') and
      (ImmAdmin.DosageQty + ' ' + ImmAdmin.DosageUnit <>
      FloatToStr(DefaultDosageQty) + ' ' + DefaultDosageUnit) then
    begin
      if MessageDlg('Dosage of "'+ImmAdmin.DosageQty+' '+ImmAdmin.DosageUnit+'" does'+ #13#10+
                    'not equal "'+FloatToStr(DefaultDosageQty)+' '+DefaultDosageUnit+'".'+ #13#10+
                    'Would you like to continue with current dosage (and not use defaults)?',
                    mtConfirmation, [mbYes, mbNo], 0) = mrNo then //if dont continue with non-defaults
      begin
        if MessageDlg('Would you like to change the current dosage to use defaults?',
                      mtConfirmation, [mbYes, mbNo], 0) = mrYes then //if change to defaults
        begin
          spnedtDosageAmount.Value := DefaultDosageQty;
          cboImmDosageUnit.Text := DefaultDosageUnit;
          ImmAdmin.DosageQty := FloatToStr(spnedtDosageAmount.Value);
          ImmAdmin.DosageUnit := cboImmDosageUnit.Text;
        end
        else
          //if defaults not selected and not using defaults
        if MessageDlg('Would you like to change the current dosage?',
                      mtConfirmation, [mbYes, mbNo], 0) = mrYes then //if change
        begin
          result := False;
          exit;
        end;
      end;
    end;

    //datetimes
    jdtAdminStartTime.Date := jdtAdminStartDate.Date;
    dtStart := DateTimeDropSeconds(jdtAdminStartTime.Date);
//    ShowDebug(DateTimeToStr(dtStart));
    jdtAdminEndTime.Date := jdtAdminEndDate.Date;
    dtEnd := DateTimeDropSeconds(jdtAdminEndTime.Date);
    if (dtEnd < dtStart) then
    begin
      result := False;
      MessageDlg('End time cannot be before start time. Please adjust.', mtError, [mbOK], 0);
      jdtAdminEndTime.SetFocus;
      exit;
    end;
    if (dtEnd > Now()) then
    begin
      result := False;
      MessageDlg('End time cannot be in the future. Please adjust.', mtError, [mbOK], 0);
      jdtAdminEndTime.SetFocus;
      exit;
    end;
    ImmAdmin.StartDateTime := sDateTimeToFMDateTime(dtStart);
    ImmAdmin.EndDateTime := sDateTimeToFMDateTime(dtEnd);

    //referential pointers
    ImmAdmin.LocationIEN := IntToStr(Encounter.Location);
    ImmAdmin.VFDStockIEN := VFDLotIEN;
    result := (ImmAdmin.VFDStockIEN <> '');
  except
    result := False;
  end;
end;

procedure TfrmImmunizationAdministration.btnSaveClick(Sender: TObject);
begin
  //final validation checks
  if FinalChecks() then //fills ImmAdmin object
    //make RPC call to save the immunization order
    if TImmAdministration.SaveImmAdministeredInfo(ImmAdmin) then
    begin
      ShowMessage('The immunization administration was successfully recorded.');
      //refresh the display in the calling form
      try
              if Assigned(frmCover) then
                if frmCover.Showing then
                  frmCover.DisplayPage;
              if Assigned(Order) then
                if Order.Showing then
                  Order.DisplayPage;
        //handle another immunization in list (if any) or close form
        if cboImmName.Items.Count > 1 then
        begin
          cboImmName.Items.Delete(cboImmName.ItemIndex);
          if MessageDlg('Would you like to administer another immunization?',
                      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          begin
            cboImmName.Text := '';
            cboImmName.ItemIndex := -1;
            cboImmNameChange(Sender);
          end
          else
            CleanClose;
        end
        else
          CleanClose;
      except
        //eat any error
      end;
    end
    else
      MessageDlg('There was an error saving the immunization record.', mtInformation, [mbOK], 0);
end;

procedure TfrmImmunizationAdministration.btnCancelClick(Sender: TObject);
begin
  CleanClose;
end;

procedure TfrmImmunizationAdministration.CleanClose;
begin
  try
    FreeStringList(slLots);
    slLots := nil;
    if Assigned(ImmAdmin) then
    begin
      ImmAdmin.Free;
      ImmAdmin := nil;
    end;
  except
    //eat error
  end;
  Close;
end;

end.
