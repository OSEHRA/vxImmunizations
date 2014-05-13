library ImmunizationAdministration;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  ShareMem,
  SysUtils,
  Classes,
  Dialogs,
  uCore,
  fOrders,
  Trpcb,
  ORNet,
  fImmunizationAdministration in 'fImmunizationAdministration.pas' {frmImmunizationAdministration},
  uImmunizations in '..\uImmunizations.pas',
  rImmunizations in 'rImmunizations.pas';

{$R *.res}

{1}function ImmunizationDialogLauncher(ThisPatient: TPatient; ThisUser: TUser;
                            ThisEncounter: TEncounter; ThisfOrder: TfrmOrders;
                            ThisBroker: TRPCBroker): boolean; stdcall; export;
var
  frmImmunizationAdministration: TfrmImmunizationAdministration; 
  iPatientDFN : integer;
begin
  try
    Result := false;
    RPCBrokerV := ThisBroker;
    //check whether to show form or not
    iPatientDFN := StrToIntDef(ThisPatient.DFN,0);
    if not TImmAdministration.bAnyPatientActiveImmOrders(iPatientDFN) then
      exit;
    //there are active immunization orders, so continue
    frmImmunizationAdministration := TfrmImmunizationAdministration.Create(ThisfOrder);
    frmImmunizationAdministration.Patient   := ThisPatient;
    frmImmunizationAdministration.User      := ThisUser;
    frmImmunizationAdministration.Encounter := ThisEncounter;
    frmImmunizationAdministration.Show;
//    ShowDebug('Just shown');
    Result := true;
  except on exception do
    Result := false;
 end;
end;

exports
 ImmunizationDialogLauncher;

begin
end.
