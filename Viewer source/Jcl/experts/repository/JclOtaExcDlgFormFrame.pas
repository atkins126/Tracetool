{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclOtaExcDlgFormFrame.pas.                                                  }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{         <outchy att users dott sourceforge dott net>                                             }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All rights reserved.     }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-01-26 12:32:00 +0100 (sam. 26 janv. 2008)                          $ }
{ Revision:      $Rev:: 2314                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaExcDlgFormFrame;

interface

{$I jcl.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  JclOtaExcDlgRepository, JclOtaWizardFrame;

type
  TJclOtaExcDlgFormPage = class(TJclWizardFrame)
    CheckBoxMail: TCheckBox;
    LabelEMailAddress: TLabel;
    EditEMail: TEdit;
    CheckBoxModalDialog: TCheckBox;
    CheckBoxSizeable: TCheckBox;
    EditSubject: TEdit;
    LabelSubject: TLabel;
    CheckBoxAutoScrollBars: TCheckBox;
    procedure CheckBoxMailClick(Sender: TObject);
  private
    FParams: TJclOtaExcDlgParams;
    procedure UpdateMailEdits;
  protected
    function GetSupportsNext: Boolean; override;
  public
    constructor Create(AOwner: TComponent; AParams: TJclOtaExcDlgParams); reintroduce;

    procedure PageActivated(Direction: TJclWizardDirection); override;
    procedure PageDesactivated(Direction: TJclWizardDirection); override;

    property Params: TJclOtaExcDlgParams read FParams write FParams;
  end;

implementation

{$R *.dfm}

uses
  JclOtaResources;

//=== { TJclOtaExcDlgFormPage } ==============================================

procedure TJclOtaExcDlgFormPage.CheckBoxMailClick(Sender: TObject);
begin
  UpdateMailEdits;
end;

constructor TJclOtaExcDlgFormPage.Create(AOwner: TComponent;
  AParams: TJclOtaExcDlgParams);
begin
  FParams := AParams;
  inherited Create(AOwner);

  Caption := RsExcDlgFormOptions;
  CheckBoxMail.Caption := RsDialogWithMailButton;
  LabelEMailAddress.Caption := RsEMail;
  LabelSubject.Caption := RsSubject;
  CheckBoxModalDialog.Caption := RsModalDialog;
  CheckBoxSizeable.Caption := RsSizeableDialog;
  CheckBoxAutoScrollBars.Caption := RsAutoScrollBars;
end;

function TJclOtaExcDlgFormPage.GetSupportsNext: Boolean;
begin
  Result := (not CheckBoxMail.Checked) or ((EditEMail.Text <> '') and (EditSubject.Text <> ''));
end;

procedure TJclOtaExcDlgFormPage.PageActivated(Direction: TJclWizardDirection);
begin
  inherited PageActivated(Direction);

  CheckBoxMail.Checked := Params.SendEMail;
  EditEMail.Text := Params.EMailAddress;
  EditSubject.Text := Params.EMailSubject;
  CheckBoxModalDialog.Checked := Params.ModalDialog;
  CheckBoxSizeable.Checked := Params.SizeableDialog;
  CheckBoxAutoScrollBars.Checked := Params.AutoScrollBars;

  UpdateMailEdits;
end;

procedure TJclOtaExcDlgFormPage.PageDesactivated(
  Direction: TJclWizardDirection);
begin
  inherited PageDesactivated(Direction);

  Params.SendEMail := CheckBoxMail.Checked;
  Params.EMailAddress := EditEMail.Text;
  Params.EMailSubject := EditSubject.Text;
  Params.ModalDialog := CheckBoxModalDialog.Checked;
  Params.SizeableDialog := CheckBoxSizeable.Checked;
  Params.AutoScrollBars := CheckBoxAutoScrollBars.Checked;
end;

procedure TJclOtaExcDlgFormPage.UpdateMailEdits;
begin
  if CheckBoxMail.Checked then
  begin
    EditEMail.Enabled := True;
    EditSubject.Enabled := True;
    EditEMail.Color := clWindow;
    EditSubject.Color := clWindow;
  end
  else
  begin
    EditEMail.Enabled := False;
    EditSubject.Enabled := False;
    EditEMail.ParentColor := True;
    EditSubject.ParentColor := True;
  end;
end;

end.
