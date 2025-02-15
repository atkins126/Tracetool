{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JediInstallerMain.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Andreas Hausladen (ahuser)                                                                     }
{   Robert Rossmair (rrossmair) - crossplatform & BCB support, refactoring                         }
{   Florent Ouchet (outchy) - new installer core                                                   }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-10-15 13:18:28 +0200 (lun. 15 oct. 2007)                           $ }
{ Revision:      $Rev:: 2197                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

{$IFNDEF PROTOTYPE}
{$IFDEF VCL}
unit JediGUIMain;
{$ELSE VisualCLX}
unit QJediGUIMain;
{$ENDIF VisualCLX}
{$ENDIF ~PROTOTYPE}

{$I jcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages, CommCtrl,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes,
  {$IFDEF VisualCLX}
  Types,
  Qt, QGraphics, QControls, QForms, QDialogs, QStdCtrls, QExtCtrls, QMenus, QButtons, QComCtrls, QImgList,
  {$ELSE}
  Graphics, Forms, Controls, Dialogs, StdCtrls, ExtCtrls, Menus, Buttons, ComCtrls, ImgList,
  {$ENDIF}
  JclBorlandTools, JclContainerIntf, JediInstall;

const
  WM_AFTERSHOW = WM_USER + 10;

type
  TMainForm = class(TForm, IJediInstallGUI)
    InstallBtn: TBitBtn;
    UninstallBtn: TBitBtn;
    QuitBtn: TBitBtn;
    JediImage: TImage;
    TitlePanel: TPanel;
    Title: TLabel;
    ProductsPageControl: TPageControl;
    StatusBevel: TBevel;
    StatusLabel: TLabel;
    Bevel1: TBevel;
    ProgressBar: TProgressBar;
    ImageList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure QuitBtnClick(Sender: TObject);
    procedure InstallBtnClick(Sender: TObject);
    procedure UninstallBtnClick(Sender: TObject);
    procedure JediImageClick(Sender: TObject);
  protected
    FPages: IJclIntfList;
    FAutoAcceptDialogs: TDialogTypes;
    FAutoCloseOnFailure: Boolean;
    FAutoCloseOnSuccess: Boolean;
    FAutoInstall: Boolean;
    FAutoUninstall: Boolean;
    procedure HandleException(Sender: TObject; E: Exception);
    procedure SetFrameIcon(Sender: TObject; const FileName: string);
    procedure WMAfterShow(var Message: TMessage); Message WM_AFTERSHOW;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowFeatureHint(var HintStr: {$IFDEF VisualCLX}WideString{$ELSE ~VisualCLX}string{$ENDIF ~VisualCLX}; var CanShow: Boolean;
      var HintInfo: THintInfo);
    // IJediInstallGUI
    function Dialog(const Text: string; DialogType: TDialogType = dtInformation;
      Options: TDialogResponses = [drOK]): TDialogResponse;
    function CreateReadmePage: IJediReadmePage;
    function CreateInstallPage: IJediInstallPage;
    function CreateProfilesPage: IJediProfilesPage;
    function GetPageCount: Integer;
    function GetPage(Index: Integer): IJediPage;
    function GetStatus: string;
    procedure SetStatus(const Value: string);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetProgress: Integer;
    procedure SetProgress(Value: Integer);
    function GetAutoAcceptDialogs: TDialogTypes;
    procedure SetAutoAcceptDialogs(Value: TDialogTypes);
    function GetAutoCloseOnFailure: Boolean;
    procedure SetAutoCloseOnFailure(Value: Boolean);
    function GetAutoCloseOnSuccess: Boolean;
    procedure SetAutoCloseOnSuccess(Value: Boolean);
    function GetAutoInstall: Boolean;
    procedure SetAutoInstall(Value: Boolean);
    function GetAutoUninstall: Boolean;
    procedure SetAutoUninstall(Value: Boolean);
    procedure Execute;
  end;

implementation

{$IFDEF VCL}
{$R *.dfm}
{$ELSE VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

uses
  {$IFDEF UNIX}
  Libc,
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  FileCtrl,
  JclDebug, JclShell, JediGUIProfiles,
  {$ENDIF MSWINDOWS}
  JclBase, JclFileUtils, JclStrings, JclSysInfo, JclSysUtils, JclArrayLists,
  {$IFDEF VisualCLX}
  QJediGUIReadme, QJediGUIInstall;
  {$ELSE ~VisualCLX}
  JediGUIReadme, JediGUIInstall;
  {$ENDIF ~VisualCLX}

const
  DelphiJediURL     = 'http://www.delphi-jedi.org/';

function CreateMainForm: IJediInstallGUI;
var
  MainForm: TMainForm;
begin
  Application.CreateForm(TMainForm, MainForm);
  Result := MainForm;
end;

//=== { TMainForm } ==========================================================

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPages := TJclIntfArrayList.Create(5);
end;

destructor TMainForm.Destroy;
begin
  FPages := nil;
  inherited Destroy;
end;

procedure TMainForm.HandleException(Sender: TObject; E: Exception);
begin
  if E is EJediInstallInitFailure then
  begin
    Dialog(E.Message, dtError);
    Application.ShowMainForm := False;
    Application.Terminate;
  end
  else
    Application.ShowException(E);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnException := HandleException;
  JediImage.Hint := DelphiJediURL;

  SetStatus('');

  {$IFDEF VCL}
  TitlePanel.DoubleBuffered := True;
  {$IFDEF COMPILER7_UP}
  TitlePanel.ParentBackground := False;
  {$ENDIF}
  {$ELSE}
  //WindowState := wsMaximized; // wouldn't work in Form resource
  {$ENDIF}
  Application.HintPause := 500;
  Application.OnShowHint := ShowFeatureHint;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  InstallCore.Close;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_AFTERSHOW, 0, 0);
end;

procedure TMainForm.ShowFeatureHint(var HintStr: {$IFDEF VisualCLX}WideString{$ELSE ~VisualCLX}string{$ENDIF ~VisualCLX};
  var CanShow: Boolean; var HintInfo: THintInfo);
var
  ATabSheet: TTabSheet;
  ScreenPos: TPoint;
begin
  if HintStr = '' then
  begin
    ScreenPos := HintInfo.HintControl.ClientToScreen(HintInfo.CursorPos);
    ATabSheet := ProductsPageControl.ActivePage;
    HintStr := (FPages.GetObject(ATabSheet.PageIndex) as IJediPage).GetHintAtPos(ScreenPos.X, ScreenPos.Y);
    HintInfo.ReshowTimeout := 100;
  end;
  CanShow := HintStr <> '';
end;

procedure TMainForm.SetFrameIcon(Sender: TObject; const FileName: string);
{$IFDEF MSWINDOWS}
var
  IconHandle: HICON;
  ModuleHandle: THandle;
  ATabSheet: TTabSheet;
{$ENDIF MSWINDOWS}
begin
  {$IFDEF MSWINDOWS}
  ATabSheet := (Sender as TInstallFrame).Parent as TTabSheet;

  IconHandle := 0;

  if SameText(ExtractFileName(FileName), '.ico') then
    IconHandle := LoadImage(0, PChar(FileName), IMAGE_ICON, ImageList.Width, ImageList.Height,
      LR_LOADFROMFILE or LR_LOADTRANSPARENT)
  else
  begin
    ModuleHandle := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE or DONT_RESOLVE_DLL_REFERENCES);
    if ModuleHandle <> 0 then
    try
      IconHandle := LoadImage(ModuleHandle, 'MAINICON', IMAGE_ICON, ImageList.Width, ImageList.Height,
        LR_LOADTRANSPARENT);
    finally
      FreeLibrary(ModuleHandle);
    end;
  end;
  if IconHandle <> 0 then
  try
    ATabSheet.ImageIndex := ImageList_AddIcon(ImageList.Handle, IconHandle);
  finally
    DestroyIcon(IconHandle);
  end;
  {$ENDIF MSWINDOWS}
end;

procedure TMainForm.QuitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.InstallBtnClick(Sender: TObject);
var
  Success: Boolean;
begin
  ProgressBar.Position := 0;
  ProgressBar.Visible := True;
  Screen.Cursor := crHourGlass;
  try
    Success := InstallCore.Install;
    if (Success and FAutoCloseOnSuccess) or (not Success and FAutoCloseOnFailure) then
      Close;
  finally
    ProgressBar.Visible := False;
    Screen.Cursor := crDefault;
  end;
  QuitBtn.SetFocus;
end;

procedure TMainForm.UninstallBtnClick(Sender: TObject);
var
  Success: Boolean;
begin
  ProgressBar.Position := 0;
  ProgressBar.Visible := True;
  Screen.Cursor := crHourGlass;
  try
    Success := InstallCore.Uninstall;
    if (Success and FAutoCloseOnSuccess) or (not Success and FAutoCloseOnFailure) then
      Close;
  finally
    ProgressBar.Visible := False;
    Screen.Cursor := crDefault;
  end;
  QuitBtn.SetFocus;
end;

procedure TMainForm.WMAfterShow(var Message: TMessage);
begin
  if FAutoInstall then
    InstallBtnClick(InstallBtn)
  else
  if FAutoUninstall then
    UninstallBtnClick(UninstallBtn);
end;

procedure TMainForm.JediImageClick(Sender: TObject);
begin
  { TODO : implement for Unix }
  {$IFDEF MSWINDOWS}
  ShellExecEx(DelphiJediURL);
  {$ENDIF MSWINDOWS}
end;

function TMainForm.Dialog(const Text: string; DialogType: TDialogType = dtInformation;
  Options: TDialogResponses = [drOK]): TDialogResponse;
const
  DlgType: array[TDialogType] of TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation);
  DlgButton: array[TDialogResponse] of TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel);
  DlgResult: array[TDialogResponse] of Word = (mrYes, mrNo, mrOK, mrCancel);
var
  Buttons: TMsgDlgButtons;
  Res: Integer;
  OldCursor: TCursor;
  DialogResponse: TDialogResponse;
begin
  if DialogType in FAutoAcceptDialogs then
  begin
    for DialogResponse := Low(TDialogResponse) to High(TDialogResponse) do
      if DialogResponse in Options then
    begin
      Result := DialogResponse;
      Exit;
    end;
  end;
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crDefault;
    Buttons := [];
    for Result := Low(TDialogResponse) to High(TDialogResponse) do
      if Result in Options then
        Include(Buttons, DlgButton[Result]);
    Res := MessageDlg(Text, DlgType[DialogType], Buttons, 0);
    for Result := Low(TDialogResponse) to High(TDialogResponse) do
      if DlgResult[Result] = Res then
        Break;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

function TMainForm.CreateReadmePage: IJediReadmePage;
var
  AReadmeFrame: TReadmeFrame;
  ATabSheet: TTabSheet;
begin
  ATabSheet := TTabSheet.Create(Self);
  ATabSheet.PageControl := ProductsPageControl;
  ATabSheet.ImageIndex := -1;

  AReadmeFrame := TReadmeFrame.Create(Self);
  AReadmeFrame.Parent := ATabSheet;
  AReadmeFrame.Align := alClient;
  AReadmeFrame.Name := '';

  Result := AReadmeFrame;
  FPages.Add(Result);
end;

function TMainForm.CreateInstallPage: IJediInstallPage;
var
  AInstallFrame: TInstallFrame;
  ATabSheet: TTabSheet;
begin
  ATabSheet := TTabSheet.Create(Self);
  ATabSheet.PageControl := ProductsPageControl;
  ATabSheet.ImageIndex := -1;

  AInstallFrame := TInstallFrame.Create(Self);
  AInstallFrame.Parent := ATabSheet;
  AInstallFrame.Align := alClient;
  AInstallFrame.TreeView.Images := ImageList;
  AInstallFrame.Name := '';
  AInstallFrame.OnSetIcon := SetFrameIcon;

  Result := AInstallFrame;
  FPages.Add(Result);
end;

function TMainForm.CreateProfilesPage: IJediProfilesPage;
var
  AProfilesFrame: TProfilesFrame;
  ATabSheet: TTabSheet;
begin
  ATabSheet := TTabSheet.Create(Self);
  ATabSheet.PageControl := ProductsPageControl;
  ATabSheet.ImageIndex := -1;

  AProfilesFrame := TProfilesFrame.Create(Self);
  AProfilesFrame.Parent := ATabSheet;
  AProfilesFrame.Align := alClient;
  AProfilesFrame.Name := '';

  Result := AProfilesFrame;
  FPages.Add(Result);
end;

function TMainForm.GetPageCount: Integer;
begin
  Result := FPages.Size;
end;

function TMainForm.GetPage(Index: Integer): IJediPage;
begin
  Result := FPages.GetObject(Index) as IJediPage;
end;

function TMainForm.GetStatus: string;
begin
  Result := StatusLabel.Caption;
end;

procedure TMainForm.SetStatus(const Value: string);
begin
  if Value = '' then
  begin
    StatusBevel.Visible := False;
    StatusLabel.Visible := False;
  end
  else
  begin
    StatusLabel.Caption := Value;
    StatusBevel.Visible := True;
    StatusLabel.Visible := True;
  end;
  Application.ProcessMessages;  //Update;
end;

function TMainForm.GetAutoAcceptDialogs: TDialogTypes;
begin
  Result := FAutoAcceptDialogs;
end;

function TMainForm.GetAutoCloseOnFailure: Boolean;
begin
  Result := FAutoCloseOnFailure;
end;

function TMainForm.GetAutoCloseOnSuccess: Boolean;
begin
  Result := FAutoCloseOnSuccess;
end;

function TMainForm.GetAutoInstall: Boolean;
begin
  Result := FAutoInstall;
end;

function TMainForm.GetAutoUninstall: Boolean;
begin
  Result := FAutoUninstall;
end;

function TMainForm.GetCaption: string;
begin
  Result := Caption;
end;

procedure TMainForm.SetAutoAcceptDialogs(Value: TDialogTypes);
begin
  FAutoAcceptDialogs := Value;
end;

procedure TMainForm.SetAutoCloseOnFailure(Value: Boolean);
begin
  FAutoCloseOnFailure := Value;
end;

procedure TMainForm.SetAutoCloseOnSuccess(Value: Boolean);
begin
  FAutoCloseOnSuccess := Value;
end;

procedure TMainForm.SetAutoInstall(Value: Boolean);
begin
  FAutoInstall := Value;
end;

procedure TMainForm.SetAutoUninstall(Value: Boolean);
begin
  FAutoUninstall := Value;
end;

procedure TMainForm.SetCaption(const Value: string);
begin
  Caption := Value;
end;

function TMainForm.GetProgress: Integer;
begin
  Result := ProgressBar.Position;
end;

procedure TMainForm.SetProgress(Value: Integer);
begin
  ProgressBar.Position := Value;
end;

procedure TMainForm.Execute;
begin
  Application.Run;
end;

initialization

InstallCore.InstallGUICreator := CreateMainForm;

end.
