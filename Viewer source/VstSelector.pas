unit VstSelector;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, contnrs,
  VirtualTrees,
  VirtualTrees.Types,
  VirtualTrees.BaseTree
  ;

type

  TVstSelector = class
  private
     fTree : TVirtualStringTree;
     fOldOnKeyDown :        TKeyEvent;
     fOldOnKeyAction:       TVTKeyActionEvent;
     fOldOnMouseDown:       TMouseEvent;
     fOldOnMouseMove:       TMouseMoveEvent;
     fOldOnMouseUp:         TMouseEvent;
     fOldOnFocusChanged:    TVTFocusChangeEvent;
     fOldOnBeforeCellPaint: TVTBeforeCellPaintEvent;

     procedure VstKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
     procedure VstKeyAction(Sender: TBaseVirtualTree;var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
     procedure VstMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
     procedure VstMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
     procedure VstMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
     procedure VstFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
     procedure VstBeforeCellPaint(Sender: TBaseVirtualTree;TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;CellPaintMode: TVTCellPaintMode; CellRect: TRect;var ContentRect: TRect);

  public
    StartSelectedColumn: integer;
    EndSelectedColumn  : integer;
    StartSelectedNode  : PVirtualNode;
    EndSelectedNode    : PVirtualNode;
    Selecting          : boolean;
    SelectingWithMouse : boolean;

    constructor Create(Tree : TVirtualStringTree);
    function IsSelected(Node: PVirtualNode; ColumnIndexToCheck:integer): boolean;
    procedure ResetSelection();

  end;


implementation

{ TVstSelector }

constructor TVstSelector.Create(Tree: TVirtualStringTree);
begin
   fTree := Tree;

   fTree.TreeOptions.SelectionOptions := fTree.TreeOptions.SelectionOptions
      + [toDisableDrawSelection]    // Prevent user from selecting with the selection rectangle in multiselect mode.
      + [toExtendedFocus]           // Entries other than in the main column can be selected, edited etc.
      - [toMultiselect];            // Allow more than one node to be selected.

   fOldOnKeyDown          := Tree.OnKeyDown;
   fOldOnKeyAction        := Tree.OnKeyAction;
   fOldOnMouseDown        := Tree.OnMouseDown;
   fOldOnMouseMove        := Tree.OnMouseMove;
   fOldOnMouseUp          := Tree.OnMouseUp;
   fOldOnFocusChanged     := Tree.OnFocusChanged;
   fOldOnBeforeCellPaint  := Tree.OnBeforeCellPaint;

   Tree.OnKeyDown         := VstKeyDown;
   Tree.OnKeyAction       := VstKeyAction;
   Tree.OnMouseDown       := VstMouseDown;
   Tree.OnMouseMove       := VstMouseMove;
   Tree.OnMouseUp         := VstMouseUp;
   Tree.OnFocusChanged    := VstFocusChanged;
   Tree.OnBeforeCellPaint := VstBeforeCellPaint;
end;

procedure TVstSelector.VstKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   // start selection is shift is pressed

   if (not Selecting) and (Key = VK_SHIFT) then  begin
      Selecting := true;
      SelectingWithMouse := false;
   end;
   if assigned (fOldOnKeyDown) then
      fOldOnKeyDown(sender,key,Shift);
end;

procedure TVstSelector.VstKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin

   // stop selection if no more Shift

   if (selecting) and not (ssShift in Shift) then begin
      Selecting           := false;
      SelectingWithMouse  := false;
      StartSelectedColumn := fTree.FocusedColumn;
      EndSelectedColumn   := fTree.FocusedColumn;
      StartSelectedNode   := fTree.GetFirstSelected;
      EndSelectedNode     := fTree.GetFirstSelected;
      fTree.Refresh;
   end;
   if assigned (fOldOnKeyAction) then
      fOldOnKeyAction(sender,CharCode,Shift,DoDefault);
end;


procedure TVstSelector.VstMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   HitInfo: THitInfo;
begin
   fTree.GetHitTestInfoAt(X, Y, True, HitInfo, []);

   if (ssShift in Shift) then begin
      // mouse down With Shift : extend selection
      EndSelectedColumn  := HitInfo.HitColumn;
      EndSelectedNode    := HitInfo.HitNode;
      // Selecting not changed

   end else begin
      // mouse down without Shift: start selection

      if (HitInfo.HitNode <> nil) then begin
         StartSelectedColumn := HitInfo.HitColumn;
         EndSelectedColumn   := HitInfo.HitColumn;
         StartSelectedNode   := HitInfo.HitNode;
         EndSelectedNode     := HitInfo.HitNode;
         Selecting := true;
         SelectingWithMouse := true;
      end;
   end;

   if assigned (fOldOnMouseDown) then
      fOldOnMouseDown(sender,Button,Shift,X,Y);

   // refresh on both mouse down and mouse up
   fTree.Refresh;
end;

procedure TVstSelector.VstMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
   HitInfo: THitInfo;
   CellText: string;
begin
   // MouseMove not called if toDisableDrawSelection is false

   if (SelectingWithMouse = false)  then
      exit;

   fTree.GetHitTestInfoAt(X, Y, True, HitInfo, []);
   if HitInfo.HitNode = nil then
      exit;

   if StartSelectedColumn = -1 then begin
      fTree.OnGetText(fTree, HitInfo.HitNode, HitInfo.HitColumn, ttNormal, CellText);
      StartSelectedColumn := HitInfo.HitColumn;
      EndSelectedColumn   := HitInfo.HitColumn;
      StartSelectedNode   := HitInfo.HitNode;
      EndSelectedNode     := HitInfo.HitNode;
   end else begin
      if (EndSelectedNode <> HitInfo.HitNode) or (EndSelectedColumn <> HitInfo.HitColumn) then begin
         fTree.OnGetText(fTree, HitInfo.HitNode, HitInfo.HitColumn, ttNormal, CellText);
         fTree.ScrollIntoView (HitInfo.HitNode,false,false);  //Center, Horizontally false
         EndSelectedColumn := HitInfo.HitColumn;
         EndSelectedNode   := HitInfo.HitNode;
      end;
   end;
   if assigned (fOldOnMouseMove) then
      fOldOnMouseMove(sender,Shift,X,Y);

   if StartSelectedColumn <> -1 then
      fTree.Refresh;
end;

procedure TVstSelector.VstMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   Selecting := false;
   SelectingWithMouse := false;
   if assigned (fOldOnMouseUp) then
      fOldOnMouseup(sender,Button,Shift,X,Y);

   fTree.Refresh;
end;

procedure TVstSelector.VstFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin

  // focus changed. Expand selection

   if not Selecting then begin
      StartSelectedColumn := Column;
      StartSelectedNode   := Node;
   end;

   EndSelectedColumn  := Column;
   EndSelectedNode    := Node;

   if assigned (fOldOnFocusChanged) then
      fOldOnFocusChanged(sender,Node,Column);
end;

procedure TVstSelector.VstBeforeCellPaint(Sender: TBaseVirtualTree;   TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
   if assigned (fOldOnBeforeCellPaint) then
      fOldOnBeforeCellPaint(sender,TargetCanvas,Node,Column, CellPaintMode,CellRect,ContentRect);

   if (IsSelected(node,Column) = false) then
      exit ;

   // Draw highlight
   if fTree.Focused then
      TargetCanvas.Brush.Color := fTree.Colors.FocusedSelectionColor
   else
      TargetCanvas.Brush.Color := fTree.Colors.UnfocusedSelectionColor;
   TargetCanvas.Brush.Style := bsSolid;
   TargetCanvas.FillRect(CellRect);
end;

function TVstSelector.IsSelected(Node: PVirtualNode;  ColumnIndexToCheck: integer): boolean;
var
  LoopEnd : PVirtualNode;
  loopNode : PVirtualNode ;
  startPosition, EndPosition, PositionToCheck: integer;
begin
   result := false;

   if (StartSelectedColumn = -1) or (EndSelectedColumn = -1) or (StartSelectedNode = nil) or (EndSelectedNode = nil) then
      exit;

   PositionToCheck := fTree.Header.Columns[ColumnIndexToCheck].Position;

   if fTree.Header.Columns[StartSelectedColumn].Position <= fTree.Header.Columns[EndSelectedColumn].Position then begin
     startPosition := fTree.Header.Columns[StartSelectedColumn].Position;
     endPosition   := fTree.Header.Columns[EndSelectedColumn].Position;
   end else begin  // reverse selection
     startPosition := fTree.Header.Columns[EndSelectedColumn].Position;
     endPosition   := fTree.Header.Columns[StartSelectedColumn].Position;
   end;

   if (PositionToCheck < startPosition) or (PositionToCheck > endPosition) then
       exit;

   if (node = StartSelectedNode) or (node = EndSelectedNode) then begin
      result := true;
      exit;
   end;

   // start and last are the same node. Looping over LastSelectedNode will always found nodes
   if (StartSelectedNode = EndSelectedNode ) then
      exit;

   if (StartSelectedNode^.Index) <= (EndSelectedNode^.Index) then begin   // Top to bottom
      loopNode := StartSelectedNode;
      loopEnd  := EndSelectedNode;
   end else begin
      loopNode := EndSelectedNode ;
      loopEnd  := StartSelectedNode;
   end;

   while loopNode <> nil do begin
      if (node = loopNode) then begin
         result := true;
         exit;
      end;
      loopNode := loopNode.NextSibling;
      if (loopNode = loopEnd) or (loopNode = nil) then
         break;
   end;

end;

procedure TVstSelector.ResetSelection;
begin
  StartSelectedColumn := -1;
  EndSelectedColumn   := -1;
  StartSelectedNode   := nil;
  EndSelectedNode     := nil;
end;



end.
