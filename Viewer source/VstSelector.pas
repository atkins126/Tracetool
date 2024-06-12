unit VstSelector;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, contnrs,
  VirtualTrees,
  VirtualTrees.Types,
  VirtualTrees.BaseTree
  ;

type

  TVirtualTreeColumnsCracker = class(TVirtualTreeColumns);
  TBaseVirtualTreeCracker = class(TBaseVirtualTree);
  TVstSelector = class;
  TVstSelectorSelectionChangedEvent = procedure(Sender: TVstSelector; selectionAsText: string) of object;

  TVstSelector = class (TComponent)
  private
     fTree : TVirtualStringTree;
     fOldOnKeyDown :        TKeyEvent;
     fOldOnKeyAction:       TVTKeyActionEvent;
     fOldOnMouseDown:       TMouseEvent;
     fOldOnMouseMove:       TMouseMoveEvent;
     fOldOnMouseUp:         TMouseEvent;
     fOldOnFocusChanged:    TVTFocusChangeEvent;
     fOldOnBeforeCellPaint: TVTBeforeCellPaintEvent;

     fOnSelectionChanged: TVstSelectorSelectionChangedEvent;

     procedure VstKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
     procedure VstKeyAction(Sender: TBaseVirtualTree;var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
     procedure VstMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
     procedure VstMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
     procedure VstMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
     procedure VstFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
     procedure VstBeforeCellPaint(Sender: TBaseVirtualTree;TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;CellPaintMode: TVTCellPaintMode; CellRect: TRect;var ContentRect: TRect);
     function CompareNodePositions(Node1, Node2: PVirtualNode; ConsiderChildrenAbove: Boolean = False): Integer;

  public
    StartSelectedColumn: integer;
    EndSelectedColumn  : integer;
    StartSelectedNode  : PVirtualNode;
    EndSelectedNode    : PVirtualNode;
    Selecting          : boolean;
    SelectingWithMouse : boolean;

    procedure Init (Tree : TVirtualStringTree);
    function IsSelected(Node: PVirtualNode; ColumnIndexToCheck:integer): boolean;
    function GetSelectionAsText(): string;
    procedure ResetSelection();
    procedure CopySelectedCells (CopyStrings: TStringList;TextQualifier : string; TextSeparator: string);

  published
    property OnSelectionChanged: TVstSelectorSelectionChangedEvent read fOnSelectionChanged write fOnSelectionChanged;

  end;


implementation

uses
   System.types,
   System.Math,
   unt_traceWin;

{ TVstSelector }

procedure TVstSelector.Init(Tree: TVirtualStringTree);
begin
   fTree := Tree;

   fTree.TreeOptions.PaintOptions := fTree.TreeOptions.PaintOptions
      - [toHideSelection];          // show a grayed selection when the tree lose the focus

   fTree.TreeOptions.SelectionOptions := fTree.TreeOptions.SelectionOptions
      + [toDisableDrawSelection]    // Prevent user from selecting with the selection rectangle in multiselect mode.
      + [toExtendedFocus]           // Entries other than in the main column can be selected, edited etc.
      + [toSimpleDrawSelection]     // Simplifies draw selection, so a node's caption does not need to intersect with the selection rectangle.
      - [toMultiselect];            // Allow more than one node to be selected.

   fTree.TreeOptions.MiscOptions := fTree.TreeOptions.MiscOptions
      - [toReportMode]              // Tree behaves like TListView in report mode.
      + [toGridExtensions];         // Use some special enhancements to simulate and support grid behavior.

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
   //TFrm_Trace.InternalTrace('TVstSelector.VstMouseDown') ;
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
begin
   // MouseMove not called if toDisableDrawSelection is false

   if (SelectingWithMouse = false)  then
      exit;

   //TFrm_Trace.InternalTrace('TVstSelector.VstMouseMove') ;

   var NodeTop: TDimension;
   var HitInfo: THitInfo;
   var ColLeft, ColRight: TDimension;

   // search node
   HitInfo.HitNode := fTree
      .GetNodeAt(
         X, Y,
         true,          // relative
         NodeTop);      // output: Top Y position of node (not needed)

   // search column
   HitInfo.HitColumn := TVirtualTreeColumnsCracker(fTree.Header.Columns)
      .GetColumnAndBounds(  // TVirtualTreeColumnsCracker is used to get protected function GetColumnAndBounds
         Point(X,Y),
         ColLeft,       // output: Left  X column
         ColRight,      // output: Right X column
         true);         // Relative

   if HitInfo.HitNode = nil then
      exit;
   //var CellText: string;
   if StartSelectedColumn = -1 then begin
      StartSelectedColumn := HitInfo.HitColumn;
      EndSelectedColumn   := HitInfo.HitColumn;
      StartSelectedNode   := HitInfo.HitNode;
      EndSelectedNode     := HitInfo.HitNode;
      //fTree.OnGetText(fTree, HitInfo.HitNode, HitInfo.HitColumn, ttNormal, CellText);
      if Assigned(fOnSelectionChanged) then
         fOnSelectionChanged (self,GetSelectionAsText());
   end;

   if (EndSelectedNode <> HitInfo.HitNode) or (EndSelectedColumn <> HitInfo.HitColumn) then begin
      EndSelectedColumn := HitInfo.HitColumn;
      EndSelectedNode   := HitInfo.HitNode;
      //fTree.OnGetText(fTree, HitInfo.HitNode, HitInfo.HitColumn, ttNormal, CellText);
      fTree.ScrollIntoView (HitInfo.HitNode,false,false);  //Center, Horizontally false
      if Assigned(fOnSelectionChanged) then
         fOnSelectionChanged (self,GetSelectionAsText());
   end;

   if assigned (fOldOnMouseMove) then
      fOldOnMouseMove(sender,Shift,X,Y);

   if StartSelectedColumn <> -1 then
      fTree.Refresh;
end;

procedure TVstSelector.VstMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   //TFrm_Trace.InternalTrace('TVstSelector.VstMouseUp') ;
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

   if Assigned(fOnSelectionChanged) then
      fOnSelectionChanged (self,GetSelectionAsText());

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

function TVstSelector.CompareNodePositions(Node1, Node2: PVirtualNode; ConsiderChildrenAbove: Boolean = False): Integer;
// TBaseVirtualTree.CompareNodePositions is private, not protected. This is a copied version :(

// Tries hard and smart to quickly determine whether Node1's structural position is before Node2's position.
// If ConsiderChildrenAbove is True, the nodes will be compared with their visual order in mind.
// Returns 0 if Node1 = Node2, < 0 if Node1 is located before Node2 else > 0.

var
  Run1,
  Run2: PVirtualNode;
  Level1,
  Level2: Cardinal;

begin
  Assert(Assigned(Node1) and Assigned(Node2), 'Nodes must never be nil.');

  if Node1 = Node2 then
    Result := 0
  else
  begin
    if fTree.HasAsParent(Node1, Node2) then
      Result := IfThen(ConsiderChildrenAbove and (toChildrenAbove in ftree.TreeOptions.PaintOptions), -1, 1)
    else
      if fTree.HasAsParent(Node2, Node1) then
        Result := IfThen(ConsiderChildrenAbove and (toChildrenAbove in ftree.TreeOptions.PaintOptions), 1, -1)
      else
      begin
        // the given nodes are neither equal nor are they parents of each other, so go up to FRoot
        // for each node and compare the child indices of the top level parents
        // Note: neither Node1 nor Node2 can be FRoot at this point as this (a bit strange) circumstance would
        //       be caught by the previous code.

        // start lookup at the same level
        Level1 := fTree.GetNodeLevel(Node1);
        Level2 := fTree.GetNodeLevel(Node2);
        Run1 := Node1;
        while Level1 > Level2 do
        begin
          Run1 := Run1.Parent;
          System.Dec(Level1);
        end;
        Run2 := Node2;
        while Level2 > Level1 do
        begin
          Run2 := Run2.Parent;
          System.Dec(Level2);
        end;

        // now go up until we find a common parent node (loop will safely stop at FRoot if the nodes
        // don't share a common parent)
        while Run1.Parent <> Run2.Parent do
        begin
          Run1 := Run1.Parent;
          Run2 := Run2.Parent;
        end;
        Result := Integer(Run1.Index) - Integer(Run2.Index);
      end;
  end;
end;

procedure TVstSelector.ResetSelection;
begin
  StartSelectedColumn := -1;
  EndSelectedColumn   := -1;
  StartSelectedNode   := nil;
  EndSelectedNode     := nil;
end;

function TVstSelector.GetSelectionAsText() : string;
var
   RowCount, ColCount : integer ;
   startColPosition, EndColPosition: integer;

   procedure CountSibling(NodeToCheck,lastNode : PVirtualNode);
   begin
      var loopNode : PVirtualNode := NodeToCheck;
      while (loopNode <> nil) do begin
         inc(RowCount);
         if (loopNode = lastNode) then
             break;
         // don't use loopNode.FirstChild and loopNode.NextSibling: node can be not yet initialized
         loopNode := fTree.GetNext(loopNode, {ConsiderChildrenAbove} true) ; // Returns next node in tree. The Result will be initialized if needed.
      end;
   end;

begin
  result := '';
  ColCount := 1;
  if (StartSelectedColumn = -1) or (EndSelectedColumn = -1) or (StartSelectedNode = nil) or (EndSelectedNode = nil) then
      exit;

  if fTree.Header.Columns[StartSelectedColumn].Position <= fTree.Header.Columns[EndSelectedColumn].Position then begin
     startColPosition := fTree.Header.Columns[StartSelectedColumn].Position;
     EndColPosition   := fTree.Header.Columns[EndSelectedColumn].Position;
  end else begin  // reverse selection
     startColPosition := fTree.Header.Columns[EndSelectedColumn].Position;
     EndColPosition   := fTree.Header.Columns[StartSelectedColumn].Position;
  end;
  ColCount := EndColPosition - startColPosition + 1;

  RowCount := 0;
  var isTopToBottom := CompareNodePositions(StartSelectedNode, EndSelectedNode,false) < 0;

  if isTopToBottom then
     CountSibling(StartSelectedNode, EndSelectedNode)
  else
     CountSibling(EndSelectedNode, StartSelectedNode);

  if (RowCount = 1) and (ColCount = 1) then
     result := ''
  else if ColCount = 1 then
     result := 'Selection:' + inttostr(RowCount) + ' rows'
  else
     result := 'Selection:' + inttostr(RowCount) + ' rows, ' + inttostr(ColCount) + ' cols';
end;


function TVstSelector.IsSelected(Node: PVirtualNode;  ColumnIndexToCheck: integer): boolean;
var
   startColPosition, EndColPosition, ColumnPositionToCheck: integer;
begin
   result := false;

   if (StartSelectedColumn = -1) or (EndSelectedColumn = -1) or (StartSelectedNode = nil) or (EndSelectedNode = nil) then
      exit;

   ColumnPositionToCheck := fTree.Header.Columns[ColumnIndexToCheck].Position;

   if fTree.Header.Columns[StartSelectedColumn].Position <= fTree.Header.Columns[EndSelectedColumn].Position then begin
     startColPosition := fTree.Header.Columns[StartSelectedColumn].Position;
     EndColPosition   := fTree.Header.Columns[EndSelectedColumn].Position;
   end else begin  // reverse selection
     startColPosition := fTree.Header.Columns[EndSelectedColumn].Position;
     EndColPosition   := fTree.Header.Columns[StartSelectedColumn].Position;
   end;

   if (ColumnPositionToCheck < startColPosition) or (ColumnPositionToCheck > EndColPosition) then
       exit;

    if (node = StartSelectedNode) or (node = EndSelectedNode) then begin
       result := true;
       exit;
    end;

    if (StartSelectedNode = EndSelectedNode ) then
       exit;

    if CompareNodePositions(StartSelectedNode, EndSelectedNode,false) < 0 then begin   // start < node < end
       if CompareNodePositions(node ,StartSelectedNode, false)     < 0 then exit       // node before start
       else if CompareNodePositions(Node, EndSelectedNode,false)   > 0 then exit       // node after end
       else result := true;  // after start and before end
    end else begin                                                                     // end < node < start
       if CompareNodePositions(node ,EndSelectedNode, false)       < 0 then exit       // node before end
       else if CompareNodePositions(node, StartSelectedNode,false) > 0 then exit       // node after start
       else result := true;  // after start and before end
    end;

end;

procedure TVstSelector.CopySelectedCells (CopyStrings: TStringList;TextQualifier : string; TextSeparator: string);
var
   orderedList : Array of integer;
   ColumnIndex : integer ;

   procedure CopyDetail (StartSelectedNode, EndSelectedNode : PVirtualNode);
   begin
      // ordered column.
      var startColPosition, EndColPosition: integer;
      if fTree.Header.Columns[StartSelectedColumn].Position <= fTree.Header.Columns[EndSelectedColumn].Position then begin
        startColPosition := fTree.Header.Columns[StartSelectedColumn].Position;
        EndColPosition   := fTree.Header.Columns[EndSelectedColumn].Position;
      end else begin  // reverse selection
        startColPosition := fTree.Header.Columns[EndSelectedColumn].Position;
        EndColPosition   := fTree.Header.Columns[StartSelectedColumn].Position;
      end;

      var loopNode : PVirtualNode := StartSelectedNode;
      while loopNode <> nil do begin
         var hasSelectionInNode : boolean := false;
         var NewLine := '' ;

         for var OrderedIndex := 0 to length(orderedList)-1 do begin
            ColumnIndex :=  orderedList[OrderedIndex];
            var ColumnPositionToCheck:integer := fTree.Header.Columns[ColumnIndex].Position;
            if (ColumnPositionToCheck >= startColPosition) and (ColumnPositionToCheck <= EndColPosition) then begin
                hasSelectionInNode := true;

                var CellText :string ;
                fTree.OnGetText(fTree, loopNode, ColumnIndex, ttNormal, CellText);
                if NewLine = '' then
                   NewLine := TextQualifier + CellText + TextQualifier
                else
                   NewLine := NewLine + TextSeparator  + TextQualifier + CellText + TextQualifier ;
            end;
         end ;
         if hasSelectionInNode then
            CopyStrings.Add(NewLine);

         if (loopNode = EndSelectedNode) then
             break;

         // don't use loopNode.FirstChild and loopNode.NextSibling: node can be not yet initialized
         loopNode := fTree.GetNext(loopNode, {ConsiderChildrenAbove} true) ; // Returns next node in tree. The Result will be initialized if needed.
      end ;
   end ;
begin

   SetLength(orderedList, fTree.header.Columns.Count);
   for ColumnIndex := 0 to fTree.header.Columns.Count-1 do
      orderedList[fTree.header.Columns[ColumnIndex].Position] := ColumnIndex ;

   var isTopToBottom := CompareNodePositions(StartSelectedNode, EndSelectedNode,false) < 0;
   if isTopToBottom then
      CopyDetail(StartSelectedNode, EndSelectedNode)
   else
      CopyDetail(EndSelectedNode, StartSelectedNode);
end;


end.
