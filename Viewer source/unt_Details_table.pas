unit unt_Details_table;

interface

uses
  system.Contnrs, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, unt_Details_base, VirtualTrees, VirtualTrees.Types,
  unt_Editor, Menus , ExtCtrls, clipbrd,
  unt_TraceWin ,
  unt_utility,
  vstSort,
  unt_tool,
  System.Generics.Collections,
  System.TypInfo,
  System.Math,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.BaseTree,
  VirtualTrees.AncestorVCL;            // VstEditor, IVstEditor, TMember

type
  PTableRec = ^TTableRec ;
  TTableRec = record
     OriginalOrder: integer; // Original order when inserted. Used to Unsort nodes
     Columns : TStringList ;
  end ;

  Tframe_table = class(Tframe_BaseDetails)
    VstTable: TVirtualStringTree;
    PopupDetail: TPopupMenu;
    CopyMenu: TMenuItem;
    N2: TMenuItem;
    SelectAllMenu: TMenuItem;
    procedure VstTableCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VstTableDblClick(Sender: TObject);
    procedure VstTableEditCancelled(Sender: TBaseVirtualTree;
      Column: TColumnIndex);
    procedure VstTableEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VstTableFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VstTableMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure VstTablePaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure VstTableKeyAction(Sender: TBaseVirtualTree;
      var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
    procedure VstTableGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VstTableChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VstTableColumnClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure VstTableFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VstTableBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VstTableMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure VstTableMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VstTableMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VstTableKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure VstTableKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    procedure WMStartEditingMember(var Message: TMessage); message WM_STARTEDITING_MEMBER;
    function IsSelected(Node: PVirtualNode; ColumnIndexToCheck:integer): boolean;
  public
    { Public declarations }
    TraceWin: TFrm_Trace;
    Sorter: TVstSort;
    Constructor Create(AOwner: TComponent);  override ;
    Procedure AddDetails(TreeRec: PTreeRec; RootMember : TMember); override;
    function HasFocus : boolean ; override;
    procedure SelectAll() ; override;
    procedure copySelected() ; override;

  end;

var
  frame_table: Tframe_table;

  StartSelectedColumn: integer;
  EndSelectedColumn: integer;
  StartSelectedNode : PVirtualNode;
  EndSelectedNode : PVirtualNode;

  Selecting: boolean;
  SelectingWithMouse : boolean;

implementation

uses
unt_TraceConfig, unt_Details_Classic,  unt_search ;

{$R *.dfm}

//------------------------------------------------------------------------------

constructor Tframe_table.Create(AOwner: TComponent);
begin
   inherited create (AOwner) ;

   TraceWin := TFrm_Trace(owner);

   // initialize sort
   Sorter := TVstSort.Create(self);
   Sorter.tree := VstTable;
   Sorter.UtilityImages := Frm_Tool.UtilityImages;
   Sorter.canUnsort := true;

   // redirect some events to the sorter
   VstTable.onHeaderClick := Sorter.onHeaderClick;
   VstTable.OnKeyUp := Sorter.OnKeyUp;
   VstTable.onHeaderDrawQueryElements := Sorter.onHeaderDrawQueryElements;
   VstTable.onAdvancedHeaderDraw := Sorter.onAdvancedHeaderDraw;

   VstTable.NodeDataSize := sizeof (TTableRec) ;
   //VstTable.Header.SortColumn := 0 ;
   VstTable.Header.MainColumn := 0 ;
   VstTable.Header.AutoSizeIndex := -1 ;     // 2

   // header must be visible to enable resize !
   VstTable.Header.Columns.Items[0].text := '' ;
   VstTable.Header.Columns.Items[1].text := '' ;
   VstTable.Header.Columns.Items[2].text := '' ;

   VstTable.Header.Options           := TraceWin.vstTrace.Header.Options ;
   VstTable.TreeOptions.AutoOptions  := TraceWin.vstTrace.TreeOptions.AutoOptions
      + [toAutoSpanColumns]           // Large entries continue into next columns
      - [toDisableAutoscrollOnFocus]  // Disable scrolling a column entirely into view if it gets focused.
      + [toDisableAutoscrollOnEdit];  // Do not center a node horizontally when it is edited.
 
   VstTable.TreeOptions.PaintOptions := TraceWin.vstTrace.TreeOptions.PaintOptions
      - [toUseBlendedImages]        // Don't use blended images
      - [toShowTreeLines]           // don't Display tree lines to show hierarchy of nodes.
      - [toHideSelection]           // show a grayed selection when the tree lose the focus
      + [toShowRoot]                // show root.
      + [toShowButtons]             // Display collapse/expand buttons left to a node.
      + [toThemeAware]              // Draw UI elements (header, tree buttons etc.) according to the current theme
      + [toHideFocusRect];          // hide focus rect

   VstTable.TreeOptions.SelectionOptions := TraceWin.vstTrace.TreeOptions.SelectionOptions
      + [toDisableDrawSelection]    // Prevent user from selecting with the selection rectangle in multiselect mode.
      + [toExtendedFocus]           // Entries other than in the main column can be selected, edited etc.
      - [toMultiselect]             // Allow more than one node to be selected.
      + [toSimpleDrawSelection]     // Simplifies draw selection, so a node's caption does not need to intersect with the selection rectangle.
      - [toFullRowSelect];          // selection highlight the whole line
 
   VstTable.TreeOptions.MiscOptions := TraceWin.vstTrace.TreeOptions.MiscOptions
      - [toReportMode]              // Tree behaves like TListView in report mode.
      + [toFullRepaintOnResize]     // Fully invalidate the tree when its window is resized (CS_HREDRAW/CS_VREDRAW).
      + [toWheelPanning]            // Support for mouse panning (wheel mice only).
      - [toFullRowDrag]             // Start node dragging by clicking anywhere in it instead only on the caption or image.
                                    // Must be used together with toDisableDrawSelection.
      + [toGridExtensions]          // Use some special enhancements to simulate and support grid behavior.
      - [toVariableNodeHeight]      // variable node height
      - [toToggleOnDblClick]        // Toggle node expansion state when it is double clicked.
      - [toEditable]                // don't allow edition. Code is used to detect double click or F2 key
      - [toCheckSupport];           // no checkboxes


   VstTable.Colors.UnfocusedColor                := TraceWin.vstTrace.Colors.UnfocusedColor ;
   VstTable.Colors.UnfocusedSelectionColor       := TraceWin.vstTrace.Colors.UnfocusedSelectionColor ;
   VstTable.Colors.UnfocusedSelectionBorderColor := TraceWin.vstTrace.Colors.UnfocusedSelectionBorderColor ;

   //VstTable.OnDrawNode := DrawNode ;

end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
   // scroll into view
   sender.ScrollIntoView (node,false,false);     // center and horizontally false
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableCreateEditor(Sender: TBaseVirtualTree;  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
   if IVstEditor = nil then begin
      VstEditor  := TMoveMemoEditLink.Create ();    // unt_tool
      IVstEditor := VstEditor ;                     // unt_tool
   end ;
   EditLink := IVstEditor ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableDblClick(Sender: TObject);
var
   P: TPoint;
   SelectedNode, MouseNode : PVirtualNode ;
   Dummy: Integer;
begin
   SelectedNode := VstTable.GetFirstSelected  ;

   // no node selected
   if SelectedNode = nil then
     exit ;

   GetCursorPos(P);
   P := VstTable.ScreenToClient(P);
   MouseNode := VstTable.GetNodeAt(P.X, P.Y, True, Dummy) ;

   // the mouse under the cursor is not the selected node
   if SelectedNode <> MouseNode then
      exit ;

   VstTable.TreeOptions.MiscOptions := VstTable.TreeOptions.MiscOptions + [toEditable] ;

   // We want to start editing the currently selected node. However it might well happen that this change event
   // here is caused by the node editor if another node is currently being edited. It causes trouble
   // to start a new edit operation if the last one is still in progress. So we post us a special message and
   // in the message handler we then can start editing the new node. This works because the posted message
   // is first executed *after* this event and the message, which triggered it is finished.
   PostMessage(Self.Handle, WM_STARTEDITING_MEMBER, Integer(SelectedNode), 0);
end;

//------------------------------------------------------------------------------

procedure Tframe_table.WMStartEditingMember(var Message: TMessage);
var
   Node: PVirtualNode;
begin
   Node := Pointer(Message.WParam);
   if Assigned(Node) then
      VstTable.EditNode(Node, VstTable.FocusedColumn);
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
begin
   VstTable.TreeOptions.MiscOptions := VstTable.TreeOptions.MiscOptions - [toEditable] ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableEdited(Sender: TBaseVirtualTree;  Node: PVirtualNode; Column: TColumnIndex);
begin
   VstTable.TreeOptions.MiscOptions := VstTable.TreeOptions.MiscOptions - [toEditable] ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableFreeNode(Sender: TBaseVirtualTree;  Node: PVirtualNode);
var
   DetailRec : PTableRec ;
   //c : integer ;
begin
   try
      DetailRec := Sender.GetNodeData(Node) ;
      DetailRec.Columns.free();
   except
      on e : exception do begin
         TFrm_Trace.InternalTrace('VstTableFreeNode exception when resetting', e.message) ;
      end ;
   end ;

end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableGetText(Sender: TBaseVirtualTree;  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;  var CellText: string);
var
   DetailRec : PTableRec ;
begin
   CellText := '' ;
   DetailRec := Sender.GetNodeData(Node) ;
   if DetailRec = nil then
      exit ;

   if Column >= DetailRec.Columns.count then
      CellText := ''
   else

   if (not (toEditable in VstTable.TreeOptions.MiscOptions)) and (Length(DetailRec.Columns[Column]) > 400) then
      CellText := Copy(DetailRec.Columns[Column], 1, 400) + '...'
   else
      CellText := DetailRec.Columns[Column] ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
begin
   if (not Selecting) and (Key = VK_SHIFT) then  begin
      Selecting := true;
      SelectingWithMouse := false;
      TFrm_Trace.InternalTrace('KeyDown selecting = true');
   end;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableMouseDown(Sender: TObject;  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   HitInfo: THitInfo;
begin
   vstTable.GetHitTestInfoAt(X, Y, True, HitInfo, []);

   if (ssShift in Shift) then begin
      // mouse down With Shift : extend selection 
      EndSelectedColumn  := HitInfo.HitColumn;
      EndSelectedNode    := HitInfo.HitNode;
      // Selecting not changed 
   
   end else begin
      // mouse down without Shift: start selection
      StartSelectedColumn := HitInfo.HitColumn;
      EndSelectedColumn   := HitInfo.HitColumn;
      StartSelectedNode   := HitInfo.HitNode;
      EndSelectedNode     := HitInfo.HitNode;
      Selecting := true;
      SelectingWithMouse := true;      
   end;
   // refresh on both mouse down and mouse up
   vstTable.Refresh;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var
   HitInfo: THitInfo;
   DetailRec : PTableRec ;
   CellText: string;
begin
   // VstTableMouseMove not called if toDisableDrawSelection is false

   if (SelectingWithMouse = false)  then
      exit;

   vstTable.GetHitTestInfoAt(X, Y, True, HitInfo, []);
   if HitInfo.HitNode = nil then
      exit;

   if StartSelectedColumn = -1 then begin
      VstTableGetText(vstTable, HitInfo.HitNode, HitInfo.HitColumn, ttNormal, CellText);
      StartSelectedColumn := HitInfo.HitColumn;
      EndSelectedColumn   := HitInfo.HitColumn;
      StartSelectedNode   := HitInfo.HitNode;
      EndSelectedNode     := HitInfo.HitNode;
   end else begin
      if (EndSelectedNode <> HitInfo.HitNode) or (EndSelectedColumn <> HitInfo.HitColumn) then begin
         DetailRec := VstTable.GetNodeData(HitInfo.HitNode) ;
         VstTableGetText(vstTable, HitInfo.HitNode, HitInfo.HitColumn, ttNormal, CellText);
         TFrm_Trace.InternalTrace('MouseMove, last, row: ' + inttostr(DetailRec.OriginalOrder) + ', column: ' + inttostr(HitInfo.HitColumn) + ', text: "' + celltext + '"') ;
         vstTable.ScrollIntoView (HitInfo.HitNode,false,false);  //Center, Horizontally false
         EndSelectedColumn := HitInfo.HitColumn;
         EndSelectedNode   := HitInfo.HitNode;
         vstTable.Refresh;
      end;
   end;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   Selecting := false;
   SelectingWithMouse := false;      
   vstTable.Refresh;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableKeyUp(Sender: TObject; var Key: Word;  Shift: TShiftState);
begin
   if ssShift in Shift then
      TFrm_Trace.InternalTrace('KeyUp Shift ' + inttostr(Key))
   else
      TFrm_Trace.InternalTrace('KeyUp no Shift ' + inttostr(Key));

//   if not (ssShift in Shift) then begin
//      Selecting := false;
//      SelectingWithMouse := false;
//      StartSelectedColumn := -1;
//      EndSelectedColumn   := -1;
//      StartSelectedNode   := nil;
//      EndSelectedNode     := nil;      
//      TFrm_Trace.InternalTrace('KeyDown selecting = false');
//   end;
end;


// Detect the F2 key.
// To not allow editing on simple click, the vstTrace.TreeOptions.MiscOptions toEditable flag is not set.
// When the F2 key is pressed or the user double click the node, the flag is set

procedure Tframe_table.VstTableKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
   if ssShift in Shift then
      TFrm_Trace.InternalTrace('KeyAction Shift ' + inttostr(charCode))
   else
      TFrm_Trace.InternalTrace('KeyAction no Shift ' + inttostr(charCode));

   if (selecting) and not (ssShift in Shift) then begin
      Selecting := false;
      SelectingWithMouse := false;
      StartSelectedColumn := VstTable.FocusedColumn;
      EndSelectedColumn   := VstTable.FocusedColumn;
      StartSelectedNode   := VstTable.GetFirstSelected;
      EndSelectedNode     := VstTable.GetFirstSelected;      
      vstTable.Refresh;
      TFrm_Trace.InternalTrace('KeyAction selecting = false');
   end;
   
   
   if CharCode = VK_F2 then
      VstTable.TreeOptions.MiscOptions := VstTable.TreeOptions.MiscOptions + [toEditable] ;
end;


//------------------------------------------------------------------------------

procedure Tframe_table.VstTableFocusChanged(Sender: TBaseVirtualTree;  Node: PVirtualNode; Column: TColumnIndex);
var
   DetailRec : PTableRec ;
   CellText: String;
begin

   if not Selecting then begin
      StartSelectedColumn := Column;      
      StartSelectedNode   := Node;
   end;
   
   EndSelectedColumn  := Column;
   EndSelectedNode    := Node;

   DetailRec := Sender.GetNodeData(Node) ;
   if DetailRec = nil then
      exit ;
   CellText := DetailRec.Columns[Column] ;
   Tframe_Classic(TraceWin.TreeDetailFrame).frameMemo.SetMemoText(CellText,false,false);
end;

//------------------------------------------------------------------------------

function Tframe_table.IsSelected( Node: PVirtualNode; ColumnIndexToCheck:integer) : boolean;
var
  LoopEnd : PVirtualNode;
  loopNode : PVirtualNode ;
  startPosition, EndPosition, PositionToCheck: integer;
begin
   result := false;

   // todo :
   // StartSelectedColumn 1 => index 0
   // LastSelectedColumn 0 => index 3
   // 0..3

   if (StartSelectedColumn = -1) or (EndSelectedColumn = -1) or (StartSelectedNode = nil) or (EndSelectedNode = nil) then
      exit;
   
   PositionToCheck := VstTable.Header.Columns[ColumnIndexToCheck].Position;

   if VstTable.Header.Columns[StartSelectedColumn].Position <= VstTable.Header.Columns[EndSelectedColumn].Position then begin
     startPosition := VstTable.Header.Columns[StartSelectedColumn].Position;
     endPosition   := VstTable.Header.Columns[EndSelectedColumn].Position;
   end else begin  // reverse selection
     startPosition := VstTable.Header.Columns[EndSelectedColumn].Position;
     endPosition   := VstTable.Header.Columns[StartSelectedColumn].Position;
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

procedure Tframe_table.VstTableBeforeCellPaint(Sender: TBaseVirtualTree;   TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
   DetailRec : PTableRec ;
begin
   DetailRec := Sender.GetNodeData(Node) ;
   if (SearchText <> '') {and (SearchKind = mrYesToAll)} then begin  //  mrYesToAll means Highlight all
      if (MatchSearch (DetailRec.Columns[Column]) <> 0) then
         DrawHighlight (TargetCanvas, CellRect,false) ;
   end;

   if (IsSelected(node,Column) = false) then
      exit ;

   //TFrm_Trace.InternalTrace('BeforeCellPaint ' +  DetailRec.Columns[0] + ' col ' + inttostr (Column) + ', NodeIsSelected : ' + BoolToStr(NodeIsSelected,true) );

   if VstTable.Focused then
     TargetCanvas.Brush.Color := VstTable.Colors.FocusedSelectionColor
   else
     TargetCanvas.Brush.Color := VstTable.Colors.UnfocusedSelectionColor;
   TargetCanvas.Brush.Style := bsSolid;
   TargetCanvas.FillRect(CellRect);
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTablePaintText(Sender: TBaseVirtualTree;  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;  TextType: TVSTTextType);
//var
//   DetailRec : PTableRec ;
begin
//   DetailRec := Sender.GetNodeData(Node) ;

   // force font
   //TraceWin.ChangeFontDetail ({IsTrace}false,TargetCanvas,  Column, DetailRec.fontDetails,(vsSelected in Node.States)) ;

//   if (IsSelected(node,Column) = false) then
//      exit ;
//
//   if VstTable.Focused then
//      TargetCanvas.Font.Color := clHighlightText
//   else
//      TargetCanvas.Font.Color := VstTable.Font.Color;
end;


//------------------------------------------------------------------------------

procedure Tframe_table.VstTableMeasureItem(Sender: TBaseVirtualTree;  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
var
//   h2,h3 : integer ;
//   DetailRec : PTableRec ;
   newNodeHeight : integer ;
begin
   if TraceWin.IsWatch then
      newNodeHeight := VstTable.DefaultNodeHeight
   else
      newNodeHeight := TraceConfig.Framework_info_NodeHeight ;
   NodeHeight := newNodeHeight ;

//   DetailRec := Sender.GetNodeData(Node) ;
//
//   // force font
//   TraceWin.ChangeFontDetail ({trace} false,TargetCanvas,  0, DetailRec.fontDetails,true) ;   // Watch/Framework , Trace/info
//   NodeHeight := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,0) ;
//
//   TraceWin.ChangeFontDetail ({trace} false,TargetCanvas,  1, DetailRec.fontDetails,true) ;
//   h2 := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,1) ;
//
//   TraceWin.ChangeFontDetail ({trace} false,TargetCanvas,  2, DetailRec.fontDetails,true) ;
//   h3 := TVirtualStringTree(sender).ComputeNodeHeight(TargetCanvas,Node,2) ;
//
//   if h2 > NodeHeight then
//      NodeHeight := h2 ;
//
//   if h3 > NodeHeight then
//      NodeHeight := h3 ;
//
//   // if multiline, NodeHeight is bigger than DefaultNodeHeight
//   if NodeHeight = 0 then
//      NodeHeight := newNodeHeight ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.VstTableColumnClick(Sender: TBaseVirtualTree;  Column: TColumnIndex; Shift: TShiftState);
var
   DetailRec : PTableRec ;
   CellText: String;
   SelectedNode: PVirtualNode ;
begin
   SelectedNode := Sender.GetFirstSelected  ;

   // no node selected
   if SelectedNode = nil then
     exit ;

   DetailRec := Sender.GetNodeData(SelectedNode) ;
   if DetailRec = nil then
      exit ;

   CellText := DetailRec.Columns[Column] ;

   Tframe_Classic(TraceWin.TreeDetailFrame).frameMemo.SetMemoText(CellText,false,false);
end;

//------------------------------------------------------------------------------

procedure Tframe_table.AddDetails(TreeRec: PTreeRec; RootMember: TMember);
var
   cols : TStringList ;
   col : TVirtualTreeColumn ;
   c : integer ;
   SubMember : TMember ;
   DetailNode :  PVirtualNode ;
   DetailRec : PTableRec ;
begin

  StartSelectedColumn := -1;
  EndSelectedColumn   := -1;
  StartSelectedNode   := nil;
  EndSelectedNode     := nil;

   VstTable.Clear ;
   VstTable.header.Columns.Clear ;

   // first member, col1 is the title
   cols := getTabStrings(pchar(RootMember.Col1)) ;
   for c := 0 to cols.Count-1 do begin
      col := VstTable.header.Columns.Add ;
      col.options  := col.options + [coAllowFocus] ;  // ensure user can focus to this column
      col.MinWidth := 10 ;
      col.MaxWidth := 1000 ;
      col.Width := 100 ;
      col.Text := cols[c] ;
   end ;
   VstTable.Header.MainColumn := 0 ;
   VstTable.Header.AutoSizeIndex := -1 ;  // auto
   cols.Free ;
   //LowTrace ('before add table');
   //TFrm_Trace.InternalTrace (FormatDateTime('yyyymmdd hh:mm:ss:zzz',now) + ' before add table');
   // add lines
   for c := 0 to RootMember.SubMembers.Count -1 do begin
      SubMember := TMember (RootMember.SubMembers.Items[c]) ;
      DetailNode := VstTable.AddChild(nil) ;
      // ensure node is initialized. Needed when the node is free to call onFreeNode
      VstTable.ReinitNode(DetailNode,false);
      DetailNode.Align := (VstTable.DefaultNodeHeight div 2)-2 ;
      DetailRec := VstTable.GetNodeData(DetailNode) ;

      cols := getTabStrings(pchar(SubMember.Col1)) ;
      DetailRec.OriginalOrder := c;   // for unsort
      DetailRec.Columns := cols ;     // free by OnFreeNodes
   end ;

   //TFrm_Trace.InternalTrace (FormatDateTime('yyyymmdd hh:mm:ss:zzz',now) + ' after add table');

   // resize all columns, using the header text and all visible (true) lines
   AutosizeAll (VstTable,true) ;

   // force last column width to maximum
   VstTable.Header.Columns[VstTable.Header.Columns.Count-1].Width := 9000 ;

   VstTable.Visible := true ;
   TFrm_Trace(Owner).CurrentViewers.add(self) ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.SelectAll;
begin
   VstTable.SelectAll(false) ;
end;

//------------------------------------------------------------------------------

function Tframe_table.HasFocus: boolean;
begin
  result := Focused or VstTable.focused ;
end;

//------------------------------------------------------------------------------

procedure Tframe_table.copySelected;
var
   CopyStrings : TStringList ;
   CopyText: PChar;
   DetailRec : PTableRec ;

   procedure CopyDetail (TestNode : PVirtualNode);
   var
      node : PVirtualNode ;
      NewLine: string;
      c : integer ;
      CellText :string ;
      //col : TVirtualTreeColumn ;
   begin
      if VstTable.Selected [TestNode] then begin
         DetailRec := VstTable.GetNodeData(TestNode) ;
         if DetailRec = nil then
            exit ;
         NewLine := '' ;
         for c := 0 to VstTable.header.Columns.Count-1 do begin
            //col := VstTable.header.Columns[c] ;
            CellText := DetailRec.Columns[c] ;
            if NewLine = '' then
               NewLine := TraceConfig.TextExport_TextQualifier + CellText + TraceConfig.TextExport_TextQualifier
            else
               NewLine := NewLine + TraceConfig.TextExport_Separator  + TraceConfig.TextExport_TextQualifier + CellText + TraceConfig.TextExport_TextQualifier ;
         end ;
         CopyStrings.Add(NewLine);
      end ;

      // multi select
      node := TestNode.FirstChild ;
      while Node <> nil do begin
         CopyDetail (node) ;
         node := node.NextSibling ;
      end ;
   end ;
begin
   CopyStrings := TStringList.Create;
   try
      CopyDetail (VstTable.RootNode);
      CopyText := CopyStrings.GetText;
   finally
      CopyStrings.Free ;
   end ;

   try
      Clipboard.SetTextBuf(CopyText);
   finally
      StrDispose(CopyText);
   end;

end;

//------------------------------------------------------------------------------


end.
