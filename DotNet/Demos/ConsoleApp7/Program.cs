// See https://aka.ms/new-console-template for more information
using TraceTool;

TTrace.Options.SocketHost = "127.0.0.1";
TTrace.Options.SendMode = SendMode.Socket;
TTrace.Options.SocketPort = 8090;
TTrace.Options.UseWorkerThread = true; // sync , default

TTrace.ClearAll();

var myClass = new MyClass();
myClass.Name = "Foo";
TTrace.Debug.SendValue("myClass", myClass);
TTrace.Debug.SendObject("myClass", myClass);

var myStruct = new MyStruct();
//myStruct.X = 1;  // readonly
TTrace.Debug.SendValue("myStruct", myStruct);
TTrace.Debug.SendObject("myStruct", myStruct);

var myRecord = new MyRecord("my foo");
myRecord.Bar = "bar";
//myRecord.Foo = "foo";  // read only
TTrace.Debug.SendValue("myRecord", myRecord);
TTrace.Debug.SendObject("myRecord", myRecord);

// Indent/Unindent on a Single thread
TTrace.Debug.Indent("Indent A, same thread");
foreach (var counter in new[] { 0, 1, 2, 3, 4, 5 })
{
    TTrace.Debug.Send($"under Indent A : [{counter}]").Show();
    // No await here !
}
TTrace.Debug.UnIndent("UnIndent A");

// Indent/Unindent with Async task
TTrace.Debug.Indent("Indent B, with async");
foreach (var counter in new[] { 0, 1, 2, 3, 4, 5 })
{
    if (counter == 3)
        await Task.Run(async () =>
        {
            await Task.Delay(500);
            TTrace.Debug.Send("under Indent B [3] Async, from 'possible' another thread");
            await TTrace.FlushAsync();
        });
    else
        TTrace.Debug.Send($"under Indent B : [{counter}]").Show();
}
TTrace.Debug.UnIndent("UnIndent B");
await TTrace.FlushAsync();

// using a TraceNode and Async
var traceNode = TTrace.Debug.Send("Using trace Node C, with async");
foreach (var counter in new[] { 0, 1, 2, 3, 4, 5 })
{
    if (counter == 3)
        await Task.Run(async () =>
        {
            await Task.Delay(1000);
            traceNode.Send("under Node C [3] Async, from 'possible' another thread");
            await TTrace.FlushAsync();
        });
    else
        traceNode.Send($"under Node C : [{counter}]").Show();
}
await TTrace.FlushAsync();


public class MyClass
{ 
    public string Name { get; set; } = null!;
}

public struct MyStruct 
{
    public double X { get; }
    public double Y { get; }
}

public record MyRecord (string Foo)
{
    public string Bar { get; set; } = null!;
}

