(* Generic test-runner state, reporting and timing utilities. *)
open Dark_compiler
type unitTestCase=string * (unit -> (unit,string) result)
type unitTestSuite={name:string;tests:unitTestCase list}
type failedTestInfo={file:string;name:string;message:string;details:string list}
type testTiming={name:string;totalTime:HostTimeSpan.t;compileTime:HostTimeSpan.t option;runtimeTime:HostTimeSpan.t option}
type passTimingEntry={number:string option;name:string;elapsed:HostTimeSpan.t}
type passTimingSection={title:string;entries:passTimingEntry list}
type passTimingColumns={ordered:passTimingSection list;byTime:passTimingSection list}
type unaccountedTimeBreakdown={unaccounted:HostTimeSpan.t;runtime:HostTimeSpan.t;overhead:HostTimeSpan.t}
type fileSuiteSummary={passed:int;failed:int;failedTests:failedTestInfo list}
type testRunState={mutable passed:int;mutable failed:int;failedTests:failedTestInfo Queue.t;timings:testTiming Queue.t;mutable passTimings:HostTimeSpan.t StringOrder.Map.t;mutable passTimingCounts:int StringOrder.Map.t;passTimingOrder:string Queue.t;completedTestReporter:(int -> unit) option}
type outputSymbols={pass:string;fail:string;sectionPrefix:string}
val testRuntimeTimingName : string
val createStateWithProgressReporter : (int -> unit) option -> testRunState
val createState : unit -> testRunState
val recordTiming : testRunState -> testTiming -> unit
val recordPassTiming : testRunState -> CompilerOptions.passTiming -> unit
val recordResults : testRunState -> int -> int -> failedTestInfo list -> unit
val formatTime : HostTimeSpan.t -> string
val calculatePassTimingsTotal : HostTimeSpan.t StringOrder.Map.t -> HostTimeSpan.t
val filterPassTimingsForOverhead : HostTimeSpan.t StringOrder.Map.t -> HostTimeSpan.t StringOrder.Map.t
val calculatePassTimingsTotalForOverhead : HostTimeSpan.t StringOrder.Map.t -> HostTimeSpan.t
val calculateUnaccountedTimeBreakdown : HostTimeSpan.t -> HostTimeSpan.t StringOrder.Map.t -> testTiming Seq.t -> unaccountedTimeBreakdown
val buildPassTimingColumns : HostTimeSpan.t StringOrder.Map.t -> string list -> HostTimeSpan.t -> passTimingColumns
val addExpectedActualDetails : string option -> string option -> string list
val runFileSuite : testRunState -> outputSymbols -> string -> string -> string array -> (string -> string) -> (string -> string) -> (string -> ('a,string) result) -> (ProgressBar.state -> string -> string -> HostTimeSpan.t -> 'a -> fileSuiteSummary) -> (ProgressBar.state -> string -> string -> HostTimeSpan.t -> string -> fileSuiteSummary) -> unit
val runUnitTestSuites : testRunState -> outputSymbols -> string -> string -> unitTestSuite array -> unit
