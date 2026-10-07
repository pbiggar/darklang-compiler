(* Timing fields store signed nanoseconds; accounting differences may be negative. *)
(* Generic test-runner state, reporting and timing utilities. *)
open Dark_compiler

type unitTestCase = string * (unit -> (unit, string) result)
type unitTestSuite = { name : string; tests : unitTestCase list }

type failedTestInfo = {
  file : string;
  name : string;
  message : string;
  details : string list;
}

type testTiming = {
  name : string;
  totalTime : int64;
  compileTime : int64 option;
  runtimeTime : int64 option;
}

type passTimingEntry = {
  number : string option;
  name : string;
  elapsed : int64;
}

type passTimingSection = { title : string; entries : passTimingEntry list }

type passTimingColumns = {
  ordered : passTimingSection list;
  byTime : passTimingSection list;
}

type unaccountedTimeBreakdown = {
  unaccounted : int64;
  runtime : int64;
  overhead : int64;
}

type fileSuiteSummary = {
  passed : int;
  failed : int;
  failedTests : failedTestInfo list;
}

type testRunState = {
  mutable passed : int;
  mutable failed : int;
  failedTests : failedTestInfo Queue.t;
  timings : testTiming Queue.t;
  mutable passTimings : int64 StringOrder.Map.t;
  mutable passTimingCounts : int StringOrder.Map.t;
  mutable overheadPassTimingTotal : int64;
  passTimingOrder : string Queue.t;
  completedTestReporter : (int -> unit) option;
}

type outputSymbols = { pass : string; fail : string; sectionPrefix : string }

val testRuntimeTimingName : string
val createStateWithProgressReporter : (int -> unit) option -> testRunState
val createState : unit -> testRunState
val recordTiming : testRunState -> testTiming -> unit
val recordPassTiming : testRunState -> CompilerOptions.passTiming -> unit
val recordResults : testRunState -> int -> int -> failedTestInfo list -> unit
val formatTime : int64 -> string
val calculatePassTimingsTotal : int64 StringOrder.Map.t -> int64

val filterPassTimingsForOverhead :
  int64 StringOrder.Map.t -> int64 StringOrder.Map.t

val calculatePassTimingsTotalForOverhead : int64 StringOrder.Map.t -> int64

val calculateUnaccountedTimeBreakdown :
  int64 ->
  int64 StringOrder.Map.t ->
  testTiming Seq.t ->
  unaccountedTimeBreakdown

val buildPassTimingColumns :
  int64 StringOrder.Map.t -> string list -> int64 -> passTimingColumns

val addExpectedActualDetails : string option -> string option -> string list

val runFileSuite :
  testRunState ->
  outputSymbols ->
  string ->
  string ->
  string array ->
  (string -> string) ->
  (string -> string) ->
  (string -> ('a, string) result) ->
  (ProgressBar.state -> string -> string -> int64 -> 'a -> fileSuiteSummary) ->
  (ProgressBar.state -> string -> string -> int64 -> string -> fileSuiteSummary) ->
  unit

val runUnitTestSuites :
  testRunState ->
  outputSymbols ->
  string ->
  string ->
  unitTestSuite array ->
  unit
