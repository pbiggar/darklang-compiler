(* Typed graph topology, coloring preferences and fixture expectations. *)
type countExpectation=Exactly of int|AtMost of int|AtLeast of int
type graphColorTest={name:string;vertices:int list;edges:(int*int) list;availableColors:int;precolored:(int*int) list;preferencePairs:(int*int) list;movePairs:(int*int) list;expectedChromatic:countExpectation option;expectedSpills:countExpectation option;expectedColored:countExpectation option;expectedColors:(int*int) list;expectedSame:(int*int) list;expectedDifferent:(int*int) list;expectMcsCoversAll:bool;expectedSelectionChecks:int option;sourceFile:string}
val parseGraphColorFileContent : string -> string -> (graphColorTest list,string) result
