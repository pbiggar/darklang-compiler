# Optimization implementer Context

## Initial Context And Prompt

In docs/investigations, there are a lot of optimization investigations. Select one part of one investigation to validate. If it is already implemented, mark it as done. Otherwise, build the optimization and test it out. It should lead to a measurable improvement, and should not lead to serious performance regressions. 

As part of the experiment, we should add tests that the optimization works, using a test for before- and after- on the appropriate IR. We should report back on the success of the new optimization, as well as its cost (in runtime and compile time, as well as implementation complexity). Rejected optimizations should be noted in a file, and that file should be checked here.
