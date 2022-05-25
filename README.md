# ghc-build-analzyer

ghc-build-analzyer is a tool for extracting build timing data from GHC. Add
`-ddump-json -dshow-passes` to your GHC command-line options and then pipe your
build tool to `ghc-build-analzyer`; GHC output will be printed normally and the
JSON messages will be decoded and analyzed (and hopefully eventually sent to
[Honeycomb]).

## Rationale

The `-ddump-timings` option shows how long each module takes to build, but
doesn't give relative timings, so it's impossible to infer parallelism from the
data, which is one of the more important metrics for determining how optimized
our build is / how optimized our build can be.

The [GHC eventlog][well-typed-eventlog] looks like it has what we want, but the
support for it is poor; in particular, you have to specify an output filename
on the command-line, so if your build tool invokes GHC multiple times (common
for larger projects) only the last eventlog will be saved. (And, because we
specify GHC options in `package.yaml` or similar files, we can't do anything
clever like `-ol$(mktemp)` to generate per-invocation eventlog files.)
`-ddump-timings -ddump-to-file` is designed as compiler diagnostics, so it
saves each timing log to a module-specific file in the build directory, but the
eventlog is designed as a generic interface for any program to log events, so
it doesn't have that sort of GHC-specific output tooling.

This leaves us with `-ddump-json -dshow-passes`, which prints a message when
each compiler pass starts and ends, for each module; by intercepting the output
in real-time, we can then infer information about our build's performance.
Unfortunately, `-dshow-passes` seems to be incompatible with `-ddump-to-file`,
so we can't run GHC “normally” and then analyze the output after-the-fact.
Fortunately, it does appear that `-ddump-json -dshow-passes` causes _all_
compiler output to be printed as JSON, which is perfect for an intercepting
tool like this.

[well-typed-eventlog]: https://well-typed.com/blog/2019/09/eventful-ghc/
[Honeycomb]: https://www.honeycomb.io/
