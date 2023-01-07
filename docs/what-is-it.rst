What is ``ghc-specter``?
========================

``ghc-specter`` is an inspecting tool and visualizer of internal
information in the GHC compilation pipeline via GHC plugins and hooks.

Motivation
----------

Glasgow Haskell Compiler (GHC) is a de facto standard Haskell compiler.
Haskell is special among popular programming languages
as a pure Functional Programming (FP) language with lazy evaluation semantics
and an advanced type system.
The GHC compiler is a culmination of compiler research of decades to aggressively
optimize lazily evaluated FP to the industry strength. Therefore, the compilation
pipeline consists of a sequence of long and complex processes: 
parsing source, typechecking, transformation of Abstract Syntax Tree (AST) to
simplified representation called GHC Core, optimization, and compilation into
low-level and machine-level representation (STG, Cmm, Asm) and external assembler
and linking steps against many architectures.

Developers often need investigation in the middle of the pipeline, but have to
rely only on built-in GHC logging or resort to a custom modification of the
GHC source code. For example, we often need to identify blocking compilation steps
dominating the build time, or to investigate a problematic Template Haskell (TH)
code that is eating up all of the available memory. It is desirable to collect
the relevant information right away from the very process of the GHC compilation
and also intervene the process to inspect the internal state of the compiler.

Fortunately, GHC provides excellent customization mechanisms for power users
-- GHC plugins and GHC hooks, -- which do not require to rebuild the whole compiler and use
that custom compiler for a project's build system.
By simple command-line interface (CLI) flags, one can insert
a custom code at a predefined plugin/hook location in the GHC compiler.


ghc-specter consists of a GHC plugin (``ghc-specter-plugin``) and a web server process
(``ghc-specter-daemon``).
``ghc-specter-plugin`` installs a bunch of plugins and hooks into the build of a given
project under inspection, and enables us to dump GHC's internal state in various
available places. The information is being sent to ``ghc-specter-daemon``, which presents
it visually for the sake of developer's convenience. As arbitrary custom code can be plugged
in by a plugin, ghc-specter also provides interactive inspection method via a web console
interface.

Note that generically, one of the most lacking area of the Haskell ecosystem
is to have good tooling for making internal idiosynchratic information available
to users. ``ghc-specter`` is invented to fill the gap of the need.

Comparison with HLS
-------------------

Haskell Language Server (HLS) is a Haskell IDE server daemon that
implements Langauge Server Protocol (LSP) to improve Haskell developer experience
with modern source code editor capabilities. There is a high-level overlap of the
goal between HLS and ``ghc-specter``. However, they are very complementary to each other.

HLS is a tool that helps Haskell programming tasks, and thus targets the editor
integration and information indexing in order to shortening typical development
iteration cycles. Therefore, its focus is leaning towards the frontend side of
the GHC pipeline: parsing and typechecking. The daemon is designed to collect
information over a longer span of development, not confining to a single run of
GHC. Above all, it is not intended to collect the information from the actual
build process.

On the contrary, ghc-specter is to gather internal information even in a single
invocation of GHC, and to render it available in an user-friendly form.
As GHC is a special compiler with idiosyncratic internal information, we need
a specialized presentation implementation, so we want graphical web interface
and web console. The focus of the tool spans over all of the phases of the GHC
pipeline, not only interested in the user-facing GHC frontend.
It aims at easing identification of GHC-related problems and monitoring the
process.

Supported GHC
-------------

GHC 9.2 and 9.4 are supported as of the version 1.0.0.0.
Because ghc-specter heavily relies on the GHC API and the API is changing
wildly version by version, we will try to support 3 major versions of GHC or less.
