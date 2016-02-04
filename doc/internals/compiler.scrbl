#lang scribble/manual

@require[racket/sandbox scribble/eval]
@(define my-evaluator
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string])
    (make-evaluator 'racket/base #:requires '(ruckus ruckus/lang/evaluator))))

@title{The Ruckus Compiler}

@section{The Ruckus Language Module}

@margin-note{Racket has built-in support for stuff like this, which is why I
used it to implement Ruckus in the first place.}

The Ruckus language is implemented as a set of rewrite rules that produce valid
Racket code at the design level.  These rewrite rules are invoked on any file
that starts with:

@codeblock{#lang ruckus}

The rewrite rules allow Ruckus forms defining geometry to be used at the
top-level of a Ruckus design language file, while postponing their actual
evaluation so that it doesn't occur during @racket[require].  This is handy,
because it's often useful to instantiate an assembly at the top level of a file
so that it can be viewed or exported --- but when it comes time to reuse
portions of that file in a new design, we rarely want the new design to grow
the other file's top-level assembly.

@subsection{Rewrite Example}

Here's an example.  It defines a reusable 2D bolt pattern, and locally defines
an extruded part to demonstrate the pattern for visual checking.

@codeblock{
  #lang ruckus

  ; Bolt parameters, not provided for other designs.
  (define bolt-spacing 25.4)
  (define bolt-diameter 4.5)
  
  ; Bolt pattern.
  (define (bolt-pattern)
    (define half-sp (bolt-spacing . / . 2))

    (for* ([x '(-1 1)]
           [y '(-1 1)])
      (at (list (* half-sp x) (* half-sp y))
          (circle #:diameter bolt-diameter))))

  ; Publish the bolt pattern for reuse
  (provide bolt-pattern)

  ; To help visualize the pattern, make a simple part around it:
  (extrude 10
    (difference
      (rect (* bolt-spacing 2) (* bolt-spacing 2))
      (bolt-pattern)))
}

If this were a Racket module, any code written at the top level (outside of a
@racket[define] or certain other forms that postpone execution) would be
evaluated the instant this module were @racket[require]-d by another.  Ruckus
rewrites the module into the following Racket code:

@codeblock{
  #lang racket

  ; This require is implicitly inserted into any Ruckus-language file.
  (require ruckus)

  (define bolt-spacing 25.4)
  (define bolt-diameter 4.5)
  
  (define (bolt-pattern)
    (define half-sp (bolt-spacing . / . 2))

    (for* ([x '(-1 1)]
           [y '(-1 1)])
      (at (list (* half-sp x) (* half-sp y))
          (circle #:diameter bolt-diameter))))

  (provide bolt-pattern)

  ; Top-level geometry is wrapped in a function.
  ;
  ; Note: magical-generated-name is not the actual name of this function.
  ; It's actually a name generated to be unique.
  (define (magical-generated-name)
    (extrude 10
      (difference
        (rect (* bolt-spacing 2) (* bolt-spacing 2))
        (bolt-pattern))))

  ; The function is exported in a submodule, under a known name,
  ; so that the Ruckus tools can find it.
  (module* ruckus-metadata #f
    (provide top-level-thunk)
    (define top-level-thunk magical-generated-name))
}

We can infer a couple of things from this example.

First, the name @racket[ruckus-metadata] must not be used in a design, because
the Ruckus design language rewrite rules are going to produce a submodule with
that name.

Second, we don't actually have to write designs using the Ruckus design
language.  We can write them in Racket, or Typed Racket, or standard Scheme,
or any other language supported by the Racket tools.


@subsection{Using Another Racket Language}

Here's a design written in the core Racket language instead of the Ruckus
design language.  You can work in whichever language you prefer, but you'll
have to manually perform the rewrite described above.

@codeblock{
  #lang racket

  ; Must import Ruckus facilities manually.
  (require ruckus)

  ; Wrap our top-level assembly in a function.  Name doesn't matter.
  (define (assembly)
    ; Ruckus language forms are available after (require ruckus)
    (sphere 300))

  ; Expose this as a Ruckus design to the Ruckus tools.
  (module* ruckus-metadata #f
    (provide top-level-thunk)
    (define top-level-thunk assembly))
}


@section{Building the AST}

Ruckus designs are Racket code, not data structures.  This means that a
geometric construct like

@codeblock{(sphere 100)}

is not simply a datum, but a syntactic form that can be evaluated to produce a
result.  But what result?  Ruckus designs don't record the value of geometric
forms like @racket[sphere], but clearly they are having an effect.

Ruckus's design language forms are side-effecting.  Evaluating a form like
@racket[sphere] attaches information about the requested geometry to a
behind-the-scenes data structure known as the @italic{EDSL stack}.

As these forms are evaluated, they build up an @italic{abstract syntax tree} or
AST structure.  This is a data structure that describes a part or assembly at
roughly the same level of abstraction as a design module: primitives like
spheres and combinators like union are represented explicitly at this level.

Because of the behind-the-scenes support for Ruckus forms, you will encounter an
error if you try to evaluate one in some arbitrary context, like the Racket
REPL.  Ruckus provides the @racket[call-with-edsl-root] function to evaluate
some geometry and return the AST.  Details below.

You can also view the AST for a given design file using the
@exec{ruckus-dump-ast} tool.

@subsection{The AST Node Structure}

@defmodule[ruckus/core/model]

@defstruct[node ([type symbol?]
                 [atts list?]
                 [children (listof node?)]
                 [id (or/c integer? #f)]
                 [color (or/c (listof real?) #f)])
                #:transparent]{
  An n-way tree structure used to represent the Ruckus AST.

  @itemlist[
    @item{@racket[node-type]: a symbol indicating the type of node, such as
          @racket['sphere] or @racket['union].}
    @item{@racket[node-atts]: a list of type-specific attribute values.}
    @item{@racket[node-children]: a list of child nodes, which may be empty for
          nodes (like @racket[sphere]) that have no children.}
    @item{@racket[node-id]: a unique identifier filled in during the rewrite
          pass (below).}
    @item{@racket[node-color]: a color assigned by the @racket[color] form,
          or @racket[#f] for the default.}
  ]
}

@subsection{The EDSL Evaluator}

@defmodule[ruckus/lang/evaluator]

@defproc[(call-with-edsl-root [p procedure?]) node?]{
  Sets up the EDSL stack and geometry environment for a new design, then calls
  @racket[p] with no arguments.

  Returns the @racket[node] that results from evaluating any Ruckus design
  language forms inside @racket[p].  If there are no such forms, the result is
  an empty node.

  @racket[call-with-edsl-root] is mostly useful if you are implementing tools
  that interact directly with Ruckus designs, or want to experiment with Ruckus
  design language forms in the REPL:

  @interaction[#:eval my-evaluator
    (require ruckus)
    (sphere 100)
    (require ruckus/lang/evaluator)
    (call-with-edsl-root (lambda () (sphere 100)))
  ]

  The result of @racket[call-with-edsl-root] is always of @racket[node-type]
  @racket['root], and is @italic{non-canonical}.  To fix this, apply
  @racket[canonicalize].
}


@section{AST Rewriting}

The AST rewrite pass is responsible for @italic{canonicalizing} the AST and
@italic{enumerating} the leaf nodes.

@subsection{Canonicalization}

@defmodule[ruckus/core/compiler/canon]

@defproc[(canonicalize [n node?]) (listof node?)]{
  Canonicalizes a node.  In general, the process involves:

  @itemlist[
    @item{Rewriting @racket['root] nodes into @racket['union].}
    @item{Converting nodes with many children into binary trees.}
    @item{Flattening combinators with a single child node.}
    @item{Combining nested transforms that can be combined (e.g. nested
          @racket[at] forms).}
    @item{Identifying and removing do-nothing transforms (e.g. translation by
          zero).}
  ]

  The result of @racket[canonicalize] may be:

  @itemlist[
    @item{Zero nodes, if the node contents were completely eliminated by
          flattening and combining,}
    @item{One node, or}
    @item{Many nodes, if @racket[n] represented a do-nothing transform with
          several children.}
  ]

  If @racket[n] is known to be a @racket['root] node, of the form returned from
  @racket[call-with-edsl-root], then the "many nodes" case won't happen.

  You can view the result of canonicalization on a design using
  @exec{ruckus-dump-ast -c}.
}

@subsection{Enumeration}

@defmodule[ruckus/core/compiler/enumerate]

@defproc[(enumerate-nodes [next-id integer?] [n node?])
         (values integer? node?)]{
  Recursively enumerates nodes in @racket[n], returning a @racket[node] of the
  same shape, but wherein every node has either children or a unique
  @racket[node-id].

  This is used in later phases that need to discriminate between different parts
  of a surface, e.g. to change colors or material properties.
}


@section{Lowering to Ruckus IR}

@defmodule[ruckus/core/compiler/lower]

After the AST is canonicalized and enumerated, we @italic{lower} it to a
different form: the Ruckus Intermediate Representation, or IR.  This breaks down
complex operations like @racket[smooth-union] or @racket[radial-repeat] into
graphs of more primitive operations.  As a result, it loses most of the
high-level structural information present in the AST, but makes it much easier
to perform target-specific code generation and optimization.

The Ruckus IR is a static single assignment code for describing branchless
arithmetic trees as value flow graphs.  IR programs are pure and the evaluation
semantics (e.g. eager, lazy) are not specified.  IR code looks vaguely like an
assembly language represented using s-expressions with an infinite register set.

The goal of an IR program is to simultaneously evaluate the Lipschitz-continuous
signed distance bound field, and to produce the @racket[node-id] of the nearest
surface.  Value ("register") zero contains the "query point," the point in
3-space where the field is being evaluated.

You can view the IR for a design using the @exec{ruckus-dump-ir} tool.  Here is
a design and its IR; the sections below will go into more detail about what this
means.

@codeblock{
  #lang ruckus

  (difference
    (at '[10 0 0] (cube 170))
    (sphere 100))
}

@codeblock{
  '(
      (assign3f 1 (sub 3 (r 0) (c3f #(struct:vec3 10 0 0))))
      (assignf 2 (box (c3f #(struct:vec3 85 85 85)) (r 1)))
      (assignu 3 (cu 0))
      (assignf 4 (sphere (cf 100) (r 0)))
      (assignu 5 (cu 1))
      (assignf 6 (sub 1 (cf 0) (r 4)))
      (assignf 7 (max (r 2) (r 6)))
      (assignu 8 (choose (> (r 2) (r 6)) (r 3) (r 5))))
  ; Signed distance bound in value 7
  ; Dominating node ID in value 8
}

@defproc[(generate-statements [n node?]) list?]{
  Generates Ruckus IR assignments implementing @racket[n].  Internally
  canonicalizes and enumerates @racket[n], so you don't need to.
}

@subsection{Assignments}

An IR program consists of a list of @italic{value assignments}.  Each assignment
defines a new register (by number) whose value is the content of a particular
expression.  The general form is:

@racket['(assign-type register-number expression)]

@itemlist[
  @item{@racket['(assignf r e)] evaluates @racket[e] for a floating point
        result, which gets stored in register number @racket[r].}
  @item{@racket['(assignu r e)] evaluates @racket[e] for an unsigned integer
        result, which gets stored in register number @racket[r].}
  @item{@racket['(assign3f r e)] evaluates @racket[e] for an 3-vector floating
        point result, which gets stored in register number @racket[r].}
]

It's an error if a single Ruckus IR sequence contains two assignments to the
same register number.  Because register zero is predefined, an IR program should
also not contain an assignment to register zero.

@subsection{Expressions}

Expressions have a form similar to Racket expressions: they are lists with the
function first and some number of arguments after that:

@racket['(operation-type arg1 arg2 ...)]

For a precise list of operation types permitted, see
@racketmodname[ruckus/core/compiler/racket].  Here are some examples for
illustrative purposes:

@itemlist[
  @item{@racket['(r n)] is the value in register @racket[n].}
  @item{@racket['(cf x)] is a floating point constant with value @racket[x].}
  @item{@racket['(cf3 x y z)] is a 3-vector floating point constant.}
  @item{@racket['(add dim a b)] adds the result of two expressions @racket[a]
        and @racket[b], which must have dimension @racket[dim] (e.g. 1 for
        scalar, 3 for e-vector, etc.).  The result has the same dimension.}
  @item{@racket['(choose flag a b)] is equivalent to Racket @racket[if] or C's
        ternary operator: depending on the value of expression @racket[flag],
        the @racket[choose] expression evaluates to one of the expressions
        @racket[a] (if true) or @racket[b] (if false).}
]

Unlike most assembly languages, expressions in Ruckus IR can be arbitrarily
nested.  The following is a valid assigment in Ruckus IR:

@racket['(assignf 10 (add 1 (r 6) (mul 1 (cf 3) (r 7))))]

That's essentially equivalent to the Racket code

@codeblock{
  (define r6 ...)
  (define r7 ...)
  (define r10 (+ r6 (* 3 r7)))
}

@subsection{Value Pruning}

@defproc[(prune-statements [statements list?] [result integer?]) list?]{
  Given a list of @racket[statements], of the sort produced by
  @racket[generate-statements], returns a similar list containing only those
  assignments that contribute to the production of the numbered value
  @racket[result].

  Because the default IR programs compute two different things simultaneously
  --- the signed distance bound field and the dominating node ID --- backends
  use @racket[prune-statements] to optimize if only one or the other is
  required.

  Note that, in general, computing the dominating node ID is more expensive than
  computing the distance field bound, even after pruning.

  You can see the result of pruning on a design by passing the @exec{-p} switch
  to the @exec{ruckus-dump-ir} tool.
}

@section{Target Backends}

Ruckus does not interpret or execute Ruckus IR programs directly.  The target
backends are responsible for transforming Ruckus IR code into some form of
executable code.

There are currently two backends implemented, one that generates Racket, and one
that generates the OpenGL Shader Language (GLSL).

@subsection{The Racket Backend}

@defmodule[ruckus/core/compiler/racket]

The Racket backend is the simplest backend, and the first one to read if you're
curious.  It converts nodes into functions (going through IR internally).

The functions returned in both cases are produced by @racket[eval], not
interpreted at the IR level.

The backend works by recursively rewriting IR expressions into equivalent Racket
expressions, and using @racket[let] forms to declare Racket variables
corresponding to IR values.  (Value number 12 produces a variable named
@tt{r12}.)

You can view the Racket code corresponding to a design using the
@exec{ruckus-dump-rkt} tool.

@defproc[(node->distance-function [n node?]) (vec3? . -> . real?)]{
  Converts @racket[n] into a Racket function that evaluates the design's signed
  distance bound function.
}

@defproc[(node->discriminator [n node?]) (vec3? . -> . integer?)]{
  Converts @racket[n] into a Racket function that finds the node ID of the
  nearest surface to a given point.
}


@subsection{The GLSL Backend}

@defmodule[ruckus/viz/glsl]

The GLSL backend is more complicated than the Racket backend, because GLSL is a
C-style braces-and-semicolons language with explicit type declarations.  (It's
still only about 100 lines of code.)

You can view the GLSL code corresponding to a design using the
@exec{ruckus-dump-glsl} tool.

@defproc[(node->glsl-distance [n node?]) (listof string?)]{
  Converts @racket[n] into the text of a GLSL function that evaluates the
  design's signed distance bound function.  The result contains one string per
  line.
}

@defproc[(node->glsl-disc [n node?]) (listof string?)]{
  Converts @racket[n] into the text of a GLSL function that finds the node ID of
  the nearest surface to a given point.  The result contains one string per
  line.
}


