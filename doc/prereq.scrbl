#lang scribble/manual

@title[#:tag "prerequisites"]{Prerequisites}

To use Ruckus, you will need:

@itemlist[
  @item{A computer running Linux (preferably) or Mac OS X (if you are brave).}
  @item{A reasonably beefy GPU, ideally from Nvidia.}
  @item{A recent version of @hyperlink["https://racket-lang.org/"]{Racket} ---
        say, at least 6.0.}
  @item{The Ruckus source code.}
  @item{Some patience and willingness to experiment.}
]

@section{Operating System}

Ruckus is developed and tested on Linux.  It should also work on other
Unix-like operating systems, such as Mac OS X or FreeBSD, but you may encounter
surprises.  @bold{I'd be interested in hearing about any problems you
encounter.}

There's no clear technical reason why Ruckus wouldn't work on Microsoft
Windows, but:

@itemlist[
  @item{Ruckus is command-line driven, which may be difficult with Windows'
        weak command-line environment.}
  @item{Ruckus visualizers lean heavily on OpenGL, which has spotty support on
        Windows.}
]

If you want to give it a shot, go for it.  @bold{I'd be interested in any
@italic{fixes} you develop for Windows,} but I probably can't help develop
them.

@section{Graphics Card}

Ruckus uses your computer's GPU to accelerate certain operations.  The better
your GPU, the faster you'll get results.

Currently, best results come from Kepler or Maxwell architecture Nvidia GPUs
(say, GTX 650 and later) with recent driver versions.  You don't need a fancy
Quadro; they're actually slower for Ruckus than the mass-market cards of
similar age.

Intel HD Graphics built into Haswell and later laptops can be used in a pinch.
I do a fair amount of development and testing on a Lenovo T440s.

Older Intel graphics will prove quite frustrating; the Lenovo T430s is unusable.

@section{Racket}

Ruckus is written in Racket, a programming language from the Lisp family.
Because Ruckus is distributed in source form, you will need a copy of the
Racket tools to run it.

Ruckus is most heavily tested on Racket version 6.3, which is a recent but not
bleeding-edge version.

@bold{Ruckus is known not to work on Racket version 5.x}, the ancient version
currently (spring 2016) being distributed with the Ubuntu LTS.  You will need
to install Racket manually on such computers.

@section{Ruckus Source Code}

Ruckus is not yet being publicly distributed.  Contact me.
