#lang scribble/manual

@title{The Ruckus Tools}

Ruckus is primarily driven from the command-line.  Here are the command-line
tools.

This page will summarize command options used with the tools, but for
up-to-date and comprehensive information, run the tool with the @exec{--help}
switch, e.g.

@exec{ruckus-3d --help}


@section{Visualization}

@subsection{ruckus-3d}

Displays a design using GPU-accelerated ray-tracing.  Typical invocation:

@exec{ruckus-3d path/to/my/design.rkt}

Drag the design to rotate it around the origin.  The mouse wheel or scroll
gesture zooms in and out.

Useful keystrokes include:

@itemlist[
  @item{@exec{F5} recompiles and reloads the design, reporting any issues to
        the terminal.}
  @item{Spacebar switches between raytraced view and two diagnostic views.
        The first shows raytracing complexity as a heat map; if a design is
        showing graphical glitches, this might be informative.  The second is
        not currently very useful.}
  @item{The square brackets @exec{[} and @exec{]} increase and decrease the
        epsilon value used to test ray intersection.  If sharp corners appear
        rounded, try decreasing epsilon, though it might drop the frame rate
        if your design is complex (or your graphics card is from Intel).}
]

@subsection{ruckus-2d}

Displays a 2D design, or the intersection of a 3D design with the XY plane.
Typical invocation:

@exec{ruckus-2d path/to/my/design.rkt}

The mouse wheel or scroll gesture zooms in and out.

Useful keystrokes include:

@itemlist[
  @item{@exec{F5} recompiles and reloads the design, reporting any issues to
        the terminal.}
  @item{The square brackets @exec{[} and @exec{]} increase and decrease the
        edge threshold, which is basically the outline width.}
]


@section{Export}

@subsection{ruckus-export-surface}

Exports the surface of a design by triangulation (STL format).  Typical
invocation:

@exec{ruckus-export-surface path/to/my/design.rkt path/to/output.stl}

Ruckus's variant of the Marching Cubes / Tetrahedra algorithms mean that this
tool takes time proportional to the number of triangles emitted.  Reducing the
triangulation quantum (below) by half increases processing time by around 4x.

If a design manages to violate the Lipschitz criterion, there may be holes in
the exported surface mesh.  This is a bug.  You can work around it by disabling
Lipschitz gradient descent in the exporter (@exec{--brute}).

STL files can be interpreted in a variety of real-world units (such as
millimeters), depending on the tools used and their configuration.  Ruckus has
no particular opinion on this; exported STL files are in your design units,
whatever those may be.  For dimensional accuracy you will need to ensure that
the tools @italic{consuming} your STL files are configured to match your
design.

Command line options (check @exec{--help} for complete list):

@itemlist[
  @item{@exec{--dimension <s>} or @exec{-d <s>} sets the size of the initial
        region of interest (ROI), in design units.  The ROI is a cube centered
        on the origin.  The surface exporter is not guaranteed to find surfaces
        outside the ROI, so set this large enough to cover your whole design.
        (On the other hand, it @italic{may} find surfaces @italic{outside} the
        ROI, so don't use this to clip your design.)

        It's safe to set the ROI pretty large; it costs very little.}

  @item{@exec{--quantum <s>} or @exec{-q <s>} controls the triangulation
        quantum.  The ROI will be chopped up into cubes of this size, which
        will then be triangulated separately.  As a result, this also controls
        the maximum size of any aliasing artifacts in the output.}

  @item{@exec{--cubes} and @exec{--tets} choose the triangulation algorithm:
        Marching Cubes (default) or Marching Tetrahedra.  The output from
        Marching Tetrahedra tends to have subjectively worse aliasing
        artifacts, but it's here in case you need it.}

  @item{@exec{--lipschitz} and @exec{--brute} choose the spatial subdivision
        algorithm: Lipschitz-gradient descent (default) or brute force.  You
        might want to switch to brute force if the mesh has holes in it, but
        be warned that it's exponentially slower.}
]

@subsection{ruckus-export-outline}

Exports the outline of a 2D design, or of the intersection of a 3D design with
the XY plane, in SVG format.  Typical invocation:

@exec{ruckus-export-outline path/to/my/design.rkt path/to/output.svg}

The output is intended for tool path planning, such as laser cutting, and so
cosmetic attributes like color and fill are not included.

Ruckus's variant of the Marching Squares algorithm mean that this tool takes
time proportional to the number of outline segments emitted.  Reducing the
segment quantum (below) by half increases processing time by around 2x.

SVG files can measure their contents in real-world units.  By default, SVG
files emitted by @exec{ruckus-export-outline} assume that one design unit is
one millimeter.  To override this, use one of the command line options listed
below.

Command line options (check @exec{--help} for complete list):

@itemlist[
  @item{@exec{--dimension <s>} or @exec{-d <s>} sets the size of the initial
        region of interest (ROI), in design units.  The ROI is a square
        centered on the origin.  The outline exporter is not guaranteed to find
        outlines outside the ROI, so set this large enough to cover your whole
        design.  (On the other hand, it @italic{may} find outlines
        @italic{outside} the ROI, so don't use this to clip your design.)

        It's safe to set the ROI pretty large; it costs very little.}

  @item{@exec{--quantum <s>} or @exec{-q <s>} controls the segment quantum.
        The ROI will be chopped up into squares of this size, which will then
        be separately stuffed with line segments.  As a result, this also
        controls the maximum size of any aliasing artifacts in the output.}

  @item{@exec{--mm}, @exec{--cm}, @exec{--inch}, @exec{--pt}, and @exec{--px}
        set the size of a design unit in the real world --- to one millimeter,
        centimeter, inch, typographical point, or pixel, respectively.

        SVG uses pixels as its default units, so most tools will interpret files
        specified in pixels as not having specific units attached.}
]
