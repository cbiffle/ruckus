#lang scribble/manual

@require[pict]
@require[(for-label ruckus)]

@title{Getting Your Ruckus On}

This is a whirlwind walkthrough of creating a simple Ruckus design.  We'll gloss
over details and skip theory to give you a feel for how the tools feel in
practice.  The later sections in this document will fill in the gaps.

You should have completed the @secref{prerequisites}.


@section{Writing a Design}

Ruckus designs are stored as text files.  Create a text file called
@exec{sphere.rkt} and enter:

@codeblock|{
  #lang ruckus

  (sphere 200)
}|

This defines a Ruckus design containing a sphere, with radius 200 units.

Save the file and display it using Ruckus's interactive visualizer,
@exec{viz/spheretrace.rkt}:

@exec{ruckus-3d sphere.rkt}

After a bit of churning, a window should pop up containing a little sphere,
something like this:

@bitmap{doc/sphere-render.png}

In the visualizer, you can use a mouse wheel or scroll gesture to zoom in and
out, and dragging will rotate the design.  But we can't tell it's rotating yet,
because it's a sphere.


@section{Combining Objects}

Let's alter the design with some simple constructive solid geometry: let's
subtract the sphere from a cube.  Without exiting the visualizer, edit the
design file to read as follows:

@codeblock|{
  #lang ruckus

  (difference
    (cube 330)
    (sphere 200))
}|

Switch back to the visualizer and press @exec{F5} to recompile the design.  If
you rotate the design by dragging the mouse, it should now look like this:

@bitmap{doc/sphere-cube-render.png}

It's now a cube with a sphere-shaped void inside.  It would make a lovely desk
trinket; let's 3D print it.


@section{Preparing for Printing}

To move a design into the real world, we need to decide how big one Ruckus
"unit" is.  3D printing software usually assumes that one design unit is one
millimeter, so let's do that.  Our current design would be 330mm across --- much
too big for a desk trinket!  Let's scale the whole thing down by a factor of 10.

@codeblock|{
  #lang ruckus

  (scale 1/10
    (difference
      (cube 330)
      (sphere 200)))
}|

Reload the design in the visualizer using @exec{F5}.  It got tiny, you may need
to zoom in.

To send the shape to the printer, we need to extract the design's surface in a
format called STL, using Ruckus's surface exporter.  We'll call the output file
@exec{sphere.stl}.

@exec{ruckus-export-surface sphere.rks sphere.stl}

You can view the result in a tool such as MeshLab.  It should look something
like this:

@bitmap{doc/sphere-cube-export-low.png}


@section{Reducing Aliasing}

Notice that, while the design looks smooth and sharp in the visualizer, it now
has slightly rounded corners and some visible artifacts.  3D printers want
objects described using tiny triangles, and Ruckus has used 1mm triangles to
cover the surface of our design.  If that's not good enough --- and if your
3D printer is high enough resolution that you'll notice the difference! ---
you can shrink the size of the triangles by adjusting the surface quantum
parameter @exec{-q}.

Let's set it to 0.25, ensuring that any aliasing artifacts are no larger than a
quarter of a millimeter.

@exec{ruckus-export-surface -q 0.25 sphere.rks sphere.stl}

The exporter is much slower (16x slower to be exact), but the output is much
prettier:

@bitmap{doc/sphere-cube-export-higher.png}

If you have enough CPU oomph available, or are just willing to wait, you can
crank the quantum arbitrarily low.  However, our 3D printer will probably not
render quarter-millimeter features anyway.

We now have an STL file of our design suitable for printing!
