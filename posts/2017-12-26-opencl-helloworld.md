---
title: Getting Started with OpenCL in Haskell
published-on: 2017-12-26
author: Jonathan Merritt <j.s.merritt@gmail.com>
keywords: OpenCL, Haskell, GPU, tutorial
---

## Introduction

[OpenCL][OPENCL_WIKIPEDIA] provides a powerful way to interface with GPUs, CPUs,
and other hardware. Over the last few weeks, I've found that Haskell has good
support for OpenCL. For a beginner, there are really three different things to
learn about:

  - OpenCL execution model
  - OpenCL C dialect
  - Haskell bindings
  
If you're brand new to OpenCL, I _highly_ recommend this very short series on 
YouTube by Justin Hensley of AMD:

<iframe 
  width="560" 
  height="315" 
  src="https://www.youtube.com/embed/aKtpZuokeEk" 
  frameborder="0" 
  gesture="media" 
  allow="encrypted-media" 
  allowfullscreen></iframe>

It has an early-90s Microsoft-instructional-video vibe, but aside from that, 
it's really a great overview.

## Which OpenCL package?

There are quite a few [OpenCL packages on Hackage][HKG_OPENCL]. I'll try to
summarise them here:

  - `OpenCLRaw` - the original, low-level, thin binding to the OpenCL C library.
    It exposes much of the API using types from `Foreign.C.Types`. Consequently,
    it's not very convenient. The original homepage linked from Hackage is also
    dead.

  - [`OpenCL`][OPENCL] - this is probably the package you want. It's a fork of
    `OpenCLRaw` that provides a slightly higher-level interface. Many of the
    lower-level types from the C library are newtyped or type-aliased in a way
    that makes it easier to use than `OpenCLRaw`. It is working for me, and the
    examples in my blog will use it.

  - `hopencl` - a binding originally written by Benedict Gaster of AMD. I tried
    this package first, but creating OpenCL `Context`s didn't work for me.

  - `OpenCLWrappers` - yet another fork of `OpenCLRaw`. It doesn't seem to offer
    much beyond what the `OpenCL` package does.

  - `language-c-quote` - not an OpenCL binding per-se, but a way to quasi-quote
    OpenCL C code. More on this later.
    
## OpenCL Versions?

I'm writing this on a MacBook Pro, running MacOS High Sierra with OpenCL 1.2.
However, I haven't had any problems (yet) running the `OpenCL` package, which
targets OpenCL 1.0. Consequently, matching the exact version of OpenCL you have
on your system to a Haskell library doesn't seem to be critical.

I'll report more on this in the future if I discover more information.

[OPENCL_WIKIPEDIA]: https://en.wikipedia.org/wiki/OpenCL
[HKG_OPENCL]: https://hackage.haskell.org/packages/search?terms=opencl
[OPENCL]: https://hackage.haskell.org/package/OpenCL

## OpenCL Native Libraries

If you're running a recent MacOS, then you're in luck: OpenCL is already
available from Apple!

If you're on Linux or Windows (or other), you'll probably have to install OpenCL
yourself. I can't provide much guidance here.
