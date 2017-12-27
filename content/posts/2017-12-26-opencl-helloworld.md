---
title: Getting Started with OpenCL in Haskell
published-on: 2017-12-26
author: Jonathan Merritt
tags: OpenCL, Haskell, GPU, tutorial
---

## Introduction

[OpenCL][OPENCL_WIKIPEDIA] provides a way to interface with GPUs, CPUs, and
other hardware to perform portable parallel processing. Over the last few weeks,
I've found that Haskell has good support for OpenCL. For a beginner like myself,
there are really three different things to learn about:

  - OpenCL execution model
  - OpenCL C dialect
  - Haskell bindings
  
If you're brand new to OpenCL, I _highly_ recommend this very short series on 
YouTube by Justin Hensley of AMD:

<p><center><iframe 
  width="560" 
  height="315" 
  src="https://www.youtube.com/embed/aKtpZuokeEk" 
  frameborder="0" 
  gesture="media" 
  allow="encrypted-media" 
  allowfullscreen></iframe></center></p>

It has an early-90s Microsoft-instructional-video vibe, but aside from that, 
it's really a great overview.

## Which OpenCL package?

There are several [OpenCL packages on Hackage][HKG_OPENCL], and it's not
particularly clear which you should use. I'll try to summarise them here:

  - [`OpenCL`][OPENCL] - this is probably the package you want. It's a fork of
    `OpenCLRaw` that provides a higher-level interface. This is the package I'll
    use in my examples. (Thanks to Anthony Cowley for suggesting this package.)

  - [`OpenCLRaw`][OPENCLRAW] - the original thin binding to the OpenCL C
    library. It exposes much of the API using types from `Foreign.C.Types` and,
    as a result, it's not very convenient. The original homepage link from
    Hackage is also dead.

  - [`hopencl`][HOPENCL] - a binding originally written by Benedict Gaster of
    AMD. I tried this package first, but creating OpenCL `Context`s didn't work
    for me.

  - [`OpenCLWrappers`][OPENCLWRAPPERS] - yet another fork of `OpenCLRaw`. It
    doesn't seem to offer much beyond what the `OpenCL` package does. I haven't
    tried using it however.

  - [`language-c-quote`][LANGUAGECQUOTE] - not an OpenCL binding per-se, but a
    way to quasi-quote OpenCL C code. More on this later.

For the sake of completeness, these are the packages I'll use in this example:

  - [`OpenCL`][OPENCL] - the OpenCL bindings
  - [`CLUtil`][CLUTIL] - utilities built on `OpenCL`
  - [`language-c-quote`][LANGUAGECQUOTE] - OpenCL C quasiquoting
  - [`mainland-pretty`][MAINLANDPRETTY] - pretty-printing quasiquoted OpenCL C
  - [`vector`][VECTOR] - indexed arrays for storing data
    
## What about OpenCL versions?

It doesn't seem to be necessary to match the OpenCL version on the machine
you're using with the OpenCL version targeted by a Haskell package. For
instance, I'm writing this on a MacBook Pro with OpenCL 1.2, but I haven't had
any problems (yet) running the `OpenCL` package, which targets OpenCL 1.0.

The OpenCL C language is intended to be backwards compatible (this is mentioned
in the spec), but I'm not sure to what extent this also extends to the runtime.
I'll report more on this in the future if I discover any important caveats.

## OpenCL Native Libraries

MacOS has had its own OpenCL implementation since Snow Leopard (10.6), which
works with the `OpenCL` Haskell package. If you're using a different platform, I
can't provide any guidance, except to say that you'll probably need to install
something to provide an Installable Client Driver (ICD) for OpenCL.

## Hello-World Example

The example I'll cover in this post is taken from my 
[haskell-opencl-examples][HSCLEX] project on GitHub. Specifically, this is
example [01-hello-world/Main.hs][HSCLEX01].

### Imports and Pragmas

I use the `QuasiQuotes` language extension to quasiquote an OpenCL C kernel.

~~~~{#pragmas .haskell}
{-# LANGUAGE QuasiQuotes #-}
~~~~

This is the full list of imports. Everything is imported explicitly or in
qualified form *except* for the `Control.Parallel.OpenCL` package.

~~~~{#imports .haskell}
import           Control.Parallel.CLUtil         (OpenCLState (OpenCLState),
                                                  bufferToVector, clContext,
                                                  clDevice, clQueue,
                                                  writeVectorToBuffer)
import           Control.Parallel.OpenCL

import           Control.Monad                   (forM_)
import           Data.Vector.Storable            (Vector)
import qualified Data.Vector.Storable            as V
import           Foreign                         (nullPtr, sizeOf)
import           Foreign.C.Types                 (CFloat)
import           Language.C.Quote.OpenCL         (cfun)
import           Text.PrettyPrint.Mainland       (prettyCompact)
import           Text.PrettyPrint.Mainland.Class (ppr)
~~~~

### Platforms, Devices and Contexts

The platform model of OpenCL centers around the notions of Platform, Device
and Context:

Platform
: A set of OpenCL Devices available to a host; allows creation of Contexts.

Device
: Something like a GPU or CPU.

Context
: A group of Devices for computation.

Most interactions with OpenCL occur in `IO`. The source examples below indicate
when they're occuring inside a `do` block of `IO`.

Important functions for enumerating the platforms and devices are the following:

- `clGetPlatformIDs` - lists OpenCL platforms
- `clGetDeviceIDs` - lists devices of a given type for a platform
- `clGetPlatformInfo` - information about a platform
- `clGetDeviceXXX` - various information about a device

We can get an overview of the OpenCL environment of a machine with a function
like `describePlatforms` below.

~~~~{#platformInfo .haskell}
-- | Summarises the OpenCL Platforms and their Devices.
--
--   The names of Platforms and all Devices belonging to them are printed to
--   stdout.
describePlatforms :: IO ()
describePlatforms = do

    -- fetch the list of OpenCL Platforms
    platformList <- clGetPlatformIDs :: IO [CLPlatformID]

    -- for each platform,
    forM_ platformList $ \platform -> do

        -- fetch the list of OpenCL Devices
        devs <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL :: IO [CLDeviceID]

        -- print the Platform name and Device names
        pname platform
        forM_ devs dname

  where
      putPair name value = putStrLn (name ++ value)
      pname p = clGetPlatformInfo p CL_PLATFORM_NAME >>= putPair "Platform: "
      dname d = clGetDeviceName d                    >>= putPair "  Device: "
~~~~

On my machine, this produces the following output:

<div class="repl">
~~~~
Platform: Apple
  Device: Intel(R) Core(TM) i7-6920HQ CPU @ 2.90GHz
  Device: Intel(R) HD Graphics 530
  Device: AMD Radeon Pro 460 Compute Engine
~~~~
</div>

### Context and Queue Creation

In order to perform a computation, we need to create a context in which the
computation will run. The context groups devices and allows creation of things
like memory buffers, queues and compiled kernels. Contexts are created using
either `clCreateContext` or `clCreateContextFromType`. In the example below,
we'll create a context for a CPU device using `clCreateContextFromType`.

OpenCL uses an asynchronous processing model. Commands are sent to a queue, and
are then executed in a way that is determined by the OpenCL implementation.
These enqueued operations can perform actions such as copying memory, executing
kernels, and so on. Dependencies between enqueued actions are expressed by
passing pointers to them at various points in the API. To create a queue for
commands, we can use `clCreateCommandQueue`

~~~~ {#create-context .haskell}
-- (... inside an IO do block ...)

-- Create a Context, Queue and a CLUtil OpenCLState
context <- clCreateContextFromType [] [CL_DEVICE_TYPE_CPU] print
device  <- head <$> clGetContextDevices context
queue   <- clCreateCommandQueue context device []
-- NB: OpenCLState is used by CLUtil when manipulating vector buffers
let state = OpenCLState
          { clDevice  = device
          , clContext = context
          , clQueue   = queue
          }
~~~~

If any errors occur, they will be thrown as exceptions in `IO`. The last
parameter to `clCreateContextFromType` is a function of type `String -> IO ()`,
which is also used to report errors.

### Kernel

The kernel is the OpenCL code that we're going to execute. This example uses
the `language-c-quote` package so that the kernel source can be quasi-quoted.

~~~~ {#kernel-source .haskell}
-- | The kernel to execute: the equivalient of 'map (*2)'.
kernelSource :: String
kernelSource = prettyCompact . ppr $ [cfun|
    /* This example kernel just does `map (*2)` */
    kernel void doubleArray(
        global float *in,
        global float *out
    ) {
        int i = get_global_id(0);
        out[i] = 2 * in[i];
    }
|]
~~~~

In this example, the kernel source is supplied to OpenCL as a `String`. The
kernel has to be compiled by OpenCL at runtime into a form that can be executed
on the hardware we've chosen.

~~~~ {#kernel-compile .haskell}
-- (... inside an IO do block ...)

-- Create the Kernel
program <- clCreateProgramWithSource context kernelSource
clBuildProgram program [device] ""
kernel <- clCreateKernel program "doubleArray"
~~~~

### Buffers

Data is sent from Haskell to OpenCL using a buffer. In this example, we're going
to use Storable Vectors (from the `vector` package) to hold data on the Haskell
side. These are a good choice because they store data contigously
under-the-hood.

In order to use Vectors easily with OpenCL, we'll make use of the
[CLUtil][CLUtil] library by Anthony Cowley et al.

~~~~ {#buffers .haskell}
-- (... inside an IO do block ...)

-- Set up memory
let
    inputData :: Vector CFloat
    inputData = V.fromList [(-4) .. 4]

    nElem  = V.length inputData
    nBytes = nElem * sizeOf (undefined :: CFloat)

-- Buffers for input and output data.
-- We request OpenCL to create a buffer on the host (CL_MEM_ALLOC_HOST_PTR)
-- since we're using CPU. The performance here may not be ideal, because
-- we're copying the buffer. However, it's safe
bufIn <- clCreateBuffer context
                        [CL_MEM_READ_ONLY, CL_MEM_ALLOC_HOST_PTR]
                        (nBytes, nullPtr)
bufOut <- clCreateBuffer context
                         [CL_MEM_WRITE_ONLY, CL_MEM_ALLOC_HOST_PTR]
                         (nBytes, nullPtr)

-- Copy our input data Vector to the input buffer; blocks until complete
writeVectorToBuffer state bufIn inputData
~~~~

### Run the kernel

To run the compiled kernel, we hook up the buffers to the kernel arguments with
and then enqueue the kernel to be run.

~~~~ {#kernel-run .haskell}
-- (... inside an IO do block ...)

-- Run the kernel
clSetKernelArgSto kernel 0 bufIn
clSetKernelArgSto kernel 1 bufOut
execEvent <- clEnqueueNDRangeKernel queue kernel [nElem] [] []
~~~~

### Copy output data

At this point, the kernel has not necessary even started running. OpenCL has an
asynchronous processing model where many execution details are left to the
implementation. Consequently, it's important to wait for computations to finish.
In this case, we'll use `bufferToVector`, from `CLUtil`, which internally waits
for the kernel execution to complete. It knows the dependent operations because
`execEvent` is passed in as a parameter. `bufferToVector` returns a new vector
when it has finished, creating it from the `bufOut` output buffer.

~~~~ {#kernel-run .haskell}
-- (... inside an IO do block ...)

-- Get the result; blocks until complete
outputData <- bufferToVector queue
                             bufOut
                             nElem
                             [execEvent]
                             :: IO (Vector CFloat)
~~~~

And that's it! We can print the `outputData` vector to confirm that the
operation worked as expected and multiplied all the elements by 2.

## Recap

This example covered:

- basics of OpenCL
- choice OpenCL package
- platform model: Platform, Context and Device
- compiling a kernel
- buffer management
- running a kernel


[OPENCL_WIKIPEDIA]: https://en.wikipedia.org/wiki/OpenCL
[HKG_OPENCL]: https://hackage.haskell.org/packages/search?terms=opencl
[OPENCL]: https://hackage.haskell.org/package/OpenCL
[OPENCLRAW]: https://hackage.haskell.org/package/OpenCLRaw
[HOPENCL]: https://hackage.haskell.org/package/hopencl
[OPENCLWRAPPERS]: https://hackage.haskell.org/package/OpenCLWrappers
[LANGUAGECQUOTE]: https://hackage.haskell.org/package/language-c-quote
[HSCLEX]: https://github.com/lancelet/haskell-opencl-examples
[HSCLEX01]: https://github.com/lancelet/haskell-opencl-examples/blob/master/app/01-hello-world/Main.hs
[CLUTIL]: https://github.com/acowley/CLUtil
[MAINLANDPRETTY]: https://hackage.haskell.org/package/mainland-pretty
[VECTOR]: https://hackage.haskell.org/package/vector
