# nanoscope

This is an attempt to build LLVM's Kaleidoscope in Haskell, step by step. Initially, we will start with very little functionality and add features little by little, until we get closer to the Kaleidoscope.

## How to run

The source code at each step can be extracted as follows.

`% git checkout step1 -b step1` 

(for step1)

Then you can run the test with 

`% make test`

See test.sh to see how the program is run.

## How to run with Docker

If you don't have LLVM or Haskell in your environment, you can also run it using Docker.

Build a Docker image with 

`% make docker-build`

then

`% make docker-run`

to run the container to get in. The rest is as described in the "How to Run" section.