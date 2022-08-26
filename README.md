# Open Sound Control

This is a common lisp implementation of the Open Sound Control Protocol aka OSC. The code should be close to the ansi standard, and does not rely on any external code/ffi/etc+ to do the basic encoding and decoding of packets. since OSC does not specify a transport layer, messages can be send using TCP or UDP (or carrier pigeons), however it seems UDP is more common amongst the programmes that communicate using the OSC protocol. the osc-examples.lisp file contains a few simple examples of how to send and recieve OSC via UDP, and so far seems reasonably compatible with the packets send from/to max-msp, pd, supercollider and liblo. more details about OSC can be found at https://opensoundcontrol.org/


## installation & usage

the current version of this code is avilable from github

`git clone https://github.com/zzkt/osc`

or via quicklisp.. .

`(ql:quickload "osc")`

There are some basic examples in `osc-examples.lisp` and the `devices/examples/osc-device-examples.lisp` file shows how to use a higher-level API for sending and receiving OSC messages.

## limitations

  - will raise an exception if input is malformed
  - no pattern matching on addresses
  - float en/decoding only tested on sbcl, cmucl, openmcl and allegro
  - only supports the type(tag)s specified in the OSC spec

## things to do in :osc

  - address patterns using pcre
  - data checking and error handling
  - portable en/decoding of floats -=> ieee754 tests
  - doubles and other defacto typetags

## things to do in :osc-ex[tensions|tras]

  - liblo like network wrapping (and devices)
  - add namespace exploration using cl-zeroconf (or similar)
