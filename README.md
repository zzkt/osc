# Open Sound Control

This is a common lisp implementation of the Open Sound Control Protocol aka OSC. The code should be close to the ansi standard, and does not rely on any external code/ffi/etc+ to do the basic encoding and decoding of packets. since OSC does not specify a transport layer, messages can be send using TCP or UDP (or carrier pigeons), however it seems UDP is more common amongst the programmes that communicate using the OSC protocol. the osc-examples.lisp file contains a few simple examples of how to send and recieve OSC via UDP, and so far seems reasonably compatible with the packets send from/to max-msp, pd, supercollider and liblo. more details about OSC can be found at http://www.cnmat.berkeley.edu/OpenSoundControl/

the current version of this code is avilable from github

    git clone https://github.com/zzkt/osc 

or via quicklisp.. .

    (ql:quickload "osc")

## limitations

  - doesn't send nested bundles or syncronisable timetags
  - will raise an exception if the input is malformed
  - doesn't do any pattern matching on addresses
  - float en/decoding only tested on sbcl, cmucl, openmcl and allegro 
  - only supports the type(tag)s specified in the OSC spec

## things to do in :osc

  - address patterns using pcre
  - data checking and error handling
  - portable en/decoding of floats -=> ieee754 tests
  - doubles and other defacto typetags

## things to do in :osc-ex[tensions|tras]

  - liblo like network wrapping
  - add namespace exploration using cl-zeroconf

# changes
  - 2019-04-02
     - encoder/decoder refactoring from Javier Olaechea @PuercoPop
  - 2017-12-10
     - osc-examples use usocket for portability from @boqs
  - 2015-08-25
     - support for 64bit ints from Erik Ronström https://github.com/erikronstrom
   - 2015-08-21
     - implement nested bundles from jamieforth https://github.com/jamieforth
  - 2011-04-19
     - converted repo from darcs->git
  - 2007-02-20
     - version 0.5
     - Allegro CL float en/decoding from vincent akkermans <vincent.akkermans@gmail.com>
  - 2006-02-11
     - version 0.4
     - partial timetag implementation
  - 2005-12-05
     - version 0.3
     - fixed openmcl float bug (decode-uint32)
  - 2005-11-29
     - version 0.2
     - openmcl float en/decoding
  - 2005-08-12
     - corrections from Matthew Kennedy <mkennedy@gentoo.org>
  - 2005-08-11
     - version 0.1
  - 2005-03-16
     - packaged as an asdf installable lump
  - 2005-03-11
     - bundle and blob en/de- coding
  - 2005-03-05
     - 'declare' scattering and other optimisations
  - 2005-02-08
     - in-package'd
     - basic dispatcher
  - 2005-03-01
     - fixed address string bug
  - 2005-01-26
     - fixed string handling bug
  - 2005-01-24
     - sends and receives multiple arguments
     - tests in osc-tests.lisp
  - 2004-12-18
     - initial version, single args only

