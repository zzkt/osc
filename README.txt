
what ->
This is a common-lisp implementation of the Open Sound Control Protocol, aka OSC. It should be close to ansi standard, and does not rely on any external code/ffi/etc+ to do the basic encoding and decoding of packets. since OSC does not specify a transport layer, messages can be send using TCP or UDP, however it seems UDP is more common amongst the programms that communicate using the protocol.

else ->
 - specific info about what is/isnt working can be found in the header of osc.lisp 
 - more details about OSC can be found at 
            http://www.cnmat.berkeley.edu/OpenSoundControl/
 - current versions of this code can be found at http://fo.am/darcs/osc



