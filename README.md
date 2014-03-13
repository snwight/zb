zb
==

the zero bureau data source connector

this crazy thing is a WIP, intended to be a backend for building a 
browser based interface for transforming data streams from a number of 
publically available earth science and astronomy data bases into various
types of encoded output streams - first version will generate MIDI note 
and velocity commands. currently i'm focusing on setting up the necessary
http API configurations for all the DBs and experimenting with abstracting
commonalities - query response data format for instance. most of the field
biology/earth sciences DBs can return JSON, but the astronomy data tends
to favor more machine-friendly binary representations - XML may have to 
be accommodated. I'm working on an IPAC parser, which is a common data 
format, is human readable, and preserves data types and column names.
