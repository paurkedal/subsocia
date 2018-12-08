[![CircleCI](https://circleci.com/gh/paurkedal/subsocia.svg?style=svg)](https://circleci.com/gh/paurkedal/subsocia)

# subsocia - People, organizational data, and access control

## Synopsis

Subsocia implements a data model for organizational data and access control
based sub-classing and binary relations.  The sub-classing relation serves
to build organizational hierarchies.  The binary relations allows
associating attributes to an people relative to their different roles in the
organization, in ways that would be ambiguous in the common directory model.

This project provides an OCaml library, a web interface, and a command line
utility, using PostgreSQL database as data back-end.
