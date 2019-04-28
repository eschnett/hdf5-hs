# [hdf5-hs](https://github.com/eschnett/hdf5-hs)

[HDF5](https://www.hdfgroup.org) bindings for Haskell

* [GitHub](https://github.com/eschnett/hdf5-hs): Source code repository
* TODO: [Hackage](http://hackage.haskell.org/package/hdf5-hs): Haskell
  package and documentation
* TODO: [Stackage](https://www.stackage.org/package/hdf5-hs): Stackage
  snapshots
* [Azure Pipelines](https://dev.azure.com/schnetter/hdf5-hs/_build):
  Build Status [![Build
  Status](https://dev.azure.com/schnetter/hdf5-hs/_apis/build/status/eschnett.hdf5-hs?branchName=master)](https://dev.azure.com/schnetter/hdf5-hs/_build/latest?definitionId=1&branchName=master)
* [CircleCI](https://circleci.com/gh/eschnett/hdf5-hs): Continuous
  integration
  [![CircleCI](https://circleci.com/gh/eschnett/hdf5-hs.svg?style=svg)](https://circleci.com/gh/eschnett/hdf5-hs)



## Overview

HDF5 (the Hierarchical Data Format) is a widely used file format and
library for storing and archiving scientific data. This library
`hdf5-hs` provides Haskell bindings for HDF5.

`hdf5-hs` provides two API levels: A low-level API gives rather direct
access to the HDF5 API, apart from certain "reasonable" mappings from
C to Haskell (e.g. output arguments that are in C stored to a pointer
are in Haskell regular return values). A high-level API simplifies
storing and retrieving arbitrary values that can be serialized. A
possible third level would look similar to the
[`yaml`](https://github.com/snoyberg/yaml) package, allowing
serialization of Haskell values similar to JSON or YAML files.
