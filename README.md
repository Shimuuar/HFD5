This library is attempt to provide convenient high level API for working with
HDF5 in haskell. It contains two packages:

 - `HDF5-direct` provides very thin wrappers for C API of HDF5.
 - `HDF5-hl` is high level API which is should be used
 
API is still very much in flux and could change without warning since I'm still
in process in figuring out how to map huge feature set of HDF5 on haskell.


# Internal API

Internal API (everything from `HDF5.Internal.*` modules which is not exported
elsewhere are part of API and subject to PVP but very little attention is paid
to their stability. Their purpose is to assist in implementation of public
API. If you had to reach for them this is clear sign of deficiencies or missing
parts in the public API.

All function that require that caller maintain some invariants are marked as
unsafe. Improper use will result in bugs, random behavior, and crashes. If
proper use lead to bugs please file issue.
