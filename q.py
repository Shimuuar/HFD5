#!/usr/bin/python
"""
"""
import h5py
import numpy as np

with h5py.File("/run/user/1000/tst.hdf5", "w") as f:
#    dset = f['dset1']
#    print(dset)
    dset = f.create_dataset("dset1", (100,), dtype='i')
    dset[12] = 12
#    print(dset)
#    print(dset[:])

with h5py.File("/run/user/1000/tst.hdf5", "r") as f:
    dset = f['dset1']
    print(dset)
    print(dset[:])
