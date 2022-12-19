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
    dset[10] = -11
    dset[14] = 256*256
    dset[15] = 256*256 + 1
    #
    dset2 = f.create_dataset("dset2", (100,), dtype='d')
    dset2[:] = np.linspace(0,1, 100)
    dset2.attrs['a1'] = 100
    dset2.attrs['a2'] = 1.2
    dset2.attrs['a3'] = np.asarray([1,2,3], dtype='f')
#    print(dset)
#    print(dset[:])

with h5py.File("/run/user/1000/tst.hdf5", "r") as f:
    dset = f['dset1']
    print(dset)
    print(dset[:])
    dset = f['dset2']
    print(dset)
    print(dset[:])
