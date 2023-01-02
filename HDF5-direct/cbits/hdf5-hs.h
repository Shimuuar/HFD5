#pragma once
#include <hdf5.h>

#define hs_hdf5_off_error2_cls_id    (offsetof(H5E_error2_t, cls_id))
#define hs_hdf5_off_error2_maj_num   (offsetof(H5E_error2_t, maj_num))
#define hs_hdf5_off_error2_min_num   (offsetof(H5E_error2_t, min_num))
#define hs_hdf5_off_error2_line      (offsetof(H5E_error2_t, line))
#define hs_hdf5_off_error2_func_name (offsetof(H5E_error2_t, func_name))
#define hs_hdf5_off_error2_file_name (offsetof(H5E_error2_t, file_name))
#define hs_hdf5_off_error2_desc      (offsetof(H5E_error2_t, desc))
