#pragma once
#include <hdf5.h>
#include <hdf5_hl.h>

// Defines for checking whether we have library with support for
// thread safety
#ifdef H5_HAVE_THREADSAFE
#define HS_H5_THREADSAFE 1
#error Support for thread-safe variant of HDF5 is not implemented yet
#else
#define HS_H5_THREADSAFE 0
#endif

// Offsets for field of H5E_error2_t struct
#define hs_hdf5_off_error2_cls_id    (offsetof(H5E_error2_t, cls_id))
#define hs_hdf5_off_error2_maj_num   (offsetof(H5E_error2_t, maj_num))
#define hs_hdf5_off_error2_min_num   (offsetof(H5E_error2_t, min_num))
#define hs_hdf5_off_error2_line      (offsetof(H5E_error2_t, line))
#define hs_hdf5_off_error2_func_name (offsetof(H5E_error2_t, func_name))
#define hs_hdf5_off_error2_file_name (offsetof(H5E_error2_t, file_name))
#define hs_hdf5_off_error2_desc      (offsetof(H5E_error2_t, desc))


// Wrapper prototypes
hid_t hs_H5Aopen(hid_t obj_id, const char *attr_name, hid_t aapl_id, hid_t *error);
herr_t hs_H5Aclose(hid_t attr_id, hid_t *error);
hid_t hs_H5Acreate2(hid_t loc_id, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id, hid_t *error);
herr_t hs_H5Aread(hid_t attr_id, hid_t type_id, void *buf, hid_t *error);
herr_t hs_H5Awrite(hid_t attr_id, hid_t type_id, const void *buf, hid_t *error);
htri_t hs_H5Aexists(hid_t obj_id, const char *attr_name, hid_t *error);
hid_t hs_H5Aget_type(hid_t attr_id, hid_t *error);
hid_t hs_H5Aget_space (hid_t attr_id, hid_t *error);

hid_t hs_H5Dopen2(hid_t loc_id, const char *name, hid_t dapl_id, hid_t *error);
hid_t hs_H5Dcreate2(hid_t loc_id, const char *name, hid_t type_id, hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id, hid_t *error);
herr_t hs_H5Dclose(hid_t dset_id, hid_t *error);
hid_t hs_H5Dget_type(hid_t dset_id, hid_t *error);
hid_t hs_H5Dget_space(hid_t dset_id, hid_t *error);
herr_t hs_H5Dread(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t dxpl_id, void *buf, hid_t *error);
herr_t hs_H5Dwrite(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t dxpl_id, const void *buf, hid_t *error);

herr_t hs_H5Eclose_msg(hid_t err_id, hid_t *error);
herr_t hs_H5Eclose_stack(hid_t stack_id, hid_t *error);
hid_t hs_H5Ecreate_stack(hid_t *error);
hid_t hs_H5Eget_current_stack(hid_t *error);
herr_t hs_H5Eget_auto2(hid_t estack_id, H5E_auto2_t *func, void **client_data, hid_t *error);
herr_t hs_H5Eset_auto2(hid_t estack_id, H5E_auto2_t func, void *client_data, hid_t *error);
herr_t hs_H5Ewalk2(hid_t err_stack, H5E_direction_t direction, H5E_walk2_t func, void *client_data, hid_t *error);
ssize_t hs_H5Eget_msg(hid_t msg_id, H5E_type_t *type, char *msg, size_t size, hid_t *error);

htri_t hs_H5Fis_accessible(const char *container_name, hid_t fapl_id, hid_t *error);
hid_t hs_H5Fcreate(const char *filename, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t *error);
hid_t hs_H5Fopen(const char *filename, unsigned flags, hid_t fapl_id, hid_t *error);
hid_t hs_H5Freopen(hid_t file_id, hid_t *error);
herr_t hs_H5Fclose(hid_t file_id, hid_t *error);
herr_t hs_H5Fdelete(const char *filename, hid_t fapl_id, hid_t *error);

hid_t hs_H5Gcreate2(hid_t loc_id, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id, hid_t *error);
hid_t hs_H5Gopen2(hid_t loc_id, const char *name, hid_t gapl_id, hid_t *error);
herr_t hs_H5Gclose(hid_t group_id, hid_t *error);

herr_t hs_H5LTdtype_to_text(hid_t dtype, char *str, H5LT_lang_t lang_type, size_t *len, hid_t *error);

herr_t hs_H5Sclose(hid_t space_id, hid_t *error);
hid_t hs_H5Screate(H5S_class_t type, hid_t *error);
hid_t hs_H5Screate_simple(int rank, const hsize_t dims[], const hsize_t maxdims[], hid_t *error);
htri_t hs_H5Sis_simple(hid_t space_id, hid_t *error);
int hs_H5Sget_simple_extent_dims(hid_t space_id, hsize_t dims[], hsize_t maxdims[], hid_t *error);
int hs_H5Sget_simple_extent_ndims(hid_t space_id, hid_t *error);
hssize_t hs_H5Sget_simple_extent_npoints(hid_t space_id, hid_t *error);
H5S_class_t hs_H5Sget_simple_extent_type(hid_t space_id, hid_t *error);

herr_t hs_H5Tclose(hid_t type_id, hid_t *error);
hid_t hs_H5Tget_super(hid_t type, hid_t *error);
size_t hs_H5Tget_size(hid_t type_id, hid_t *error);
H5T_class_t hs_H5Tget_class(hid_t type_id, hid_t *error);
H5T_order_t hs_H5Tget_order(hid_t type_id, hid_t *error);
size_t hs_H5Tget_precision(hid_t type_id, hid_t *error);
H5T_sign_t hs_H5Tget_sign(hid_t type_id, hid_t *error);
hid_t hs_H5Tarray_create2(hid_t base_id, unsigned ndims, const hsize_t dim[], hid_t *error);
int hs_H5Tget_array_ndims(hid_t type_id, hid_t *error);
int hs_H5Tget_array_dims2(hid_t type_id, hsize_t dims[], hid_t *error);

herr_t hs_H5Literate2(hid_t grp_id, H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, H5L_iterate2_t op, void *op_data, hid_t* error);
