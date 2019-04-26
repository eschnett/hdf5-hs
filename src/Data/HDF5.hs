module Data.HDF5
  ( -- * Files
    File(..)
  , H5F_ACC(..)
  , h5f_close
  , h5f_create
  , h5f_open
    -- * Groups
  , Group(..)
  , h5g_close
  , h5g_create
  , h5g_open
    -- * Locations
  , Location(..)
  , h5l_create_hard
  , h5l_create_soft
  , h5l_create_external
  , h5l_exists
  , h5l_move
  , h5l_delete
    -- * Datasets
  , Dataset(..)
  , h5d_create
  , h5d_open
  , h5d_close
  , h5d_get_space
  , h5d_get_type
  , h5d_write
  , h5d_read
    -- * Dataspaces
  , Dataspace(..)
  , H5S(..)
  , h5s_create
  , h5s_create_simple
  , h5s_close
  , h5s_get_simple_extent_ndims
  , h5s_get_simple_extent_dims
    -- * Datatypes
  , Datatype(..)
  , h5t_native_CChar
  , h5t_native_CSChar
  , h5t_native_CUChar
  , h5t_native_CInt
  , h5t_native_CUInt
  , h5t_native_CLong
  , h5t_native_CULong
  , h5t_native_CLLong
  , h5t_native_CULLong
  , h5t_native_CFloat
  , h5t_native_CDouble
  ) where

import Control.Exception (assert)
import Data.Maybe (maybe)

import Foreign.C
import Foreign.Marshal
import Foreign.Ptr

import Bindings.HDF5



-- H5D: Datasets ---------------------------------------------------------------

data Dataset = Dataset C'hid_t
  deriving (Eq, Ord, Read, Show)

h5d_create :: Location loc
           => loc -> String -> Datatype -> Dataspace -> IO Dataset
h5d_create loc name (Datatype dtype) (Dataspace space) =
  do dataset <- withCString name \cname ->
       c'H5Dcreate2 (fromLoc loc) cname dtype space
         c'H5P_LINK_CREATE_DEFAULT c'H5P_DATASET_CREATE_DEFAULT
         c'H5P_DATASET_ACCESS_DEFAULT
     return (Dataset dataset)

h5d_open :: Location loc => loc -> String -> IO Dataset
h5d_open loc name =
  do dataset <- withCString name \cname ->
       c'H5Dopen2 (fromLoc loc) cname c'H5P_DATASET_ACCESS_DEFAULT
     return (Dataset dataset)

h5d_close :: Dataset -> IO ()
h5d_close (Dataset dataset) =
  do herr <- c'H5Dclose dataset
     return ()

h5d_get_space :: Dataset -> IO Dataspace
h5d_get_space (Dataset dataset) =
  do space <- c'H5Dget_space dataset
     return (Dataspace space)

h5d_get_type :: Dataset -> IO Datatype
h5d_get_type (Dataset dataset) =
  do dtype <- c'H5Dget_type dataset
     return (Datatype dtype)

h5d_write :: Dataset -> Datatype -> Dataspace -> Dataspace -> Ptr () -> IO ()
h5d_write (Dataset dataset) (Datatype memtype) (Dataspace memspace)
  (Dataspace filespace) buf =
  do herr <- c'H5Dwrite dataset memtype memspace filespace
       c'H5P_DATASET_XFER_DEFAULT buf
     return ()

h5d_read :: Dataset -> Datatype -> Dataspace -> Dataspace -> Ptr () -> IO ()
h5d_read (Dataset dataset) (Datatype memtype) (Dataspace memspace)
  (Dataspace filespace) buf =
  do herr <- c'H5Dread dataset memtype memspace filespace
       c'H5P_DATASET_XFER_DEFAULT buf
     return ()



-- H5F: Files ------------------------------------------------------------------

data File = File C'hid_t
  deriving (Eq, Ord, Read, Show)

instance Location File where
  fromLoc (File hid) = hid
  toLoc hid = File hid

data H5F_ACC = H5F_ACC_RDONLY
             | H5F_ACC_RDWR
             | H5F_ACC_TRUNC
             | H5F_ACC_EXCL
             | H5F_ACC_DEBUG
             | H5F_ACC_CREAT
             | H5F_ACC_DEFAULT

c_h5f_acc :: H5F_ACC -> CUInt
c_h5f_acc H5F_ACC_RDONLY  = c'H5F_ACC_RDONLY
c_h5f_acc H5F_ACC_RDWR    = c'H5F_ACC_RDWR
c_h5f_acc H5F_ACC_TRUNC   = c'H5F_ACC_TRUNC
c_h5f_acc H5F_ACC_EXCL    = c'H5F_ACC_EXCL
c_h5f_acc H5F_ACC_DEBUG   = c'H5F_ACC_DEBUG
c_h5f_acc H5F_ACC_CREAT   = c'H5F_ACC_CREAT
c_h5f_acc H5F_ACC_DEFAULT = c'H5F_ACC_DEFAULT

h5f_create :: String -> H5F_ACC -> IO File
h5f_create name acc =
  do file <- withCString name \cname ->
       c'H5Fcreate cname (c_h5f_acc acc) c'H5P_FILE_CREATE_DEFAULT
         c'H5P_FILE_ACCESS_DEFAULT
     return (File file)

h5f_open :: String -> H5F_ACC -> IO File
h5f_open name acc =
  do file <- withCString name \cname ->
       c'H5Fopen cname (c_h5f_acc acc) c'H5P_FILE_ACCESS_DEFAULT
     return (File file)

h5f_close :: File -> IO ()
h5f_close (File hid) =
  do herr <- c'H5Fclose hid
     return ()



-- H5G: Groups -----------------------------------------------------------------

data Group = Group C'hid_t
  deriving (Eq, Ord, Read, Show)

instance Location Group where
  fromLoc (Group hid) = hid
  toLoc hid = Group hid

h5g_create :: Location loc => loc -> String -> IO Group
h5g_create loc name =
  do group <- withCString name \cname ->
       c'H5Gcreate2 (fromLoc loc) cname c'H5P_LINK_CREATE_DEFAULT
         c'H5P_GROUP_CREATE_DEFAULT c'H5P_GROUP_ACCESS_DEFAULT
     return (Group group)

h5g_open :: Location loc => loc -> String -> IO Group
h5g_open loc name =
  do group <- withCString name \cname ->
       c'H5Gopen2 (fromLoc loc) cname c'H5P_DEFAULT
     return (Group group)

h5g_close :: Group -> IO ()
h5g_close (Group group) =
  do herr <- c'H5Gclose group
     return ()



-- H5L: Locations --------------------------------------------------------------

class Location loc where
  fromLoc :: loc -> C'hid_t
  toLoc :: C'hid_t -> loc

h5l_create_hard :: (Location obj_loc, Location link_loc)
                => obj_loc -> String -> link_loc -> String -> IO ()
h5l_create_hard obj_loc obj_name link_loc link_name =
  do herr <- withCString obj_name \c_obj_name ->
       withCString link_name \c_link_name ->
       c'H5Lcreate_hard (fromLoc obj_loc) c_obj_name (fromLoc link_loc)
       c_link_name c'H5P_LINK_CREATE_DEFAULT c'H5P_LINK_ACCESS_DEFAULT
     return ()

h5l_create_soft :: Location link_loc => String -> link_loc -> String -> IO ()
h5l_create_soft target_path link_loc link_name =
  do herr <- withCString target_path \c_target_path ->
       withCString link_name \c_link_name ->
       c'H5Lcreate_soft c_target_path (fromLoc link_loc) c_link_name
       c'H5P_LINK_CREATE_DEFAULT c'H5P_LINK_ACCESS_DEFAULT
     return ()

h5l_create_external :: Location link_loc
                    => String -> String -> link_loc -> String -> IO ()
h5l_create_external target_file_name target_obj_name link_loc link_name =
  do herr <- withCString target_file_name \c_target_file_name ->
       withCString target_obj_name \c_target_obj_name ->
       withCString link_name \c_link_name ->
       c'H5Lcreate_external c_target_file_name c_target_obj_name
       (fromLoc link_loc) c_link_name c'H5P_LINK_CREATE_DEFAULT
       c'H5P_LINK_ACCESS_DEFAULT
     return ()

h5l_exists :: Location loc => loc -> String -> IO Bool
h5l_exists loc name =
  do htri <- withCString name \cname ->
       c'H5Lexists (fromLoc loc) cname c'H5P_LINK_ACCESS_DEFAULT
     return (htri > 0)

h5l_move :: (Location src_loc, Location dest_loc)
         => src_loc -> String -> dest_loc -> String -> IO ()
h5l_move src_loc src_name dest_loc dest_name =
  do herr <- withCString src_name \c_src_name ->
       withCString dest_name \c_dest_name ->
       c'H5Lmove (fromLoc src_loc) c_src_name (fromLoc dest_loc) c_dest_name
       c'H5P_LINK_CREATE_DEFAULT c'H5P_LINK_ACCESS_DEFAULT
     return ()

h5l_delete :: Location loc => loc -> String -> IO ()
h5l_delete loc name =
  do herr <- withCString name \cname ->
       c'H5Ldelete (fromLoc loc) cname c'H5P_LINK_ACCESS_DEFAULT
     return ()



-- H5S: Dataspaces -------------------------------------------------------------

data Dataspace = Dataspace C'hid_t
  deriving (Eq, Ord, Read, Show)

data H5S = H5S_NO_CLASS
         | H5S_SCALAR
         | H5S_SIMPLE
         | H5S_NULL

c_h5s :: H5S -> C'H5S_class_t
c_h5s H5S_NO_CLASS = c'H5S_NO_CLASS
c_h5s H5S_SCALAR   = c'H5S_SCALAR
c_h5s H5S_SIMPLE   = c'H5S_SIMPLE
c_h5s H5S_NULL     = c'H5S_NULL

h5s_create :: H5S -> IO Dataspace
h5s_create stype =
  do space <- c'H5Screate (c_h5s stype)
     return (Dataspace space)

h5s_create_simple :: [Int] -> Maybe [Int] -> IO Dataspace
h5s_create_simple current_dims maximum_dims =
  assert (maybe True (\md -> length md == length current_dims) maximum_dims) $
  do let rank = length current_dims
     let c_rank = fromIntegral rank
     space <- withArray (fromIntegral <$> current_dims) \c_current_dims ->
       let go c_md = c'H5Screate_simple c_rank c_current_dims c_md
       in case maximum_dims of
           Nothing -> go nullPtr
           Just md -> withArray (fromIntegral <$> md) \c_md -> go c_md
     return (Dataspace space)

h5s_close :: Dataspace -> IO ()
h5s_close (Dataspace space) =
  do herr <- c'H5Sclose space
     return ()

h5s_get_simple_extent_ndims :: Dataspace -> IO Int
h5s_get_simple_extent_ndims (Dataspace space) =
  do c_ndims <- c'H5Sget_simple_extent_ndims space
     return (fromIntegral c_ndims)

h5s_get_simple_extent_dims :: Dataspace -> IO ([Int], [Int])
h5s_get_simple_extent_dims (Dataspace space) =
  do c_ndims <- c'H5Sget_simple_extent_ndims space
     let ndims = fromIntegral c_ndims
     (c_dims, c_maxdims) <- allocaArray ndims \c_dims_ptr ->
       allocaArray ndims \c_maxdims_ptr ->
       do herr <- c'H5Sget_simple_extent_dims space c_dims_ptr c_maxdims_ptr
          c_dims <- peekArray ndims c_dims_ptr
          c_maxdims <- peekArray ndims c_maxdims_ptr
          return (c_dims, c_maxdims)
     return (fromIntegral <$> c_dims, fromIntegral <$> c_maxdims)



-- H5T: Datatypes --------------------------------------------------------------

data Datatype = Datatype C'hid_t
  deriving (Eq, Ord, Read, Show)

h5t_native_CChar :: Datatype
h5t_native_CChar = Datatype c'H5T_NATIVE_CHAR
h5t_native_CSChar :: Datatype
h5t_native_CSChar = Datatype c'H5T_NATIVE_SCHAR
h5t_native_CUChar :: Datatype
h5t_native_CUChar = Datatype c'H5T_NATIVE_UCHAR
h5t_native_CInt :: Datatype
h5t_native_CInt = Datatype c'H5T_NATIVE_INT
h5t_native_CUInt :: Datatype
h5t_native_CUInt = Datatype c'H5T_NATIVE_UINT
h5t_native_CLong :: Datatype
h5t_native_CLong = Datatype c'H5T_NATIVE_LONG
h5t_native_CULong :: Datatype
h5t_native_CULong = Datatype c'H5T_NATIVE_ULONG
h5t_native_CLLong :: Datatype
h5t_native_CLLong = Datatype c'H5T_NATIVE_LLONG
h5t_native_CULLong :: Datatype
h5t_native_CULLong = Datatype c'H5T_NATIVE_ULLONG
h5t_native_CFloat :: Datatype
h5t_native_CFloat = Datatype c'H5T_NATIVE_FLOAT
h5t_native_CDouble :: Datatype
h5t_native_CDouble = Datatype c'H5T_NATIVE_DOUBLE
-- h5t_native_haddr   = Datatype c'H5T_NATIVE_HADDR
-- h5t_native_hsize   = Datatype c'H5T_NATIVE_HSIZE
-- h5t_native_hssize  = Datatype c'H5T_NATIVE_HSSIZE
-- h5t_native_herr    = Datatype c'H5T_NATIVE_HERR
-- h5t_native_hbool   = Datatype c'H5T_NATIVE_HBOOL  
