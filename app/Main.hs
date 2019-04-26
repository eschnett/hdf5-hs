module Main where

import Foreign.Marshal
import Foreign.Ptr

import Data.HDF5



mkArray :: Int -> Int -> Int -> [Double]
mkArray ni nj nk = [ fromIntegral (10000 * k + 100 * j + i)
                   | i <- [0..ni-1], j <- [0..nj-1], k <- [0..nk-1] ]



writeData :: String -> IO ()
writeData name =
  do file <- h5f_create name H5F_ACC_TRUNC
     let (ni, nj, nk) = (3, 4, 5)
     space <- h5s_create_simple [ni, nj, nk] Nothing
     let dtype = h5t_native_CDouble
     dataset <- h5d_create file "mydata" dtype space
     withArray (mkArray ni nj nk) \buf ->
       h5d_write dataset h5t_native_CDouble space space (castPtr buf)
     h5d_close dataset
     h5s_close space
     h5f_close file

readData :: String -> IO ()
readData name =
  do file <- h5f_open name H5F_ACC_RDONLY
     dataset <- h5d_open file "mydata"
     space <- h5d_get_space dataset
     ([ni, nj, nk], _) <- h5s_get_simple_extent_dims space
     let np = ni * nj * nk
     arr <- allocaArray np \buf ->
       do h5d_read dataset h5t_native_CDouble space space (castPtr buf)
          peekArray np buf
     if arr == mkArray ni nj nk
       then return ()
       else do putStrLn $ show arr
               error "Incorrect dataset content"
     h5s_close space
     h5d_close dataset
     h5f_close file

main :: IO ()
main = do h5_check_version h5_vers_major h5_vers_minor h5_vers_release
          putStrLn "Creating example HDF5 file..."
          writeData "example.h5"
          putStrLn "Opening example HDF5 file..."
          readData "example.h5"
          putStrLn "Done."
