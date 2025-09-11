import glob
import os
import math

import pandas as pd

import h5py

import numpy as np
import scipy
from scipy.interpolate import interp1d

#------------------------------------------------------------------------------#
def fill_missing(Y, kind="linear"):
    initial_shape = Y.shape
    Y = Y.reshape((initial_shape[0], -1))
    for i in range(Y.shape[-1]):
        y = Y[:, i]
        x = np.flatnonzero(~np.isnan(y))
        if len(x) > 3:
            f = interp1d(x, y[x], kind=kind, fill_value=np.nan, bounds_error=False)
            xq = np.flatnonzero(np.isnan(y))
            y[xq] = f(xq)
            mask = np.isnan(y)
            if np.any(mask):
                valid_x = np.flatnonzero(~mask)
                valid_y = y[~mask]
                missing_x = np.flatnonzero(mask)
                # only fill NaNs strictly between observed values
                inside = (missing_x > valid_x.min()) & (missing_x < valid_x.max())
                y[missing_x[inside]] = np.interp(missing_x[inside], valid_x, valid_y)

            Y[:, i] = y
    Y = Y.reshape(initial_shape)
    return Y
#------------------------------------------------------------------------------#

def data_filter(in_dir):
    df = pd.DataFrame()
    df_body = pd.DataFrame()
    files = glob.glob(in_dir + "/*.h5")
    for f_name in files:
        with h5py.File(f_name, "r") as f:
            dset_names = list(f.keys())
            locations = f["tracks"][:].T
            node_names = [n.decode() for n in f["node_names"][:]]
        
        locations = fill_missing(locations)
        
        for i_ind in range(locations.shape[3]):
            for i_coord in range(locations.shape[2]):
                for i_nodes in range(locations.shape[1]):
                    locations[:, i_nodes, i_coord, i_ind] = scipy.signal.medfilt(locations[:, i_nodes, i_coord, i_ind], 5)
        
        video = os.path.splitext(os.path.basename(f_name))[0]
        
        locations_abdomen = locations[:, node_names.index('tail'), :, :]
        locations_headtip = locations[:, node_names.index('head'), :, :]

        for i_ind in range(locations.shape[3]):
            df_temp = {
                "video": video,
                "ind": i_ind,
                "x": (locations_abdomen[:, 0, i_ind] + locations_headtip[:, 0, i_ind])/2,
                "y": (locations_abdomen[:, 1, i_ind] + locations_headtip[:, 1, i_ind])/2,
                "vec_x": (locations_headtip[:, 0, i_ind] - locations_abdomen[:, 0, i_ind]), 
                "vec_y": (locations_headtip[:, 1, i_ind] - locations_abdomen[:, 1, i_ind]), 
                "body_length": np.sqrt((locations_abdomen[:, 1, i_ind] - locations_headtip[:, 1, i_ind])**2 + (locations_abdomen[:, 0, i_ind] - locations_headtip[:, 0, i_ind])**2)
            }
            df_temp = pd.DataFrame(df_temp)
            df = pd.concat([df, df_temp])
  
    return df
#------------------------------------------------------------------------------#

def main_data_filter():
    data_place = "data_raw/sleap"
    df = data_filter(in_dir = data_place)
    filename = "df_nymph_speed.feather"
    df.reset_index().to_feather("data_fmt/" + filename)
#------------------------------------------------------------------------------#

if __name__ == "__main__":
    main_data_filter()
