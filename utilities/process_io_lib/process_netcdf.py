"""
PROCESS I/O library NetCDF API.

Used for reading/writing NetCDF PROCESS data.
"""

import os

import numpy as np
from netCDF4 import Dataset

from .mfile import MFile

class NetCDFWriter(object):
    
    """Takes PROCESS data and writes it to a NetCDF file."""
    
    def __init__(self, netcdf_filename, append=True, overwrite=False):
        self.netcdf_filename = os.path.abspath(netcdf_filename)
        try:
            mode = "a" if (os.path.exists(netcdf_filename) and append) else "w"
            self.root = Dataset(netcdf_filename, mode, clobber=overwrite)
        except RuntimeError:
            raise FileExistsError("Cannot create {} - file may already "
                              "exist".format(self.netcdf_filename))
        
    def write_mfile_data(self, mfile, run_id):
        """Write the provided mfile instance out as a run within the NetCDF."""
        mfile_data = mfile.data
        mfile_vars = self.root.createGroup("output_{}".format(run_id))
        
        metadata = mfile_vars.createGroup("run_metadata")
        
        for k, v in mfile_data.items():
            if k.endswith("."):
                continue
            print("Key {}".format(k))
            if k in ("procver", "date", "time", "username", "isweep", "nsweep"):
                print("Setting metadata: {}".format(k))
                setattr(metadata, v["var_description"], v["scan1"])
                continue
            
            key = k.replace("/", "_slash_").replace("*", "_asterisk_").replace(".", "_dot_")
            var_group = mfile_vars.createGroup(key)
            print(k, v)
            var_group.description = v["var_description"]
            var_group.name = v["var_name"]
            var_group.unit = v["var_unit"] if v["var_unit"] is not None else "None"
            for scan, scanval in v.items():
                if "scan" not in scan:
                    continue
                else:
                    try:
                        var_type = "f8"
                        stored_val = np.array(scanval, dtype=var_type)
                    except ValueError:
                        var_type = "str"
                        stored_val = np.array(scanval, dtype=var_type)
                    
                    print(scan, var_type, scanval)
                    stored_var = var_group.createVariable(scan, var_type)
                    stored_var[:] = stored_val
        
    def close(self):
        """Correctly flush data to NetCDF file and close for writing."""
        self.root.close()
        
        
class NetCDFReader(object):

    """Capable of reading NetCDF PROCESS data files and returning [...]."""
    
    def __init__(self, netcdf_filename):
        self.netcdf_filename = os.path.abspath(netcdf_filename)
        try:
            self.root = Dataset(self.netcdf_filename, "r")
        except RuntimeError:
            raise FileExistsError("Cannot read {}".format(self.netcdf_filename))
            
    def get_mfile(self, run_id):
        """Return an MFile instance from the NetCDF file."""
        try:
            root = Dataset(self.netcdf_filename, "r")
        except RuntimeError:
            raise FileExistsError("Cannot create {} - file may already "
                                  "exist".format(self.netcdf_filename))
        
        mf = MFile("virtual_mfile_{}".format(run_id))
        try:
            mfile_data = root.groups("output_{}".format(run_id))
        except:
            print("Cannot access output_{} in {}".format(run_id, self.netcdf_filename))
        
        mf.add_to_mfile_variable()
                                  
    def get_all_mfiles(self):
        """Return a dict of run_id:mfile key/value pairs."""
        #TODO
        pass

    
#def netcdf_to_mfile(netcdf_filename):
#    try:
#        root = Dataset(netcdf_filename, "r")
#    except RuntimeError:
#        raise FileExistsError("Cannot create {} - file may already "
#                              "exist".format(netcdf_filename))
#    
#    print(root.groups["output_1"].variables.__dict__)
    
if __name__ == "__main__":
    mf = MFile("/home/edwardsj/example_MFILE.DAT")
    
    ncdf_writer = NetCDFWriter("/home/edwardsj/test.nc", append=False,
                               overwrite=True)
    ncdf_writer.write_mfile_data(mf, 1)
    ncdf_writer.close()
