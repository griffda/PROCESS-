"""
PROCESS I/O library NetCDF API.

Used for reading/writing NetCDF PROCESS data.
"""

import os
import re

import numpy as np
from netCDF4 import Dataset

from mfile import MFile

NAME_MAPPINGS = {"/": "_slash_",
                 "*": "_asterisk_",
                 ".": "_dot_"}

class NetCDFWriter(object):
    
    """Takes PROCESS data and writes it to a NetCDF file."""
    
    def __init__(self, netcdf_filename, append=True, overwrite=False):
        self.netcdf_filename = os.path.abspath(netcdf_filename)
        self._append = append
        self._overwrite = overwrite

    def __enter__(self):
        self._open()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self._close()
        return True

    def _open(self):
        try:
            mode = "a" if (os.path.exists(self.netcdf_filename) and self._append) else "w"
            self.root = Dataset(self.netcdf_filename, mode, clobber=self._overwrite)
        except RuntimeError:
            raise FileExistsError("Cannot create {} - file may already "
                              "exist".format(self.netcdf_filename))

    def _close(self):
        """Correctly flush data to NetCDF file and close for writing."""
        try:
            self.root.close()
        except AttributeError:
            print("File not initially opened by NetCDFWriter")

    def write_mfile_data(self, mfile, run_id):
        """Write the provided mfile instance out as a run within the NetCDF."""
        mfile_data = mfile.data
        mfile_vars = self.root.createGroup("output_{}".format(run_id))
        
        metadata = mfile_vars.createGroup("run_metadata")
        
        for k, v in mfile_data.items():
            if k.endswith("."):
                continue
            if k in ("procver", "date", "time", "username", "isweep", "nsweep"):
                print("Setting metadata: {}".format(k))
                setattr(metadata, v["var_description"], v["scan1"])
                continue

            # Swap illegal characters in key with substitutes in NAME_MAPPINGS
            rep_key = dict((re.escape(k), v) for k, v in NAME_MAPPINGS.items())
            pattern = re.compile("|".join(rep_key.keys()))
            # Swaps all illegal characters in one go
            key = pattern.sub(lambda m: rep_key[re.escape(m.group(0))], k)

            var_group = mfile_vars.createGroup(key)
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

                    stored_var = var_group.createVariable(scan, var_type)
                    stored_var[:] = stored_val
        
        
class NetCDFReader(object):

    """Capable of reading NetCDF PROCESS data files and returning [...]."""
    
    def __init__(self, netcdf_filename):
        self.netcdf_filename = os.path.abspath(netcdf_filename)

    def __enter__(self):
        self._open()

    def __exit__(self, exc_type, exc_val, exc_tb):
        self._close()
        return True

    def _open(self):
        """Open the NetCDF file for reading."""
        try:
            self.root = Dataset(self.netcdf_filename, "r")
        except RuntimeError:
            raise FileExistsError("Cannot read {}".format(self.netcdf_filename))

    def _close(self):
        """Correctly close NetCDF file handle."""
        try:
            self.root.close()
        except AttributeError:
            print("File not initially opened by NetCDFReader")

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
            print("Cannot access output_{} in {}".format(run_id,
                                                         self.netcdf_filename))
        print(mfile_data)

        # var_name =
        # var_value = sort_value(line[2])
        # var_unit = get_unit(var_des)
        # self.add_to_mfile_variable(var_des, var_name, var_value, var_unit)
                                  
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

    # Writer example - now uses context manager (i.e. with statement)
    with NetCDFWriter("/home/edwardsj/test.nc", append=False,
                      overwrite=True) as ncdf_writer:
        ncdf_writer.write_mfile_data(mf, 1)

    # Reader example - gives back MFile instances