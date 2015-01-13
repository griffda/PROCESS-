"""
PROCESS I/O library NetCDF API.

Used for reading/writing NetCDF PROCESS data.
"""

import os
import re

import numpy as np
from netCDF4 import Dataset

from .mfile import MFile

NAME_MAPPINGS = {"/": "_slash_",
                 "*": "_asterisk_",
                 ".": "_dot_"}
METADATA = ("procver", "date", "time", "username", "isweep", "nsweep")

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

    def _open(self):
        try:
            mode = "a" if (os.path.exists(self.netcdf_filename) and self._append) else "w"
            self.root = Dataset(self.netcdf_filename, mode, clobber=self._overwrite)
        except RuntimeError:
            raise OSError("Cannot create {} - file may already "
                              "exist".format(self.netcdf_filename))

    def _close(self):
        """Correctly flush data to NetCDF file and close for writing."""
        try:
            self.root.close()
        except AttributeError:
            print("File not initially opened by NetCDFWriter")

    def write_mfile_data(self, mfile, run_id, save_vars=list()):
        """Write the provided mfile instance out as a run within the NetCDF."""
        mfile_data = mfile.data
        mfile_vars = self.root.createGroup("output_{}".format(run_id))

        # Make sure we include metadata
        save_vars += METADATA

        for k, v in mfile_data.items():
            if k.endswith("."):
                continue

            # Swap illegal characters in key with substitutes in NAME_MAPPINGS
            rep_key = dict((re.escape(k), v) for k, v in NAME_MAPPINGS.items())
            pattern = re.compile("|".join(rep_key.keys()))
            # Swaps all illegal characters in one go
            key = pattern.sub(lambda m: rep_key[re.escape(m.group(0))], k)

            if key in save_vars or save_vars is None:
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
        """Open NetCDF file and provide with statement usage."""
        self._open()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Close NetCDF file and provide with statement usage."""
        self._close()

    def _open(self):
        """Open the NetCDF file for reading."""
        try:
            self.root = Dataset(self.netcdf_filename, "r")
        except RuntimeError:
            raise FileNotFoundError("Cannot read"
                                    " {}".format(self.netcdf_filename))

    def _close(self):
        """Correctly close NetCDF file handle."""
        try:
            self.root.close()
        except AttributeError:
            print("File not initially opened by NetCDFReader")

    def _get_mfile(self, path):
        mf = MFile(filename=None)
        try:
            mfile_data = self.root.groups[path]
        except:
            raise KeyError("Cannot access {} in "
                           "{}".format(run_id, self.netcdf_filename))
        for group_name, group in mfile_data.groups.items():
            for var_name, variable in group.variables.items():
                scan_num = None
                if "scan" in var_name:
                    scan_num = int(re.search("\d+", var_name).group())
                mf.add_to_mfile_variable(group.description, group.name,
                                         variable.getValue(), group.unit,
                                         scan_num)
        return mf

    def get_run(self, run_id=1):
        """Return the run_id data as an MFile instance.

        run_id is the ID number of the PROCESS output to retrieve. If it cannot
        be found, a KeyError is raised.
        """
        return self._get_mfile("output_{}".format(run_id))

    def runs(self, start_run=1):
        """Generator to provide each run, starting from the ID start_run."""
        for run in self.root.groups.keys():
            run_id = int(run.split("_")[1])
            if run_id >= start_run:
                yield self._get_mfile(run)
            else:
                pass


if __name__ == "__main__":
    import sys

    mf = MFile("/home/edwardsj/example_MFILE.DAT")


    if sys.argv[1] == "write":
        # Writer example - now uses context manager (i.e. with statement)
        with NetCDFWriter("/home/edwardsj/test.nc", append=True,
                          overwrite=False) as ncdf_writer:
            ncdf_writer.write_mfile_data(mf, 4, save_vars=[])
    elif sys.argv[1] == "read":
        # Reader example - gives back MFile instances
        with NetCDFReader("/home/edwardsj/test.nc") as ncdf_reader:
            # Get an individual run
            returned_mf = ncdf_reader.get_run(4)
            print(returned_mf.data)
            # Get multiple runs in a loop, starting at run 1
            # for mfile in ncdf_reader.runs(start_run=2):
            #     print(mfile)

