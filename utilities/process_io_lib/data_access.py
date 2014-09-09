"""
data_access.py: Generic data access/processing layer for PROCESS utilities.

This module implements generic methods to allow PROCESS tools to perform
traditionally I/O intensive tasks (e.g. many PROCESS executions in a short
time) without impacting physical file system performance.

The requirement for this is that PROCESS currently operates on a singular
file-in/file-out cycle, and does not provide multi-stage/many iteration
processing.
"""

from fs.memoryfs import MemoryFS as MemFS
from fs.osfs import OSFS
import fs.utils as fsutils

class Access(object):
    
    """
    
    """
    
    def __init__(self, binary_path, data_path=None):
        """Setup virtual filesystem - copy the binary to memory and the
        associated data directory path it requires, if any."""
        self._binary_path= binary_path
        self._data_path = data_path
        self.physical_fs = OSFS("~/")
        self.scratch_fs = MemFS()
    
    def _setup_virtual_working_dir(self):
        """Initialise self.scratch_fs virtual filesystem for the binary."""
        fsutils.copyfile(self.physical_fs, binary_path, self.scratch_fs,
                         self._binary_path)
        if self._data_path:
            fsutils.copydir((self.physical_fs, self._data_path),
                            (self.scratch_fs, self._data_path))
    
    def execute(self):
        """TODO: how?"""
        pass
    