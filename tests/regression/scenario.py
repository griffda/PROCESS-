"""Scenario class for an individual regression test case."""
from pathlib import Path
import logging
import sys
import os
import shutil

# TODO This isn't good: put MFile into process package?
sys.path.append(os.path.join(os.path.dirname(__file__), '../utilities/'))

from process import process
from process_io_lib.mfile import MFile

# Variables to ignore when comparing differences (set)
EXCLUSIONS = {"normres", "nitvar", "itvar", "xcm"}

logger = logging.getLogger(__name__)

class Scenario():
    """A scenario for a single regression test."""
    def __init__(self, ref_dir):
        """Initialise with reference directory to test.

        :param ref_dir: reference directory
        :type ref_dir: Path
        """
        self.ref_dir = ref_dir # Dir containing the test files
        self.test_dir = None # Where the actual test takes place
        self.name = ref_dir.name # Name of the scenario
        self.version = None # Process version
        self.ref_mfile = None # Reference MFile (expected)
        self.new_mfile = None # Newly-created MFile (observed)
        self.vars_unique_ref = set() # Vars unique to reference
        self.vars_unique_new = set() # Vars unique to new
        self.vars_common_both = set() # Vars in both reference and new
        self.over_tolerance_diff_items = [] # Differences over tolerance

    def run(self, test_dir):
        """Run Process for this scenario.

        :param test_dir: temporary directory for running the test in
        :type test_dir: Path
        :return: True if no exceptions thrown, False if not
        :rtype: bool
        """
        self.test_dir = test_dir
        logger.info(f"Running {self.name} scenario")

        # Run Process using the input file in the "test_dir" directory,
        # catching any errors
        input_path_str = str(self.test_dir / 'IN.DAT')
        try:
            process.main(args=['--input', input_path_str])
            return True
        except:
            logger.exception(f"Process threw an exception when running "
                "scenario: {self.name}")
            return False

    def check_mfile_length(self):
        """Ensure there is something in the MFile.

        :return: True if is has non-zero length, False if not
        :rtype: bool
        """
        with open(self.test_dir / 'MFILE.DAT', "r") as mfile:
            mfile_len = len(mfile.readlines())

        if mfile_len == 0:
            return False
        else:
            return True

    def read_mfiles(self):
        """Read in reference and newly-output MFILEs, creating MFile objects."""
        ref_mfile_path_str = str(self.test_dir / 'ref.MFILE.DAT')
        new_mfile_path_str = str(self.test_dir / 'MFILE.DAT')

        try:
            self.ref_mfile = MFile(ref_mfile_path_str)
            self.new_mfile = MFile(new_mfile_path_str)
        except:
            logger.exception("There was an error creating an MFile object.")
            raise

    def set_version(self):
        """Set process version number."""
        self.version = self.new_mfile.data["tagno"].get_scan(-1)

    def check_ifail(self):
        """Test the value of ifail, the solver error return flag.

        :return: True if ifail is 1, False otherwise
        :rtype: bool
        """
        self.ifail = self.new_mfile.data["ifail"].get_scan(-1)

        if self.ifail != 1:
            return False
        else:
            return True

    def add_diff_item(self, diff_item):
        """Add a diff item tuple that is outside the accepted tolerance.

        :param diff_item: a diff that exceeds the tolerance for this variable
        :type diff_item: tuple
        """
        self.over_tolerance_diff_items.append(diff_item)

    def set_mfile_var_sets(self):
        """Set unique and common variables in ref and new MFiles."""
        # Get variable keys from the reference and observed MFiles
        # and convert to sets
        ref_vars = set(self.ref_mfile.data.keys())
        new_vars = set(self.new_mfile.data.keys())

        # Filter out the excluded vars using set difference; ones we aren't 
        # interested in
        ref_vars = ref_vars - EXCLUSIONS
        new_vars = new_vars - EXCLUSIONS

        # Set difference and intersection
        self.unique_to_new = new_vars - ref_vars
        self.vars_unique_ref = ref_vars - new_vars
        self.vars_common_both = new_vars & ref_vars
        
    def get_mfile_diffs(self):
        """Generator for differences between the ref and new MFiles.

        :yield: diff_item for each difference
        :rtype: tuple
        """
        # Scan number for comparing the final scan in each MFile
        scan_number = -1

        # Determine which variables are unique/common to each/both MFiles
        self.set_mfile_var_sets()

        # For the variables in both reference and new MFiles, find the
        # difference in values
        for var_name in self.vars_common_both:
            # Get expected and observed values
            try:
                exp = float(self.ref_mfile.data[var_name].get_scan(scan_number))
                obs = float(self.new_mfile.data[var_name].get_scan(scan_number))
            except ValueError:
                # This is to catch (and ignore) values that can't be converted 
                # to floats, e.g. strings in the MFILE (fileprefix = 'IN.DAT')
                continue
            
            # Calulate percentage change
            # TODO This could be made more intelligent in the case of zero or
            # very small absolute changes
            if obs == 0 or exp == 0:
                chg = 0.0
            else:
                chg = (obs / exp - 1) * 100

            # Create "expected and observed" comparison tuple
            # TODO Should this be a class?
            diff_item = (var_name, exp, obs, chg)
            yield diff_item

    def log_summary(self):
        """Log a summary of the scenario's test result."""
        # Log differences outside tolerance
        logger.warning(f"Total of {len(self.over_tolerance_diff_items)} "
            "differences outside tolerance.")

        # Create table
        logger.warning(f"{'Variable':40}\t{'Ref':10}\t{'New':10}\t{'Diff (%)':10}")
        logger.warning("-"*40)
        for diff_item in self.over_tolerance_diff_items:
            var_name, exp, obs, chg = diff_item
            logger.warning(f"{var_name:40}\t{exp:10.3g}\t{obs:10.3g}\t"
                f"{chg:10.2f}")

        # Log variables in ref but not in new
        logger.warning(f"There are {len(self.vars_unique_ref)} variables in "
            "ref not in new:")
        for var in self.vars_unique_ref:
            logger.warning(var)

        # Log variables in new but not in ref
        logger.warning(f"There are {len(self.vars_unique_new)} variables in "
            "new not in ref:")
        for var in self.vars_unique_new:
            logger.warning(var)

    def get_diff_items(self):
        """Return list of diffs that exceed the tolerance.

        :return: list of diff_item tuples that are over tolerance
        :rtype: list
        """
        return self.over_tolerance_diff_items