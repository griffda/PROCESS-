# TODO Convert to pytest format
# def amend_utility_log(u):
#     """ Update utility log with header for utility
#     """
#     f = open("utilities.log", "a")
#     f.write("\n## {0:<38} ##\n".format(u))
#     f.write("#"*44)
#     f.write("\n")
#     f.close()


# def test_utilities(file_dict, utils_only):
#     """Testing python utilities for PROCESS

#     :param file_dict: dictionary of the test files for PROCESS.
#     """
    
#     # utility testing message to terminal and summary.log
#     print(BColours.BOLD + "\nUtilities and Python libraries testing\n" +
#           BColours.ENDC)
#     save_summary("\n\nUtilities and Python libraries testing\n\n")

#     # utility testing message to terminal and summary.log
#     print("Check utilities.log for warnings and errors.\n")
#     save_summary("\n\nCheck utilities.log for warnings and errors.\n\n")

#     # test mfile.py library
#     test_mfile_lib(file_dict, utils_only)

#     # test in_dat.py library
#     test_in_dat_lib(file_dict, utils_only)

#     # test make_plot_dat
#     # TODO test_make_plot_dat(file_dict)

#     # test plot_proc.py
#     test_plot_proc(file_dict, utils_only)

#     # test convert_in_dat.py
#     # TODO test_convert_in_dat(file_dict)

#     # move "utilities.log"
#     subprocess.call(["mv", "utilities.log", "test_area"])

#     return


# def test_mfile_lib(fs, utils_only):
#     """Test the PROCESS mfile library

#     :param fs: files to test
#     """

#     import process_io_lib.mfile as mf

#     # results list
#     results = list()

#     # add t utilities.log
#     amend_utility_log("mfile.py")

#     sys.stdout = open("utilities.log", "a")

#     # test all MFILEs
#     for key in fs.keys():
#         if "error_" not in key:
#             if utils_only:
#                 file_name = "test_files/{0}/ref.MFILE.DAT".format(key)
#             else:
#                 file_name = "test_area/{0}/new.MFILE.DAT".format(key)
#             results.append(mf.test(file_name))

#     sys.stdout = sys.__stdout__

#     # Output message
#     msg = "Test ==>  {0:<40}".format("mfile.py")

#     # check results
#     if len(results) == results.count(True):
#         lmsg = msg + "OK\n"
#         msg += BColours.OKGREEN + "OK" + BColours.ENDC

#     else:
#         lmsg = msg + "ERROR\n"
#         msg += BColours.FAIL + "ERROR" + BColours.ENDC

#     print(msg)
#     save_summary(lmsg)


# def test_in_dat_lib(fs, utils_only):
#     """Test the PROCESS in_dat library

#     :param fs: files to test
#     """

#     import process_io_lib.in_dat as indat

#     # results list
#     results = list()
#     files_in_order = list()

#     # add to utilities.log
#     amend_utility_log("in_dat.py")

#     sys.stdout = open("utilities.log", "a")

#     # test all MFILEs
#     if is_ci:
#         for key in fs.keys():
#             if "error_" not in key:
#                 file_name = "test_area/{0}/IN.DAT".format(key)
#                 # file_name = fs[key]["path"] + "IN.DAT"
#                 results.append(indat.test(file_name))
#     else:
#         for key in fs.keys():
#             if "error_" not in key:
#                 if "IFE" not in key:
#                     if utils_only:
#                         file_name = "test_files/{0}/ref.IN.DAT".format(key)
#                     else:
#                         file_name = "test_area/{0}/IN.DAT".format(key)
#                     files_in_order.append(file_name)
#                     # file_name = fs[key]["path"] + "IN.DAT"
#                     results.append(indat.test(file_name))

#     sys.stdout = sys.__stdout__

#     # Output message
#     msg = "Test ==>  {0:<40}".format("in_dat.py")

#     # check results
#     error_status = False
#     if len(results) == results.count(True):
#         lmsg = msg + "OK\n"
#         msg += BColours.OKGREEN + "OK" + BColours.ENDC
#     else:
#         lmsg = msg + "ERROR\n"
#         msg += BColours.FAIL + "ERROR" + BColours.ENDC
#         error_status = True

#     print(msg)
#     save_summary(lmsg)

#     if is_ci is False:
#         if error_status:
#             print(results)
#             print(files_in_order)
#             sys.exit("in_dat test_suite failure")

# def test_plot_proc(fs, utils_only):
#     """Test the PROCESS in_dat library

#     :param fs: files to test
#     """

#     import plot_proc as pp

#     # results list
#     results = list()

#     # add to utilities.log
#     amend_utility_log("plot_proc.py")

#     # sys.stdout = open("utilities.log", "a")

#     # test all MFILEs
#     for key in fs.keys():
#         if "error_" not in key:
#             if "stellarator" not in key:
#                 if is_ci:
#                     file_name = "test_area/{0}/new.MFILE.DAT".format(key)
#                     results.append(pp.test(file_name))
#                 elif "IFE" not in key:
#                     if utils_only:
#                         file_name = "test_files/{0}/ref.MFILE.DAT".format(key)
#                     else:
#                         file_name = "test_area/{0}/new.MFILE.DAT".format(key)
#                     # file_name = fs[key]["path"] + "new.MFILE.DAT"
#                     results.append(pp.test(file_name))

#     # Output message
#     msg = "Test ==>  {0:<40}".format("plot_proc.py")

#     # check results
#     error_status = False
#     if len(results) == results.count(True):
#         lmsg = msg + "OK\n"
#         msg += BColours.OKGREEN + "OK" + BColours.ENDC
#     else:
#         lmsg = msg + "ERROR\n"
#         msg += BColours.FAIL + "ERROR" + BColours.ENDC
#         error_status = True

#     print(msg)
#     save_summary(lmsg)

#     if not is_ci and error_status:
#         sys.exit("plot_proc test_suite failure")