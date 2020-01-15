"""Creates a variable descriptions markdown file.

Ford outputs a project object that contains information about the Fortran source
code. This module uses that object to create variable descriptions in 
markdown format, which can then be used by mkdocs to create the variable 
descriptions page on the gitpages site.
"""
from collections import OrderedDict

# Assume the script is being run from the Process root dir
# This is ok as this module is only run from the cmake "dicts" target, which 
# must be run from the Process root dir
MARKDOWN_FILE_PATH = "documentation/proc-pages/vardes.md"
# Define table column width CSS
TABLE_STYLE = ("<style>\n"
    "\ttable th:first-of-type {width: 40%}\n"
    "\ttable th:nth-of-type(2) {width: 10%}\n"
    "\ttable th:nth-of-type(3) {width: 10%}\n"
    "\ttable th:nth-of-type(4) {width: 40%}\n"
    "</style>\n\n")

INTRO = ("Variables labelled with FIX are initialised with the given default "
    "value (shown between / / characters), but currently are not available to "
    "be changed in the input file.\n"
    "All other variables shown with a default value (including arrays boundl, "
    "boundu and sweep) can be changed in the input file.\n"
    "Variables not shown with a default value are calculated within PROCESS, "
    "so need not be initialised.\n")

class VarDes(object):
    """A collection of the variable descriptions for PROCESS source code."""
    def __init__(self, project):
        """Stores the project data.
        
        :param project: Ford's project object for the Fortran source code
        :type project: obj
        """
        self.project = project
        self.vars = OrderedDict()
        # Ordered dict to hold the processed variable information

    def process_project(self):
        """Process Ford's project object.
        
        Extract variable information from the project object and save it in a 
        structured ordered dict. This dict contains each module name, then a 
        name, type, initial value and description for each variable in each 
        module. This determines the order modules and vars are displayed.
        """
        # Sort modules by module name, alphabetically
        modules = sorted(self.project.modules, key=lambda module: module.name)
        for module in modules:
            self.vars[module.name] = OrderedDict()
            # Sort module variables by name, alphabetically
            variables = sorted(module.variables, key=lambda var: var.name)
            for var in variables:
                var_info = OrderedDict()
                var_info["var_type"] = var.vartype
                var_info["initial"] = var.initial
                var_info["description"] = var.doc
                self.vars[module.name][var.name] = var_info

    def format_value(self, value):
        """Format a value to ensure a string with no undesirable characters.
        
        :param value: Some unformatted information about the variable
        :type value: str
        :return: Formatted information about the variable
        :rtype: str
        """
        # Guard against non-strings
        if type(value) is not str:
            value = str(value)

        # Take out non-breaking spaces: these can make the columns too wide
        value = value.replace("&nbsp;", " ")
        # Remove <p> tags, these aren't necessary
        value = value.replace("<p>", "")
        value = value.replace("</p>", "")

        return value
    
    def format_var_info(self, var, info):
        """Format the variable information to remove undesirable characters.

        :param var: The variable name
        :type var: str
        :param info: The variable information
        :type info: dict
        :return: Formatted var info
        :rtype: dict
        """
        fmt_info = OrderedDict()

        fmt_info["var"] = self.format_value(var)
        fmt_info["var_type"] = self.format_value(info["var_type"])
        fmt_info["initial"] = self.format_value(info["initial"])
        fmt_info["description"] = self.format_value(info["description"])

        return fmt_info

    def write_markdown(self):
        """Write the variable descriptions out in a markdown table."""
        md_file = open(MARKDOWN_FILE_PATH, 'w')

        # Write the table style CSS
        md_file.write(TABLE_STYLE)

        # Title
        md_file.write("# PROCESS Variable Descriptions\n")
        md_file.write("---\n")
        md_file.write("## Introduction\n")
        md_file.write(INTRO)
        md_file.write("---\n\n")

        for module, variables in self.vars.items():
            # Module title and table column headings
            md_file.write("## " + module + "\n")
            md_file.write("|Name|Type|Initial|Description|\n")
            md_file.write("|---|---|---|---|\n")
            
            for var, info in variables.items():
                # Format the var name and info for markdown output
                fmt_var_info = self.format_var_info(var, info)

                # Write the value of each of the fields in a separate cell
                for value in fmt_var_info.values():
                    md_file.write("|" + value)

                # End of table row (one variable written)
                md_file.write("|\n")

            # End of module
            md_file.write("\n")

        md_file.close()

def create_vardes(project):
    """Create the vardes markdown file.
    
    :param project: The Process project object from Ford
    :type project: obj
    """
    var_des = VarDes(project)
    var_des.process_project()
    var_des.write_markdown()
