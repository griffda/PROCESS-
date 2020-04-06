"""Creates a variable descriptions markdown file.

Ford outputs a project object that contains information about the Fortran source
code. This module uses that object to create variable descriptions in 
markdown format, which can then be used by mkdocs to create the variable 
descriptions page on the gitpages site.

This script is always called from Ford; when the cmake "dicts" target is run
from the process home dir, Ford's working dir is also the process home dir.
"""
from collections import OrderedDict
import re

MARKDOWN_FILE_PATH = "documentation/proc-pages/vardes.md"
# Relative to the process home dir, as this module is being called from Ford 
# in cmake "dicts"

TITLE = "# PROCESS Variable Descriptions\n---\n"

INTRO = ("## Introduction\n"
    "Variables labelled with FIX are initialised with the given default "
    "value (shown between / / characters), but currently are not available to "
    "be changed in the input file.\n"
    "All other variables shown with a default value (including arrays boundl, "
    "boundu and sweep) can be changed in the input file.\n"
    "Variables not shown with a default value are calculated within PROCESS, "
    "so need not be initialised.\n"
    "---\n\n")

# Table column widths and headings
COL_HEADINGS = ["Name", "Type", "Initial", "Description"]

# Excluded modules
EXCLUDED_MODS = ("autodoc_data")

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

        # Filter to exclude unwanted modules
        modules = [module for module in modules if module.name not in EXCLUDED_MODS]

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

        # Remove html tags and nbsps
        value = re.sub(r"</*\w+>|&nbsp;", "", value)

        # Replace "$" signs with escaped ones "\$", as otherwise these act as
        # unintentional inline Mathjax delimiters, which cause havoc. These are 
        # used elsewhere in the docs, but exclude them here
        value = re.sub(r"\$", "\$", value)

        # Replace "\n"s with "<br>"s; these interfere with the table formatting 
        # otherwise
        value = re.sub(r"\n", "<br>", value)

        return value
    
    def truncate_value(self, value):
        """Shorten a long value (like a big array) to avoid taking up space.
        
        :param value: A value to shorten, if required
        :type value: str
        :return: A value equal to or under the maximum length
        :rtype: str
        """
        MAX_LENGTH = 30
        if len(value) > MAX_LENGTH:
            value = value[:MAX_LENGTH] + "..."
        
        return value

    def format_var_info(self, var, info):
        """Format the required variable information.

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
        
        # Truncate the initial value if it's too long
        initial = self.format_value(info["initial"])
        fmt_info["initial"] = self.truncate_value(initial)
        
        fmt_info["description"] = self.format_value(info["description"])

        return fmt_info

    def create_table_heading(self):
        """Create an HTML table header row string.
        
        Use nbsps to force the correct width for each column.
        :return: HTML string of the header row
        :rtype: str
        """
        headings_str = "\t<tr>\n"
        for i, heading in enumerate(COL_HEADINGS):
            # Write the header
            col_number = str(i + 1)
            headings_str += ("\t\t<th class=\"col" + col_number + "\">" + 
                heading + "</th>\n")

        headings_str += "\t</tr>\n"
        return headings_str

    def create_table_row(self, var, info):
        """Create a variable description table row for a single variable.
        
        :param var: The variable name
        :type var: str
        :param info: Info associated with that variable
        :type info: dict
        :return: HTML string for the table row
        :rtype: str
        """
        row_str = "\t<tr>\n"
        
        # Format the var name and info for markdown output
        fmt_var_info = self.format_var_info(var, info)

        # Write the value of each of the fields in a separate cell
        for value in fmt_var_info.values():
            row_str += "\t\t<td>" + value + "</td>\n"

        # End of table row (one variable written)
        row_str += "\t</tr>\n"
        
        return row_str

    def write_markdown(self):
        """Write the variable descriptions to a .md file.
        
        Use markdown for text and HTML for tables.
        """
        md_file = open(MARKDOWN_FILE_PATH, 'w')

        # Title and intro
        md_file.write(TITLE)
        md_file.write(INTRO)

        # Write module headings and variable tables
        for module, variables in self.vars.items():
            # Module title
            md_file.write("## " + module + "\n")
            
            # Create HTML table and table headings
            table = "<table class=\"vardes\">\n"
            table += self.create_table_heading()
            
            # Create table content rows (variable descriptions)
            for var, info in variables.items():
                table += self.create_table_row(var, info)

            # End of module's table
            table += "</table>\n\n"
            md_file.write(table)

        md_file.close()

def create_vardes(project):
    """Create the vardes markdown file.
    
    :param project: The Process project object from Ford
    :type project: obj
    """
    var_des = VarDes(project)
    var_des.process_project()
    var_des.write_markdown()
