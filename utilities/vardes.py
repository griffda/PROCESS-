"""Creates a variable descriptions markdown file.

Ford outputs a project object that contains information about the Fortran source
code. This module uses that object to create variable descriptions in 
markdown format, which can then be used by mkdocs to create the variable 
descriptions page on the gitpages site.
"""
from collections import OrderedDict
import re

# Assume the script is being run from the Process root dir
# This is ok as this module is only run from the cmake "dicts" target, which 
# must be run from the Process root dir
MARKDOWN_FILE_PATH = "documentation/proc-pages/vardes.md"

INTRO = ("Variables labelled with FIX are initialised with the given default "
    "value (shown between / / characters), but currently are not available to "
    "be changed in the input file.\n"
    "All other variables shown with a default value (including arrays boundl, "
    "boundu and sweep) can be changed in the input file.\n"
    "Variables not shown with a default value are calculated within PROCESS, "
    "so need not be initialised.\n")

# Table column widths and headings
COL_HEADINGS = ["Name", "Type", "Initial", "Description"]
COL_HEADER_WIDTHS = [40, 15, 24, 80]
# Number of chars in each header cell
COL_WIDTHS = [16, 10, 9, 38]
# Number of chars in each content cell
# These have been arbitrarily tweaked to make all tables the same dimensions.
# They are different because the nbsps used in the header cells are narrower
# than most characters.

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

        # Remove html tags and nbsps
        value = re.sub(r"</*\w+>|&nbsp;", "", value)

        # Replace "\n"s with "<br>"s; these interfere with the table formatting 
        # otherwise
        value = re.sub(r"\n", "<br>", value)

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
        fmt_info["initial"] = self.format_value(info["initial"])
        fmt_info["description"] = self.format_value(info["description"])

        return fmt_info

    def break_var_info(self, var_info):
        """Insert line breaks into variable info strings.
        
        This is necessary because markdown tables are very difficult to style,
        as assigning CSS classes or ids appears to be impossible. Therefore 
        table dimensions are governed by the table header's nbsps, and these 
        line breaks are designed not to exceed those dimensions.
        :param var_info: Information about a given variable
        :type var_info: dict
        :return: Line-broken var_info
        :rtype: dict
        """
        for (key, value), width in zip(var_info.items(), COL_WIDTHS):
            # Start of a new table cell
            broken_info = ""
            # The line-breaked string
            count = 0
            # The current number of characters in the current line
            words = value.split(" ")
            # Break the value string into a list of words

            # Does the word fit on this line?
            for word in words:
                # Set up space vars depending on start of new line or not
                if count == 0:
                    spaces = 0
                    space_str = ""
                else:
                    spaces = 1
                    space_str = " "

                if spaces + len(word) < width - count:
                    # Word will fit on current line
                    broken_info += space_str + word
                    count += spaces + len(word)
                elif len(word) > width:
                    # Word won't fit on this line, and word is longer than the 
                    # width of the next one; break it up mid-word
                    word_slices = []

                    # First slice on remainder of first line
                    first_line_space = width - count
                    word_slices.append(word[0:first_line_space])

                    # Append subsequent full-width slices
                    word_slices.extend([word[i:i+width] for i in range(
                        first_line_space, len(word), width)])
                    
                    # Add line breaks in between the word slices
                    for i, word_slice in enumerate(word_slices):
                        if i < len(word_slices):
                            # Add break if not last line
                            broken_info += word_slice + "<br>"
                        else:
                            # Last line; don't break and update count
                            broken_info += word_slice
                            count = len(word_slice)
                else:
                    # Word won't fit on this line, but will on new one
                    broken_info += "<br>" + word
                    count = len(word)

            # Overwrite the var_info string with the line-broken version
            var_info[key] = broken_info

        return var_info

    def create_table_heading(self):
        """Create a markdown table header row string.
        
        Use nbsps to force the correct width for each column.
        :return: Markdown string of the header row
        :rtype: str
        """
        headings_str = "|"
        for width, heading in zip(COL_HEADER_WIDTHS, COL_HEADINGS):
            # Write the header, then the right number of spaces
            headings_str += heading
            spaces = width - len(heading)
            headings_str += "&nbsp;" * spaces
            headings_str += "|"

        headings_str += "\n"
        return headings_str

    def create_table_row(self, var, info):
        """Create a variable description table row for a single variable.
        
        :param var: The variable name
        :type var: str
        :param info: Info associated with that variable
        :type info: dict
        :return: Markdown string for the table row
        :rtype: str
        """
        row_str = "|"
        
        # Format the var name and info for markdown output
        fmt_var_info = self.format_var_info(var, info)

        # Insert line breaks in the formatted values
        fmt_var_info = self.break_var_info(fmt_var_info)

        # Write the value of each of the fields in a separate cell
        for value in fmt_var_info.values():
            row_str += value + "|"

        # End of table row (one variable written)
        row_str += "\n"
        
        return row_str

    def write_markdown(self):
        """Write the variable descriptions out in a markdown table."""
        md_file = open(MARKDOWN_FILE_PATH, 'w')

        # Title
        title = "# PROCESS Variable Descriptions\n"
        title += "---\n"
        md_file.write(title)

        # Intro
        intro = "## Introduction\n"
        intro += INTRO
        intro += "---\n\n"
        md_file.write(intro)

        # Write module headings and variable tables
        for module, variables in self.vars.items():
            # Module title
            module_title = ("## " + module + "\n")
            md_file.write(module_title)
            
            # Table heading
            table = self.create_table_heading()
            table += "|---|---|---|---|\n"
            
            # Create table content rows (variable descriptions)
            for var, info in variables.items():
                table += self.create_table_row(var, info)

            # End of module's table
            table += "\n"
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
