import os
import pathlib
import yaml
import jinja2
from pathlib import Path
from datetime import date


currentdir = pathlib.Path(__file__).resolve().parent


yaml_files = currentdir.glob("*.yaml")

# List of all .yaml files in the current directory.


changelog_dict = {
    "Added": [],
    "Changed": [],
    "Deprecated": [],
    "Fixed": [],
    "Removed": [],
}

# Defining the changloge dictionary that is the same structure as the yaml files that users have generated using the
# create_changelog.py script. Each header is able to have mutliple changes.


def read_yaml_file(filename):
    with open(filename, "r") as stream:
        try:
            yaml_file = yaml.safe_load(stream)

            for header in changelog_dict:
                changelog_dict[header] += yaml_file[header]

        except yaml.YAMLError as exc:
            print(exc)


for file in yaml_files:
    read_yaml_file(file)

# Defining a function for looping through and reading the yaml files in the currentdir and loading them into the changelog_dict.

for header in changelog_dict:
    changelog_dict[header] = [i for i in changelog_dict[header] if i is not None]


changelog_dict = dict(
    (header, change) for header, change in changelog_dict.items() if change
)

# Only keeping the headers in the changelog dictionary if they have a changelog entry associated with them.
# So if the 'Added' header has no entries between the .yaml files, it is not included in the changelog dict for use in the changelog.

environment = jinja2.Environment(
    loader=jinja2.FileSystemLoader(str(Path(__file__).resolve().parent))
)

template = environment.get_template("changelog_template.jinja2")

context = {
    "date_of_upgrade": date.today(),
    "process_version": input("Which version of Process is being released?"),
    "changelog_dict": changelog_dict,
}

# The changelog_template.jinja2 file holds the structure for producing the markdown file that is the output of this script.
# It creates a .md file with the headers and values from users' yaml scripts and is then available for copy and pasting into the
# changelog upon process upversioning. It takes todays date so you know when the script/changelog was updated and asks the user
# for the version of process that is being utilised in the upgrade.

with open(currentdir / "ChangelogUpdate.md", "w") as f:
    f.write(template.render(**context))

# Using the above logic, a file named 'ChangelogUpdate.md' is output in the currentdir- a markdown file.


files_in_directory = os.listdir(currentdir)
filtered_files = [file for file in files_in_directory if file.endswith(".yaml")]
for file in filtered_files:
    path_to_file = os.path.join(currentdir, file)
    os.remove(path_to_file)

# Checks the files in the currentdir after creating the changelog.md file for this upversion. It then finds which
# are .yaml files and removes them so that once they are uploaded, they are not stored and get uploaded twice in the next merge.
# This is safe as if the .yaml files did need to be accessed they would be in a previous commit.
