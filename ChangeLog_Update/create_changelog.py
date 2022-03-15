import pathlib
import sys
from subprocess import check_output
import jinja2
from pathlib import Path


currentdir = pathlib.Path(__file__).resolve().parent

# Defining the currentdir such that the script is always execcuted within the correct 'ChangeLog_Update' directory and
# that the YAML files are output here.

out = check_output(["git", "branch"]).decode("utf8")
current = next(line for line in out.split("\n") if line.startswith("*"))
git_branch = current.strip("*").strip()
git_branch_no_split = git_branch.partition("-")
git_branch_no = git_branch_no_split[0]

# Finding the name of the current git branch which shulde have the follwowing structure
# 1234-issue-name-from-merge-request
# Then the name is split by using the '-' as the partition.
# The number is wanted to use as the name for the .yaml file so this is taken as the 'git_branch_no'


generated_yaml_file = Path(f"{currentdir}/{git_branch_no}.yaml")

# This is just defining the yaml file that will be created by the running of this script.
# Showing that the generated_yaml_file will always be in the currentdir and named as git_branch_no.yaml e.g 1234.yaml

if generated_yaml_file.is_file():
    print(
        "This YAML file already exists- please locate it in the current directory and edit there."
    )
    sys.exit()
else:
    print(
        f"Your file will be created in the ChangeLog_Update directory and called: {git_branch_no}.yaml"
    )

# The if statement checks if this script has already been run in the current branch and therefore if the yaml file you are about
# to create already exists. If this is the case, then the yaml file should be located in currentdir and edited with any changes
# there. This prevents overwriting of any changes that have already been documented in the yaml file.
# If the yaml file hasnt been created, the console will print the name your yaml file will have so you are able to locate it in
# the current directory.

environment = jinja2.Environment(
    loader=jinja2.FileSystemLoader(str(Path(__file__).resolve().parent))
)

yaml_template = environment.get_template("yaml_template.jinja2")

# Using jinja2 to template the standard .yaml file that is to be generated (empty) for user to fill out with their changes.

with open(f"{currentdir}/{git_branch_no}.yaml", "w") as f:
    f.write(yaml_template.render())

# Using the jinja2 template to write out a .yaml file in the current directory with the name being the users
# git branch number.
