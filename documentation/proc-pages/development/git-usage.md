# git Usage
## Commit Logs
To see the commit messages you can use the `git log` command. There are various options
described below.

| Command | Description | Example |
| -------- | -------- | --------- |
| `-(n)`   | show the last `n` commits  | `git log -5` |
| `--since or --after` |  limits the logs to be from date given | `git log --since "21-01-15"` |
| | can use `number.scale` where scale=year, month, week, day and minute | `git log --since 2.weeks` |
| `--until or --before` |  limits the logs to be up to date given | `git log --until "22-01-15"` |
| `--author` | only shows commits from given author | `git log --author "morrisj"` |
| `--grep` | only show commits with a commit message containing the string given | `git log --grep "magnet"`  |
| `--stat` | if you want to see some abbreviated stats for each commit | `git log --stat` |
| `--oneline` | Outputs commit number, date and message to a single line | `git log --oneline` |
| `--graph` | display commits in a ASCI graph/timeline | `git log --graph` |
| `-S` | only show commits adding or removing code matching the string | `git log -S "find_me"` |

- to output the log to a file add `>> file_name.log` to the end of the command

## Changing the code
Major changes

- Create a new branch (e.g. "model_a_development") on the GitLab webpage.
    - or run `git branch new_branch_name` after cloning the repo (next bullet)
- Clone the repository `git clone git@git.ccfe.ac.uk:process/process.git`
- Swap to your branch `git checkout new_branch_name`

Minor changes (e.g. single line changes)

- Clone the repository `git clone git.git.ccfe.ac.uk:process/process.git`

## Committing changes
- Make your changes to the code and at suitable stages commit locally:
    - `git add file_changed_1 file_changed_2`
    - `git commit -m "COMMIT MESSAGE"`
- The commit message should be informative and give useful information for future development.
  - Such as:
  ```
  Made changes to the TF coil magnet model. Updated the allowable stress in the coils
  to be 600MPa. Remove side-wall case. Ran test suite and everything OK.
  ```
  - not
  ```
  Update to magnet model
  ```

- Before pushing back to the repository make sure that your branch is up to date
with any changes that might have been made by other developers, `git pull`
- When you wish to push your branch back to the repository enter `git push`

## Merging
### Develop into your branch
When you have finished making a major change on a new branch, you will need to merge your branch with the develop branch to keep up with the latest changes.

- Make sure you have committed all of your changes to your local branch.
- Update your local repo with `git pull`
- Checkout the development branch `git checkout develop`
- Check remote repo again `git pull`
- Checkout your new branch `git checkout my_branch_name`
- Merge develop into your branch `git merge develop`
- If there are conflicts check the files listed for the following:
```
This line was edited in develop branch
```

- Resolve any conflicts then `git add file_1 file_2` where file_1 and file_2 are
files that had conflicts.
- Commit the changes `git commit`
- Push the branch back to the remote repo `git push`

### Your branch into develop
After having developed your branch, and merged develop into it as detailed in the previous comment, you will need to merge your branch with develop.

- Check your repo is up to date `git pull`
- `git checkout my_branch_name`
- `git checkout develop`
- `git merge my_branch_name`
- Resolve conflicts in similar manner to section above
- `git push`

## Tagging
Version takes the form `x.y.z` for internal development versions and takes the form `x.y` 
for external master releases. 

This is similar to the .NET convention of version numbering


`[major version]`.`[minor version]`.`[revision number]`

`[major version]` - release containing numerous major changes

`[minor version]` - medium change, i.e. new model, major bug fix

`[revision number]` - weekly or on demand build/change


To add a tag to a commit do the following:

- In subroutine `inform` in the file `process.f90`, change the value of `progver` by
incrementing the revision appropriately (given guidance above) to `x.y.z` and the release date. It
is important to keep exactly the same format.
- Add a brief comment to the bottom of the source file `process.f90` describing the changes made
since the last commit in the same branch.  Start the line with `! x.y.z:`, following the
existing examples
- If any of the User Guide `.tex` files have been modified, edit the value of `\version`
in `process.tex` by changing the Revision to `x.y.z` and the date
- If you have changed any ”use” statements in the code, or any compilation dependencies in the Makefile, run
`rm -rf build`
- To ensure that all the code and documentation compiles successfully re-run the compiler, including the making of dicts, docs, etc.
- Check `test_suite.py` in the `test_suite` folder runs successfully
- Add files changed `git add file_1 file_2 ...`
- Commit changes `git commit -m "COMMIT MESSAGE"`
- Add a tag number `git tag -a x.y.z -m "Version x.y.z"`
- `git push`
- `git push origin x.y.z`

When releasing a ***tagged version of the code the user should compile a release note for the repo***.
This note should outline the major changes for the release, including issues from GitLab that
were resolved, bugs fixed, etc.

Between user tags Git will create tags in the following format:

```
1.0.12-11-g3f1b433
```

- `1.0.12` is the last manually entered tag by the user
- `11` is the number of commits since that tag
- `g3f1b433` is a unique identifier for this specific commit

This allows the user to checkout a specific commit between tagged versions. PROCESS now outputs this information into the `OUT.DAT` and `MFILE.DAT` and is 
updated upon compilation. This way each output file is trackable to a specific commit.


**Tagging Commands**
| Command | Description |
| -------- | -------- | 
| `git describe --tags`   | show the current tag  | 
| `git tag -l "1.0.*"` | list tags contained in `1.0.z` |
| `git checkout tags/<tag name>` | checkout a specific tag |