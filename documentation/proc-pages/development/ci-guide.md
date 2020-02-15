
# Introduction

!!! Note "CI Branches"
    Currently runs on `develop` and any branch with a name starting with `issue-`

The CI jobs run using the Ubuntu image that has been uploaded to the repository 
called `git.ccfe.ac.uk:4567/process/process/ci-image`.

## CI Configuration File

The GitLab CI system configuration is defined in the file

> `./.gitlab-ci.yml`

Located in the main directory of the repository.

## Stages

The GitLab CI pipeline can be separated into user defined stages. The stages 
are executed in order and stages can be conditional on if the previous stage was 
successful or not.

```yaml
stages:
  - build
  - testing
  - standards
  - pages
```

## Setup Scripts

### Ubuntu

The following part of the config file sets up the environment on the Ubuntu 
image.

```yaml
.before_script_template: &build-ubuntu-setup
  before_script:
    - export PATH=$PATH:~/.local/bin
    - export LANG=C.UTF-8
    - pwd
    - gcc --version
    - ls -lah
    - cmake -H. -Bbuild
```

### Freia

The following part of the config file sets up the environment on the Freia 
image.

```yaml
.before_script_template: &build-freia-setup
  before_script:
    - export LOGNAME=root
    - source /etc/profile.d/modules.sh
    - module use /usr/local/modules/default
    - module unload python
    - module load python/3.5.1
    - module load texlive/2017
    - module unload ifort 
    - module load gfortran
    - export GTEST=/home/PROCESS/testing_frameworks/googletest/googletest
    - echo ld_library_path=$LD_LIBRARY_PATH
    - export PATH=$PATH:~/.local/bin
    - export LANG=en_GB.utf8
```

## Stage: Build

| Job | Description |
| --- | ----------- |
| `make` | build source code | 
| `make_developerguide` | build developer guide PDF | 
| `make_dicts` | build python dictionaries for utilies | 
| `nake_optsolverdoc` | build optimisation solver guide PDF | 
| `make_tfdoc` | build TF coil model PDF documentation | 
| `make_utilitiesdoc` | build utilities guide PDF | 


## Stage: Testing

| Job | Description |
| --- | ----------- |
| `baseline_2018` | Run 2018 EU-DEMO baseline and compare to reference output | 
| `baseline_2019` | Run 2019 EU-DEMO baseline and compare to reference output | 
| `unit_tests` | Run PROCESS unit tests | 
| `test_suite` | Run PROCESS test suite | 


## Stage: Standards

| Job | Description |
| --- | ----------- |
| `line_length` | Check line length compliance | 

## Stage: Freia

There is a Freia stage of jobs that only run on the develop branch. These jobs are the same as the jobs in build, testing and standards but using the Freia image.

## Stage: Pages

| Job | Description |
| --- | ----------- |
| `pages` | Build and publish GitLab Pages website for PROCESS | 

## Offline Jobs

!!! Note
    Only the non-freia jobs can be run offline

To run to PROCESS GitLab jobs locally do the following:
* Install `gitlab-runner` (see [here](https://docs.gitlab.com/runner/install/))
* Install docker (see [here](https://docs.docker.com/install/))
* In the top level of the repository run
  * `gitlab-runner exec docker <job_name>`
  * Where `<job_name>` is the name of the job from the tables above (excluding Freia jobs).