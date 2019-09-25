
# Introduction

!!! Note "CI Branches"
    Currently runs on `develop` and any branch with a name starting with `issue-`

## CI Configuration File

The GitLab CI system configuration is defined in the file

> `./.gitlab-ci.yml`

Located in the main directory of the repository.

### Stages

The GitLab CI pipeline can be separated into user defined stages. The stages 
are executed in order and stages can be conditional on if the previous stage was 
successful or not.

```yaml
stages:
  - build
  - testing
  - jenkins
  - baseline
  - standards
  - pages
```

### Setup Scripts

The following part of the config file sets up the environment on the Freia 
image.

```yaml
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
```

### Stage: Build

### Stage: Testing

### Stage: Baseline

### Stage: Standards

### Stage: Pages

### Stage: Jenkins

## GitLab Pipelines

## Linking to Merge Requests and Issues

