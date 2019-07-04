cde v 0.4.1
================
Rob Briers
2019-07-04

## Test environments

  - Local Win 7 Enterprise, R 3.6.0 (via R CMD check –as-cran)
  - Local Windows 10, R 3.6.0 (via R CMD check –as-cran)
  - ubuntu 14.04.5, R: release (travis-ci)
  - ubuntu 14.04.5, R: old-rel 3.5.3 (travis-ci)
  - macOS High Sierra 10.13.3, R: release (travis-ci)
  - macOS High Sierra 10.13.3, R: old-rel (travis-ci)
  - Fedora Linux, R-devel, clang, gfortran (rhub)
  - Ubuntu Linux 16.04 LTS, R-release, GCC (rhub)
  - Windows Server 2008 R2 SP1, R-devel, 32⁄64 bit (rhub)

## R CMD check results

There were no ERRORs or WARNINGs.

There is one NOTE:

CDE (4:53) RNAG (11:64) WFD (3:44, 9:3, 10:49) cde (8:18) rOpenSci
(17:42) Possibly mis-spelled words in DESCRIPTION: waterbodies (9:28,
12:22)

These are all correct and are mostly abbreviations explained in
supporting docs.

## Downstream dependencies

There aren’t any.
