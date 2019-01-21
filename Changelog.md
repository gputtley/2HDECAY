## [1.1.0] - 2018-10-30
### New
- Implemented the parameter conversion for directly comparing results computed in distinct renormalization schemes.
### Changed
- The input file format was changed: in line 59 of the input file, an additional input parameter REFSCHEM was added which is used for defining the reference scheme for the parameter conversion. Trying to use the input file format that was used in 2HDECAY 1.0.0, 1.0.1 and 1.0.2 will throw an error and exceptionally terminate the program.
- The output file format was changed to account for the parameter conversion: the scalar mixing angles alpha and beta and the LO decay width and branching ratios are given for each renormalization scheme that is chosen.

## [1.0.2] - 2018-10-30
### Changed
- Fixed some typos in the Yukawa couplings for the real corrections to leptonic decays in 2HDM types X and Y.

## [1.0.1] - 2018-10-29
### Changed
- Fixed a bug that would prevent the compilation of 2HDECAY when the path to 2HDECAY would contain whitespace characters (on all operating systems).
- Updated some copyright information.