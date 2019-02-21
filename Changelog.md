## [1.1.0] - 2019-02-22
### New
- Implemented the parameter conversion for directly comparing results computed in distinct renormalization schemes.
- Implemented the scale evolution of ![](https://latex.codecogs.com/gif.latex?%5Coverline%7B%5Ctext%7BMS%7D%7D "\overline{\text{MS}}") input parameters from an input renormalization scale (INSCALE) to the renormalization scale at which the NLO calculations are performed (OUTSCALE).
- Implemented six new renormalization schemes for the scalar mixing angles ![](https://latex.codecogs.com/gif.latex?%5Calpha "\alpha") and ![](https://latex.codecogs.com/gif.latex?%5Cbeta "\beta"):
  - "OS1 scheme" as described in *A. Denner, S. Dittmaier, J.-N. Lang, J. High Energ. Phys. (2018) 2018: 104* with scheme identifier 12.
  - "OS2 scheme" as described in *A. Denner, S. Dittmaier, J.-N. Lang, J. High Energ. Phys. (2018) 2018: 104* with scheme identifier 13.
  - "OS12 scheme" as described in *A. Denner, S. Dittmaier, J.-N. Lang, J. High Energ. Phys. (2018) 2018: 104* with scheme identifier 14.
  - "BFMS scheme" as described in *A. Denner, S. Dittmaier, J.-N. Lang, J. High Energ. Phys. (2018) 2018: 104* with scheme identifier 15.
  - ![](https://latex.codecogs.com/gif.latex?%5Coverline%7B%5Ctext%7BMS%7D%7D "\overline{\text{MS}}") (standard tadpole scheme) with scheme identifier 16.
  - ![](https://latex.codecogs.com/gif.latex?%5Coverline%7B%5Ctext%7BMS%7D%7D "\overline{\text{MS}}") (alternative tadpole scheme) with scheme identifier 17.
### Removed
- Removed the three process-dependent schemes "proc.-dep. 1 (standard tadpole scheme)", "proc.-dep. 2 (standard tadpole scheme)" and "proc.-dep. 3 (standard tadpole scheme)" due to redundancy, since they yield the same result as the process-dependent schemes defined in the alternative tadpole scheme.
### Changed
- Updated the manual according to all changes made in version 1.1.0.
- The input file format was changed. Trying to use the input file format that was used in 2HDECAY 1.0.0, 1.0.1 and 1.0.2 will throw an error and exceptionally terminate the program.. The following lines changed in the new input file format:
  - in line 59 of the input file, an additional input parameter REFSCHEM was added which is used for defining the reference scheme for the parameter conversion. 
  - in line 64 of the input file, an additional input parameter OUTSCALE was added which overtakes the function of the formerly defined INSCALE, i.e. OUTSCALE is the renormalization scale at which the one-loop integrals and counterterms are computed.
- The input parameter INSCALE, defined in line 63 of the input file (according to the new input file format), now has a new meaning compared to 2HDECAY 1.0.0, 1.0.1 and 1.0.2. It is now the renormalization scale at which all ![](https://latex.codecogs.com/gif.latex?%5Coverline%7B%5Ctext%7BMS%7D%7D "\overline{\text{MS}}") input parameters are defined.
- The output file format was changed to account for the parameter conversion: the scalar mixing angles alpha and beta, the soft-![](https://latex.codecogs.com/gif.latex?%5Cmathbb%7BZ%7D_2 "\mathbb{Z}_2")-breaking squared mass scale ![](https://latex.codecogs.com/gif.latex?m_%7B12%7D%5E2 "m_{12}^2") and the LO decay width and branching ratios are given for each renormalization scheme that is chosen.
- Due to the removal of the three process-dependent schemes in the standard tadpole scheme, the numbering of the schemes changed:
  - The scheme "proc.-dep. 1 (alternative tadpole scheme)" now has scheme identifier 9 (former identifier: 10).
  - The scheme "proc.-dep. 2 (alternative tadpole scheme)" now has scheme identifier 10 (former identifier: 12). 
  - The scheme "proc.-dep. 3 (alternative tadpole scheme)" now has scheme identifier 11 (former identifier: 14).
- Fixed some bugs concerning the rescaling of the Fermi constant ![](https://latex.codecogs.com/gif.latex?G_F "G_F") in the HDECAY subroutine.
- Fixed a bug concerning the print-out of the values of alpha in the intermediate output file that is read in by HDECAY if RENSCHEM was set to values larger than 9.

## [1.0.2] - 2018-10-30
### Changed
- Fixed some typos in the Yukawa couplings for the real corrections to leptonic decays in 2HDM types X and Y.

## [1.0.1] - 2018-10-29
### Changed
- Fixed a bug that would prevent the compilation of 2HDECAY when the path to 2HDECAY would contain whitespace characters (on all operating systems).
- Updated some copyright information.