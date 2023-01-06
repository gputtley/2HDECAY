import numpy as np
import math as m
import os
import pandas as pd
import copy
import ROOT
from array import array

## change these ##

mphis = [100.0,110.0,125.0,140.0,160.0,180.0,200.0,250.0,300.0]
mphis = [100.0,200.0]

mintanb = 0.1
maxtanb = 120

ntanb = 100

out_file = "typeII_BR.root"

##################

def WriteListToFile(lst,output_name):
  textfile = open(output_name, "w")
  for i in lst:
    textfile.write(i + "\n")
  textfile.close()

#loop_dict = {"tanb":[],"mphi":[],"mA":[]}

loop_dict = {}
#tanbs = list(np.linspace(mintanb,maxtanb,ntanb))
tanbs = list(np.logspace(np.log10(mintanb),np.log10(maxtanb),ntanb))
#mAs = list(np.linspace(minmA,maxmA,nmA))
mAs = [60.0,70.0,80.0,90.0,100.0,125.0,140.0,160.0]
#mAs = [100.0]

first_loop = True
for mphi in mphis:
  print "m_{phi} =", mphi
  loop_dict[mphi] = {}
  for mA in mAs:
    loop_dict[mphi][mA] = {"tanb":array( 'd' )}
    print "  m_{A} =", mA
    for tanb in tanbs:
      print "    tanbeta =", tanb
      if mphi < 125:
        sinbma = 0.0
        mH = 125.0
        mh = mphi
        phi = "h"
        hSM ="H"
      else:
        sinbma = 1.0
        mH = mphi
        mh = 125.0
        phi = "H"
        hSM = "h"
      
      mhc = mphi
  
      a = -m.asin(sinbma) + m.atan(tanb)
      b = m.atan(tanb)
      m12_2 = mphi**2 * m.sin(b) * m.cos(b)
     
      file_list = [
                   "SLHAIN   = 1",
                   "SLHAOUT  = 1",
                   "COUPVAR  = 1",
                   "HIGGS    = 5",
                   "OMIT ELW = 1",
                   "OMIT ELW2= 0",
                   "SM4      = 0",
                   "FERMPHOB = 0",
                   "2HDM     = 1",
                   "MODEL    = 1",
                   "TGBET    = 5.07403e+01",
                   "MABEG    = 4.67967e+02",
                   "MAEND    = 4.67967e+02",
                   "NMA      = 1",
                   "********************* hMSSM (MODEL = 10) *********************************",
                   "MHL      = 125.D0",
                   "**************************************************************************",
                   "ALS(MZ)  = 1.18000e-01",
                   "MSBAR(2) = 9.50000e-02",
                   "MCBAR(3) = 0.98600e+00",
                   "MBBAR(MB)= 4.18000e+00",
                   "MT       = 1.73200e+02",
                   "MTAU     = 1.77682e+00",
                   "MMUON    = 1.056583715e-01",
                   "1/ALPHA  = 1.37036e+02",
                   "ALPHAMZ  = 7.754222173973729e-03",
                   "GF       = 1.1663787e-05",
                   "GFCALC   = 0.000000000",
                   "GAMW     = 2.08500e+00",
                   "GAMZ     = 2.49520e+00",
                   "MZ       = 9.11876e+01",
                   "MW       = 8.0385e+01",
                   "VTB      = 9.9910e-01",
                   "VTS      = 4.040e-02",
                   "VTD      = 8.67e-03",
                   "VCB      = 4.12e-02",
                   "VCS      = 9.7344e-01",
                   "VCD      = 2.252e-01",
                   "VUB      = 3.51e-03",
                   "VUS      = 2.2534e-01",
                   "VUD      = 9.7427e-01",
                   "********************* 4TH GENERATION *************************************",
                   "  SCENARIO FOR ELW. CORRECTIONS TO H -> GG (EVERYTHING IN GEV):",
                   "  GG_ELW = 1: MTP = 500    MBP = 450    MNUP = 375    MEP = 450",
                   "  GG_ELW = 2: MBP = MNUP = MEP = 600    MTP = MBP+50*(1+LOG(M_H/115)/5)",
                   "",
                   "GG_ELW   = 1",
                   "MTP      = 500.D0",
                   "MBP      = 450.D0",
                   "MNUP     = 375.D0",
                   "MEP      = 450.D0",
                   "************************** 2 Higgs Doublet Model *************************",
                   "  TYPE: 1 (I), 2 (II), 3 (lepton-specific), 4 (flipped)",
                   "  PARAM: 1 (masses), 2 (lambda_i)",
                   "",
                   "PARAM    = 1",
                   "TYPE     = 2",
                   "RENSCHEM = 7",
                   "REFSCHEM = 5",
                   "********************",
                   "TGBET2HDM= {}D0".format(tanb),
                   "M_12^2   = {}D0".format(m12_2),
                   "INSCALE  = 125.0D0",
                   "OUTSCALE = MIN",
                   "******************** PARAM=1:",
                   "ALPHA_H  = {}D0".format(a),
                   "MHL      = {}D0".format(mh),
                   "MHH      = {}D0".format(mH),
                   "MHA      = {}D0".format(mA),
                   "MH+-     = {}D0".format(mhc),
                   "******************** PARAM=2:",
                   "LAMBDA1  = 6.368674377530086700D0",
                   "LAMBDA2  = 0.235570240072350970D0",
                   "LAMBDA3  = 1.780416490847621700D0",
                   "LAMBDA4  = -1.52623758540479430D0",
                   "LAMBDA5  = 0.074592764717552856D0",
                   "**************************************************************************",
                   "SUSYSCALE= 2.22449e+03",
                   "MU       = -1.86701e+03",
                   "M2       = -2.39071e+02",
                   "MGLUINO  = 7.32754e+02",
                   "MSL1     = 1.49552e+03",
                   "MER1     = 1.62210e+03",
                   "MQL1     = 9.30379e+01",
                   "MUR1     = 2.77029e+03",
                   "MDR1     = 1.76481e+03",
                   "MSL      = 1.97714e+03",
                   "MER      = 9.29678e+02",
                   "MSQ      = 2.68124e+03",
                   "MUR      = 1.85939e+03",
                   "MDR      = 2.28235e+03",
                   "AL       = -4.62984e+03",
                   "AU       = 5.31164e+03",
                   "AD       = 2.54430e+03",
                   "ON-SHELL = 0",
                   "ON-SH-WZ = 0",
                   "IPOLE    = 0",
                   "OFF-SUSY = 0",
                   "INDIDEC  = 0",
                   "NF-GG    = 5",
                   "IGOLD    = 0",
                   "MPLANCK  = 2.40000e+18",
                   "MGOLD    = 1.00000e-13",
                   "******************* VARIATION OF HIGGS COUPLINGS *************************",
                   "ELWK     = 1",
                   "CW       = 1.D0",
                   "CZ       = 1.D0",
                   "Ctau     = 1.D0",
                   "Cmu      = 1.D0",
                   "Ct       = 1.D0",
                   "Cb       = 1.D0",
                   "Cc       = 1.D0",
                   "Cs       = 1.D0",
                   "Cgaga    = 0.D0",
                   "Cgg      = 0.D0",
                   "CZga     = 0.D0",
                   "********************* 4TH GENERATION *************************************",
                   "Ctp      = 0.D0",
                   "Cbp      = 0.D0",
                   "Cnup     = 0.D0",
                   "Cep      = 0.D0",
                   ]
      
      WriteListToFile(file_list,"Input/2hdecay.in")
      
      os.system("python 2HDECAY.py &> output.txt")
      
      with open('Results/2hdecay_BR.out') as f:
        use_lines = False
        for line in f:
          if "QCD and EW" in line: use_lines = True
          if "QCD Only" in line: use_lines = False
  
          if use_lines and "BR(" in line and ("h ->" in line or "H ->" in line or "A ->" in line):
  
            # add hSM and phi
            if ("h ->" in line or "H ->" in line):
              if "{} ->".format(phi) in line:
                name_phi = str("".join(line.split("(")[1].split(")")[0].rstrip().replace("h ","phi ").replace("H ","phi ").split()))
              elif "{} ->".format(hSM) in line:
                name_phi = str("".join(line.split("(")[1].split(")")[0].rstrip().replace("h ","hSM ").replace("H ","hSM ").split()))
              if name_phi in loop_dict[mphi][mA].keys():
                loop_dict[mphi][mA][name_phi].append(float(line.split()[0].replace("E","e")))
              else:
                loop_dict[mphi][mA][name_phi] = array('d', [0.0]*len(loop_dict[mphi][mA]["tanb"]))
                loop_dict[mphi][mA][name_phi].append(float(line.split()[0].replace("E","e")))
  
            # add h, H and A
            name = str("".join(line.split("(")[1].split(")")[0].rstrip().split()))
            if first_loop: loop_dict[mphi][mA][name] = array( 'd' )
            if name in loop_dict[mphi][mA].keys():
              loop_dict[mphi][mA][name].append(float(line.split()[0].replace("E","e")))
            else:
              loop_dict[mphi][mA][name] = array('d', [0.0]*len(loop_dict[mphi][mA]["tanb"]))
              loop_dict[mphi][mA][name].append(float(line.split()[0].replace("E","e")))
 
 
        loop_dict[mphi][mA]["tanb"].append(tanb)
        for key, val in loop_dict[mphi][mA].items(): 
          if len(val) != len(loop_dict[mphi][mA]["tanb"]):
            loop_dict[mphi][mA][key].append(0.0)
        first_loop = False

to_write = list()
for k_mphi, v_mphi in loop_dict.items():
  for k_mA, v_mA in v_mphi.items():
    for k_decay, v_decay in v_mA.items():
      if "tanb" == k_decay: continue
      name = k_decay.replace("->","_to_").replace("-","").replace("+","")+"_mphi"+str(int(k_mphi))+"_mA"+str(int(k_mA))
      tg = ROOT.TGraph(len(v_decay),v_mA["tanb"],v_decay)
      tg.SetName(name)
      to_write.append(tg)

fout = ROOT.TFile(out_file, 'RECREATE')
for i in to_write: i.Write()




