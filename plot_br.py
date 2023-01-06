import ROOT
import seaborn as sns

#mphi = [100,110,125,140,160,180,200,250,300] 
#mA = [60,70,80,90,100,125,140,160] 

mphi = [100,125,160,200,300] 
mA = [60,80,100,125,160]
#mA = [100,125,160]

infile = "typeX_BR.root"
plot = "mphi"
other_mass = "100"
logx = True

f = ROOT.TFile(infile)
print f

if plot == "mA":
  mass = mA
  label = "A"
  other = "mphi"
elif plot == "mphi":
  mass = mphi
  label = "#phi"
  other = "mA"

A_hist = []
phi_hist = []
for m in mass:
  if plot == "mA":
    A_hist.append(f.Get("A_to_tautau_mphi{}_mA{}".format(other_mass,m)))
    phi_hist.append(f.Get("phi_to_tautau_mphi{}_mA{}".format(other_mass,m)))
  elif plot == "mphi":
    A_hist.append(f.Get("A_to_tautau_mphi{}_mA{}".format(m,other_mass)))
    phi_hist.append(f.Get("phi_to_tautau_mphi{}_mA{}".format(m,other_mass)))

def PlotFunc(output_name, hists, xlabel, ylabel, logx=True):
  c = ROOT.TCanvas('c','c',700,700)
  c.SetBottomMargin(0.15)
  c.SetLeftMargin(0.15)
  if logx: c.SetLogx()
  mg = ROOT.TMultiGraph()
  mg.SetTitle(""+";"+xlabel+";"+ylabel)
  palette = sns.color_palette("dark", len(hists))
  for ind, val in enumerate(hists):
    hists[ind].SetLineColor(ROOT.TColor.GetColor(palette[ind][0],palette[ind][1],palette[ind][2]))
    hists[ind].SetLineWidth(3)
    mg.Add(hists[ind])
  mg.Draw("AC")
  mg.GetHistogram().GetXaxis().SetLimits(0.1,100.0)
  mg.GetHistogram().GetYaxis().SetRangeUser(0,1.5)
  mg.GetHistogram().GetXaxis().SetTitleOffset(1.2)
  mg.GetHistogram().GetXaxis().SetTitleSize(0.05)
  mg.GetHistogram().GetYaxis().SetTitleSize(0.05)
  if logx:
    mg.GetHistogram().GetXaxis().SetMoreLogLabels()
    mg.GetHistogram().GetXaxis().SetNoExponent()

  l = ROOT.TLegend(0.2,0.65,0.45,0.88)
  l.SetBorderSize(0)
  l.SetTextSize(0.03)
  for ind, val in enumerate(hists):
    l.AddEntry(hists[ind],"m_{#phi}="+hists[ind].GetName().split("mphi")[1].split("_")[0] + " GeV, m_{A}="+hists[ind].GetName().split("mA")[1]+" GeV",'l')  
  l.Draw()

  c.Print(output_name+'.pdf')


PlotFunc("A_br_plot_{}{}".format(other,other_mass), A_hist, "tan#beta", "A#rightarrow#tau#tau  BR", logx=logx)
PlotFunc("phi_br_plot_{}{}".format(other,other_mass), phi_hist, "tan#beta", "#phi#rightarrow#tau#tau  BR", logx=logx)
