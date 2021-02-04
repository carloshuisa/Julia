

const TOTAL_PERIODS = 36
const big_M = 999999
const EPSILON = 0.0000001
#const SCENARIOS = [Symbol("Specific - 15d"), Symbol("Specific - 30d"),Symbol("Specific - 60d"), Symbol("Systemic - 15d"), Symbol("Systemic - 30d"),Symbol("Systemic - 60d")]
const SCENARIOS = [Symbol("LCR_SPECIFIC_15D"), Symbol("LCR_SPECIFIC_30D"),Symbol("LCR_SPECIFIC_60D"),Symbol("LCR_SYSTEMIC_15D"), Symbol("LCR_SYSTEMIC_30D"),Symbol("LCR_SYSTEMIC_60D")]
##TODO These ratio parameters make the problem infeasible, it is suggested to use it when you have full data in
const GAMMA_LOW_DEPO = 0 # 0.7 #TODO it makes the modle infeasible
const OMEGA_LOW_DEPO = 0 #  0.8 #TODO it makes the modle infeasible
const KSEE_UPPER_DEPO = 100 # 1.2    #TODO it makes the modle infeasible
const UPPER_PRO_FUNDING_RATIO = 100 # 0.55   #TODO it makes the modle infeasible
const T_TAX_RATE = 0.295

const MONTH = "Septiembre"


const PHI = 6000
const EURO = 900
const POUND = 19123
const YEN = 0.28
const Pp = 0.68
const Aa = 1.5
const THORN = 0.28
const Ss = 0.28
const Yy = 0.76
const qp = 0.08

struct Variables
    x_assets::Containers.SparseAxisArray
    z_assets::Containers.SparseAxisArray
    y_assets::Containers.SparseAxisArray
    x_liabilit::Containers.SparseAxisArray
    z_liability::Containers.SparseAxisArray
    y_liability::Containers.SparseAxisArray
    y_liaBin::Containers.SparseAxisArray
    y_liaBin2::Containers.SparseAxisArray
    x_assetBin::Containers.SparseAxisArray
    #y_liaBin1_4::Containers.SparseAxisArray
    d_obligations::Containers.DenseAxisArray
    d_obl_min::Containers.DenseAxisArray
    d_obl_min_threshold::Containers.DenseAxisArray
    d_omega::Containers.DenseAxisArray
    b_collaterAsset::Containers.SparseAxisArray
    u_assets::Containers.SparseAxisArray
    w_assets_liab::Containers.DenseAxisArray
    helper_BCRP_3::Containers.SparseAxisArray
    w_helper_a_l::Containers.DenseAxisArray
    o::VariableRef
    fo_l1::VariableRef
    fo_l2::VariableRef
    fo_l34::VariableRef
    fo_l56::VariableRef
    tose_r_p::Containers.DenseAxisArray
    tose_s_sd_p::Containers.DenseAxisArray
    tose_p::Containers.DenseAxisArray
    lr_s_sd_p_1::Containers.DenseAxisArray
    lr_s_sd_p_2::Containers.DenseAxisArray
    lr_pen::Containers.DenseAxisArray
    tose_r_u::Containers.DenseAxisArray
    tose_s_wtm::Containers.DenseAxisArray
    tose_s_comex::Containers.DenseAxisArray
    tose_s_sd_u::Containers.DenseAxisArray
    tose_u::Containers.DenseAxisArray
    lr_r_1::Containers.DenseAxisArray
    lr_r_2::Containers.DenseAxisArray
    lr_r_3::Containers.DenseAxisArray
    lr_ra_hv::Containers.DenseAxisArray
    lr_ra_tl::Containers.DenseAxisArray
    lr_usd::Containers.DenseAxisArray
    bcrp_acc_p::Containers.DenseAxisArray
    bcrp_acc_u_1::Containers.DenseAxisArray
    bcrp_acc_u_2::Containers.DenseAxisArray
    bcrp_acc_u_3::Containers.DenseAxisArray
    max_aux_1::Containers.DenseAxisArray
    max_aux_2::Containers.DenseAxisArray
    balance_diff::VariableRef
    lr_omega_1::Containers.DenseAxisArray
    lr_omega_2::Containers.DenseAxisArray
    lr_omega_3::Containers.DenseAxisArray
    lr_omega_4::Containers.DenseAxisArray
    lr_omega_5::Containers.DenseAxisArray
    lr_omega_6::Containers.DenseAxisArray
    lr_omega_7::Containers.DenseAxisArray
    lr_omega_8::Containers.DenseAxisArray
    lr_omega_9::Containers.DenseAxisArray
    lr_omega_10::Containers.DenseAxisArray
    lr_omega_11::Containers.DenseAxisArray
    lr_omega_12::Containers.DenseAxisArray
    lr_omega_13::Containers.DenseAxisArray
    ven::Containers.DenseAxisArray
    ce::Containers.DenseAxisArray
    nr::Containers.DenseAxisArray
    nr2::Containers.DenseAxisArray
    wr::Containers.DenseAxisArray
    lr::Containers.DenseAxisArray
    b::Containers.DenseAxisArray
    zr::Containers.DenseAxisArray
    yr::Containers.DenseAxisArray
    vr::Containers.DenseAxisArray
    br::Containers.DenseAxisArray
    ato_b::Containers.DenseAxisArray
    ato_b2::Containers.DenseAxisArray
    mc::Containers.SparseAxisArray
    t_repo::Containers.DenseAxisArray
    mc_omega_1::Containers.SparseAxisArray
    asf::Containers.DenseAxisArray
    rsf::Containers.DenseAxisArray
    asf_1::Containers.DenseAxisArray
    rsf_1::Containers.DenseAxisArray
    bcrp_repo::Variables_BCRP_repo
    hqla::Containers.DenseAxisArray
    of::Containers.DenseAxisArray
end


  model = Model(optimizer_with_attributes(Cbc.Optimizer, "max_cpu_time" => 10*60))

  @variable(model, z_assets[a in par.asset_A1_Codes,
          t in (par.periods[a][1]==36 ? (par.periods[a][1]:par.periods[a][2]) : (par.periods[a][1]+1:par.periods[a][2]) ) ] >= 0)
  # code changed, dimension t added
  @variable(model, y_assets[a in par.asset_A1_Codes,
          t in par.periods[a][1]:par.periods[a][2]] >= 0)

  @variable(model, x_assets[a in par.asset_A1_Codes,
          t in par.periods[a][1]:par.periods[a][2]] >= 0)

  @variable(model, x_liabilit[a in par.liabilities_L1_Codes,
          t in par.periods[a][1]:par.periods[a][2]] >= 0)

  @variable(model, z_liability[a in par.liabilities_L1_Codes,
          t in par.periods[a][1]+1:par.periods[a][2]] >= 0)
  # code changed, dimension t added
  @variable(model, y_liability[a in par.liabilities_L1_Codes,
          t in par.periods[a][1]:par.periods[a][2]] >= 0)

  #@variable(model, y_liaBin[a in par.liabilities_L1_Codes], Bin)
  @variable(model, y_liaBin[a in par.liabilities_L1_Codes,
          t in par.periods[a][1]:par.periods[a][2]], Bin)

  inst_curr = Dict()
  for l in unique(par.limits[:,:INSTRUMENT])
      inst_curr[l] = unique(par.limits[par.limits[:INSTRUMENT].==l,:][:CURRENCY])
  end

  liab_inst = unique(filter(r-> r.FLG_L1 == 1 && r.DECISION_VARIABLE == "Y" && r.INSTRUMENT !=0, par.balanceInventory)[:,:INSTRUMENT] )
  inst_liab_r = filter(r-> r.ACQUISITION_LOWER_LIMIT_MM>0 && r.ACQUISITION_LOWER_LIMIT_MM<9999, par.limits)[:,:INSTRUMENT]
  inst_liab = unique(intersect(inst_liab_r,liab_inst))    

  @variable(model, y_liaBin2[l in inst_liab, c in inst_curr[l], t in 1:TOTAL_PERIODS], Bin)

  assets_inst = unique(filter(r-> r.DECISION_VARIABLE == "Y" && r.FLG_A1 == 1  && !ismissing(r[Symbol("MATURITY_PERIOD")])  && r.INSTRUMENT !=0, par.balanceInventory)[:,:INSTRUMENT] )
  inst_ass = unique(intersect(filter(r-> r.BALANCE_LOWER_LIMIT_MM>0, par.limits)[:,:INSTRUMENT],assets_inst))

  @variable(model, x_assetBin[l in inst_ass, c in inst_curr[l], t in 1:TOTAL_PERIODS], Bin)

  @variable(model, d_obligations[t in 0:TOTAL_PERIODS, c in CURRENCY, s_tau in SCENARIOS ] >= 0)

  @variable(model, d_obl_min[t in 0:TOTAL_PERIODS, c in CURRENCY, s_tau in SCENARIOS ] >= 0)

  @variable(model, d_obl_min_threshold[t in 0:TOTAL_PERIODS, c in CURRENCY, s_tau in SCENARIOS ] >= 0)

  @variable(model, d_omega[t in 0:TOTAL_PERIODS, c in CURRENCY, s_tau in SCENARIOS ], Bin)


  a1_2_12_3Codes = filter(r-> (r[Symbol("FLG_A1_2_1")] == 1 || r[Symbol("FLG_A1_2_2")] == 1 || r[Symbol("FLG_A1_1_3")] == 1 || r[Symbol("FLG_A1_2")] == 1) && r.BALANCE_CODE in par.asset_A1_Codes ,par.balanceInventory)[:,:BALANCE_CODE]
  @variable(model, b_collaterAsset[a in a1_2_12_3Codes, t in par.periods[a][1]:par.periods[a][2],s_tau in SCENARIOS] >= 0)

  a1_2Codes = filter(r-> r[Symbol("FLG_A1_2")] == 1 && r.BALANCE_CODE in par.asset_A1_Codes,par.balanceInventory)[:,:BALANCE_CODE]
  @variable(model, u_assets[a in [a1_2Codes;"a.1.7.1.2"], t in par.periods[a][1]:par.periods[a][2]] >= 0)


  l1_45_2Codes = filter(r-> (r[Symbol("FLG_L1_4")] == 1 || r[Symbol("FLG_L1_2_5")] == 1) && r.BALANCE_CODE in par.liabilities_L1_Codes ,par.balanceInventory)[:,:BALANCE_CODE]
  a1_2567_3Codes = filter(r-> (r[Symbol("FLG_A1_2")] == 1 || r[Symbol("FLG_A1_1_3")] == 1)
      && r.BALANCE_CODE in par.asset_A1_Codes ,par.balanceInventory)[:,:BALANCE_CODE]
  @variable(model, w_assets_liab[a in a1_2567_3Codes, l in l1_45_2Codes,
          t in 0:TOTAL_PERIODS] >= 0)

  l1_2_5Codes = filter(r-> (r[Symbol("FLG_L1_2_5")] == 1 && r.BALANCE_CODE in par.liabilities_L1_Codes ),par.balanceInventory)[:,:BALANCE_CODE]
  @variable(model, helper_BCRP_3[ l in l1_2_5Codes,t in par.periods[l][1]+1:par.periods[l][2]], Bin)


  a1_2_12_3Codes = filter(r-> (r[Symbol("FLG_A1_2_1")] == 1 || r[Symbol("FLG_A1_2_2")] == 1 || r[Symbol("FLG_A1_1_3")] == 1) && r.BALANCE_CODE in par.asset_A1_Codes ,par.balanceInventory)[:,:BALANCE_CODE]
  l1_4Codes = filter(r-> (r[Symbol("FLG_L1_4")] == 1 && r.BALANCE_CODE in par.liabilities_L1_Codes ),par.balanceInventory)[:,:BALANCE_CODE]
  @variable(model, w_helper_a_l[a in a1_2_12_3Codes, l in l1_4Codes, t in 0:TOTAL_PERIODS] >= 0)



  @variable(model, o )



  @variable(model, fo_l1 )
  @variable(model, fo_l2 )
  @variable(model, fo_l34 )
  @variable(model, fo_l56 )


  @variable(model, tose_r_p[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, tose_s_sd_p[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, tose_p[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, lr_s_sd_p_1[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, lr_s_sd_p_2[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, lr_pen[t in 0:TOTAL_PERIODS] >= 0)

  @variable(model, tose_r_u[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, tose_s_wtm[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, tose_s_comex[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, tose_s_sd_u[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, tose_u[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, lr_r_1[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, lr_r_2[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, lr_r_3[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, lr_ra_hv[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, lr_ra_tl[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, lr_usd[t in 0:TOTAL_PERIODS] >= 0)

  @variable(model, bcrp_acc_p[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, bcrp_acc_u_1[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, bcrp_acc_u_2[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, bcrp_acc_u_3[t in 0:TOTAL_PERIODS] >= 0)

  @variable(model, max_aux_1[t in 0:TOTAL_PERIODS] >= 0)
  @variable(model, max_aux_2[t in 0:TOTAL_PERIODS] >= 0)

  @variable(model, balance_diff >= 0)

  @variable(model, lr_omega_1[t in 0:TOTAL_PERIODS], Bin)
  @variable(model, lr_omega_2[t in 0:TOTAL_PERIODS], Bin)
  @variable(model, lr_omega_3[t in 0:TOTAL_PERIODS], Bin)
  @variable(model, lr_omega_4[t in 0:TOTAL_PERIODS], Bin)
  @variable(model, lr_omega_5[t in 0:TOTAL_PERIODS], Bin)
  @variable(model, lr_omega_6[t in 0:TOTAL_PERIODS], Bin)
  @variable(model, lr_omega_7[t in 0:TOTAL_PERIODS], Bin)
  @variable(model, lr_omega_8[t in 0:TOTAL_PERIODS], Bin)
  @variable(model, lr_omega_9[t in 0:TOTAL_PERIODS], Bin)
  @variable(model, lr_omega_10[t in 0:TOTAL_PERIODS], Bin)
  @variable(model, lr_omega_11[t in 0:TOTAL_PERIODS], Bin)
  @variable(model, lr_omega_12[t in 0:TOTAL_PERIODS], Bin)
  @variable(model, lr_omega_13[t in 0:TOTAL_PERIODS], Bin)
  @variable(model, ven[t in 0:TOTAL_PERIODS,c in CURRENCY])
  @variable(model, ce[t in 0:TOTAL_PERIODS,c in CURRENCY])


  @variable(model, nr[n in par.clasif_codes, t in 0:TOTAL_PERIODS,k in 0:TOTAL_PERIODS+13 ] >= 0)
  @variable(model, nr2[n in par.clasif_codes, t in 0:TOTAL_PERIODS,k in 0:TOTAL_PERIODS+12 ] >= 0)
  @variable(model, wr[n in par.clasif_codes, t in 0:TOTAL_PERIODS,k in 0:TOTAL_PERIODS+13 ] >= 0)
  @variable(model, lr[n in par.clasif_codes, t in 0:TOTAL_PERIODS,k in 0:TOTAL_PERIODS+13 ] >= 0)
  @variable(model, b[n in par.clasif_codes, t in 0:TOTAL_PERIODS], Bin)
  @variable(model, zr[n in par.clasif_codes, t in 0:TOTAL_PERIODS,s in 1:TOTAL_PERIODS] >= 0)
  @variable(model, yr[n in par.clasif_codes, t in 0:TOTAL_PERIODS,s in 1:TOTAL_PERIODS] >= 0)
  @variable(model, vr[n in par.clasif_codes, t in 0:TOTAL_PERIODS,s in 1:TOTAL_PERIODS] >= 0)
  @variable(model, br[n in par.clasif_codes, t in 0:TOTAL_PERIODS,s in 1:TOTAL_PERIODS], Bin)
  @variable(model, ato_b[n in par.clasif_codes, t in 0:TOTAL_PERIODS,k in 0:TOTAL_PERIODS+12], Bin)
  @variable(model, ato_b2[n in par.clasif_codes, t in 0:TOTAL_PERIODS,k in 0:TOTAL_PERIODS+12], Int)


  l1_2_6Codes = filter(r-> (r[Symbol("FLG_L1_2_6")] == 1 && r.BALANCE_CODE in par.liabilities_L1_Codes ),par.balanceInventory)[:,:BALANCE_CODE]
  @variable(model, mc[ l in l1_2_6Codes,t in par.periods[l][1]:par.liabilities_L1_Tables[(l,)][1,Symbol("MATURITY_PERIOD_REAL")]] >=0)
  @variable(model, t_repo[ l in l1_2_6Codes] >=0)
  @variable(model, mc_omega_1[l in l1_2_6Codes,t in par.periods[l][1]:par.liabilities_L1_Tables[(l,)][1,Symbol("MATURITY_PERIOD_REAL")]], Bin)

  @variable(model, asf[t in 0:TOTAL_PERIODS,c in CURRENCY])
  @variable(model, asf_1[i in 1:8, t in 0:TOTAL_PERIODS,c in CURRENCY])
  @variable(model, rsf[t in 0:TOTAL_PERIODS,c in CURRENCY])
  @variable(model, rsf_1[i in 1:4, t in 0:TOTAL_PERIODS,c in CURRENCY])
  bcrp_repo = defineVar_BCRP_REPO(model,par)


  a11_minus_111A_113A = filter(r-> r[Symbol("FLG_A1_1")] == 1 && r[Symbol("FLG_A1_1_1")] != 1 && r[Symbol("FLG_A1_1_3")] != 1 && r.BALANCE_CODE in par.asset_A1_Codes , par.balanceInventory)[:,:BALANCE_CODE]
  a12_A = filter(r-> r[Symbol("FLG_A1_2")] == 1 && r.BALANCE_CODE in par.asset_A1_Codes , par.balanceInventory)[:,:BALANCE_CODE]
  a113_A = filter(r-> r[Symbol("FLG_A1_1_3")] == 1 && r.BALANCE_CODE in par.asset_A1_Codes , par.balanceInventory)[:,:BALANCE_CODE]
  a31_A = filter(r-> r[Symbol("FLG_A3_1")] == 1 , par.balanceInventory)[:,:BALANCE_CODE]


  @variable(model, hqla[a in unique(union(a11_minus_111A_113A,a12_A,a113_A,a31_A)) , t in 0:TOTAL_PERIODS, s_tau in SCENARIOS] )

  l2_and_10F = filter(r-> r.FLG_L2 == 1 || r.FLG_L10 == 1 || r.FLG_L3_1 == 1 || r.FLG_L1_1 == 1, par.balanceInventory)[:BALANCE_CODE]
  l12_or_l13_orl14H = filter(r-> (r[Symbol("FLG_L1_2")] == 1 || r[Symbol("FLG_L1_3")] == 1 || r[Symbol("FLG_L1_4")] == 1 ) && r.BALANCE_CODE in par.liabilities_L1_Codes, par.balanceInventory)[:BALANCE_CODE]
  o_2 = filter(r-> r.FLG_O2_1 == 1 || r.FLG_O2_3 == 1 || r.FLG_O3 == 1, par.balanceInventory)[:BALANCE_CODE]

  @variable(model, of[a in unique(union(l2_and_10F,l12_or_l13_orl14H,o_2)) , t in 0:TOTAL_PERIODS, s_tau in SCENARIOS] )

  Variables(x_assets,z_assets,y_assets,x_liabilit,z_liability,y_liability,y_liaBin,y_liaBin2,x_assetBin,
 d_obligations,d_obl_min,d_obl_min_threshold,d_omega,b_collaterAsset,u_assets,w_assets_liab,helper_BCRP_3,w_helper_a_l,o,fo_l1,fo_l2,fo_l34,fo_l56,
 tose_r_p,tose_s_sd_p,tose_p,lr_s_sd_p_1,lr_s_sd_p_2,lr_pen,tose_r_u,tose_s_wtm,tose_s_comex,tose_s_sd_u,tose_u,lr_r_1,lr_r_2,lr_r_3,lr_ra_hv,lr_ra_tl,lr_usd,
 bcrp_acc_p,bcrp_acc_u_1,bcrp_acc_u_2,bcrp_acc_u_3,max_aux_1,max_aux_2,balance_diff,lr_omega_1,lr_omega_2,lr_omega_3,lr_omega_4,lr_omega_5,lr_omega_6,lr_omega_7,lr_omega_8,lr_omega_9,lr_omega_10,lr_omega_11,lr_omega_12,lr_omega_13,ven,ce,
 nr,nr2,wr,lr,b,zr,yr,vr,br,ato_b,ato_b2,mc,t_repo,mc_omega_1, asf, rsf, asf_1,rsf_1, bcrp_repo,hqla,of)




l1_1_p = filter(r-> (r[Symbol("FLG_L1_1")] == 1 && r.DECISION_VARIABLE == "Y" && r[Symbol("CURRENCY")] == "PEN"),par.balanceInventory)[:,:BALANCE_CODE]
l2_p = filter(r-> (r[Symbol("FLG_L2")] == 1 && r[Symbol("CURRENCY")] == "PEN"),par.balanceInventory)

l1_1_u = filter(r-> (r[Symbol("FLG_L1_1")] == 1 && r.DECISION_VARIABLE == "Y" && r[Symbol("CURRENCY")] == "USD"),par.balanceInventory)[:,:BALANCE_CODE]
l2_u = filter(r-> (r[Symbol("FLG_L2")] == 1 && r[Symbol("CURRENCY")] == "USD"),par.balanceInventory)


@constraint(model, lr_1[ t in 1:TOTAL_PERIODS],
      var.x_assets["a.1.1.1.1",t] >=  par.LR_params_c["PCD","PEN"][t+1]*(sum( [df[Symbol(t)] for df in eachrow(l2_p)] ) + 
  sum(var.x_liabilit[a,t] for a in l1_1_p if t in par.periods[a][1]:par.periods[a][2]) ) )

@constraint(model, lr_2[ t in 1:TOTAL_PERIODS],
    var.x_assets["a.1.1.1.2",t] >=  par.LR_params_c["PCD","USD"][t+1]*(sum( [df[Symbol(t)] for df in eachrow(l2_u)] ) + 
  sum(var.x_liabilit[a,t] for a in l1_1_u if t in par.periods[a][1]:par.periods[a][2]) ) )

l2_no23_p = filter(r-> (r[Symbol("FLG_L2")] == 1 && r[Symbol("FLG_L2_3")] != 1 && r[Symbol("CURRENCY")] == "PEN"),par.balanceInventory)

l1_1_no112_p = filter(r-> (r[Symbol("FLG_L1_1")] == 1 && r[Symbol("FLG_L1_1_2")] != 1 && r.DECISION_VARIABLE == "Y" && r[Symbol("CURRENCY")] == "PEN"),par.balanceInventory)[:,:BALANCE_CODE]

l1_2_567_p = filter(r-> ( (r[Symbol("FLG_L1_2_5")] == 1 || r[Symbol("FLG_L1_2_6")] == 1 || r[Symbol("FLG_L1_2_7")] == 1) && r.DECISION_VARIABLE == "Y" && r[Symbol("CURRENCY")] == "PEN"),par.balanceInventory)[:,:BALANCE_CODE]
  
l13_no135_p = filter(r-> ( r[Symbol("FLG_L1_3")] == 1 && r[Symbol("FLG_L1_3_5")] != 1 && r.DECISION_VARIABLE == "Y" && r[Symbol("CURRENCY")] == "PEN"),par.balanceInventory)[:,:BALANCE_CODE]

l1_2_p = filter(r-> (r[Symbol("FLG_L1_2")] == 1 && r.DECISION_VARIABLE == "Y" && r[Symbol("CURRENCY")] == "PEN"),par.balanceInventory)[:,:BALANCE_CODE]
l11_p = filter(r-> (r[Symbol("FLG_L11")] == 1 && r.DECISION_VARIABLE == "Y" && r[Symbol("CURRENCY")] == "PEN"),par.balanceInventory)[:,:BALANCE_CODE]
l1_3_5_p = filter(r-> (r[Symbol("FLG_L1_3_5")] == 1 && r.DECISION_VARIABLE == "Y" && r[Symbol("CURRENCY")] == "PEN"),par.balanceInventory)[:,:BALANCE_CODE]

set_lr3 = unique(union(intersect(union(l13_no135_p,setdiff(l1_2_p,l1_2_567_p)),l11_p),l1_3_5_p))

@constraint(model, lr_3_4[ t in 1:TOTAL_PERIODS],
      var.tose_r_p[t] ==  (1-par.LR_params_c["P_SD","PEN"][t+1])*(sum(var.x_liabilit[a,t] for a in l1_1_no112_p if t in par.periods[a][1]:par.periods[a][2])) +
      (sum( [df[Symbol(t)] for df in eachrow(l2_no23_p)] ) + sum(var.x_liabilit[a,t] for a in set_lr3 if t in par.periods[a][1]:par.periods[a][2]) ) )

@constraint(model, lr_5[ t in 1:TOTAL_PERIODS],
      var.tose_s_sd_p[t] ==  (par.LR_params_c["P_SD","PEN"][t+1])*(sum(var.x_liabilit[a,t] for a in l1_1_no112_p if t in par.periods[a][1]:par.periods[a][2])) )

@constraint(model, lr_6[ t in 1:TOTAL_PERIODS], var.tose_p[t] ==  var.tose_r_p[t] + var.tose_s_sd_p[t] )

@constraint(model, lr_9_1[ t in 1:TOTAL_PERIODS], var.lr_s_sd_p_1[t] <=  par.LR_params_c["L_S_SD","PEN"][t+1] + big_M*var.lr_omega_1[t] )
@constraint(model, lr_9_2[ t in 1:TOTAL_PERIODS], var.lr_s_sd_p_1[t] <=  var.tose_s_sd_p[t] + big_M*(1-var.lr_omega_1[t]) )
@constraint(model, lr_9_4[ t in 1:TOTAL_PERIODS], var.lr_s_sd_p_1[t] >=  par.LR_params_c["L_S_SD","PEN"][t+1] - big_M*var.lr_omega_1[t] )
@constraint(model, lr_9_5[ t in 1:TOTAL_PERIODS], var.lr_s_sd_p_1[t] >=  var.tose_s_sd_p[t] - big_M*(1-var.lr_omega_1[t])  )
@constraint(model, lr_9_8[ t in 1:TOTAL_PERIODS], par.LR_params_c["L_S_SD","PEN"][t+1] - big_M*var.lr_omega_1[t] <=  var.tose_s_sd_p[t]   )
@constraint(model, lr_9_9[ t in 1:TOTAL_PERIODS], par.LR_params_c["L_S_SD","PEN"][t+1] + big_M*(1-var.lr_omega_1[t]) >=  var.tose_s_sd_p[t]  )

