library(doParallel)

# The first method is based on minima of unadjusted p-values
minima_of_unadjusted_p_values = function (FUN ,Bonferroni_critical_value,n,g,x){
  tuning_para = numeric(length(g))
  for (i in 1 : length(g)){
    tuning_para[i] = FUN(n,g[i],x) - Bonferroni_critical_value[i]
  }
   #erg = c(g[which.max(tuning_para)], which.max(tuning_para))
  return(g[which.max(tuning_para)]) # tuning para meter and the position in the set
}
# Henze Visagie Bonferroni test 
HV_bonferroni_test <- function(n,x){
  Hv_bonferroni_cri_10_20_50 = matrix(c(160.862, 147.659, 129.298, 116.101, 103.774, 95.361,
                                     479.722, 374.011, 261.226, 221.368, 172.910, 156.583,
                                     1027.325, 712.183,421.370, 323.026, 235.081, 192.133), 
                                   byrow = T , nrow = 3, ncol = 6)
  g_hv = c(2.5, 3, 4, 5, 7, 10)
  if(n ==10){ Hv_bonferroni_cri = Hv_bonferroni_cri_10_20_50[1,]}
  if(n ==20){ Hv_bonferroni_cri = Hv_bonferroni_cri_10_20_50[2,]}
  if(n ==50){ Hv_bonferroni_cri = Hv_bonferroni_cri_10_20_50[3,]}
  tunning_para= minima_of_unadjusted_p_values(Henz_Visagie_test, Hv_bonferroni_cri, n, g_hv, x)
  return(Henz_Visagie_test(n, tunning_para, x))
}

# Zghoul Bonferroni test
Z_bonferroni_test <- function(n,x){
  Z_bonferroni_cri_10_20_50 = matrix(c(5881.684,1170.158,156.544,16.197, 1.571,0.226, 
                                        27734.385, 3613.259, 347.660, 30.118, 2.510, 0.332, 
                                        101681.229, 9047.307, 587.411, 40.690, 3.063, 0.409), 
                                      byrow = T , nrow = 3, ncol = 6)
  g_Z = c(1.5, 2, 3, 5, 9, 15)
  if(n ==10){ Z_bonferroni_cri = Z_bonferroni_cri_10_20_50[1,]}
  if(n ==20){ Z_bonferroni_cri = Z_bonferroni_cri_10_20_50[2,]}
  if(n ==50){ Z_bonferroni_cri = Z_bonferroni_cri_10_20_50[3,]}
  tunning_para= minima_of_unadjusted_p_values(Zghoul_test, Z_bonferroni_cri, n, g_Z, x)
  return(Zghoul_test(n, tunning_para, x))
}















