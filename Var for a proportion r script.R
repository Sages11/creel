#---
#  title: "SEAK Marine Creeel"
#author: "Sarah Power"
#date: "December 4, 2017"
#output: html_document
#---
  
  
#  Import data setS and filter to juneau biweek 10, selecting apropriate variables to check for the proportion of pelagic, for testing.


library(dplyr)
library(tidyr)

jmse = read.csv("S:\\Creel\\2099\\2015 SAS files\\PSF\\E_jnu_2015_mc_mse_variable.csv", header = TRUE)
jmss = read.csv("S:\\Creel\\2099\\2015 SAS files\\PSF\\E_jnu_2015_mc_mss_variable.csv", header = TRUE)
jmsi = read.csv("S:\\Creel\\2099\\2015 SAS files\\PSF\\E_jnu_2015_mc_msis_wBhat_ij.csv", header = TRUE)
str(jmss)

unique(jmse$BIWEEK)

biweek_selection <- 10
jmss <- jmss %>% filter(BIWEEK == biweek_selection)
jmse <- jmse %>% filter(BIWEEK == biweek_selection)
jmsi <- jmsi %>% filter(BIWEEK == biweek_selection)

head(jmse)
names(jmsi)
jmsi <- jmsi %>%
  select(DATE, HARBOR, N_MISSED, #NUMBER OF MISSED BOATS
         N_NOT_SF, # NUMBER OF BOATS NOT SPORT FISHING. 
         INTVNUMB, #INTERVIEW NUMBER
         CLASS,  # PRIVATE OR CHARTER  
         BIWEEK, # TWO WEEKS
         #RODHOURS, SALHOURS, HALHOURS, ROCHOURS,
         #estimated number of boats: P= private, c = Charter, U = unknown, SF, sportfishing = P+C
         #lz_ij, lz_Pij, lz_Cij, lz_Uij, #lz = little z = number of boats interviewed on that day and harbor, P = private, C = charter, U = unknown.
         lb_ij,
         lb_Pij,
         lb_Cij,
         lb_Uij,
         lbhat_Pij,
         lbhat_Cij,
         la_ij, # Num. boats SF.and.not.SF, 
         BA_ij, #All.boats, 
         BBhat_ij, #Est.num.SF.boats, 
         KRF, #kept rockfish
         KRFS, # kept rockfish sampled (for biological data)
         KROCN, 
         KROCS, #kEPT UNKNOWN ROCKFISH
         KPLGV, KPLGS, # v = verified, s = sampled
         KPLG   #kept pelagic rockfish
  ) %>%
  mutate(KKRF = KRF- KROCS, 
         n_mhijk = KKRF, 
         N_mhijk = KRF, 
         y_hijk = KPLG/n_mhijk, 
         prop_p = y_hijk, 
         f_4hijk = n_mhijk/N_mhijk)


#Calculate:
#  The number of Kept rockfish that are identified: KKRF (Kept, Known, RockFish)
#  The proportion of known rockfish that are pelagic: pel_p
#  The sample variance: n*/(n-1)*p(1-p)
#  The finite populationcorrection factor: (N-n)/N: fpc

prop_p_samp_var <- function(N , n, p){
  if (n == 1) {
    return(NA) #Cannot calculate variance of one item. 
  } else {
    samp_var = n/(n-1)*p*(1-p)
    return(samp_var)
  }
}

b_hij_calc <- function(b_p, b_c, class_pc){
  if(class_pc == 1){
    b_hij <- b_p
  }else if (class_pc == 2){
    b_hij <- b_c
  }else{
    stop("No class defined.")
  }
}
boat <- jmsi %>%
  group_by(DATE, HARBOR, INTVNUMB) %>%
  filter(N_mhijk > 0) %>%
  mutate(s_4hijk = prop_p_samp_var(N_mhijk , n_mhijk, prop_p), 
         b_hij = b_hij_calc(lb_Pij, lb_Cij, CLASS))
    
    #s_4_hijk = prop_p_samp_var(N_mhijk , n_mhijk, prop_p)) 

#Note: in biweek 10 there are 8 instances of RF caught, 
#and there is either a complete census of the RF or there are 0 Pelagic, 
#so variance is always 0 within boats

dock <- boat %>%
  group_by(DATE, HARBOR) %>%
  mutate(Nbar_mhij = mean(N_mhijk)) # equation 4 in 2015-2017 op plan

#s_3hij = var(prop_p), 
boat <- merge(boat, dock)

boat <- boat %>%
  mutate(w_4hijk = N_mhijk/Nbar_mhij, ybar_whijk = w_4hijk*y_hijk)

dock <- boat %>%
  group_by(DATE, HARBOR) %>%
  mutate(s_3hij = var(ybar_whijk), b_double_prime_mhij = count(s_4hijk))
  
