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

str(jmss)

unique(jmse$BIWEEK)

jmss <- jmss %>%
  filter(BIWEEK == 10)

jmse <- jmse %>%
  select(DATE, HARBOR, N_MISSED, #NUMBER OF MISSED BOATS
         N_NOT_SF, # NUMBER OF BOATS NOT SPORT FISHING. 
         INTVNUMB, #INTERVIEW NUMBER
         CLASS,  # PRIVATE OR CHARTER  
         BIWEEK, # TWO WEEKS
         #RODHOURS, SALHOURS, HALHOURS, ROCHOURS,
         #estimated number of boats: P= private, c = Charter, U = unknown, SF, sportfishing = P+C
         lz_ij, lz_Pij, lz_Cij, lz_Uij, #lz = little z = number of boats interviewed on that day and harbor, P = private, C = charter, U = unknown.
         la_ij, # Num. boats SF.and.not.SF, 
         BA_ij, #All.boats, 
         BBhat_ij, #Est.num.SF.boats, 
         N, # What is N? 
         KRFV, #kept rockfish
         KRFS, # kept rockfish sampled (for biological data)
         KROCN, 
         KROCS, #kEPT UNKNOWN ROCKFISH
         KPLGV, KPLGS, # v = verified, s = sampled
         KPLG   #kept pelagic rockfish
         
  ) %>%
  filter(BIWEEK == 10) #BW 10 few enough to calculate BW13 most RF


#Calculate:
#  The number of Kept rockfish that are identified: KKRF (Kept, Known, RockFish)
#  The proportion of known rockfish that are pelagic: pel_p
#  The sample variance: n*/(n-1)*p(1-p)
#  The finite populationcorrection factor: (N-n)/N: fpc

pel_p_samp_var <- function(KRFV, KKRF, pel_p){
  if (KKRF == 1) {
    return(0)
  } else {
    fpc = (KRFV - KKRF)/KRFV
    samp_var = fpc * KKRF/(KKRF-1)*pel_p*(1-pel_p)
    return(samp_var)
  }
}


jmse <- jmse %>%
  group_by(DATE, HARBOR, INTVNUMB) %>%
  filter(KRFV > 0) %>%
  mutate(KKRF = KRFV - KROCS, pel_p = KPLG/KKRF, s_4_hijk = pel_p_samp_var(KRFV, KKRF, pel_p)) 

#Note: in biweek 10 there are 8 instances of RF caught, 
#and there is either a complete census of the RF or there are 0 Pelagic, 
#so variance is always 0 with in boats

jmse <- jmse %>%
  group_by(DATE, HARBOR)
  mutate(s_3_hij = var(pel_p))

