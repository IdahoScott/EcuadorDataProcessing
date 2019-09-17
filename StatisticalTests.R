#ChlA Stats

urban_chlA <- c(162.8632, 158.0432, 141.8584) #significantly different from rural with pval = 0.00131
suburban_chlA <- c(12.436, 8.7432, 22.1008) #significantly different from urban, with pval of 0.0001639
rural_chlA <- c(7.012, 3.8672, 3.0888) #NOt significantly different from suburban with pval of 0.129

t.test(urban_chlA, suburban_chlA)
