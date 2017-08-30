mtx_step_pro <- matrix(nrow = 104,ncol = 2)
colnames(mtx_step_pro) <- c("step_trend","procr_trend")
Procra_value_all <- Transaction_InnerJoin$Procra_value
for (i in (1:length(Transaction_InnerJoin$Procra_value)))
{
  mtx_step_pro[i,"procr_trend"] <- as.integer(Procra_value_all[i+1])  - as.integer(Procra_value_all[i]) 
}

step_all <- Transaction_InnerJoin$step_mean
for (i in (1:length(Transaction_InnerJoin$step_mean)))
{
  mtx_step_pro[i,"step_trend"] <- as.integer(step_all[i+1])  - as.integer(step_all[i]) 
}

p_up_s_up = 0
p_up_s_down = 0
p_up_s_same = 0
p_down_s_up = 0
p_down_s_down = 0
p_down_s_same = 0
p_same_s_up = 0
p_same_s_down = 0
p_same_s_same = 0
p_s_na = 0


for(i in (1:104))
{
  if(is.na(mtx_step_pro[i,"procr_trend"])||is.na(mtx_step_pro[i,"step_trend"]))
  {
    p_s_na = p_s_na + 1
    print(paste("p_s_na",p_s_na))
    next
  } 
  else
  {
    proca = as.integer(mtx_step_pro[i,"procr_trend"])
    step = as.integer(mtx_step_pro[i,"step_trend"])
    if( proca > 0 && step > 0 ) # 1. p_up_s_up 
    {
      p_up_s_up = p_up_s_up +1 
      print(paste("p_up_s_up",p_up_s_up))
    } 
    else if( proca > 0 && step < 0 ) # 2. p_up_s_down
    { 
      p_up_s_down  = p_up_s_down +1 
      print(paste("p_up_s_down",p_up_s_down))
    } 
    else if( proca > 0 && step == 0 ) # 3. p_up_s_same
    { 
      p_up_s_same = p_up_s_same +1 
      print(paste("p_up_s_same",p_up_s_same))
    } 
    else if ( proca < 0 && step > 0 ) # 4. p_down_s_up
    { 
      p_down_s_up = p_down_s_up +1 
      print(paste("p_down_s_up",p_down_s_up))
    } 
    else if ( proca < 0 && step < 0 ) # 5. p_down_s_down
    { 
      p_down_s_down = p_down_s_down +1 
      print(paste("p_down_s_down",p_down_s_down))
    } 
    else if ( proca < 0 && step == 0 ) # 6. p_down_s_same
    { 
      p_down_s_same = p_down_s_same +1 
      print(paste("p_down_s_same",p_down_s_same))
    } 
    else if ( proca == 0 && step > 0 ) # 7. p_same_s_up
    { 
      p_same_s_up = p_same_s_up +1 
      print(paste("p_same_s_up",p_same_s_up))
    } 
    else if ( proca == 0 && step < 0 ) # 8. p_same_s_down
    { 
      p_same_s_down = p_same_s_down +1 
      print(paste("p_same_s_down",p_same_s_down))
    }
    else if ( proca == 0 && step == 0 ) # 9. p_same_s_same
    { 
      p_same_s_same = p_same_s_same +1 
      print(paste("p_same_s_same",p_same_s_same))

    }
    else 
    {
      print(paste(proca,step))
    }
  }
}

