### These are the lavaan models

lavaan_model ="
            ### Measurement Model
              hardl          =~ violent + burn
              
              softl          =~ recount + criticize + court
              
              participation =~ participation_yard + participation_volunteer + 
                               participation_protest + participation_contact  +  participation_donate
              
              concern        =~ concern_covid + concern_lines + concern_intimidate + 
                                concern_accept + concern_delay + concern_illegal
            
              ### Structural Model  

              
              hardl          ~ trump_vote + post1 + post2 + TT1 + TT2 
              softl          ~ trump_vote + post1 + post2 + TT1 + TT2 
              participation ~ trump_vote + post1 + post2 + TT1 + TT2 
              concern       ~ trump_vote + post1 + post2 + TT1 + TT2 
              trust_federal        ~ trump_vote + post1 + post2 + TT1 + TT2 
              trust_state       ~ trump_vote + post1 + post2 + TT1 + TT2 
              confidence_ballot ~ trump_vote + post1 + post2 + TT1 + TT2 
              efficacy_complicated ~ trump_vote + post1 + post2 + TT1 + TT2 
              efficacy_dontcare ~ trump_vote + post1 + post2 + TT1 + TT2 
"

lavaan_model_causal ="
            ### Measurement Modeld
              hardl          =~ violent + burn
              
              softl          =~ recount + criticize + court
              
              concern        =~ concern_covid + concern_lines + concern_intimidate + 
                                concern_accept + concern_delay + concern_illegal
            
              ### Structural Model  

              
              hardl          ~ trump_vote + post1 + post2 + TT1 + TT2 
              softl          ~ trump_vote + post1 + post2 + TT1 + TT2 
              concern       ~  trump_vote + post1 + post2 + TT1 + TT2 
              trust_federal        ~ trump_vote + post1 + post2 + TT1 + TT2 
              trust_state       ~ trump_vote + post1 + post2 + TT1 + TT2 
              confidence_ballot ~ trump_vote + post1 + post2 + TT1 + TT2 
"


### This is useful for simpler models, eg, the state analysis.
### However, even here, I had difficulties and ended up just reporting
### OLS results


lavaan_model_state2 ="
### Measurement Modeld
hard          =~ 1*violent + 1*burn

soft          =~ recount + criticize + court

### Structural Model  

hard          ~ trump_vote + post1 + post2 + TT1 + TT2
soft          ~ trump_vote + post1 + post2 + TT1 + TT2
"

### These are for the different factor models in factor_models.R

model1a <-  " hard =~ violent + burn
              
              soft =~ recount + criticize + court
              
              participation =~ participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              
              
              trust1 =~ trust_congress + trust_president + trust_sc
              trust2 =~ trust_governor + trust_stateleg 
              
              concern =~ concern_covid + concern_lines + concern_intimidate + 
              concern_accept + concern_delay + concern_illegal
              

"

model1b <-  "   
              soft =~ recount + criticize + court + violent + burn
              
              participation =~ participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              
              
              trust1 =~ trust_congress + trust_president + trust_sc
              trust2 =~ trust_governor + trust_stateleg 
              
              concern =~ concern_covid + concern_lines + concern_intimidate + 
              concern_accept + concern_delay + concern_illegal
              

"

model1c <-  "   

              participation =~ participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate + 
              recount + criticize + court + violent + burn
              
              
              trust1 =~ trust_congress + trust_president + trust_sc
              trust2 =~ trust_governor + trust_stateleg 
              
              concern =~ concern_covid + concern_lines + concern_intimidate + 
              concern_accept + concern_delay + concern_illegal
              

"

model1d <-  "   

              participation =~ participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate  

              
              trust1 =~ trust_congress + trust_president + trust_sc + 
              recount + criticize + court + violent + burn
              trust2 =~ trust_governor + trust_stateleg  
              
              
              concern =~ concern_covid + concern_lines + concern_intimidate + 
              concern_accept + concern_delay + concern_illegal
              

"


model1e <-  "   

              participation =~ participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate  

              
              trust1 =~ trust_congress + trust_president + trust_sc
              trust2 =~ trust_governor + trust_stateleg  + 
              recount + criticize + court + violent + burn
              
              concern =~ concern_covid + concern_lines + concern_intimidate + 
              concern_accept + concern_delay + concern_illegal
              

"

model1f <-  "   

              participation =~ participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate  

              
              trust1 =~ trust_congress + trust_president + trust_sc
              trust2 =~ trust_governor + trust_stateleg  
              
              
              concern =~ concern_covid + concern_lines + concern_intimidate + 
              concern_accept + concern_delay + concern_illegal + 
              recount + criticize + court + violent + burn
              

"


