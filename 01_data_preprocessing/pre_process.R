projroot="your_path_here/final_data"

########################################################################
######################## BEGIN FUNCTIONS ###############################
########################################################################

##########DATA PRE-PROCESSING FUNCTION ####################

##########END DATA SPLITTING FUNCTION ####################
########################################################################
######################## END FUNCTIONS ###############################
########################################################################

file1="all_data.csv"



DAT.save=read.csv(file.path(projroot, file1))
cn=c("recordId","note_id","model", "diagnosis", "GPT.o3.mini_binary","response_length","gt",
     "model_family","model_generation", "model_size" ,"multimodal", "open_source",
     "date_of_release","note_length" )
# note_id is the per-patient/per-note identifier; recordId is per (note, diagnosis) pair and is
# not a valid unit for note-level bootstrap resampling, but is kept as a join key.
library(dplyr)
DAT=DAT.save %>% select(cn)

# Pinned rather than Sys.Date(), so duration_since_release doesn't drift when this script is rerun.
reference_date <- as.Date("2025-06-06")
DAT$duration_since_release <- as.numeric(reference_date - as.Date(DAT$date_of_release))

save(DAT,file=file.path(projroot,"DAT.RData"))

