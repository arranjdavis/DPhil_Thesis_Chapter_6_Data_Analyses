################################################################################################################################################

### NOTES ###

#lines 13 through 662 create the data set used in the final analyses; this included cleaning and merging data from the MATLAB script and Qualtrics survey
#the analyses desrcribed in the main text begins on line 674 with the loading of "total_data.csv" (created in lines 13 through 662)

#participants #1-15 participated in pilots of the study (with varying experimental methods) and were thus excluded from analyses
#participants #27, 32, 55, and 85 were excluded due to malfunctions with experimental equipment
#this script further excludes participants #17, 19, 22, 34, 47, 65, 74, and 79 for not following the experimental instructions, having incomplete data, or guessing the experimental hypothesis

#working directory
setwd("/Users/arrandavis/Desktop/Study 3 Results")

#clean environment
rm(list = ls())

################################################################################################################################################

### THIS WILL CREATE A DATA FRAME OF THE VOLTAGE DATA (THE MEASURE OR PARTICIPANTS' PERFORMANCE ON THE EXERCISE TASK) ###

library(R.matlab)

#folder containing voltage data
datfolder = "/Users/arrandavis/Desktop/Study 3 Results/"

#this takes all the subfolders in the results folder 
subfolders = list.dirs(datfolder)
l = (1 + length(subfolders))
subfolders = subfolders[2:l]

#skip the last subfolder (NA)
subfolders = subfolders[1:(length(subfolders) - 1)]

#counter for the for loop below
count = 1

#list for all the numbers of voltage readings (the minumum per session)
totreadings = list()

#this will get the minimum number of voltage readings for a given trial in each experimental sessions
for (i in subfolders) {
  
  sub1 = subfolders[count]
  files1 = list.files(sub1)
  
  #this gets the MATLAB file with the voltage data
  D = readMat(paste(sub1, files1[1], sep = '/'))
  
  #this gets the voltage data (its a list of 42, since there are 42 trials)
  voltage = D$record.V
  
  #this for loop will get the number of voltage readings for each trial
  
  #this creates a list to see the minimum amount of voltage readings in the sessions (so the smallest number for the 42 trials)
  sess_readings = list()
  
  #counter for the for loop below
  count2 = 1
  
  #this will go through all the trials and get the number of readings
  for (i in voltage) {
    
    cur_trial = voltage[count2]
    
    trial_readings = ncol(as.data.frame(cur_trial))
    
    sess_readings = append(sess_readings, trial_readings)
    
    count2 = count2 +1
  }
  
  #this gets the minimum amount of voltage readings in sess_readings (the number of readings for the trial with the smallest amount of readings)
  #for the current session for the current participant
  
  #flips 'sess_readings' so that its a matrix with one column and 42 rows
  sess_readings = t(sess_readings)
  
  #makes it a data frame
  p = as.data.frame(sess_readings)
  
  #flips it so that it has only one column
  p = t(p)
  
  #make data frame again
  p = as.data.frame(p)
  
  #convert from a list to numeric and get the minimum value (for the variable 'V1,' which is the only column)
  minimum_trial_readings = min(as.numeric(p$V1))
  
  #now add the minumum for this session to the list of all the minimum readings for all the sessions
  totreadings = append(totreadings, minimum_trial_readings)
  
  count = count + 1
}

#this gets the minimum reading from all trials from all the sessions of all the participants
q = as.data.frame(totreadings)
q = t(q)
q = as.data.frame(q)
grand_minimum = min(as.numeric(q$V1)) #160 is 20 readings per second (should provide sufficient data)

#this resamples all trials (from every session of every participant) so that each trial has 160 voltage readings

#this creates an empty data frame to be filled with the resampled (160) voltage readings from each trial for each participant
total.data = data.frame()

#counter for the for loop below
count3 = 1

#this for loop will go through all the trials for each session for all the participants and resample the voltage readings
for (i in subfolders) {
  
  sub1 = subfolders[count3]
  files1 = list.files(sub1)
  
  #this gets the MATLAB file with the voltage data
  D = readMat(paste(sub1, files1[1], sep = '/'))
  
  #this gets the MATLAB file with the other data (condition, etc)
  E = readMat(paste(sub1, files1[2], sep = '/'))
  
  tempdat = as.data.frame(E$record.dat[,1:21])
  tempdat$participant = E$subDetails[1]
  tempdat$placebo = E$subDetails[5]
  
  #variable names
  headings = c('trial', #1
               'supp_cont', #2 - 1 equals the supportive face, 2 equals the control face
               'start_time', #3
               'NAN1', #4
               'Q1_start_time', #5
               'Q2_start_time', #6
               'blank1', #7
               'q1_response_time', #8
               'q2_response_time', #9
               'blank2', #10
               'NAN2', #11
               'Q1_start_pos', #12
               'Q2_start_pos', #13
               'blank3', #14
               'Q1_ans_pos', #15
               'Q2_ans_pos', #16
               'blank4', #17
               'Q1_ans_perc', #18
               'Q2_ans_perc', #19
               'blank5', #20
               'trial_difficulty', #21
               'participant', #22
               'placebo') #23
  
  colnames(x = tempdat) = headings
  
  #this gets the current participant's number
  participant_number = as.character(tempdat$participant[1:1])
  
  #this gets the current participant's placebo condition
  participant_placebo = as.character(tempdat$placebo[1:1])
  
  #this gets the current participant's experimental session number (first or second)
  session_number = as.numeric(D$subDetails[4])
  
  #this gets the current participant's maximum grip strength reading (during the grip strength test)
  max_grip = as.numeric(D$subDetails[6])
  
  #this gets the current participant's minimum grip reading (during the grip strength test)
  min_grip = as.numeric(D$subDetails[7])
  
  #this gets the current participant's grip strength
  grip_strength = (min_grip - max_grip)
  
  #this gets the voltage data (its a list of 42, since there are 42 trials)
  voltage = D$record.V
  
  #counter for the for loop below
  count4 = 1
  
  #this for loop will go through all the trials in the current session and resample the voltage readings so that there are 160
  #it will then add these 160 readings to a data set along with the participant number and condition
  #there are 42 lists in voltage
  for (i in voltage) {
    
    #this gets the trial difficulty of the current trial (i will range from 1 to 42 - the number of trials per session)
    trial_diff = as.numeric(tempdat$trial_difficulty[count4])
    
    #this gets the face (support or control) for the current trial (i will range from 1 to 42 - the number of trials per session)
    support_control = as.numeric(tempdat$supp_cont[count4])
    
    #the below will resample the current trial and add it to the total data frame
    
    #this gets the voltage data for the current trial
    current_dat = as.data.frame(voltage[count4])
    
    #this makes the data frame one variable
    current_dat = t(current_dat)
    
    #this renames the row names from "X1" to "1"
    row.names(current_dat) = sub("X","", row.names(current_dat))
    
    #this samples 160 voltage readings (they will not be in order in 'sampled_volt')
    sample_volt = as.data.frame(current_dat[sample(nrow(current_dat), 160), ])
    
    #this changes the name of the column
    colnames(sample_volt) = 'voltage'
    
    #this creates a variable that is the same as the row names, and then sorts by this variable (same as sorting by row)
    sample_volt$reading_number = as.numeric(row.names(sample_volt))
    ordered_sample_volt = sample_volt[order(sample_volt$reading_number), ]
    
    #this creates a variable that is 1 - 160 ('reading_number' will vary from 1 - 240)
    x = row(ordered_sample_volt)
    ordered_sample_volt["sample_reading_number"] = x
    
    #now add all the other variables for the particular trial
    
    #this adds the trial difficulty
    ordered_sample_volt["trial_difficulty"] = trial_diff
    
    #this adds if the trial was with a support or control face
    ordered_sample_volt["support_or_control"] = support_control
    
    #this adds whether the trial was in the placebo or control condition
    ordered_sample_volt["placebo_condition"] = participant_placebo
    
    #this adds the experimental session number of the trial (first or second)
    ordered_sample_volt["session_number"] = session_number
    
    #this adds the trial number
    ordered_sample_volt["trial_number"] = count4
    
    #this adds the maximum grip 
    ordered_sample_volt["max_grip"] = max_grip
    
    #this adds the minimum grip
    ordered_sample_volt["min_grip"] = min_grip
    
    #this adds the grip strength
    ordered_sample_volt["grip_strength"] = grip_strength
    
    #this adds the participant number (making it the first column)
    ordered_sample_volt = cbind(participant = participant_number, ordered_sample_volt)
    
    #add to counter
    count4 = count4 + 1
    
    #this adds the sample of voltage readings from the current trial to the data set with all samples from all the trials
    total.data = rbind(total.data, ordered_sample_volt)
    
    #clears the data from the last trial
    rm(current_dat)
  }
  
  #adds one to this counter, which is for going through all the sessions for each participant
  count3 = count3 + 1
}

#some trials are labled 'OFF' instead of 'off' for 'placebo_condition' - this fixes the labeling
total.data$placebo_condition <- ifelse(total.data$placebo_condition == "OFF", "off", ifelse(total.data$placebo_condition == "on", "on", ifelse(total.data$placebo_condition == "off", "off", NA)))

#'total.data$sample_reading_number' is a matrix with two columns, we just need the first column
total.data$sample_reading_number = total.data$sample_reading_number[,1]

################################################################################################################################################

### FIND PARTICIPANTS WITH STRANGE DATA, REMOVE UNUSABLE DATA ###

#the range of the voltage data is "-1193 to 750"; find participants who have a large range
participants = unique(total.data$participant)

#this for loop will find the participant with the strange data
for (i in participants) {
  
  dat.sub = subset(total.data, participant == i)
  
  voltage.range = range(dat.sub$voltage)
  
  
  if (min(voltage.range) < 400){
    print(i)
    print(voltage.range)
  }  
}

#the strange row number in 'total.dat' are in the from (121, 1221, 123, 1241, 1252, etc.; if there is already a '125' it adds a number at the end)

#only 'sub_34' has a minimum reading less than 400; so look at a subset of the data with only this 
weird.dat = subset(total.data, participant == "sub_34")

trials = unique(weird.dat$trial_number)

#this for loop will find the trials with the strange data
for (i in trials) {
  
  weird.sub = subset(weird.dat, trial_number == i)
  
  w_volt = weird.sub$voltage
  
  
  if (min(w_volt) < 0){
    print(i)
    print(w_volt)
  }  
}

#'sub_34' also said that he was not trying, so remove his data from the data set
total.data = subset(total.data, participant != "sub_34")

### THIS WILL CONVERT THE VOLTAGE DATA TO PERCENTAGES OF PARTICIPANTS' MAXIMUM GRIP ###

#'bar_unit' is a variable in the MATLAB script (lines 191-193) that is involved in the equations that convert voltage data to pixels on the screen
total.data$bar_unit_easy = (300 * (50/40)) / (total.data$min_grip - total.data$max_grip)
total.data$bar_unit_medium = (300 / (total.data$min_grip - total.data$max_grip))
total.data$bar_unit_hard= (300 * (50/60)) / (total.data$min_grip - total.data$max_grip)

#'Grip_px' is a variable in the MATLAB script (line 303) that is involved in the equations that convert voltage data to pixels on the screen
total.data$grip_px = 750 - total.data$voltage

#'Grip_dV' is a variable in the MATLAB script (line 320) that is involved in the equations that convert voltage data to pixels on the screen
total.data$trial_difficulty = as.numeric(total.data$trial_difficulty)
total.data$grip_dv = ifelse(total.data$trial_difficulty == 1, (total.data$grip_px / total.data$bar_unit_easy), ifelse(total.data$trial_difficulty == 2, (total.data$grip_px / total.data$bar_unit_medium), ifelse(total.data$trial_difficulty == 3, (total.data$grip_px / total.data$bar_unit_hard), NA)))

#this will give the actual voltage reading (instead of the pixel at which the bar is displayed)
total.data$real_voltage = -1 * (total.data$grip_dv - total.data$min_grip)

#this will give the percentage of the participant's maximum of the current reading (the percent of their total grip strength they are currently performing at)
total.data$percent_of_maximum = (total.data$min_grip - total.data$real_voltage) / (total.data$min_grip - total.data$max_grip)

################################################################################################################################################

### THE NEXT STEP IS TO MERGE 'TOTAL.DATA' WITH THE QUALTRICS DATA (THE PRE AND POST-EXPERIMENT QUESTIONS THAT THE PARTICIPANTS ANSWERED) ###

#this is the location of the Qualtrics data
setwd("/Users/arrandavis/Desktop/R")

#loads the Qualtrics data
survey_dat = read.csv("Study 3 Qualtrics Data - pre-R.csv", sep = ",")

### ### ###

#creates score for Big-Five Inventory (BFI) Personality Test (first need to convert answers to numeric)

#recode all answers to the BFI so that they are numeric (note that some questions are reversed scored)
survey_dat$talkative <- ifelse(survey_dat$talkative == "Disagree strongly", 1, ifelse(survey_dat$talkative == "Disagree a little", 2, ifelse(survey_dat$talkative == "Neither agree nor disagree", 3, ifelse(survey_dat$talkative == "Agree a little", 4, ifelse(survey_dat$talkative == "Strongly agree", 5, NA)))))
survey_dat$fault_with_others <- ifelse(survey_dat$fault_with_others == "Disagree strongly", 5, ifelse(survey_dat$fault_with_others == "Disagree a little", 4, ifelse(survey_dat$fault_with_others == "Neither agree nor disagree", 3, ifelse(survey_dat$fault_with_others == "Agree a little", 2, ifelse(survey_dat$fault_with_others == "Strongly agree", 1, NA)))))
survey_dat$thorough_job <- ifelse(survey_dat$thorough_job == "Disagree strongly", 1, ifelse(survey_dat$thorough_job == "Disagree a little", 2, ifelse(survey_dat$thorough_job == "Neither agree nor disagree", 3, ifelse(survey_dat$thorough_job == "Agree a little", 4, ifelse(survey_dat$thorough_job == "Strongly agree", 5, NA)))))
survey_dat$depressed <- ifelse(survey_dat$depressed == "Disagree strongly", 1, ifelse(survey_dat$depressed == "Disagree a little", 2, ifelse(survey_dat$depressed == "Neither agree nor disagree", 3, ifelse(survey_dat$depressed == "Agree a little", 4, ifelse(survey_dat$depressed == "Strongly agree", 5, NA)))))
survey_dat$original <- ifelse(survey_dat$original == "Disagree strongly", 1, ifelse(survey_dat$original == "Disagree a little", 2, ifelse(survey_dat$original == "Neither agree nor disagree", 3, ifelse(survey_dat$original == "Agree a little", 4, ifelse(survey_dat$original == "Strongly agree", 5, NA)))))
survey_dat$reserved <- ifelse(survey_dat$reserved == "Disagree strongly", 5, ifelse(survey_dat$reserved == "Disagree a little", 4, ifelse(survey_dat$reserved == "Neither agree nor disagree", 3, ifelse(survey_dat$reserved == "Agree a little", 2, ifelse(survey_dat$reserved == "Strongly agree", 1, NA)))))
survey_dat$helpful <- ifelse(survey_dat$helpful == "Disagree strongly", 1, ifelse(survey_dat$helpful == "Disagree a little", 2, ifelse(survey_dat$helpful == "Neither agree nor disagree", 3, ifelse(survey_dat$helpful == "Agree a little", 4, ifelse(survey_dat$helpful == "Strongly agree", 5, NA)))))
survey_dat$careless <- ifelse(survey_dat$careless == "Disagree strongly", 5, ifelse(survey_dat$careless == "Disagree a little", 4, ifelse(survey_dat$careless == "Neither agree nor disagree", 3, ifelse(survey_dat$careless == "Agree a little", 2, ifelse(survey_dat$careless == "Strongly agree", 1, NA)))))
survey_dat$relaxed <- ifelse(survey_dat$relaxed == "Disagree strongly", 5, ifelse(survey_dat$relaxed == "Disagree a little", 4, ifelse(survey_dat$relaxed == "Neither agree nor disagree", 3, ifelse(survey_dat$relaxed == "Agree a little", 2, ifelse(survey_dat$relaxed == "Strongly agree", 1, NA)))))
survey_dat$curious <- ifelse(survey_dat$curious == "Disagree strongly", 1, ifelse(survey_dat$curious == "Disagree a little", 2, ifelse(survey_dat$curious == "Neither agree nor disagree", 3, ifelse(survey_dat$curious == "Agree a little", 4, ifelse(survey_dat$curious == "Strongly agree", 5, NA)))))
survey_dat$energy <- ifelse(survey_dat$energy == "Disagree strongly", 1, ifelse(survey_dat$energy == "Disagree a little", 2, ifelse(survey_dat$energy == "Neither agree nor disagree", 3, ifelse(survey_dat$energy == "Agree a little", 4, ifelse(survey_dat$energy == "Strongly agree", 5, NA)))))
survey_dat$quarrels <- ifelse(survey_dat$quarrels == "Disagree strongly", 5, ifelse(survey_dat$quarrels == "Disagree a little", 4, ifelse(survey_dat$quarrels == "Neither agree nor disagree", 3, ifelse(survey_dat$quarrels == "Agree a little", 2, ifelse(survey_dat$quarrels == "Strongly agree", 1, NA)))))
survey_dat$reliable <- ifelse(survey_dat$reliable == "Disagree strongly", 1, ifelse(survey_dat$reliable == "Disagree a little", 2, ifelse(survey_dat$reliable == "Neither agree nor disagree", 3, ifelse(survey_dat$reliable == "Agree a little", 4, ifelse(survey_dat$reliable == "Strongly agree", 5, NA)))))
survey_dat$tense <- ifelse(survey_dat$tense == "Disagree strongly", 1, ifelse(survey_dat$tense == "Disagree a little", 2, ifelse(survey_dat$tense == "Neither agree nor disagree", 3, ifelse(survey_dat$tense == "Agree a little", 4, ifelse(survey_dat$tense == "Strongly agree", 5, NA)))))
survey_dat$ingenious <- ifelse(survey_dat$ingenious == "Disagree strongly", 1, ifelse(survey_dat$ingenious == "Disagree a little", 2, ifelse(survey_dat$ingenious == "Neither agree nor disagree", 3, ifelse(survey_dat$ingenious == "Agree a little", 4, ifelse(survey_dat$ingenious == "Strongly agree", 5, NA)))))
survey_dat$enthusiam <- ifelse(survey_dat$enthusiam == "Disagree strongly", 1, ifelse(survey_dat$enthusiam == "Disagree a little", 2, ifelse(survey_dat$enthusiam == "Neither agree nor disagree", 3, ifelse(survey_dat$enthusiam == "Agree a little", 4, ifelse(survey_dat$enthusiam == "Strongly agree", 5, NA)))))
survey_dat$forgiving <- ifelse(survey_dat$forgiving == "Disagree strongly", 1, ifelse(survey_dat$forgiving == "Disagree a little", 2, ifelse(survey_dat$forgiving == "Neither agree nor disagree", 3, ifelse(survey_dat$forgiving == "Agree a little", 4, ifelse(survey_dat$forgiving == "Strongly agree", 5, NA)))))
survey_dat$disorganised <- ifelse(survey_dat$disorganised == "Disagree strongly", 5, ifelse(survey_dat$disorganised == "Disagree a little", 4, ifelse(survey_dat$disorganised == "Neither agree nor disagree", 3, ifelse(survey_dat$disorganised == "Agree a little", 2, ifelse(survey_dat$disorganised == "Strongly agree", 1, NA)))))
survey_dat$worries <- ifelse(survey_dat$worries == "Disagree strongly", 1, ifelse(survey_dat$worries == "Disagree a little", 2, ifelse(survey_dat$worries == "Neither agree nor disagree", 3, ifelse(survey_dat$worries == "Agree a little", 4, ifelse(survey_dat$worries == "Strongly agree", 5, NA)))))
survey_dat$active_imagination <- ifelse(survey_dat$active_imagination == "Disagree strongly", 1, ifelse(survey_dat$active_imagination == "Disagree a little", 2, ifelse(survey_dat$active_imagination == "Neither agree nor disagree", 3, ifelse(survey_dat$active_imagination == "Agree a little", 4, ifelse(survey_dat$active_imagination == "Strongly agree", 5, NA)))))
survey_dat$quiet <- ifelse(survey_dat$quiet == "Disagree strongly", 5, ifelse(survey_dat$quiet == "Disagree a little", 4, ifelse(survey_dat$quiet == "Neither agree nor disagree", 3, ifelse(survey_dat$quiet == "Agree a little", 2, ifelse(survey_dat$quiet == "Strongly agree", 1, NA)))))
survey_dat$trusting <- ifelse(survey_dat$trusting == "Disagree strongly", 1, ifelse(survey_dat$trusting == "Disagree a little", 2, ifelse(survey_dat$trusting == "Neither agree nor disagree", 3, ifelse(survey_dat$trusting == "Agree a little", 4, ifelse(survey_dat$trusting == "Strongly agree", 5, NA)))))
survey_dat$lazy <- ifelse(survey_dat$lazy == "Disagree strongly", 5, ifelse(survey_dat$lazy == "Disagree a little", 4, ifelse(survey_dat$lazy == "Neither agree nor disagree", 3, ifelse(survey_dat$lazy == "Agree a little", 2, ifelse(survey_dat$lazy == "Strongly agree", 1, NA)))))
survey_dat$emotionally_stable <- ifelse(survey_dat$emotionally_stable == "Disagree strongly", 5, ifelse(survey_dat$emotionally_stable == "Disagree a little", 4, ifelse(survey_dat$emotionally_stable == "Neither agree nor disagree", 3, ifelse(survey_dat$emotionally_stable == "Agree a little", 2, ifelse(survey_dat$emotionally_stable == "Strongly agree", 1, NA)))))
survey_dat$inventive <- ifelse(survey_dat$inventive == "Disagree strongly", 1, ifelse(survey_dat$inventive == "Disagree a little", 2, ifelse(survey_dat$inventive == "Neither agree nor disagree", 3, ifelse(survey_dat$inventive == "Agree a little", 4, ifelse(survey_dat$inventive == "Strongly agree", 5, NA)))))
survey_dat$assertive <- ifelse(survey_dat$assertive == "Disagree strongly", 1, ifelse(survey_dat$assertive == "Disagree a little", 2, ifelse(survey_dat$assertive == "Neither agree nor disagree", 3, ifelse(survey_dat$assertive == "Agree a little", 4, ifelse(survey_dat$assertive == "Strongly agree", 5, NA)))))
survey_dat$cold <- ifelse(survey_dat$cold == "Disagree strongly", 5, ifelse(survey_dat$cold == "Disagree a little", 4, ifelse(survey_dat$cold == "Neither agree nor disagree", 3, ifelse(survey_dat$cold == "Agree a little", 2, ifelse(survey_dat$cold == "Strongly agree", 1, NA)))))
survey_dat$perserveres <- ifelse(survey_dat$perserveres == "Disagree strongly", 1, ifelse(survey_dat$perserveres == "Disagree a little", 2, ifelse(survey_dat$perserveres == "Neither agree nor disagree", 3, ifelse(survey_dat$perserveres == "Agree a little", 4, ifelse(survey_dat$perserveres == "Strongly agree", 5, NA)))))
survey_dat$moody <- ifelse(survey_dat$moody == "Disagree strongly", 1, ifelse(survey_dat$moody == "Disagree a little", 2, ifelse(survey_dat$moody == "Neither agree nor disagree", 3, ifelse(survey_dat$moody == "Agree a little", 4, ifelse(survey_dat$moody == "Strongly agree", 5, NA)))))
survey_dat$values_art <- ifelse(survey_dat$values_art == "Disagree strongly", 1, ifelse(survey_dat$values_art == "Disagree a little", 2, ifelse(survey_dat$values_art == "Neither agree nor disagree", 3, ifelse(survey_dat$values_art == "Agree a little", 4, ifelse(survey_dat$values_art == "Strongly agree", 5, NA)))))
survey_dat$shy <- ifelse(survey_dat$shy == "Disagree strongly", 5, ifelse(survey_dat$shy == "Disagree a little", 4, ifelse(survey_dat$shy == "Neither agree nor disagree", 3, ifelse(survey_dat$shy == "Agree a little", 2, ifelse(survey_dat$shy == "Strongly agree", 1, NA)))))
survey_dat$considerate <- ifelse(survey_dat$considerate == "Disagree strongly", 1, ifelse(survey_dat$considerate == "Disagree a little", 2, ifelse(survey_dat$considerate == "Neither agree nor disagree", 3, ifelse(survey_dat$considerate == "Agree a little", 4, ifelse(survey_dat$considerate == "Strongly agree", 5, NA)))))
survey_dat$efficient <- ifelse(survey_dat$efficient == "Disagree strongly", 1, ifelse(survey_dat$efficient == "Disagree a little", 2, ifelse(survey_dat$efficient == "Neither agree nor disagree", 3, ifelse(survey_dat$efficient == "Agree a little", 4, ifelse(survey_dat$efficient == "Strongly agree", 5, NA)))))
survey_dat$calm <- ifelse(survey_dat$calm == "Disagree strongly", 5, ifelse(survey_dat$calm == "Disagree a little", 4, ifelse(survey_dat$calm == "Neither agree nor disagree", 3, ifelse(survey_dat$calm == "Agree a little", 2, ifelse(survey_dat$calm == "Strongly agree", 1, NA)))))
survey_dat$prefers_routine_work <- ifelse(survey_dat$prefers_routine_work == "Disagree strongly", 5, ifelse(survey_dat$prefers_routine_work == "Disagree a little", 4, ifelse(survey_dat$prefers_routine_work == "Neither agree nor disagree", 3, ifelse(survey_dat$prefers_routine_work == "Agree a little", 2, ifelse(survey_dat$prefers_routine_work == "Strongly agree", 1, NA)))))
survey_dat$outgoing <- ifelse(survey_dat$outgoing == "Disagree strongly", 1, ifelse(survey_dat$outgoing == "Disagree a little", 2, ifelse(survey_dat$outgoing == "Neither agree nor disagree", 3, ifelse(survey_dat$outgoing == "Agree a little", 4, ifelse(survey_dat$outgoing == "Strongly agree", 5, NA)))))
survey_dat$rude <- ifelse(survey_dat$rude == "Disagree strongly", 5, ifelse(survey_dat$rude == "Disagree a little", 4, ifelse(survey_dat$rude == "Neither agree nor disagree", 3, ifelse(survey_dat$rude == "Agree a little", 2, ifelse(survey_dat$rude == "Strongly agree", 1, NA)))))
survey_dat$makes_plans <- ifelse(survey_dat$makes_plans == "Disagree strongly", 1, ifelse(survey_dat$makes_plans == "Disagree a little", 2, ifelse(survey_dat$makes_plans == "Neither agree nor disagree", 3, ifelse(survey_dat$makes_plans == "Agree a little", 4, ifelse(survey_dat$makes_plans == "Strongly agree", 5, NA)))))
survey_dat$get_nervous <- ifelse(survey_dat$get_nervous == "Disagree strongly", 1, ifelse(survey_dat$get_nervous == "Disagree a little", 2, ifelse(survey_dat$get_nervous == "Neither agree nor disagree", 3, ifelse(survey_dat$get_nervous == "Agree a little", 4, ifelse(survey_dat$get_nervous == "Strongly agree", 5, NA)))))
survey_dat$likes_to_reflect <- ifelse(survey_dat$likes_to_reflect == "Disagree strongly", 1, ifelse(survey_dat$likes_to_reflect == "Disagree a little", 2, ifelse(survey_dat$likes_to_reflect == "Neither agree nor disagree", 3, ifelse(survey_dat$likes_to_reflect == "Agree a little", 4, ifelse(survey_dat$likes_to_reflect == "Strongly agree", 5, NA)))))
survey_dat$few_artistic_interests <- ifelse(survey_dat$few_artistic_interests == "Disagree strongly", 5, ifelse(survey_dat$few_artistic_interests == "Disagree a little", 4, ifelse(survey_dat$few_artistic_interests == "Neither agree nor disagree", 3, ifelse(survey_dat$few_artistic_interests == "Agree a little", 2, ifelse(survey_dat$few_artistic_interests == "Strongly agree", 1, NA)))))
survey_dat$cooperative <- ifelse(survey_dat$cooperative == "Disagree strongly", 1, ifelse(survey_dat$cooperative == "Disagree a little", 2, ifelse(survey_dat$cooperative == "Neither agree nor disagree", 3, ifelse(survey_dat$cooperative == "Agree a little", 4, ifelse(survey_dat$cooperative == "Strongly agree", 5, NA)))))
survey_dat$distracted <- ifelse(survey_dat$distracted == "Disagree strongly", 5, ifelse(survey_dat$distracted == "Disagree a little", 4, ifelse(survey_dat$distracted == "Neither agree nor disagree", 3, ifelse(survey_dat$distracted == "Agree a little", 2, ifelse(survey_dat$distracted == "Strongly agree", 1, NA)))))
survey_dat$sophisticated <- ifelse(survey_dat$sophisticated == "Disagree strongly", 1, ifelse(survey_dat$sophisticated == "Disagree a little", 2, ifelse(survey_dat$sophisticated == "Neither agree nor disagree", 3, ifelse(survey_dat$sophisticated == "Agree a little", 4, ifelse(survey_dat$sophisticated == "Strongly agree", 5, NA)))))

#this will create the participants' extraversion score
survey_dat$extraversion = (survey_dat$talkative + survey_dat$reserved + survey_dat$energy + survey_dat$enthusiam + survey_dat$quiet + survey_dat$assertive + survey_dat$shy + survey_dat$outgoing)/8

#this will create the participants' agreeableness score
survey_dat$agreeableness = (survey_dat$fault_with_others + survey_dat$helpful + survey_dat$quarrels + survey_dat$forgiving + survey_dat$trusting + survey_dat$cold + survey_dat$considerate + survey_dat$rude + survey_dat$cooperative)/9

#this will create the participants' conscientiousness score
survey_dat$conscientiousness = (survey_dat$thorough_job + survey_dat$careless + survey_dat$reliable + survey_dat$disorganised + survey_dat$lazy + survey_dat$perserveres + survey_dat$efficient + survey_dat$makes_plans + survey_dat$distracted)/9

#this will create the participants' neuroticism score
survey_dat$neuroticism = (survey_dat$depressed + survey_dat$relaxed + survey_dat$tense + survey_dat$worries + survey_dat$emotionally_stable + survey_dat$moody + survey_dat$calm + survey_dat$get_nervous)/8

#this will create the participants' openness score
survey_dat$openness = (survey_dat$original + survey_dat$curious + survey_dat$ingenious + survey_dat$active_imagination + survey_dat$inventive + survey_dat$values_art + survey_dat$prefers_routine_work + survey_dat$likes_to_reflect + survey_dat$few_artistic_interests + survey_dat$sophisticated)/10

### ### ###

#creates score for Revised Adult Attachment Scale (RAAS; first need to convert answers to numeric)

#recode all answers to the RAAS so that they are numeric (note that some questions are reversed scored)
survey_dat$easy_to_get_close <- ifelse(survey_dat$easy_to_get_close == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$easy_to_get_close == "-2", 2, ifelse(survey_dat$easy_to_get_close == "-3", 3, ifelse(survey_dat$easy_to_get_close == "-4", 4, ifelse(survey_dat$easy_to_get_close == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$difficult_depending_on_others <- ifelse(survey_dat$difficult_depending_on_others == "Not at all characteristic of me\n(1)\n", 5, ifelse(survey_dat$difficult_depending_on_others == "-2", 4, ifelse(survey_dat$difficult_depending_on_others == "-3", 3, ifelse(survey_dat$difficult_depending_on_others == "-4", 2, ifelse(survey_dat$difficult_depending_on_others == "Very characteristic of me\n(5)\n", 1, NA)))))
survey_dat$worry_people_dont_love_me <- ifelse(survey_dat$worry_people_dont_love_me == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$worry_people_dont_love_me == "-2", 2, ifelse(survey_dat$worry_people_dont_love_me == "-3", 3, ifelse(survey_dat$worry_people_dont_love_me == "-4", 4, ifelse(survey_dat$worry_people_dont_love_me == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$others_reluctant_to_get_close <- ifelse(survey_dat$others_reluctant_to_get_close == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$others_reluctant_to_get_close == "-2", 2, ifelse(survey_dat$others_reluctant_to_get_close == "-3", 3, ifelse(survey_dat$others_reluctant_to_get_close == "-4", 4, ifelse(survey_dat$others_reluctant_to_get_close == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$comfortable_depending_on_others <- ifelse(survey_dat$comfortable_depending_on_others == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$comfortable_depending_on_others == "-2", 2, ifelse(survey_dat$comfortable_depending_on_others == "-3", 3, ifelse(survey_dat$comfortable_depending_on_others == "-4", 4, ifelse(survey_dat$comfortable_depending_on_others == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$dont_worry_about_people_getting_too_close <- ifelse(survey_dat$dont_worry_about_people_getting_too_close == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$dont_worry_about_people_getting_too_close == "-2", 2, ifelse(survey_dat$dont_worry_about_people_getting_too_close == "-3", 3, ifelse(survey_dat$dont_worry_about_people_getting_too_close == "-4", 4, ifelse(survey_dat$dont_worry_about_people_getting_too_close == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$people_never_there_when_needed <- ifelse(survey_dat$people_never_there_when_needed == "Not at all characteristic of me\n(1)\n", 5, ifelse(survey_dat$people_never_there_when_needed == "-2", 4, ifelse(survey_dat$people_never_there_when_needed == "-3", 3, ifelse(survey_dat$people_never_there_when_needed == "-4", 2, ifelse(survey_dat$people_never_there_when_needed == "Very characteristic of me\n(5)\n", 1, NA)))))
survey_dat$uncomfortable_being_close_to_others <- ifelse(survey_dat$uncomfortable_being_close_to_others == "Not at all characteristic of me\n(1)\n", 5, ifelse(survey_dat$uncomfortable_being_close_to_others == "-2", 4, ifelse(survey_dat$uncomfortable_being_close_to_others == "-3", 3, ifelse(survey_dat$uncomfortable_being_close_to_others == "-4", 2, ifelse(survey_dat$uncomfortable_being_close_to_others == "Very characteristic of me\n(5)\n", 1, NA)))))
survey_dat$worry_people_wont_stay_with_me <- ifelse(survey_dat$worry_people_wont_stay_with_me == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$worry_people_wont_stay_with_me == "-2", 2, ifelse(survey_dat$worry_people_wont_stay_with_me == "-3", 3, ifelse(survey_dat$worry_people_wont_stay_with_me == "-4", 4, ifelse(survey_dat$worry_people_wont_stay_with_me == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$afraid_feelings_not_reciprocated <- ifelse(survey_dat$afraid_feelings_not_reciprocated == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$afraid_feelings_not_reciprocated == "-2", 2, ifelse(survey_dat$afraid_feelings_not_reciprocated == "-3", 3, ifelse(survey_dat$afraid_feelings_not_reciprocated == "-4", 4, ifelse(survey_dat$afraid_feelings_not_reciprocated == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$wonder_whether_people_really_care <- ifelse(survey_dat$wonder_whether_people_really_care == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$wonder_whether_people_really_care == "-2", 2, ifelse(survey_dat$wonder_whether_people_really_care == "-3", 3, ifelse(survey_dat$wonder_whether_people_really_care == "-4", 4, ifelse(survey_dat$wonder_whether_people_really_care == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$comfortable_developing_close_relationships <- ifelse(survey_dat$comfortable_developing_close_relationships == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$comfortable_developing_close_relationships == "-2", 2, ifelse(survey_dat$comfortable_developing_close_relationships == "-3", 3, ifelse(survey_dat$comfortable_developing_close_relationships == "-4", 4, ifelse(survey_dat$comfortable_developing_close_relationships == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$uncomfortable_with_people_getting_too_emotionally_close <- ifelse(survey_dat$uncomfortable_with_people_getting_too_emotionally_close == "Not at all characteristic of me\n(1)\n", 5, ifelse(survey_dat$uncomfortable_with_people_getting_too_emotionally_close == "-2", 4, ifelse(survey_dat$uncomfortable_with_people_getting_too_emotionally_close == "-3", 3, ifelse(survey_dat$uncomfortable_with_people_getting_too_emotionally_close == "-4", 2, ifelse(survey_dat$uncomfortable_with_people_getting_too_emotionally_close == "Very characteristic of me\n(5)\n", 1, NA)))))
survey_dat$people_there_when_needed <- ifelse(survey_dat$people_there_when_needed == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$people_there_when_needed == "-2", 2, ifelse(survey_dat$people_there_when_needed == "-3", 3, ifelse(survey_dat$people_there_when_needed == "-4", 4, ifelse(survey_dat$people_there_when_needed == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$worry_about_being_hurt <- ifelse(survey_dat$worry_about_being_hurt == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$worry_about_being_hurt == "-2", 2, ifelse(survey_dat$worry_about_being_hurt == "-3", 3, ifelse(survey_dat$worry_about_being_hurt == "-4", 4, ifelse(survey_dat$worry_about_being_hurt == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$difficult_to_trust_others <- ifelse(survey_dat$difficult_to_trust_others == "Not at all characteristic of me\n(1)\n", 5, ifelse(survey_dat$difficult_to_trust_others == "-2", 4, ifelse(survey_dat$difficult_to_trust_others == "-3", 3, ifelse(survey_dat$difficult_to_trust_others == "-4", 2, ifelse(survey_dat$difficult_to_trust_others == "Very characteristic of me\n(5)\n", 1, NA)))))
survey_dat$people_want_to_be_closer_than_wanted <- ifelse(survey_dat$people_want_to_be_closer_than_wanted == "Not at all characteristic of me\n(1)\n", 5, ifelse(survey_dat$people_want_to_be_closer_than_wanted == "-2", 4, ifelse(survey_dat$people_want_to_be_closer_than_wanted == "-3", 3, ifelse(survey_dat$people_want_to_be_closer_than_wanted == "-4", 2, ifelse(survey_dat$people_want_to_be_closer_than_wanted == "Very characteristic of me\n(5)\n", 1, NA)))))
survey_dat$not_sure_if_people_can_be_depended_on <- ifelse(survey_dat$not_sure_if_people_can_be_depended_on == "Not at all characteristic of me\n(1)\n", 5, ifelse(survey_dat$not_sure_if_people_can_be_depended_on == "-2", 4, ifelse(survey_dat$not_sure_if_people_can_be_depended_on == "-3", 3, ifelse(survey_dat$not_sure_if_people_can_be_depended_on == "-4", 2, ifelse(survey_dat$not_sure_if_people_can_be_depended_on == "Very characteristic of me\n(5)\n", 1, NA)))))

#this will create the participants' score on the 'CLOSE' subscale
survey_dat$close = (survey_dat$easy_to_get_close + survey_dat$dont_worry_about_people_getting_too_close + survey_dat$uncomfortable_being_close_to_others + survey_dat$comfortable_developing_close_relationships + survey_dat$uncomfortable_with_people_getting_too_emotionally_close + survey_dat$people_want_to_be_closer_than_wanted)/6

#this will create the participants' score on the 'DEPEND' subscale
survey_dat$depend = (survey_dat$difficult_depending_on_others + survey_dat$comfortable_depending_on_others + survey_dat$people_never_there_when_needed + survey_dat$people_there_when_needed + survey_dat$difficult_to_trust_others + survey_dat$not_sure_if_people_can_be_depended_on)/6

#this will create the participants' score on the 'ANXIETY' subscale
survey_dat$anxiety = (survey_dat$worry_people_dont_love_me + survey_dat$others_reluctant_to_get_close + survey_dat$worry_people_wont_stay_with_me + survey_dat$afraid_feelings_not_reciprocated + survey_dat$wonder_whether_people_really_care + survey_dat$worry_about_being_hurt)/6

### ### ###

#creates score for Fear of Pain Questionnare (FPQ); the questionnare was modified to include a tenth item related to muscle cramps

#recode all answers to the FPQ so that they are numeric
survey_dat$breaking_arm = ifelse(survey_dat$breaking_arm == "Not at all", 1, ifelse(survey_dat$breaking_arm == "A little", 2, ifelse(survey_dat$breaking_arm == "A fair amount", 3, ifelse(survey_dat$breaking_arm == "Very much", 4, ifelse(survey_dat$breaking_arm == "Extreme", 5, NA)))))
survey_dat$wart_removal = ifelse(survey_dat$wart_removal == "Not at all", 1, ifelse(survey_dat$wart_removal == "A little", 2, ifelse(survey_dat$wart_removal == "A fair amount", 3, ifelse(survey_dat$wart_removal == "Very much", 4, ifelse(survey_dat$wart_removal == "Extreme", 5, NA)))))
survey_dat$paper_cut = ifelse(survey_dat$paper_cut == "Not at all", 1, ifelse(survey_dat$paper_cut == "A little", 2, ifelse(survey_dat$paper_cut == "A fair amount", 3, ifelse(survey_dat$paper_cut == "Very much", 4, ifelse(survey_dat$paper_cut == "Extreme", 5, NA)))))
survey_dat$injection_in_mouth = ifelse(survey_dat$injection_in_mouth == "Not at all", 1, ifelse(survey_dat$injection_in_mouth == "A little", 2, ifelse(survey_dat$injection_in_mouth == "A fair amount", 3, ifelse(survey_dat$injection_in_mouth == "Very much", 4, ifelse(survey_dat$injection_in_mouth == "Extreme", 5, NA)))))
survey_dat$soap_in_eyes = ifelse(survey_dat$soap_in_eyes == "Not at all", 1, ifelse(survey_dat$soap_in_eyes == "A little", 2, ifelse(survey_dat$soap_in_eyes == "A fair amount", 3, ifelse(survey_dat$soap_in_eyes == "Very much", 4, ifelse(survey_dat$soap_in_eyes == "Extreme", 5, NA)))))
survey_dat$slam_door_on_hand = ifelse(survey_dat$slam_door_on_hand == "Not at all", 1, ifelse(survey_dat$slam_door_on_hand == "A little", 2, ifelse(survey_dat$slam_door_on_hand == "A fair amount", 3, ifelse(survey_dat$slam_door_on_hand == "Very much", 4, ifelse(survey_dat$slam_door_on_hand == "Extreme", 5, NA)))))
survey_dat$gulping_hot_drink = ifelse(survey_dat$gulping_hot_drink == "Not at all", 1, ifelse(survey_dat$gulping_hot_drink == "A little", 2, ifelse(survey_dat$gulping_hot_drink == "A fair amount", 3, ifelse(survey_dat$gulping_hot_drink == "Very much", 4, ifelse(survey_dat$gulping_hot_drink == "Extreme", 5, NA)))))
survey_dat$injection_in_hip = ifelse(survey_dat$injection_in_hip == "Not at all", 1, ifelse(survey_dat$injection_in_hip == "A little", 2, ifelse(survey_dat$injection_in_hip == "A fair amount", 3, ifelse(survey_dat$injection_in_hip == "Very much", 4, ifelse(survey_dat$injection_in_hip == "Extreme", 5, NA)))))
survey_dat$falling_down = ifelse(survey_dat$falling_down == "Not at all", 1, ifelse(survey_dat$falling_down == "A little", 2, ifelse(survey_dat$falling_down == "A fair amount", 3, ifelse(survey_dat$falling_down == "Very much", 4, ifelse(survey_dat$falling_down == "Extreme", 5, NA)))))
survey_dat$cramp = ifelse(survey_dat$cramp == "Not at all", 1, ifelse(survey_dat$cramp == "A little", 2, ifelse(survey_dat$cramp == "A fair amount", 3, ifelse(survey_dat$cramp == "Very much", 4, ifelse(survey_dat$cramp == "Extreme", 5, NA)))))

#this will create the participants' total score for the FPQ
survey_dat$fear_of_pain = (survey_dat$breaking_arm + survey_dat$wart_removal + survey_dat$paper_cut + survey_dat$injection_in_mouth + survey_dat$soap_in_eyes + survey_dat$slam_door_on_hand + survey_dat$gulping_hot_drink + survey_dat$injection_in_hip + survey_dat$falling_down + survey_dat$cramp)/10

### ### ###

#creates score for participants' answer to "I felt close to my friend, family member, or partner when I saw their photo during the exercise trials."
survey_dat$close_to_support_figure_during_exercise = ifelse(survey_dat$close_to_support_figure_during_exercise == "Strongly disagree", 1, ifelse(survey_dat$close_to_support_figure_during_exercise == "Disagree a little", 2, ifelse(survey_dat$close_to_support_figure_during_exercise == "Neither agree nor disagree", 3, ifelse(survey_dat$close_to_support_figure_during_exercise == "Agree a little", 4, ifelse(survey_dat$close_to_support_figure_during_exercise == "Strongly agree", 5, NA)))))

#creates score for participants' answer to "I felt close to the other person whose photo I saw during the exercise trials."
survey_dat$close_to_stranger_during_exercise = ifelse(survey_dat$close_to_stranger_during_exercise == "Strongly disagree", 1, ifelse(survey_dat$close_to_stranger_during_exercise == "Disagree a little", 2, ifelse(survey_dat$close_to_stranger_during_exercise == "Neither agree nor disagree", 3, ifelse(survey_dat$close_to_stranger_during_exercise == "Agree a little", 4, ifelse(survey_dat$close_to_stranger_during_exercise == "Strongly agree", 5, NA)))))

#the Qualtrics script did not force a response for this question (it did force a response for the question below it on the effects of beta-alanine on how hard the trials were),
#it can be assumed that the NA's represent scores of 50, since the sliding scale always started at 50, participants would have not moved the scale from 50 if they were happy with this response
table(survey_dat$beta_alanine_performance_effect, useNA = "ifany")

library(anchors)

#this will replace all NA's with 50
survey_dat <- replace.value(survey_dat, c("beta_alanine_performance_effect"), from = NA, to = as.integer(50), verbose = FALSE)

#the question "What effect did the beta-alanine have on how hard the exercise trials were? was reversed scored (0 = "It made them easier, 50 = "No effect", 100 = "It made them harder")
survey_dat$beta_alanine_difficulty_effect = (100 - survey_dat$beta_alanine_difficulty_effect)

#this will make a variable that is how long participants have known their support figure (in months)
survey_dat$months_known = (survey_dat$years * 12) + survey_dat$months

### ### ###

#creates score for Social Assurance Scale (SAS; 1 = "Disagree", 7 = "Agree")
survey_dat$SAS = (survey_dat$comfortable_when_constantly_with_someone + survey_dat$at_ease_doing_things_with_others + survey_dat$prefer_work_side_by_side + survey_dat$life_incomplete_without_buddy + survey_dat$hard_to_use_skills_without_partner + survey_dat$stick_to_friends + survey_dat$join_groups_for_friends + survey_dat$wish_to_be_with_someone_always)/8

### ### ###

### THIS WILL DROP THE UNNECCESARY VARIABLES FROM THE SURVEY DATA SET ###

#there are now 114 variables in 'survey_dat,' not all of these variables need to be included in the main data set ('total.data') since most are questions that make up factors or constructs
length(survey_dat) #114

#this gives an untruncated list of all the variables in 'survey_dat'
str(survey_dat, list.len=ncol(survey_dat))

#this will drop the unwanted columns (dropped columns will mostly consist of the individual items or questions in a factor or construct, the constructs themselves, such as 'neuroticism,' will be retained)

#this is a character vector with each column number being the index of the variable name
columns = names(survey_dat)

#these are the variables that will be retained
keepers = c(columns[1], columns[8:9], columns[12], columns[86:89], columns[99], columns[105:115]) 

#this returns 'survey_dat' without the dropped columns (variables)
survey_dat = survey_dat[keepers]

### ### ###

### THIS WILL PREPARE THE TWO DATA SETS TO BE MERGED ###

#this will add 'sub_' to all participant numbers (to match the 'participant' variable in 'total.data'), and coerce an integer value input into a character value output
survey_dat$participant.char = sub("^", "sub_", survey_dat$Participant)

#merge the two data sets and then drop the columns with "sub_" and keep only the "Participant" column

#merges two data sets with non-matching column names
total.data = merge(total.data, survey_dat, by.x="participant", by.y="participant.char")

#this will drop the column containing the old participant classification system (the one with "sub_" as the prefix)
drops = c("participant")
total.data = total.data[ , !(names(total.data) %in% drops)]

#this will move the other 'Participant' variable to the first column in the data set
library(dplyr)
total.data =  total.data %>% select(Participant, everything())

################################################################################################################################################

### THIS WILL CREATE A DATA FRAME OF THE DATA FROM PARTICIPANTS' ANSWERS FOR THE QUESTIONS ON EXERCISE EFFORT AND DIFFICULTY FROM THE MATLAB SCRIPT, AND MERGE IT WITH 'TOTAL.DATA' ###

#reset working directory
setwd("/Users/arrandavis/Desktop/Study 3 Results")

library(R.matlab)
library(ggplot2)

#folder containing voltage data
datfolder = "/Users/arrandavis/Desktop/Study 3 Results/"

#this takes all the subfolders in the results folder
subfolders = list.dirs(datfolder)
l = (length(subfolders))

#the first folder in the list is just the main folder, so exclude this [list starts at 1]
subfolders = subfolders[2:l]
subfolders = subfolders[1:length(subfolders)]

#counter for the for loop
count = 1

#this creates an empty data frame
matdat = data.frame(matrix(NA, nrow = 0, ncol = 23))

#for testing
empty = data.frame(matrix(NA, nrow = 0, ncol = 82))

#this will go through all the subfolders
for (i in subfolders) {
  
  sub1 = subfolders[count]
  files1 = list.files(sub1)
  
  D = readMat(paste(sub1, files1[2], sep = '/'))
  
  #this gets the first 21 columns from the MATLAB data set
  tempdat = as.data.frame(D$record.dat[,1:21])
  
  #this gives participant data that is in a 42 item-long list
  tempdat$participant = D$subDetails[1]
  
  #this unlists the first item in the list (they are all the same, the participant number) so that it becomes a character vector
  tempdat$participant = sapply(tempdat$participant[1], paste0, collapse="")
  tempdat$participant = tempdat$participant[1]
  
  #this gets the placebo condition
  tempdat$placebo = D$subDetails[5]
  
  #this gets the experimental condition ('sub1' is a string and this takes the last character, which is the experimental session) and converts it to numeric
  tempdat$experimental_session = substr(sub1, start = 55, stop = 55)
  tempdat$experimental_session = as.numeric(tempdat$experimental_session)
  
  #this creates a new data frame with both sessions from the participant
  matdat <- rbind(matdat, tempdat)
  
  #just for testing
  empty = rbind(empty, tempdat[,21])
  
  count = count + 1
}

#MATLAB variable names
headings = c('trial', #1
             'supp_cont', #2 - 1 equals the supportive face, 2 equals the control face
             'start_time', #3
             'NAN1', #4
             'Q1_start_time', #5 Q1 is "How much effort did you put in to keep the ball above the line?"
             'Q2_start_time', #6 Q2 is "How hard was it to keep the ball above the line?"
             'blank1', #7
             'q1_response_time', #8
             'q2_response_time', #9
             'blank2', #10
             'NAN2', #11
             'Q1_start_pos', #12
             'Q2_start_pos', #13
             'blank3', #14
             'Q1_ans_pos', #15
             'Q2_ans_pos', #16
             'blank4', #17
             'Q1_ans_perc', #18
             'Q2_ans_perc', #19
             'blank5', #20
             'trial_difficulty', #21
             'participant', #22
             'placebo', #23
             'experimental_session') #24

colnames(x = matdat) = headings

#there is one participant with "OFF" instead of "off" for the 'placebo' variable, this fixes the labeling
matdat$placebo <- ifelse(matdat$placebo == "OFF", "off", ifelse(matdat$placebo == "on", "on", ifelse(matdat$placebo == "off", "off", NA)))

#make the placebo variable a factor
matdat$placebo = as.factor(matdat$placebo)

### ### ###

### THIS WILL PREPARE THE TWO DATA SETS TO BE MERGED ###

#first drop the unwanted columns from 'matdat'
matdat.drops = c("start_time", "NAN1", "Q1_start_time", "Q2_start_time", "blank1", "q1_response_time", "q2_response_time", "blank2", "NAN2", "Q1_start_pos", "Q2_start_pos", "blank3", "Q1_ans_pos", "Q2_ans_pos", "blank4", "blank5")
matdat = matdat[ , !(names(matdat) %in% matdat.drops)]

#this will add 'sub_' to all participant numbers (to match the 'participant' variable in 'matdat'), and coerce an integer value input into a character value output
total.data$participant.char = sub("^", "sub_", total.data$Participant)
#merge the two data sets and then drop the columns with "sub_" and keep only the "Participant" column

#merges two data sets with non-matching column names
total.data = merge(total.data, matdat, by.x= c("participant.char", "session_number", "trial_number"), by.y= c("participant", "experimental_session", "trial"))

#this will drop the column containing the old participant classification system (the one with "sub_" as the prefix), and other unneeded or repetitive variables
drops = c("participant.char", "trial_difficulty.y", "supp_cont", "placebo", "bar_unit_easy", "bar_unit_medium", "bar_unit_hard", "grip_px", "grip_dv")
total.data = total.data[ , !(names(total.data) %in% drops)]

#this will move the other 'Participant' variable to the first column in the data set
library(dplyr)
total.data =  total.data %>% select(Participant, everything())

#this will reorder and rename the rest of the variables
rename(total.data, c("trial_difficulty.x" = "trial_difficulty"))

#reorder the columns of the data frame
total.data = total.data[c("Participant", "age", "sex", "placebo_condition", "support_or_control", "four_conditions", "trial_difficulty", "session_number", "trial_number",
                          "sample_reading_number", "reading_number", "real_voltage", "percent_of_maximum", "grip_strength", "min_grip", "max_grip", "voltage", "Q1_ans_perc", "Q2_ans_perc",
                          "does_support_figure_match_description", "months_known", "close_to_support_figure_during_exercise", "close_to_stranger_during_exercise",
                          "beta_alanine_performance_effect", "beta_alanine_difficulty_effect", "heard_of_beta_alanine",
                          "extraversion", "agreeableness", "conscientiousness", "neuroticism", "openness", "close", "depend", "anxiety", "fear_of_pain", "SAS")]

#the 'percent_of_maximum' variable is actually a decimal, so it needs to be multiplied by 100
total.data$percent_of_maximum = total.data$percent_of_maximum * 100

#make clear what the social support conditions are (it is now numeric)
total.data$support_or_control = ifelse(total.data$support_or_control == 1, 'support', ifelse(total.data$support_or_control == 2, 'control', NA))

#this will output the 'total.data' data frame to .csv
write.csv(total.data, file = "total_data.csv", row.names=FALSE)

################################################################################################################################################

### REMOVAL OF PARTICIPANTS THAT DID NOT FOLLOW EXPERIMENTAL PROCEDURES OR GUESSED THE EXPPERIMENTAL HYPOTHESIS ###

#participants were not following the experimental task if their mean outputs did not increase with trial difficulty (i.e., if they continually put in minumal effort)

#this is the location of the final data set
setwd("/Users/arrandavis/Desktop/R")

#loads the final data set
total.data = read.csv("total_data.csv", sep = ",")

#creates a list of the participants
participants = unique(total.data$Participant)

#makes sure participant numbers are classified as a factor
total.data$Participant = as.factor(total.data$Participant)

#makes a list to be filled with people who are not trying
slacking = c()

#goes through all the participants and calculates average percentage of total grip strength for each difficulty level
for (i in participants) {
  
  #subsets data by participant and for each difficulty level
  trial_diff_1 = subset(total.data, Participant == i & trial_difficulty == 1)
  trial_diff_2 = subset(total.data, Participant == i & trial_difficulty == 2)
  trial_diff_3 = subset(total.data, Participant == i & trial_difficulty == 3)
  
  #gets participant i's mean percentage of maximum grip strength for each difficulty level
  mean_td1 = mean(trial_diff_1$percent_of_maximum)
  mean_td2 = mean(trial_diff_2$percent_of_maximum) 
  mean_td3 = mean(trial_diff_3$percent_of_maximum)
  
  #count as slacking any participant whose average grip strength does not increase with trial difficulty
  if (mean_td1 < mean_td2 & mean_td2 < mean_td3){
    
  } else {
    
    slacking = append(slacking, i)
  }
}

#print a list of the the participants who were slacking
unique(slacking)

#remove all the rows from the participants that did not meet the criteria set out above - only the data from the remaining participants will be included in the following analyses
total.data.analyses = total.data[ ! total.data$Participant %in% slacking, ]

#drop unused participants (factors)
total.data.analyses$Participant = droplevels(total.data.analyses$Participant)

################################################################################################################################################

### PARTICIPANT DESCRIPTIVE STATISTICS ###

#this will give the total number of participants (after exclusions)
length(unique(total.data.analyses$Participant))

#this will give the percentage of female participants
table(total.data.analyses$sex)[1] / (sum(table(total.data.analyses$sex)))

#this will give the number of female participants
(table(total.data.analyses$sex)[1] / (sum(table(total.data.analyses$sex)))) * length(unique(total.data.analyses$Participant))

#this will give descriptives of participants' age
mean(total.data.analyses$age)
sd(total.data.analyses$age)
range(total.data.analyses$age)

################################################################################################################################################

### DESCRIPTIVES AND TESTS OF POST-EXERCISE QUESTIONS ###

#these analyses need to be run on a data set that has just one row per trial, since each question is answered only once per trial (the first row and the last row will be the same - only the voltage / hand-grip data changes)
question.data = subset(total.data.analyses, sample_reading_number == 1)

#get one row per participant (this will get the data for the pre- and post-experiment questions)
post.experiment.data = subset(question.data, trial_number == 1 & session_number == 1)

#this will get descriptives on participants' familiarity with beta-alanine

#number of participants who had heard of beta-alanine
table(post.experiment.data$heard_of_beta_alanine)[3]

#percentage of participants who had heard of beta-alanine
table(post.experiment.data$heard_of_beta_alanine)[3] / sum(table(post.experiment.data$heard_of_beta_alanine))

#this will test if participants felt that the beta-alanine (i.e., the placebo) significantly improved their performance

#get the mean, sd, and range of the question
mean(post.experiment.data$beta_alanine_performance_effect)
sd(post.experiment.data$beta_alanine_performance_effect)
range(post.experiment.data$beta_alanine_performance_effect)

#one sample t-test to see if scores differ from 0
#make 50 (no difference) 0
post.experiment.data$beta_alanine_performance_effect = post.experiment.data$beta_alanine_performance_effect - 50
t.test(post.experiment.data$beta_alanine_performance_effect)

#this will test if participants felt that the beta-alanine (i.e., the placebo) made the exercise trials easier

#get the mean, sd, and range of the question
mean(post.experiment.data$beta_alanine_difficulty_effect)
sd(post.experiment.data$beta_alanine_difficulty_effect)
range(post.experiment.data$beta_alanine_difficulty_effect)

#one sample t-test to see if scores differ from 0
#make 50 (no difference) 0
post.experiment.data$beta_alanine_difficulty_effect = post.experiment.data$beta_alanine_difficulty_effect - 50
t.test(post.experiment.data$beta_alanine_difficulty_effect)

#this will test if participants felt closer to the support figure than they did to the stranger

#get the mean, sd, and range of the question (on support figure)
mean(post.experiment.data$close_to_support_figure_during_exercise)
sd(post.experiment.data$close_to_support_figure_during_exercise)
range(post.experiment.data$close_to_support_figure_during_exercise)

#get the mean, sd, and range of the question (on stranger)
mean(post.experiment.data$close_to_stranger_during_exercise)
sd(post.experiment.data$close_to_stranger_during_exercise)
range(post.experiment.data$close_to_stranger_during_exercise)

#do a Wilcoxon rank-sum test (paired  = TRUE, since measures are from the same participant)
wilcox.test(post.experiment.data$close_to_support_figure_during_exercise, post.experiment.data$close_to_stranger_during_exercise, paired = TRUE)

#this will get descriptives on the question about how well participants' support figures met the description of “someone you feel you have a close connection with and that you can depend on in times of need"

#get the descriptives for the question

#percentage of participants who answered the question with either a 6 or 7
(sum(table(post.experiment.data$does_support_figure_match_description)[c(3,4)]) / sum(table(post.experiment.data$does_support_figure_match_description))) * 100

mean(post.experiment.data$does_support_figure_match_description)
sd(post.experiment.data$does_support_figure_match_description)
range(post.experiment.data$does_support_figure_match_description)

#time participants reported knowing their support figures
mean(post.experiment.data$months_known) / 12
sd(post.experiment.data$months_known) / 12
range(post.experiment.data$months_known) / 12

#this will get descriptive statistics on the variables used to predict social support effects

#need for social assurance
mean(post.experiment.data$SAS)
sd(post.experiment.data$SAS)
range(post.experiment.data$SAS)

#ability to depend upon others
mean(post.experiment.data$depend)
sd(post.experiment.data$depend)
range(post.experiment.data$depend)

#neuroticism
mean(post.experiment.data$neuroticism)
sd(post.experiment.data$neuroticism)
range(post.experiment.data$neuroticism)

#ratio of felt closeness to support figure v. felt closeness to stranger
post.experiment.data$support.to.stranger.ratio = post.experiment.data$close_to_support_figure_during_exercise / post.experiment.data$close_to_stranger_during_exercise
mean(post.experiment.data$support.to.stranger.ratio)
sd(post.experiment.data$support.to.stranger.ratio)
range(post.experiment.data$support.to.stranger.ratio)

#this will get descriptive statistics of participants reported fear of pain
mean(post.experiment.data$fear_of_pain)
sd(post.experiment.data$fear_of_pain)
range(post.experiment.data$fear_of_pain)

################################################################################################################################################

library(dplyr)

### GET GRIP STRENGTH OUTPUT BY TRIAL DIFFICULTY ###

mean_data <- group_by(total.data.analyses, trial_difficulty) %>%
  summarise(volt_mean = mean(percent_of_maximum, na.rm = TRUE))

mean_data$volt_mean

### GET GRIP STRENGTH OUTPUT BY TRIAL DIFFICULTY ###

mean_data <- group_by(total.data.analyses, session_number) %>%
  summarise(volt_mean = mean(percent_of_maximum, na.rm = TRUE))

mean_data$volt_mean

### PERCENTAGE OF GRIP STRENGTH READINGS GREATER THAN 100% OF MAXIMUM ###

high_reading = subset(total.data.analyses, percent_of_maximum > 100)

length(unique(high_reading$Participant))
nrow(high_reading)
(nrow(high_reading) / nrow(total.data.analyses)) * 100

### GET PERCEIVED DIFFICULTY BY TRIAL DIFFICULTY ###

mean_data <- group_by(total.data.analyses, trial_difficulty) %>%
  summarise(diff = mean(Q2_ans_perc, na.rm = TRUE))

mean_data$diff

### GET EFFORT BY TRIAL DIFFICULTY ###

mean_data <- group_by(total.data.analyses, trial_difficulty) %>%
  summarise(effort = mean(Q1_ans_perc, na.rm = TRUE))

mean_data$effort

### GET PERCEIVED DIFFICULTY BY EXERCISE BLOCK ###

mean_data <- group_by(total.data.analyses, session_number ) %>%
  summarise(diff = mean(Q2_ans_perc, na.rm = TRUE))

mean_data$diff

### GET EFFORT BY EXERCISE BLOCK ###

mean_data <- group_by(total.data.analyses, session_number) %>%
  summarise(effort = mean(Q1_ans_perc, na.rm = TRUE))

mean_data$effort

### CENSORING OF POST-TRIAL QUESTIONS ###

#correlation beteen participants' answers to the two questions
cor(question.data$Q1_ans_perc, question.data$Q2_ans_perc)

#is the question data censored?
hist(question.data$Q1_ans_perc, main = "", xlab = "Percent of sliding scale (0 = 'no effort at all', 100 = 'maximum effort')", ylim = c(0, 800), breaks = 101, cex=2.5)
hist(question.data$Q2_ans_perc, main = "", xlab = "Percent of sliding scale (0 = 'not hard at all', 100 = 'extremely hard')",  ylim = c(0, 800), breaks = 101)

#get the percentage of answers at right of sliding scale
nrow(subset(question.data, Q2_ans_perc == 100)) / nrow(question.data) * 100
nrow(subset(question.data, Q1_ans_perc == 100)) / nrow(question.data) * 100

#get the mode answer
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(question.data$Q2_ans_perc)

#the data for both questions is heavily right censored

################################################################################################################################################

### MAIN ANALYSES - FOUND IN MAIN TEXT ###

### MAIN MULTILEVEL MODEL ON PARTICIPANTS' GRIP STRENGTH DATA ###

library(lme4)

#this will change the appropriate variables to factors
total.data.analyses$placebo_condition = as.factor(total.data.analyses$placebo_condition)
total.data.analyses$Participant = as.factor(total.data.analyses$Participant)
total.data.analyses$trial_difficulty.f = as.factor(total.data.analyses$trial_difficulty)

#this will change the appropriate variables to integers
total.data.analyses$session_number = as.integer(total.data.analyses$session_number)
total.data.analyses$trial_number = as.integer(total.data.analyses$trial_number)
total.data.analyses$trial_difficulty = as.integer(total.data.analyses$trial_difficulty)

#ordered factor
total.data.analyses$trial_difficulty.o = ordered(total.data.analyses$trial_difficulty)

#this will allow for p-values (but need to rerun the model)
library(lmerTest)
library(optimx)
library(nloptr)
library(dfoptim)
library(RCurl)
library(MuMIn)

#run the models

#test the interaction between placebo and exercise block; this is the most complex random effects structure that allowed for convergence
placeb.exercise.block.interaction = lmer(percent_of_maximum ~ placebo_condition*session_number + trial_number + sex + Q1_ans_perc + (1 + placebo_condition | Participant), data = total.data.analyses)
summary(placeb.exercise.block.interaction)

#the interaction is non-significant, therefore the interaction was dropped from future analyses (but session number will continue to be included as a covariate)

#next test the three-way interaction between trial difficulty, placebo, and social support; this is the most complex random effects structure that allowed for convergence
three.way.interaction = lmer(percent_of_maximum ~ 1 + trial_difficulty*placebo_condition*support_or_control + trial_number + session_number + sex + Q1_ans_perc + (1 + placebo_condition | Participant), data = total.data.analyses)
summary(three.way.interaction)

#a constituent interaction is signifiant (social support by trial difficulty), therefor the three-way interaction will be maintained in further analyses

#this is the most complex random effects structure that allows for model convergence (fails to converge with participant sex as a covariate)
full.model = lmer(percent_of_maximum ~ 1 + trial_difficulty*placebo_condition*support_or_control + session_number + Q1_ans_perc + trial_number + (1 + support_or_control + placebo_condition | Participant), data = total.data.analyses)

#model summary and R-squared values
summary(full.model)
r.squaredGLMM(full.model)

#test post-hoc contrasts
emm_options(pbkrtest.limit = 981120)

emmeans(full.model,
        data = total.data.analyses,
        pairwise ~ trial_difficulty  | placebo_condition)

#this is the model that tests the main effects of the social support and placebo manipulations
plain.model = lmer(percent_of_maximum ~ 1 + placebo_condition + support_or_control + trial_difficulty + session_number + Q1_ans_perc + trial_number + (1 + support_or_control + placebo_condition | Participant), data = total.data.analyses)

#model summary and R-squared values
summary(plain.model)
r.squaredGLMM(plain.model)

################################################################################################################################################

### MULTILEVEL CENSORED REGRESSION MODELS ON POST EXERCISE TRIAL QUESTION ON DIFFUCULTY (Q2) ###

#Q2_ans_perc is participants' answer to the question "How hard was it to keep the bar above the line?" (percentage of sliding scale)

#these analyses need to be run on a data set that has just one row per trial since each questions is answered only once per trial (the first row and the last row will be the same - only the voltage / hand-grip data changes)
question.data = subset(total.data.analyses, sample_reading_number == 1)

#prepare the data for a censored regression model

#open packages
library(censReg)

#multilevel censored regression model for question data (create data frame that will later need to be transformed to a 'pdata.frame' object)
rData = data.frame(question.data$Participant)

#add the main predictor variables ('support or control' needs to be made a factor)
rData$placebo = question.data$placebo_condition
rData$support = question.data$support_or_control

#add other covariates
rData$sex = question.data$sex
rData$trial_number = question.data$trial_number
rData$session = question.data$session_number
rData$trial_difficulty = question.data$trial_difficulty

#create a time variable
rData$time = ifelse(rData$session == 1, rData$trial_number, ifelse(rData$session == 2, 42 + rData$trial_number, NA ))

#add the variable on trial difficulty (Q2) and effort (Q1)
rData$y.Q2 = question.data$Q2_ans_perc
rData$y.Q1 = question.data$Q1_ans_perc

library(plm)

#convert data frame to class 'pdata.frame' based on the id and time variables
rData = pdata.frame( rData, c( "question.data.Participant", "time" ) )

#convert variables to factors
rData$session = as.factor(rData$session)

#multilevel censored regression on participants’ answers to the question about how hard the previous trial was (controlling for effort)

#first test to see if there is an interaction between experimental session and the placebo treatment
cens.reg.session.placebo.interaction = censReg( y.Q2 ~ placebo*session + trial_number + y.Q1, left = 0, right = 100, data = rData, method = "BHHH" )
summary(cens.reg.session.placebo.interaction)

#the interaction is highly significant, so it will remain in the model and be included in a four-way interaction with trial difficulty, placebo, and social support

#final model inlcuding the four-way interaction and participants' reported effort levels as a covariate
cens.reg.4way = censReg( y.Q2 ~ placebo*support*trial_difficulty*session + trial_number + y.Q1, left = 0, right = 100, data = rData, method = "BHHH" ) 
summary(cens.reg.4way)

#this model does not produce estimates by participant or residuals; to compare the interaction between placebo and exercise block (session) a t-test will be used
no.placebo.block1 = subset(question.data,  placebo_condition == "off" & session_number == 1)
no.placebo.block2 = subset(question.data,  placebo_condition == "off" & session_number == 2)
placebo.block1 = subset(question.data,  placebo_condition == "on" & session_number == 1)
placebo.block2 = subset(question.data,  placebo_condition == "on" & session_number == 2)

#compare mean difficulty ratings by exercise block and placebo condition
mean(no.placebo.block1$Q2_ans_perc)
sd(no.placebo.block1$Q2_ans_perc)

mean(placebo.block1$Q2_ans_perc)
sd(placebo.block1$Q2_ans_perc)

mean(no.placebo.block2$Q2_ans_perc)
sd(no.placebo.block2$Q2_ans_perc)

mean(placebo.block2$Q2_ans_perc)
sd(placebo.block2$Q2_ans_perc)

#compare difficutly ratings between those who received no placebo before the first block of exercise to the ratings of those who received the placebo before the first block of exercise
t.test(no.placebo.block1$Q2_ans_perc, placebo.block1$Q2_ans_perc)

#compare difficutly ratings between those who received no placebo before the second block of exercise to the ratings of those who received the placebo before the second block of exercise
t.test(no.placebo.block2$Q2_ans_perc, placebo.block2$Q2_ans_perc)

#compare difficutly ratings between those who received no placebo before the first block of exercise to the ratings of those who received no placebo before the second block of exercise
t.test(no.placebo.block1$Q2_ans_perc, no.placebo.block2$Q2_ans_perc)

#compare difficutly ratings between those who received no placebo before the first block to the ratings of those who received the placebo before the second block of exercise
t.test(no.placebo.block1$Q2_ans_perc, placebo.block2$Q2_ans_perc)

#compare difficutly ratings between those who received the placebo before the first block of exercise to the ratings of those who received no placebo before the second block of exercise
t.test(placebo.block1$Q2_ans_perc, no.placebo.block2$Q2_ans_perc)

#compare difficutly ratings between those who received the placebo before the first block of exercise to the ratings of those who received the placebo before the second block of exercise
t.test(placebo.block1$Q2_ans_perc, placebo.block2$Q2_ans_perc)

#mean differences between the 'no placebo, seconds exercise block' condition and all other placebo by exercise block conditions
mean(no.placebo.block2$Q2_ans_perc) - mean(no.placebo.block1$Q2_ans_perc)
mean(no.placebo.block2$Q2_ans_perc) - mean(placebo.block1$Q2_ans_perc)
mean(no.placebo.block2$Q2_ans_perc) - mean(placebo.block2$Q2_ans_perc)

### ### ###

#get get hand grip outputs for the four placebo by exercise block conditions
no.placebo.block1.hg = subset(total.data.analyses,  placebo_condition == "off" & session_number == 1)
no.placebo.block2.hg = subset(total.data.analyses,  placebo_condition == "off" & session_number == 2)
placebo.block1.hg = subset(total.data.analyses,  placebo_condition == "on" & session_number == 1)
placebo.block2.hg = subset(total.data.analyses,  placebo_condition == "on" & session_number == 2)

#compare mean hand grip outputs by exercise block and placebo condition
mean(no.placebo.block1.hg$percent_of_maximum)
sd(no.placebo.block1.hg$percent_of_maximum)

mean(placebo.block1.hg$percent_of_maximum)
sd(placebo.block1.hg$percent_of_maximum)

mean(no.placebo.block2.hg$percent_of_maximum)
sd(no.placebo.block2.hg$percent_of_maximum)

mean(placebo.block2.hg$percent_of_maximum)
sd(placebo.block2.hg$percent_of_maximum)

################################################################################################################################################

### PREDICTORS OF SOCIAL SUPPORT EFFECTS ###

### CREATE SOCIAL SUPPORT RESPONSE VARIABLE ###

#this creates a data frame with one (the first) observation per participant
g = total.data.analyses[!duplicated(total.data.analyses$Participant),]

#this will get the participant average for when in the stranger condition
support.control = subset(total.data.analyses, support_or_control == 'control')
participant.means.control = as.list(aggregate(percent_of_maximum ~ Participant, data = support.off, FUN=function(x) c(mean=mean(x))))[2]

#add the above list to g
g = cbind(g, participant.means.control)
#rename the column
colnames(g)[length(colnames(g))] = "percentage.maximum.off.support"

#this will get the participant average for when in the social support condition
support.on = subset(total.data.analyses, support_or_control == 'support')
participant.means.support = as.list(aggregate(percent_of_maximum ~ Participant, data = support.on, FUN=function(x) c(mean=mean(x))))[2]

#add the above list to g
g = cbind(g, participant.means.support)
#rename the column
colnames(g)[length(colnames(g))] = "percentage.maximum.on.support" 

#this will create a varaiable that indicates how the participant responded to social support
g$support.response = g$percentage.maximum.on.support - g$percentage.maximum.off.support

#this will get participants' sex
g$sex = g$sex

### NEED FOR SOCIAL ASSURANCE ###

model1 = lm(support.response ~ SAS + sex, data = g)
summary(model1)

### NEUROTICISM ###

model2 = lm(support.response ~ neuroticism + sex, data = g)
summary(model2)

### ABILITY TO DEPEND ON OTHERS ###

model3 = lm(support.response ~ depend + sex, data = g)
summary(model3)

### RATIO OF FELT CLOSENESS TO SUPPORT FIGURE TO FELT CLOSENESS TO STRANGER ###

#this will get participants' ratio of felt closeness to support figure to felt closeness to stranger
g$support.to.stranger.ratio = g$close_to_support_figure_during_exercise / g$close_to_stranger_during_exercise
mean(g$support.to.stranger.ratio)
sd(g$support.to.stranger.ratio)
range(g$support.to.stranger.ratio)

#plot participants' response to placebo against their ratio
plot(g$support.to.stranger.ratio, g$support.response, xlab = "Ratio of closeness to support figure to closeness to stranger", ylab = "Social support effect on grip strength")
abline(lm(g$support.response ~ g$support.to.stranger.ratio))

model4 = lm(support.response ~ support.to.stranger.ratio + sex, data = g)
summary(model4)

################################################################################################################################################

### GRAPHS ###

library(ggplot2)
library(dplyr)

### MEAN GRIP STRENGTH (AS A PERCENTAGE OF MAXIMUM) OVER TIME BY TRIAL TARGET GRIP STRENGTH  ###

#this creates the mean for of the trial difficulty levels
mean_data <- group_by(total.data.analyses, sample_reading_number, trial_difficulty) %>%
  summarise(volt_mean = mean(percent_of_maximum, na.rm = TRUE))

#rename trial difficulty variable
mean_data$trial_difficulty = ifelse(mean_data$trial_difficulty == 1, "40% of maximum grip", ifelse(mean_data$trial_difficulty == 2, "50% of maximum grip", ifelse(mean_data$trial_difficulty == 3, "60% of maximum grip", NA)))

#output graph
ggplot(na.omit(mean_data), aes(x = sample_reading_number, y = volt_mean, colour = trial_difficulty)) + geom_point() + geom_line() + scale_color_manual(values=c("forestgreen", "yellow", "red")) +
  theme_set(theme_bw() + theme(legend.key=element_blank(), plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
                               panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2),
                               axis.title.x = element_text(margin = unit(c(9, 0, 0, 0), "mm")), axis.title.y = element_text(margin = unit(c(0, 9, 0, 0), "mm")), text = element_text(size = 22, family = "sans"),
                               legend.key.height = unit(2, "lines"))) + 
  ylab("Percentage of maximum grip strength") + xlab("Time (grip strength reading number)") + labs(color = "Target grip strength") +
  geom_hline(yintercept=c(40, 50, 60), linetype="dotted") + 
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60)) 

###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###

### MEAN GRIP STRENGTH (AS A PERCENTAGE OF MAXIMUM) BY TRIAL DIFFICULTY AND TARGET GRIP STRENGTH  ###

#this creates the mean for of the trial difficulty levels
mean_data <- group_by(total.data.analyses, session_number, trial_difficulty) %>%
  summarise(volt_mean = mean(percent_of_maximum, na.rm = TRUE))

#create factors for the variables
mean_data$session_number = as.factor(mean_data$session_number)
mean_data$trial_difficulty = as.factor(mean_data$trial_difficulty)

#load all above packages to get "oob = rescale_none" to work
#output graph
ggplot(data = mean_data, aes(x = trial_difficulty , y = volt_mean, fill = factor(session_number))) +
  geom_bar(position = "dodge", stat = "identity") + 
  ylab("Percentage of maximum grip strength") + xlab("Target grip strength") + 
  scale_fill_manual("Exercise block", values=c("snow2", "snow3")) +
  scale_x_discrete(labels = c("1" = "40% of maximum", "2" = "50% of maximum", "3" = "60% of maximum")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2), axis.title.x = element_text(margin = unit(c(6, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 6, 0, 0), "mm")), text = element_text(size = 22, family = "sans"))

###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###

### MEAN GRIP STRENGTH BY TRIAL DIFFICULTY BY SOCIAL SUPPORT ###

#first summarise the data to be plotted

#create function to summarise (and get standard error)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

detach(package: dplyr)

#create the data set (using function from above)
dat2plot <- summarySE(total.data.analyses, measurevar="percent_of_maximum", groupvars=c("support_or_control","trial_difficulty"))

#create factors for the variables
dat2plot$support_or_control = as.factor(dat2plot$support_or_control)
dat2plot$placebo_condition = as.factor(dat2plot$trial_difficulty)

#rename 'trial_difficulty' variable
dat2plot$trial_difficulty = ifelse(dat2plot$trial_difficulty == 1, "40% of maximum", ifelse(dat2plot$trial_difficulty == 2, "50% of maximum", ifelse(dat2plot$trial_difficulty == 3, "60% of maximum", NA)))

#rename 'support_or_control' variable
dat2plot$support_or_control = ifelse(dat2plot$support_or_control == "control", "Stranger face", ifelse(dat2plot$support_or_control == "support", "Support figure face", NA))

###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ##   ###   ###   ###   ###   ###   ###   ###   ###

### VOLTAGE MEAN GRAPH ###

#output graph
ggplot(dat2plot, aes(x = support_or_control, y = percent_of_maximum, group = trial_difficulty, color = trial_difficulty)) + 
  geom_errorbar(aes(ymin = percent_of_maximum - (4*se), ymax = percent_of_maximum + (4*se)), width=.1) +
  geom_line() +
  geom_point() + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2),
        legend.key = element_blank(), axis.title.x = element_text(margin = unit(c(9, 0, 0, 0), "mm")), axis.title.y = element_text(margin = unit(c(0, 9, 0, 0), "mm")),
        text = element_text(size = 22, family = "sans"), legend.key.height = unit(2, "lines")) +
  ylab("Percentage of maximum grip strength") + 
  xlab("Social support condition") + 
  scale_y_continuous(breaks=seq(41,58,1)) +
  #this will give the axis label and the colour the lines
  scale_color_manual(name = "Trial difficulty", values = c("grey70", "grey35", "black"))

###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###

### MEAN GRIP STRENGTH BY TRIAL DIFFICULTY BY PLACEBO ###

#create the data set (using function from above)
dat2plot <- summarySE(total.data.analyses, measurevar="percent_of_maximum", groupvars=c("placebo_condition","trial_difficulty"))

#create factors for the variables
dat2plot$placebo_condition = as.factor(dat2plot$placebo_condition)

#rename 'trial_difficulty' variable
dat2plot$trial_difficulty = ifelse(dat2plot$trial_difficulty == 1, "40% of maximum", ifelse(dat2plot$trial_difficulty == 2, "50% of maximum", ifelse(dat2plot$trial_difficulty == 3, "60% of maximum", NA)))

#rename 'placebo_condition' variable
dat2plot$placebo_condition = ifelse(dat2plot$placebo_condition == "off", "Off", ifelse(dat2plot$placebo_condition == "on", "On", NA))

### VOLTAGE MEAN GRAPH ###

#output graph
ggplot(dat2plot, aes(x = placebo_condition, y = percent_of_maximum, group = trial_difficulty, color = trial_difficulty)) + 
  geom_errorbar(aes(ymin = percent_of_maximum - (4*se), ymax = percent_of_maximum + (4*se)), width=.1) +
  geom_line() +
  geom_point() + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2),
        legend.key = element_blank(), axis.title.x = element_text(margin = unit(c(9, 0, 0, 0), "mm")), axis.title.y = element_text(margin = unit(c(0, 9, 0, 0), "mm")),
        text = element_text(size = 22, family = "sans"), legend.key.height = unit(2, "lines")) +
  ylab("Percentage of maximum grip strength") + 
  xlab("Placebo condition") + 
  scale_y_continuous(breaks=seq(41,58,1)) +
  #this will give the axis label and the colour the lines
  scale_color_manual(name = "Trial difficulty", values = c("grey70", "grey35", "black"))

###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###

### PERCEIVED DIFFICULTY BY PLACEBO BY EXERCISE BLOCK ###

#create the data set
dat2plot <- summarySE(question.data, measurevar="Q2_ans_perc", groupvars=c("placebo_condition","session_number"))

#create factors for the variables
dat2plot$session_number = as.factor(dat2plot$session_number)
dat2plot$placebo_condition = as.factor(dat2plot$placebo_condition)

#rename 'session' number variable
dat2plot$session_number = ifelse(dat2plot$session_number == 1, "First", ifelse(dat2plot$session_number == 2, "Second", NA))

#rename 'placebo_condition' variable
dat2plot$placebo_condition = ifelse(dat2plot$placebo_condition == "off", "No placebo", ifelse(dat2plot$placebo_condition == "on", "Placebo", NA))

#this will give a high resolution TIFF
tiff('difficulty_by_placebo_by_exercise_block_TEST.tiff', units="in", width=5, height=5, res=600)

ggplot(dat2plot, aes(x=placebo_condition, y= Q2_ans_perc, group = session_number, color = session_number)) + 
  geom_errorbar(aes(ymin = Q2_ans_perc-(se), ymax = Q2_ans_perc + (se)), width=.1) +
  geom_line() +
  geom_point() + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2),
        legend.key = element_blank(), legend.position = c(.85, .9), axis.title.x = element_text(margin = unit(c(9, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 9, 0, 0), "mm")), text = element_text(size = 11, family = "sans")) +
  ylab("Perceived dificulty") + 
  xlab("Placebo condition") + 
  #this will give the axis label and the colour the lines
  scale_color_manual(name = "Exercise block", values = c("grey", "black")) + 
  scale_y_continuous(breaks=c(69, 70, 71,72,73,74,75,76)) 

dev.off()

#same graph as above but with exercise block on x-axis
ggplot(dat2plot, aes(x=session_number, y= Q2_ans_perc, group = placebo_condition, color = placebo_condition)) + 
  geom_errorbar(aes(ymin = Q2_ans_perc-(se), ymax = Q2_ans_perc + (se)), width=.1) +
  geom_line() +
  geom_point() + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2),
        legend.key = element_blank(), legend.position = c(.15, .9), axis.title.x = element_text(margin = unit(c(9, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 9, 0, 0), "mm")), text = element_text(size = 22, family = "sans"), legend.key.height = unit(2, "lines")) +
  ylab("Perceived dificulty") + 
  xlab("Exercise block") + 
  #this will give the axis label and the colour the lines
  scale_color_manual(name = "Placebo condition", values = c("grey", "black")) + 
  scale_y_continuous(breaks=c(67,68,69, 70, 71,72,73,74,75,76)) 

################################################################################################################################################

### THIS SCRIPT, USED FOR MODEL ASSUMPTION CHECKS ON ALL MODELS WITH SIGNIFICANT RESULTS, CAN BE FOUND HERE: http://www.stats.ox.ac.uk/~snijders/ch10.r ###

###################################################################
###  ch10.r                                                     ###
###                                                             ###
###  This is an R script for producing examples in              ###
###  Chapter 10 of                                              ###
###  Snijders, Tom A.B., and Bosker, Roel J.                    ###
###  Multilevel Analysis:                                       ###
###  An Introduction to Basic and Advanced Multilevel Modeling, ###
###  second edition.                                            ###
###  London etc.: Sage Publishers, 2012                         ###
###                                                             ###
###  version April 4, 2014                                      ###
###                                                             ###
###################################################################

### MULTILEVEL MODELS ON PARTICIPANT HAND GRIP OUTPUTS ###

#use either the full model (full.model) or the model without interactions that was used to test the main effects of the placebo and social support conditions (plain.model; this model is commented out below)

library(lubridate)
library(lme4)
library(lmerTest)
library(RColorBrewer)
library(nlme)

###################################################################
###                                                             ###
### Within - group OLS residuals                                ###
###                                                             ###
###################################################################

#this calculates OLS regressions per participant

#this will run a liner model at every level of the grouping factor that is present in the data set (so, a linear model will be run on every participants' data)

#this renames the variables (for plot title purposes)
total.data.analyses$Social_support = total.data.analyses$support_or_control
total.data.analyses$Placebo= total.data.analyses$placebo_condition
total.data.analyses$Trial_difficulty = total.data.analyses$trial_difficulty
total.data.analyses$Exercise_block = total.data.analyses$session_number
total.data.analyses$Perceived_difficulty = total.data.analyses$Q1_ans_perc
total.data.analyses$Trial_number = total.data.analyses$trial_number

#the Perceived_difficulty variable was excluded because including it caused the model to fail to converge

#one linear model per participant
olselist <- lmList(percent_of_maximum ~ Placebo*Social_support*Trial_difficulty + Exercise_block + Trial_number | Participant, data = total.data.analyses )
#olselist <- lmList(percent_of_maximum ~ Placebo + Social_support + Trial_difficulty + Exercise_block + Trial_number | Participant, data = total.data.analyses )

#full model
summary(mod1 <- lme(percent_of_maximum ~  Placebo*Social_support*Trial_difficulty + Exercise_block  + Trial_number, random = ~ Placebo + Social_support | Participant, data = total.data.analyses))
#summary(mod1 <- lme(percent_of_maximum ~  Placebo + Social_support + Trial_difficulty + Exercise_block  + Trial_number, random = ~ Placebo + Social_support | Participant, data = total.data.analyses))

#The compareFits function compares the OLS estimates with the posterior means of the multilevel model. This is done only for the level-1 effects.
#The graph produced below will compare the variance of the OLS regression estimates from each participant to the posterior means calculated in the multilevel model.
comp.models <- compareFits(coef(olselist), coef(mod1))
plot(comp.models, ylab = "")

#the plots do not indicate larger variability for the OLS estimates

### ### ### HERE HERE HERE HERE HERE ### ### ### 

### LEVEL ONE HETEROSCEDASTICITY ###

#the null is homoscedasticity, and it has a chi-sauare distribution under the null

#get the within-group residual standard deviations
summary(olselist[[1]])$sigma

#make a plot of residual variance against residual d.f.:
sigma2_res <- sapply(olselist, function(x){(summary(x)$sigma)^2})
plot(df_res, sigma2_res) 

#Formula (10.3); "Used to detect heteroscedasticity in the form of between-group differences in level-one residual variance" (Snijders & Bosker, 2012; p. 159).

#test heteroscedasticity
ls_tot <- sum((df_res*log(sigma2_res)))/sum(df_res)
d <- (sqrt(0.5*df_res))*(log(sigma2_res) - ls_tot)
(H <- sum((d^2)))

# The associated p-value is
1-pchisq(H, sum(df_res) - 1)

#this will plot d, which is Gausian if there is level-one homoscedasticity
qqnorm(d)
qqline(d)

### ### ###

# Example 10.2

#the goal here is to inspect level-one residuals 

#step one is to plot the unstandardised OLS residuals against level-one explanatory variables to check for non-linearity
#this step is skipped since the level-one explanatory variables of interest are binary or categorical

#step two is to make a normal probability plot of the standardised OLS residuals to check the assumption of a normal distribution

### MAKE A NORMAL PROBABILITY PLOT OF THE STANDARDISED OLS RESIDUALS TO CHECK THE ASSUMPTIONS OF A NORMAL DISTRIBUTION ###

#first define a function that gives the studentized OLS residuals for this model in a given data frame:
res_st5 <- function(x){
  rstudent(lm(percent_of_maximum ~ Placebo*Social_support*Trial_difficulty + Exercise_block + Trial_number,
              data=x))
}

#compute within-participant studentized OLS residuals
resi_st <- by(total.data.analyses, total.data.analyses$Participant, res_st5)
rs <- unlist(resi_st)

#make a QQ plot with use the <use> vector defined above (to only include the participants that have enough runs)
qqnorm(rs, main = "")
qqline(rs)

###################################################################
###                                                             ###
### Residuals at level two                                      ###
###                                                             ###
###################################################################

# Example 10.3

#the goal here is to inspect level-two residuals

#step one is to plot the unstandardised level-two residuals against "relevant level-two variables" to check for non-linearity
#step one will be skipped, since there are no relevant level-two predictor variables

#step two is to make normal probability plots of standardised level-two residuals to check the assumption of a normal distribution, and

#step three is to plot the squared standardised level-two residuals as a function of level-two variables to check homoscedasticity
#step three will be skipped, since there are no relevant level-two predictor variables

#lme4 is used for these checks
detach("package:nlme")
library(lme4)

#this is the model for which the level-two residuals will be inspected (this should be the full model)
(mod103 <- lmer(percent_of_maximum ~ 1 + trial_difficulty*placebo_condition*support_or_control + session_number + Q1_ans_perc + trial_number + (1 + support_or_control + placebo_condition | Participant), data = total.data.analyses))
#(mod103 <- lmer(percent_of_maximum ~ 1 + trial_difficulty + placebo_condition + support_or_control + session_number + Q1_ans_perc + trial_number + (1 + support_or_control + placebo_condition | Participant), data = total.data.analyses))

#get the random effects for this model
re.mod103 <- ranef(mod103, condVar=TRUE, standard=TRUE)

#this will get the posterior means (taking out the columns of random slopes and intercepts for each participant):
postmean  <- re.mod103$Participant$`(Intercept)` #first column is the intercept (this is the same as above)
postslope.support <- re.mod103$Participant$support_or_controlsupport
postslope.placebo <- re.mod103$Participant$placebo_conditionon

#this will get the posterior variances:
#for example, attr(re.mod103$athnumber,'postVar')[,,10] will give the posterior variabnces for the 10th participant
#the first row/column is the intercept, the second row/column is for placebo condition, and the third is for social support condition
#so, attr(re.mod103$athnumber,'postVar')[2,2,10] gives the posterior variance for the placebo condition for the 10th participant
postmeanvar <-  attr(re.mod103$Participant,'postVar')
postslopevar.placebo <-  attr(re.mod103$Participant,'postVar')[2,2,] 
postslopevar.support <-  attr(re.mod103$Participant,'postVar')[3,3,]

#these are the comparative variances, cf. Section 4.8. in Snijders & Bosker (2012; p. 65)

#posterior means are computed by taking information from the overall mean and the group (here, participant) mean, which is influenced by the size and variability of the observations within the group
#posterior means, in other words, are the empirical Bayes' estiamtes of the random slopes and intercepts (Snijders & Bosker, 2012; p. 64)

#the parameters of the random part
VarCorr(mod103)   

#diagnostic variance is calculated using 4.18 from Snijders & Bosker (2012; p. 65):
diagmeanvar  <- VarCorr(mod103)$Participant[1,1] - postmeanvar
diagslopevar.placebo <- VarCorr(mod103)$Participant[2,2] - postslopevar.placebo
diagslopevar.support <- VarCorr(mod103)$Participant[3,3] - postslopevar.support

### NORMAL PROBABILITY PLOTS OF STANDARDISED LEVEL-TWO RESIDUALS TO CHECK FOR LEVEL-TWO RESIDUAL NORMALITY ###

#make a plot like figure 10.6 (Snijders & Bosker, 2012; p. 166) for the intercepts and both the social support and placebo effect slopes:

#for the intercepts
postmean.stand <- postmean/sqrt(diagmeanvar)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

#for the social support variable
postmean.stand <- postslope.support/sqrt(diagslopevar.support)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

#for the placebo variable
postmean.stand <- postslope.placebo/sqrt(diagslopevar.placebo) 
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

###################################################################
###                                                             ###
### Influence of level-two units                                ###
###                                                             ###
###################################################################

library(influence.ME)

#this will estimate the model over and over again, excluding each participannt and seeing how the model changes
alt.est <- influence(full.model, group ="Participant")
#alt.est <- influence(plain.model, group ="Participant")

#Cook's distances
cooks.distance.estex(alt.est)
range(cooks.distance.estex(alt.est))
mean(cooks.distance(alt.est))
sd(cooks.distance(alt.est))
hist(cooks.distance(alt.est), breaks = 100, main = "", xlab = "Cook's distance")

#percent change in parameter estimates when including and not including each level-two unit (i.e., participant)
pchange(alt.est)

influencers = cooks.distance.estex(alt.est)
p = length(influencers)

#this will give the percentage of participants with potentially problematic Cook's distances
count4 = 0
for (i in influencers) {
  
  #threshold for potentially problematic Cook's distances; either 4 / n where n in the number of level-two units (Nieuwenhuis et al., 2012) or 1 (Field et al., 2014)
  if (i > 1){
    print(i)
    count4 = count4 + 1
    
  }
}

#percentage of participants above threshold for potentially problematic Cook's distances; either 4 / n where n in the number of level-two units (Nieuwenhuis et al., 2012) or 1 (Field et al., 2014)
count4
print(count4 / p)

#the sigtest function tests whether excluding a particular level-two unit changes the statistical significance (at 1.96) of a model’s predictor variables (Nieuwenhuis et al. 2012)
sig.test.dataframe = as.data.frame(sigtest(alt.est))

#social support by trial difficulty interaction

#this gives the range of t-values produced by interatively excluding the sampled participants one at a time
range(sig.test.dataframe$trial_difficulty.support_or_controlsupport.Altered.Teststat)
hist(sig.test.dataframe$trial_difficulty.support_or_controlsupport.Altered.Teststat)

#test whether excluding this level-two unit changes the significance of the interaction
table(sig.test.dataframe$trial_difficulty.support_or_controlsupport.Changed.Sig)

#placebo by trial difficulty interaction

#this gives the range of t-values produced by interatively excluding the sampled participants one at a time
range(sig.test.dataframe$trial_difficulty.placebo_conditionon.Altered.Teststat)
hist(sig.test.dataframe$trial_difficulty.placebo_conditionon.Altered.Teststat)

#test whether excluding this level-two unit changes the significance of the interaction
table(sig.test.dataframe$trial_difficulty.placebo_conditionon.Changed.Sig)

#social support (need to run a seperate model for this - one with no interactions - see above)

#this gives the range of t-values produced by interatively excluding the sampled participants one at a time
range(sig.test.dataframe$support_or_controlsupport.Altered.Teststat)
hist(sig.test.dataframe$support_or_controlsupport.Altered.Teststat)

#test whether excluding this level-two unit changes the significance of the interaction
table(sig.test.dataframe$support_or_controlsupport.Changed.Sig)

###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###

### SUBSIDIARY MULTILEVEL CENSORED REGRESSION MODELS ON POST EXERCISE TRIAL QUESTIONS (USED TO CHECK ASSUMPTIONS OF MULTILEVEL CENSORED REGRESSION MODEL) ###

#the 'censReg' function does not produce residuals, so these models (censored regression models and multilevel models) will provide checks of the level-one and level-two residuals
library(AER)
library(lme4)
library(lmerTest)

### censored regression (with participant as a factor) on participants’ answers to the question about how hard the previous trial was

#this is the closest possible model to the model reported above and in the main text; addiding session_number to the interaction causes a failure to converge (since individuals have not done the placebo in both exercise blocks)
tobit.Q2 = tobit(Q2_ans_perc ~ support_or_control*placebo_condition*trial_difficulty + session_number + session_number + trial_number + Participant + Q1_ans_perc, left = 0, right = 100, data = question.data)
summary(tobit.Q2)

### multilevel model on participants’ answers to the question about how hard the previous trial was

Q2.results.full.model = lmer(Q2_ans_perc ~ 1 + placebo_condition*support_or_control*trial_difficulty*session_number + trial_number + Q1_ans_perc + (1 | Participant), data = question.data)
summary(Q2.results.full.model)

### LINEARITY ###

#(look for random pattern around 0)
plot(fitted(tobit.Q2), resid(tobit.Q2), ylab = "Residuals", xlab = "Fitted values")
plot(tobit.Q2$linear.predictors, question.data$Q2_ans_perc, ylab = "Observed values", xlab = "Predicted values")

### MULTICOLLINEARITY ###

#variance inflation factor (VIF)

#this will get the VIF range
range(vif(tobit.Q2)[,1])

#this will get the mean VIF
y = sqrt(vif(tobit.Q2))
mean(y[,1]^2)

### NORMALITY OF RESIDUALS ###

hist(residuals(tobit.Q2), breaks = 50, main = "", xlab = "Residuals")
library(e1071)
skewness(residuals(tobit.Q2))
qqnorm(residuals(tobit.Q2), main = "")
qqline(residuals(tobit.Q2))

### INFLUENTIAL DATA POINTS ###

#(from this package: https://cran.r-project.org/web/packages/influence.ME/influence.ME.pdf)

library(influence.ME)

alt.est <- influence(Q2.results.full.model, group= "Participant")

#Cook's distances
cooks.distance.estex(alt.est) 
mean(cooks.distance(alt.est))
sd(cooks.distance(alt.est))
range(cooks.distance(alt.est))
hist(cooks.distance(alt.est), breaks = 30, main = "", xlab = "Cook's distance")

### LEVEL-TWO RESIDUAL NORMALITY ###

#(from: http://www.stats.ox.ac.uk/~snijders/ch10.r)

vrtm.re <- ranef(Q2.results.full.model, condVar = TRUE, standard = TRUE)

str(vrtm.re)

#get a dataset of all non-standardized Empirical Bayes (EB) residuals (use this for level-two fixed effects; Snijders & Bosker, 2012; p. 64)
postmean <- vrtm.re$Participant[,1]

#posterior variances
postmeanvar <- attr(vrtm.re$Participant, 'postVar')[1,1,]

#diagnostic variances
diagmeanvar <- VarCorr(Q2.results.full.model)$Participant[1,1] - postmeanvar

#level-two normality
postmean.stand <- postmean/sqrt(diagmeanvar)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)
shapiro.test(postmean.stand)
