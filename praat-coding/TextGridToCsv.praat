####
#### Praat script TextGridToCsv
#### Version 1.01
#### Version date: November 12, 2020
#### Dan Villarreal (d.vill@pitt.edu)
####
#### Reads a TextGrid formatted for variationist coding and creates a csv file.
#### A single TextGrid must be selected in the Objects window, or must be the 
#### only TextGrid in the Objects window). Any tiers that are specified to be
#### included in the csv file must have identical aligned intervals, with
#### identical patterns of filled/empty intervals.
####
#### This script is best used with Praat version at least 6.1.28, as this will
#### allow the user to see tier names in the settings window created by the
#### script, but it should work with less recent versions of Praat.
####

##Check Praat version (version 6.1.28 allows multiline text display in pause window)
multilineTiers = 0
dot1 = index(praatVersion$, ".")
majorVers = number(left$(praatVersion$, dot1-1))
if majorVers >= 7
	multilineTiers = 1
elsif majorVers >= 6
	nonMajorVers$ = right$(praatVersion$, length(praatVersion$)-dot1)
	dot2 = index(nonMajorVers$, ".")
	if dot2 > 0
		minorVers = number(left$(nonMajorVers$, dot2-1))
		buildVers = number(replace_regex$(right$(nonMajorVers$, length(nonMajorVers$)-dot2), "[^0-9]", "", 0))
	else
		minorVers = number(replace_regex$(nonMajorVers$, "[^0-9]", "", 0))
		buildVers = 0
	endif
	if minorVers >= 2
		multilineTiers = 1
	elsif minorVers = 1 and buildVers >= 28
		multilineTiers = 1
	endif
endif

##Select TextGrid
numTGs = numberOfSelected("TextGrid")
if numTGs = 1
	tg = selected("TextGrid")
else 
	select all
	numTGs = numberOfSelected("TextGrid")
	if numTGs = 1
		tg = selected("TextGrid")
	else
		exitScript: "You must select one TextGrid before running the script."
	endif
endif
selectObject: tg
tgName$ = selected$("TextGrid")

##Ask user which tiers to include in the csv, and output file
numTiers = Get number of tiers
tiersNames$ = "Tiers in " + tgName$ + ":"
tierNums$ = ""
for tier from 1 to numTiers
	tierNm$ = Get tier name: tier
	tiersNames$ += (newline$ + string$(tier) + ". " + tierNm$)
	tierNums$ += (string$(tier) + " ")
endfor
tierNums$ = left$(tierNums$, length(tierNums$)-1)
##Format the pause window differently based on Praat version
if multilineTiers
	beginPause: "Select tiers"
		text: "tierNames", tiersNames$, numTiers+1
		comment: "Select tiers to extract by entering tier numbers with spaces in between."
		sentence: "Tiers to extract", tierNums$
		word: "Output csv file", tgName$ + ".csv"
	endPause: "Continue", 1
else
	beginPause: "Select tiers"
		comment: "Select tiers to extract by entering tier numbers with spaces in between."
		sentence: "Tiers to extract", tierNums$
		word: "Output csv file", tgName$ + ".csv"
	endPause: "Continue", 1
endif

##Error handling: Ensure tiers_to_extract$ is correctly formatted
if tiers_to_extract$ = ""
	exitScript: "'Tiers to extract' must not be empty."
endif
if index_regex(tiers_to_extract$, "[^0-9 ]") > 0
	exitScript: "'Tiers to extract' must contain only tier numbers and spaces."
endif
if index_regex(tiers_to_extract$, "\d") = 0
	exitScript: "'Tiers to extract' must contain tier numbers."
endif

##Get selected tiers
##Strip leading whitespace
tiers_to_extract$ = replace_regex$(tiers_to_extract$, "^\s+", "", 0)
numSelected = 0
tiers_to_extract$ += " "
while length(tiers_to_extract$) > 0
	##Get selected tier
	numSelected += 1
	digRight = index(tiers_to_extract$, " ")
	selTier[numSelected] = number(left$(tiers_to_extract$, digRight-1))
	##Head off errors w/ nonexistent tiers
	if selTier[numSelected] > numTiers
		exitScript: "TextGrid " + tgName$ + " has no tier " + string$(selTier[numSelected]x) + "."
	endif
	##Strip digits and whitespace from start of string
	tiers_to_extract$ = replace_regex$(tiers_to_extract$, "^\d+\s+", "", 0)
endwhile

##Error handling: check that all selected tiers are aligned to one another and have the same filled/empty intervals
if numSelected > 1
	##Check that all selected tiers have the same number of intervals (thanks to the transitive property)
	for ctr from 1 to numSelected
		tier = selTier[ctr]
		numInts[tier] = Get number of intervals: tier
	endfor
	for ctr from 2 to numSelected
		tier1 = selTier[1]
		tier2 = selTier[ctr]
		if numInts[tier2] <> numInts[tier1]
			exitScript: "All selected tiers must have identical intervals. Detected differences between number of intervals in tiers " + string$(tier1) + " and " + string$(tier2) + "."
		endif
	endfor
	numInts = numInts[selTier[1]]
	
	##Check that all selected tiers are aligned
	for ctr1 from 1 to numSelected
		tier1 = selTier[ctr1]
		
		for ctr2 from 2 to numSelected
			tier2 = selTier[ctr2]
			
			if tier1 < tier2
				for int from 1 to numInts
					##Check start times
					tier1Start = Get start time of interval: tier1, int
					tier2Start = Get start time of interval: tier2, int
					if tier1Start <> tier2Start
						exitScript: "All selected tiers must have identical intervals. Detected differences between tiers " + string$(tier1) + " and " + string$(tier2) + " in start time of interval " + string$(int) + "."
					endif
					
					##Check end times
					tier1End = Get end time of interval: tier1, int
					tier2End = Get end time of interval: tier2, int
					if tier1End <> tier2End
						exitScript: "All selected tiers must have identical intervals. Detected differences between tiers " + string$(tier1) + " and " + string$(tier2) + " in end time of interval " + string$(int) + "."
					endif
					
					##Check filledness of labels
					tier1Label$ = Get label of interval: tier1, int
					# tier1LabelFilled = (length(tier1Label$) > 0)
					tier1LabelFilled = index_regex(tier1Label$, "\S")
					tier2Label$ = Get label of interval: tier2, int
					# tier2LabelFilled = (length(tier2Label$) > 0)
					tier2LabelFilled = index_regex(tier2Label$, "\S")
					if tier1LabelFilled + tier2LabelFilled = 1
						if tier1LabelFilled
							tier1Status$ = "filled"
						else
							tier1Status$ = "empty"
						endif
						if tier2LabelFilled
							tier2Status$ = "filled"
						else
							tier2Status$ = "empty"
						endif
						exitScript: "All selected tiers must have identical intervals. In interval " + string$(int) + ", tier " + string$(tier1) + " label is " + tier1Status$ + " and tier " + string$(tier2) + " label is " + tier2Status$ + "."
					endif
				endfor
			endif
		endfor
	endfor
else
	##If only one tier: still need to set numInts
	numInts = Get number of intervals: selTier[1]
endif

##Add data to table
##Initialize table
colNames$ = "TokenStart TokenEnd "
for ctr from 1 to numSelected
	tier = selTier[ctr]
	tierName$[tier] = Get tier name: tier
	tierName$[tier] = replace$(tierName$[tier], " ", "_", 0)
	colNames$ += (tierName$[tier] + " ")
endfor
colNames$ = left$(colNames$, length(colNames$)-1)
table = Create Table with column names: output_csv_file$, 0, colNames$
numRows = 0
##Loop through intervals and populate table
for int from 1 to numInts
	selectObject: tg
	intLabel$ = Get label of interval: selTier[1], int
	if intLabel$ <> ""
		##If the first selected tier has a nonempty interval label, add a new row and loop through selected tiers
		selectObject: table
		Append row
		numRows += 1
		##Add token timing
		selectObject: tg
		intStart = Get start time of interval: selTier[1], int
		intEnd = Get end time of interval: selTier[1], int
		selectObject: table
		Set numeric value: numRows, "TokenStart", intStart
		Set numeric value: numRows, "TokenEnd", intEnd
		
		##Add tier labels
		for ctr from 1 to numSelected
			##Get label
			tier = selTier[ctr]
			selectObject: tg
			label$ = Get label of interval: tier, int
			##Strip whitespace
			label$ = replace_regex$(label$, "^\s+", "", 0)
			label$ = replace_regex$(label$, "\s+$", "", 0)
			##Add to table
			selectObject: table
			Set string value: numRows, tierName$[tier], label$
		endfor
	endif
endfor

##Save Table as csv
##Check that output filename ends in ".csv"
if not endsWith(output_csv_file$, ".csv")
	output_csv_file$ += ".csv"
endif
selectObject: table
##Ask before overwriting file
if fileReadable(output_csv_file$)
	beginPause: "Overwrite file?"
		comment: output_csv_file$ + " already exists. Do you want to overwrite it?"
	continue = endPause: "Yes", "No", 1
	if continue = 1
		Save as comma-separated file: output_csv_file$
		beginPause: "File saved"
			comment: output_csv_file$ + " saved."
		endPause: "OK", 1
	else
		beginPause: "File not saved"
			comment: output_csv_file$ + " not overwritten."
		endPause: "OK", 1
	endif
else
	Save as comma-separated file: output_csv_file$
	beginPause: "File saved"
		comment: output_csv_file$ + " saved."
	endPause: "OK", 1
endif
