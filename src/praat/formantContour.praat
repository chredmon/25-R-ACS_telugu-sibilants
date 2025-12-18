########## CONTROL VARIABLES ###############
seg_tier = 1
meta_tier = 2
directory$ = "/media/redmon/BASE/data/Telugu/2017_RJZ_TEL-Sib-System/audio1/femaleSet"
dataPath$ = "/media/redmon/BASE/projects/2017_RJZ_TEL-Sibilant-System/data"
dataFile$ = "femaleFormantData.txt"
sex$ = "female"
#######################################

strings = Create Strings as file list: "list", directory$ + "/*.wav"
nF = Get number of strings

clearinfo
writeInfoLine: "Meta", tab$, "V1", tab$, "C", tab$, "V2", tab$, "v1F2_1", tab$, "v1F2_2", tab$, "v1F2_3", tab$, "v1F2_4", tab$, "v1F2_5", tab$, "v1F3_1", tab$, "v1F3_2", tab$, "v1F3_3", tab$, "v1F3_4", tab$, "v1F3_5", tab$, "v2F2_1", tab$, "v2F2_2", tab$, "v2F2_3", tab$, "v2F2_4", tab$, "v2F2_5", tab$, "v2F3_1", tab$, "v2F3_2", tab$, "v2F3_3", tab$, "v2F3_4", tab$, "v2F3_5"

for iF from 1 to nF

	selectObject: strings
	fileName$ = Get string: iF
	Read from file: directory$ + "/" + fileName$
	fileName$ = selected$("Sound")
	Read from file: directory$ + "/" + fileName$ + ".TextGrid"
	selectObject: "Sound " + fileName$, "TextGrid " + fileName$
	Scale times

	sound = selected("Sound")
	textgrid = selected("TextGrid")

	selectObject: sound
	if sex$ = "female"
		selectObject: sound
		formant_i = To Formant (burg): 0, 5, 5500, 0.025, 50
		selectObject: sound
		formant_e = To Formant (burg): 0, 5, 5500, 0.025, 50
		selectObject: sound
		formant_a = To Formant (burg): 0, 5, 5500, 0.025, 50
		selectObject: sound
		formant_o = To Formant (burg): 0, 5, 5100, 0.025, 50
		selectObject: sound
		formant_u = To Formant (burg): 0, 5, 5200, 0.025, 50
	elsif sex$ = "male"
		selectObject: sound
		formant_i = To Formant (burg): 0, 5, 5800, 0.025, 50
		selectObject: sound
		formant_e = To Formant (burg): 0, 5, 5800, 0.025, 50
		selectObject: sound
		formant_a = To Formant (burg): 0, 5, 5800, 0.025, 50
		selectObject: sound
		formant_o = To Formant (burg): 0, 5, 4300, 0.025, 50
		selectObject: sound
		formant_u = To Formant (burg): 0, 5, 4300, 0.025, 50
	endif

	v1F2_1 = 0
	v1F2_2 = 0
	v1F2_3 = 0
	v1F2_4 = 0
	v1F2_5 = 0
	v1F3_1 = 0
	v1F3_2 = 0
	v1F3_3 = 0
	v1F3_4 = 0
	v1F3_5 = 0
	v2F2_1 = 0
	v2F2_2 = 0
	v2F2_3 = 0
	v2F2_4 = 0
	v2F2_5 = 0
	v2F3_1 = 0
	v2F3_2 = 0
	v2F3_3 = 0
	v2F3_4 = 0
	v2F3_5 = 0

	selectObject: textgrid
	mLabel$ = Get label of interval: meta_tier, 2
	v1label$ = "NA"
	v2label$ = "NA"
	n = Get number of intervals: seg_tier

	for i from 1 to n
		if (i < n)
			selectObject: textgrid
			sLabel$ = Get label of interval: seg_tier, i
			nextLabel$ = Get label of interval: seg_tier, i+1
			if (sLabel$ <> "" and sLabel$ <> "s" and sLabel$ <> "S" and sLabel$ <> "sh")

				v_start = Get start point: seg_tier, i
				v_end = Get end point: seg_tier, i
				v_mid = (v_start + v_end)/2
				v_dur = v_end - v_start
			
				if (sLabel$ = "a" or sLabel$ = "aa" or sLabel$ = "6")
					formant = formant_a
				elsif (sLabel$ = "i" or sLabel$ = "ii" or sLabel$ = "I")
					formant = formant_i
				elsif (sLabel$ = "e" or sLabel$ = "ee")
					formant = formant_e
				elsif (sLabel$ = "o" or sLabel$ = "oo")
					formant = formant_o
				elsif (sLabel$ = "u" or sLabel$ = "uu" or sLabel$ = "U")
					formant = formant_u
				endif

				if (nextLabel$ = "")
					v2label$ = sLabel$
					cLabel$ = Get label of interval: seg_tier, i - 1
					t1 = v_start + 0.1*v_dur
					t2 = v_start + 0.2*v_dur
					t3 = v_start + 0.3*v_dur
					t4 = v_start + 0.4*v_dur
					t5 = v_start + 0.5*v_dur
					selectObject: formant
					v2F2_1 = Get value at time: 2, t1, "Hertz", "Linear"
					v2F3_1 = Get value at time: 3, t1, "Hertz", "Linear"
					v2F2_2 = Get value at time: 2, t2, "Hertz", "Linear"
					v2F3_2 = Get value at time: 3, t2, "Hertz", "Linear"
					v2F2_3 = Get value at time: 2, t3, "Hertz", "Linear"
					v2F3_3 = Get value at time: 3, t3, "Hertz", "Linear"
					v2F2_4 = Get value at time: 2, t4, "Hertz", "Linear"
					v2F3_4 = Get value at time: 3, t4, "Hertz", "Linear"
					v2F2_5 = Get value at time: 2, t5, "Hertz", "Linear"
					v2F3_5 = Get value at time: 3, t5, "Hertz", "Linear"
				elsif (nextLabel$ = "s" or nextLabel$ = "S" or nextLabel$ = "sh")
					v1label$ = sLabel$
					cLabel$ = nextLabel$
					t1 = v_start + 0.5*v_dur
					t2 = v_start + 0.6*v_dur
					t3 = v_start + 0.7*v_dur
					t4 = v_start + 0.8*v_dur
					t5 = v_start + 0.9*v_dur
					selectObject: formant
					v1F2_1 = Get value at time: 2, t1, "Hertz", "Linear"
					v1F3_1 = Get value at time: 3, t1, "Hertz", "Linear"
					v1F2_2 = Get value at time: 2, t2, "Hertz", "Linear"
					v1F3_2 = Get value at time: 3, t2, "Hertz", "Linear"
					v1F2_3 = Get value at time: 2, t3, "Hertz", "Linear"
					v1F3_3 = Get value at time: 3, t3, "Hertz", "Linear"
					v1F2_4 = Get value at time: 2, t4, "Hertz", "Linear"
					v1F3_4 = Get value at time: 3, t4, "Hertz", "Linear"
					v1F2_5 = Get value at time: 2, t5, "Hertz", "Linear"
					v1F3_5 = Get value at time: 3, t5, "Hertz", "Linear"
				endif
		
			endif

		endif

	endfor

	appendInfoLine: mLabel$, tab$, v1label$, tab$, cLabel$, tab$, v2label$, tab$, fixed$(v1F2_1, 1), tab$, fixed$(v1F2_2, 1), tab$, fixed$(v1F2_3, 1), tab$, fixed$(v1F2_4, 1), tab$, fixed$(v1F2_5, 1), tab$, fixed$(v1F3_1, 1), tab$, fixed$(v1F3_2, 1), tab$, fixed$(v1F3_3, 1), tab$, fixed$(v1F3_4, 1), tab$, fixed$(v1F3_5, 1), tab$, fixed$(v2F2_1, 1), tab$, fixed$(v2F2_2, 1), tab$, fixed$(v2F2_3, 1), tab$, fixed$(v2F2_4, 1), tab$, fixed$(v2F2_5, 1), tab$, fixed$(v2F3_1, 1), tab$, fixed$(v2F3_2, 1), tab$, fixed$(v2F3_3, 1), tab$, fixed$(v2F3_4, 1), tab$, fixed$(v2F3_5, 1)
	appendFile: dataPath$ + "/" + dataFile$, info$()
	clearinfo
	removeObject: sound, textgrid, formant_a, formant_i, formant_e, formant_o, formant_u

endfor
