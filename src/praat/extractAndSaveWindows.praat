segTier = 1
metaTier = 2
directory$ = "/media/redmon/BASE/data/Telugu/2017_RJZ_TEL-Sib-System/audio1/items"
outdir$ = "/media/redmon/BASE/data/Telugu/2017_RJZ_TEL-Sib-System/audio1/consonantWindows"
extension$ = ".wav"

strings = Create Strings as file list: "list", directory$ + "/*.wav"
nF = Get number of strings

for iF to nF

	selectObject: strings
	fileName$ = Get string: iF
	Read from file: directory$ + "/" + fileName$
	fileName$ = selected$("Sound")
	Read from file: directory$ + "/" + fileName$ + ".TextGrid"
	selectObject: "Sound " + fileName$, "TextGrid " + fileName$
	Scale times

	sound0 = selected("Sound")
	textgrid = selected("TextGrid")

	selectObject: sound0
	sound = Filter (pass Hann band): 500, 0, 50

	selectObject: textgrid
	nSeg = Get number of intervals: segTier

	for i from 1 to nSeg

		selectObject: textgrid
		label$ = Get label of interval: segTier, i

		if (label$ = "s" | label$ = "S" | label$ = "sh")

			cStart = Get start point: segTier, i
			cEnd  = Get end point: segTier, i
			cDur = cEnd - cStart
			cMid = cStart + 0.5*cDur
			selectObject: sound
			win = Extract part: cMid - 0.02, cMid + 0.02, "Hanning", 1, "yes"

			selectObject: win
     			Write to WAV file: outdir$ + "/" + fileName$ + ".wav"

			removeObject: win

		endif

	endfor

	removeObject: sound0, sound, textgrid

endfor


