########## CONTROL VARIABLES ##########
segTier = 1
metaTier = 2
directory$ = "/media/redmon/BASE/data/Telugu/2017_RJZ_TEL-Sib-System/audio1/items"
dataPath$ = "/media/redmon/BASE/projects/2017_RJZ_TEL-Sibilant-System/data"
dataFile$ = "consonantMeasures.txt"
#######################################

strings = Create Strings as file list: "list", directory$ + "/*.wav"
nF = Get number of strings

clearinfo
writeInfoLine: "Meta", tab$, "V1", tab$, "C", tab$, "V2", tab$, "amp", tab$, "peakF", tab$, "tiltLF", tab$, "tiltHF", tab$, "mu1", tab$, "mu2", tab$, "mu3", tab$, "mu4"

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
	sound = Filter (pass Hann band): 500, 10525, 50

	selectObject: textgrid
	metaLabel$ = Get label of interval: metaTier, 2
	nSeg = Get number of intervals: segTier

	for i from 1 to nSeg

		selectObject: textgrid
		label$ = Get label of interval: segTier, i		

		if (label$ = "s" | label$ = "S" | label$ = "sh")

			c$ = label$
			v1$ = Get label of interval: segTier, i - 1
			v2$ = Get label of interval: segTier, i + 1
			if (v1$ = "")
				v1$ = "NA"
			endif
			if (v2$ = "")
				v2$ = "NA"
			endif

			cStart = Get start point: segTier, i
			cEnd  = Get end point: segTier, i
			cDur = cEnd - cStart
			cMid = cStart + 0.5*cDur
			selectObject: sound
			win = Extract part: cMid - 0.02, cMid + 0.02, "Hanning", 1, "yes"

			selectObject: win
			amp0 = Get root-mean-square: 0, 0
			amp = 20*log10(amp0/0.00002)
			
			spec = To Spectrum: "yes"
			selectObject: spec
			mu1 = Get centre of gravity: 2
			mu2 = Get standard deviation: 2
			mu3 = Get skewness: 2
			mu4 = Get kurtosis: 2
			
			ltas = To Ltas (1-to-1)
			selectObject: ltas
			peakF = Get frequency of maximum: 500, 10525, "Cubic"

			if (peakF < 600)
				lft = 0
				reportHF$ = Report spectral tilt: peakF, 10525, "Linear", "Robust"
				hft = extractNumber(reportHF$, "Slope: ")
			elsif (peakF > 10400)
				hft = 0
				reportLF$ = Report spectral tilt: 500, peakF, "Linear", "Robust"
				lft = extractNumber(reportLF$, "Slope: ")
			else
				reportHF$ = Report spectral tilt: peakF, 10525, "Linear", "Robust"
				hft = extractNumber(reportHF$, "Slope: ")
				reportLF$ = Report spectral tilt: 500, peakF, "Linear", "Robust"
				lft = extractNumber(reportLF$, "Slope: ")
			endif
			
			appendInfoLine: metaLabel$, tab$, v1$, tab$, c$, tab$, v2$, tab$, fixed$(amp, 2), tab$, fixed$(peakF, 1), tab$, fixed$(lft, 3), tab$, fixed$(hft, 3), tab$, fixed$(mu1, 1), tab$, fixed$(mu2, 1), tab$, fixed$(mu3, 3), tab$, fixed$(mu4, 3)
			removeObject: win, spec, ltas

		endif

	endfor

	removeObject: sound0, sound, textgrid

endfor

appendFile: dataPath$ + "/" + dataFile$, info$()
