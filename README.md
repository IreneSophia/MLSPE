# ML_speech

This repository contains the preprocessing pipeline from the *ML_speech* study. This pipeline has been developed by me, Irene Sophia Plank, at the NEVIA lab (https://nevialab.com/). It also contains code from an earlier version by Afton Nelson. The study has been preregistrated on OSF (https://osf.io/jhetr), and investigates speech and interactional features of autistic and non-autistic people in dyadic communication. In this project, we aim to use automatic extraction of speech (acoustic and prosodic) as well as interactional features.

These results have been published: 
Plank IS, Koehler JC, Nelson AM, Koutsouleris N and Falter-Wagner CM (2023) Automated extraction of speech and turn-taking parameters in autism allows for diagnostic classification using a multivariable prediction model. Front. Psychiatry. 14:1257569. doi: 10.3389/fpsyt.2023.1257569

The data was collected at the Medical Faculty of the LMU Munich and is part of a larger machine learning project on autism. Strangers were paired in either heterogeneous (one autistic, one non-autistic interaction partner) or homogeneous (two non-autistic interaction partners) dyads and asked to have two ten-minute conversations, one about their hobbies and one to collaboratively plan a menu with foods and drinks that they both dislike. We collected audio, video and psychophysiological data during these conversation. The analysis of the facial expressions and the body movement has been published as a preprint (Koehler et al., 2022: https://doi.org/10.1101/2022.12.20.22283571).

The preprocessing pipeline uses praat, python and R. It was developed by Irene Sophia Plank based on an earlier version from Afton Nelson. It uses the uhm-o-meter by De Jonge and colleagues (https://sites.google.com/view/uhm-o-meter). In each script, the paths need to be adjusted to where the data is stored. The data should be in .wav format and contain one channel per participant or alternatively two .wav files containing one participant each.All output is saved in the same folder as the data. 

## Overview
![speech_pipeline](https://user-images.githubusercontent.com/40594634/214036786-319e1aea-abe2-4e58-b33f-9e32c91d1447.PNG)


## Scripts

The scripts start with a number indicating the order of execution:

0. This praat script separates audio channels. If the participants' audio is already saved in two separate files, this step can be skipped
1. This praat script estimates pitch floor and ceiling
2. This python script chooses one floor and ceiling per participant: since there are two conditions in this study, separate limits are extracted for both conditions. This script always chooses one per participant by taking the more extreme option.
3. This praat script extracts:
	- composite pitch and intensity information based on individual settings (Hirst, 2011)
	- continuous pitch and intensity using the same parameters for all individuals
  
Between the third and the fouth script, the uhm-o-meter should be used to extract syllable nuclei, thereby, giving information about sound and silence.

4. This python scripts transforms the praat output textgrids to csvs
5. This R script identifies turn-based information
6. This R script computes synchrony values
7. This R script merges all the available information to two dataframes, one on the dyad and one on the individual level
