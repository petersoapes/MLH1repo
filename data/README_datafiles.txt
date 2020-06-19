MLH1_Anon_formated:
Batch - I quantified MLH1 counts by anonymizing file names and pooling in manageable batch sizes (by sex, since that can't be anonymized).
Quality, 1(best) to 5(worst), 
n (number of bivalents, including XY in males).
nMLH1.foci  - doesn't include MLH1 on XY
achiasmate - number of bivalents without a CO.
asynapsis - number of bivalents with partial areas of incomplete synapsis.
REDO.crop - this was to keep track of if there was extra SC noise in the image. I had plans to go back and clean up all the images, but it was taking too much time.

DMC1_data:  
'bars' & 'left.over' column names refer to my annotated counting, marking 20 and 1 foci respectively. The image files are on my work computer, but should be sharable in the future. The stages 'early' and late reference zygotene. There is one P –(pachytene).


Total.SC_DF_5.31.20:
This is the merged MLH1 file and total SC measures from Richard's python script.
bin_size; 'binary size', total area after turning red channel binary.
skel_size;'skeleton size',  Sum of single pixel wide trace of bivalents.


Curated_BivData_5.30.20
These are notes from the curation process of confirming the accuracy of the values spit out by DNACrossOver by looking at the 'image' outputs from the software. The outputs show what the algorithm segmented as single objects from the whole cell image in indexed boxes.

ChromosomeLength - length of synapsed SC.
boxNumber - Index box number for a single bivalent object.
Obj.ID - Unique ID for each object (fileName_boxnumber).
SC.pass - If the object segments well, '1'. '0' otherwise. 
Foci.pass - If foci were segmented correctly '1', all 'real' foci ones and no extras. Note there are ~230 bivalents where the segmentation of bivalent shape is good, but foci didn't clearly pass. These were excluded from the paper analysis.
