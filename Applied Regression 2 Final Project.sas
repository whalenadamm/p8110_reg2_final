/* Applied Regression 2 
   Final Project        */
  
/* Import the data */
proc import datafile= "/home/u45120387/sasuser.v94/finalProject.csv"
out = final
dbms = csv replace;
getnames = yes;
run;

data final;
set final;
if txt = "Control" then trt = 0;
else if txt = "Intervention" then trt = 1;
if health = "Good" then hlth = 1;
else if health = "Poor" then hlth = 0;
if agegroup = '15-24' then age = 1;
else if agegroup = '25-34' then age = 2;
else if agegroup = '35+' then age = 3;
run;
 
/* Descriptive statistics */
proc freq data = final;
table txt health agegroup health*(txt agegroup)/ chisq measures;
run;


/* Create GEE model and test to see which correlation matrix is appropriate */
/* UN assumption */
proc genmod data=final descending;
class trt (ref = '0') time (ref = '1') id (ref = first) age (ref = '1')/param=ref;
model hlth = trt time age trt*time time*age trt*age trt*time*age /dist=nor link=identity type3 wald;
repeated subject=id /type=un modelse;
run;
/* QIC = 295.0176 */

/* CS assumption */
proc genmod data=final descending;
class trt (ref = '0') time (ref = '1') id (ref = first) age (ref = '1')/param=ref;
model hlth = trt time age trt*time time*age trt*age trt*time*age /dist=nor link=identity type3 wald;
repeated subject=id /type=cs modelse;
run;
/* QIC = 294.1921 */

/* AR(1) assumption */
proc genmod data=final descending;
class trt (ref = '0') time (ref = '1') id (ref = first) age (ref = '1')/param=ref;
model hlth = trt time age trt*time time*age trt*age trt*time*age /dist=nor link=identity type3 wald;
repeated subject=id /type=AR(1) modelse;
run;
/* QIC = 294.5520 */

/* Best to use model with CS as the correlation matrix */


