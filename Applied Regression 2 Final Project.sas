/* Applied Regression 2 
   Final Project        */
  
/* Import the data */
proc import datafile= "/home/u45120387/sasuser.v94/finalProject.csv"
out = final
dbms = csv replace;
getnames = yes;
run;

/* Descriptive statistics at baseline*/
proc freq data = final;
where time = 1;
table txt health agegroup txt*(health agegroup)/ chisq measures;
run;

/* Frequency in the 35+ age group is small so will combine with 25-34 age group. Converting values to numeric for analysis */

data final;
set final;
if txt = "Control" then trt = 0;
else if txt = "Intervention" then trt = 1;
if health = "Good" then hlth = 1;
else if health = "Poor" then hlth = 0;
if agegroup = '15-24' then age = 1;
else if agegroup = '25-34' then age = 2;
else if agegroup = '35+' then age = 2;
run;

/* Descriptive statistics at baseline with new coded variables*/
proc freq data=final;
table trt*time/ chisq measures;
run;

proc freq data = final;
where time = 1;
table trt hlth age trt*(hlth age)/ chisq measures;
run;

 
/* Run GEE model with three way interaction between treatment, time, and age. Will use AR(1) correlation matrix for now */
proc genmod data=final descending;
class trt (ref = '0') time (ref = '1') id (ref = first) age (ref = '1')/param=ref;
model hlth = trt time age trt*time time*age trt*age trt*time*age /dist=bin link=logit type3 wald;
repeated subject=id /type=AR(1) modelse within=time;
run;

/* Three-way interaction and interaction between treatment and age are not significant, so will remove from the model. 
Will also remove time*age interaction from model since it does not add value to the model*/

/* Create final GEE model and test to see which correlation matrix is appropriate */
/* UN assumption */
proc genmod data=final descending;
class trt (ref = '0') time (ref = '1') id (ref = first) age (ref = '1')/param=ref;
model hlth = trt time age trt*time /dist=bin link=logit type3 wald;
repeated subject=id /type=un modelse within=time;
run;
/* QIC = 340.8317 */

/* CS assumption */
proc genmod data=final descending;
class trt (ref = '0') time (ref = '1') id (ref = first) age (ref = '1')/param=ref;
model hlth = trt time age trt*time /dist=bin link=logit type3 wald;
repeated subject=id /type=cs modelse within=time;
run;
/* QIC = 340.0733 */

/* AR(1) assumption */
proc genmod data=final descending;
class trt (ref = '0') time (ref = '1') id (ref = first) age (ref = '1')/param=ref;
model hlth = trt time age trt*time /dist=bin link=logit type3 wald;
repeated subject=id /type=AR(1) modelse within=time;
run;
/* QIC = 340.5094 */

/* Best to use model with CS as the correlation matrix, since that model has the smallest QIC*/

proc glm data=final ;
class trt (ref = '0') time (ref = '1') id (ref = first) age (ref = '1') hlth (ref = '0');
model hlth = trt time age trt*time;
lsmeans trt*time / slice=trt cl pdiff e;
lsmeans trt*time / cl pdiff e;
lsmeans trt / cl pdiff e;
lsmeans time / cl pdiff e;
lsmeans age / cl pdiff e;
run;

proc logistic data=final;
class trt (ref = '0') time (ref = '1') id (ref = first) age (ref = '1') hlth (ref = '0')/ param=glm;
model hlth = trt time age trt*time;
oddsratio trt;
lsmeans trt / e diff oddsratio cl adjust=bon;
run;