/* Import the data */

proc import 
	datafile = "/home/u45121442/Regression 2/Final/finalProject.csv"
	out = final
	dbms = csv replace;
	getnames = yes;
	datarow = 2;
run;

data final;
set final;
run;

data final;
set final;
	*treatment;
	if txt = "Intervention" then trt = 1;
	else if txt = "Control" then trt = 0;
	else trt = .;
	*health;
	if health = "Good" then hlth = 1;
	else if health = "Poor" then hlth = 0;
	else hlth = .;
	*agegroup;
	if agegroup = "15-24" then agecat = 1;
	else if agegroup = "25-34" then agecat = 2;
	else if agegroup = "35+" then agecat = 3;
	else agecat = .;
run;
	
/* check for missingness; can we still use GLM? */
proc freq data = final;
table hlth*time;
run;
*fine to use GLM, missingness is not a huge concern;

/* check distribution of the covariates */
proc freq data = final;
tables agecat hlth;
run;
*third age group is very small compared to the other two;
*condense into the second;

data final;
set final;
	if agecat = 1 then age = 1;
	else if agecat = 2 then age = 2;
	else if agecat = 3 then age = 2;
	else age = .;
run;


/* table 1 statistics */
proc tabulate data = final missing;
class time trt hlth age;
table (all time trt age), (hlth all)*(n colpctn);
run;


/* Run the model */
	*using AR(1) wc matrix because it fits our theory;
		*measurements are less similar as they get further apart in time;
ods graphics on;
proc genmod data = final desc plots(unpack) = dfbeta;
class time (ref = '1') id trt (ref = '0') 
	agecat (ref = '1') / param = glm;
model hlth = trt time age trt*time trt*age time*age 
	trt*time*age / dist = bin link = logit type3 wald;
repeated subject = id / type = ar(1) within = time modelse;
run;
ods graphics off;

/* the three-way interaction term is not significant on any level. let's take it out */
ods graphics on;
proc genmod data = final desc;
class time (ref = '1') id trt (ref = '0') 
	agecat (ref = '1') / param = glm;
model hlth = trt time age trt*time trt*age time*age / dist = bin link = logit type3 wald;
repeated subject = id / type = ar(1) within = time modelse;
run;
ods graphics off;

/* the interaction between age and trt is not significant either. however, 
all of the other variables are significant for at least one of their levels
or at the global level, so we will keep the rest. now, let's determine which
wc matrix has the highest QIC */

/* model 1: AR(1) wc matrix */
ods graphics on;
proc genmod data = final desc;
class time (ref = '1') id trt (ref = '0') 
	agecat (ref = '1') / param = glm;
model hlth = trt time age trt*time time*age / dist = bin link = logit type3 wald;
repeated subject = id / type = ar(1) within = time modelse;
run;
ods graphics off;
	*QIC = 337.1355;

/* model 2: compound symmetry */
ods graphics on;
proc genmod data = final desc;
class time (ref = '1') id trt (ref = '0') 
	agecat (ref = '1') / param = glm;
model hlth = trt time age trt*time time*age / dist = bin link = logit type3 wald;
repeated subject = id / type = cs within = time modelse;
run;
ods graphics off;
	*QIC = 336.7727;

/* model 3: unstructured */
ods graphics on;
proc genmod data = final desc;
class time (ref = '1') id trt (ref = '0') 
	agecat (ref = '1') / param = glm;
model hlth = trt time age trt*time time*age / dist = bin link = logit type3 wald;
repeated subject = id / type = unstr within = time modelse;
run;
ods graphics off;
	*QIC = 337.6207;
	
/* the cs model is the best fit; that is our final model */
