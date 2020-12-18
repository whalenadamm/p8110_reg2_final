***********************************
* P8110 Applied Regression II
* Fall 2020
* Final Project
* Group 5:
* 	Maya Bunyan (mb4650)
*	Lin Feng (lf2649)
*	Chuchu Mei (cm3930)
* 	Adam Whalen (amw2275)
***********************************;

***********************************
* Set Up and Univariate Analysis
***********************************;

/* This data set contains results of an RCT evaluating the 
effect of an educational intervention on the self-rated health
of women of childbearing age. */

/* Import the data and convert to numeric values for analysis */

proc import 
	datafile = "/home/u45121442/Regression 2/Final/finalProject.csv"
	out = final
	dbms = csv replace;
	getnames = yes;
	datarow = 2;
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

/* Check for missingness; can we still use GEE? */
proc freq data = final;
table hlth*time;
run;
	/* Some missing data present, particularly for later observations. 
	But it's not substantial enough that we would need to use a mixed
	effects model. GEE should be fine. */

/* Check distribution of the covariates */
proc freq data = final;
tables agecat hlth;
run;
	/* The third age group is very small compared to the other two.
	We will condense into the second to prevent convergence issues. */

/* Condense age categories */
data final;
set final;
	if agecat = 1 then age = 1;
	else if agecat = 2 then age = 2;
	else if agecat = 3 then age = 2;
	else age = .;
run;

/* Check distribution of the adjusted variables */
proc freq data = final;
table trt*time / chisq measures;
run;

proc freq data = final;
where time = 1;
table trt hlth age trt*(hlth age) / chisq measures;
run;

/* Build univariate analysis tables */
proc tabulate data = final;
class time trt age;
table (all trt age), (time all)*(n colpctn);
run;

proc tabulate data = final;
class time hlth age;
table (all time age), (hlth all)*(n colpctn);
run;


***********************************
* Model Building and Analysis
***********************************;

/* Because we are measuring a binary outcome with repeated measures,
we will use a generalized estimating equation (GEE) model with a 
logit link and binary outcomne distribution. */

/* Start with fully saturated model, including three-way interaction.
We will initially use an AR(1) working correlation matrix, because our
theory is that measures that are closer in time will be more similar
than measures that are further apart. */

/* Initial model */
proc genmod data = final desc;
class time (ref = '1') id trt (ref = '0') 
	agecat (ref = '1') / param = glm;
model hlth = trt time age trt*time trt*age time*age 
	trt*time*age / dist = bin link = logit type3 wald;
repeated subject = id / type = ar(1) within = time modelse;
run;
	/* The three-way interaction term is not significant, so let's remove it. */

/* Model 2 */
proc genmod data = final desc;
class time (ref = '1') id trt (ref = '0') 
	agecat (ref = '1') / param = glm;
model hlth = trt time age trt*time trt*age time*age / dist = bin link = logit type3 wald;
repeated subject = id / type = ar(1) within = time modelse;
run;
	/* The interaction between age group and treatment is also not 
	statistically significant, so we will remove it. Additionally, 
	the interaction term between age and time does not meaningfully
	add any additional explanation for our causal theory, so we will
	remove that as well. We should be left only with main effects and
	the interaction between treatment and time. */
	
/* Now that we have a parsimonious model, we will evaluate the QIC 
for different working correlation matrices to determine the best fit.
Smaller QIC means better fit for the data. */

/* AR(1) WC Model */
proc genmod data = final desc;
class time (ref = '1') id trt (ref = '0') 
	agecat (ref = '1') / param = glm;
model hlth = trt time age trt*time / dist = bin link = logit type3 wald;
repeated subject = id / type = ar(1) within = time modelse;
run;
	*QIC: 340.5094;

/* Compound Symmetry WC Model */
proc genmod data = final desc;
class time (ref = '1') id trt (ref = '0') 
	agecat (ref = '1') / param = glm;
model hlth = trt time age trt*time / dist = bin link = logit type3 wald;
repeated subject = id / type = cs within = time modelse;
run;
	*QIC: 340.0733;

/* Unstructured WC Model */
proc genmod data = final desc;
class time (ref = '1') id trt (ref = '0') 
	agecat (ref = '1') / param = glm;
model hlth = trt time age trt*time / dist = bin link = logit type3 wald;
repeated subject = id / type = unstr within = time modelse;
run;
	*QIC: 340.8317;

/* The model using a compound symmetry working correlation matrix
had the smallest QIC, so we will use the results of that model
for our analysis as our final model. */
