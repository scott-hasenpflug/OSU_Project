## README

Repository for OSU Salary Analysis project

## Variable Definitions

name.full = Full name

gender = Estimated gender assigned using data from the US SSA using employee's 
  first name. "Unknown" if not enough data to estimate (typically for uncommon 
  first names)
  
type.employee = Staff, Faculty, or Appointee ("Emeritus")

job.title = Job Title

pay.annual.adj = Annual salary rate adjusted for part time, hourly, and monthly
  compensation schedules
  
tenure.yrs = Number of full years eployed by OSU, rounded down

job.category = Which of 11 colleges or 14 administrative divisions the job falls
  under
  
loaner = TRUE if employee's job is outside their home organization

full.time = TRUE if a full-time employee

rank.admin = One of 26 Administrative Ranks, including None

rank.acad = One of 8 Academic Ranks, including None

id = Custom-generated employee ID built only for the purpose of this 
  analysis
  
tenure.log = TRUE if employee is in a tenured track

appointee = TRUE if employee is in an unpaid "Emeritus/Emerita" status

senior = TRUE if "Senior" appears in their job title

first.hired = Date first hired

date.adj.serv = Date of initial employment at OSU, or in some cases, the State 
  of Oregon 
  
date.rank.eff = Date the employee achieved their current rank

date.appt.begin = Date the job originally started. This date is not changed 
  when a job is reactivated after summer leave
  
date.appt.end = The date the job is scheduled to end.  If null, the job is 
  indefinite
  
pay.annual = Salary equivalent of this job at full time (100% appointment)

pay.monthly= Monthly salary if applicable

pay.hourly = Hourly salary if applicable

percent.time = Indicates the percentage of full time the position uses

posn.code = Position code. Unknown utility for now

pay.monthly.equiv = Monthly salary equivalent for hourly employees

bin.tenure = Custom bins indicating years of employment in 5 year chunks

bin.pay = Custom bins indicating pay level based on pay.annual.adj

job.type = Job type coded as P (primary), S (secondary) and O (overload)

name.first = Separated first name

name.last = Separated last name

home.code = Home organization's code designation

job.code = Job organization's code designation

home.org = Home organization (over 250 types)

home.category = Which of 11 colleges or 14 administrative divisions the home
  organization falls under
  
job.org = Job organization (over 250 types)

contract.length = Length or type of contract

rank.name = Rank as pulled from document before adjustment

## Faculty PDF Definitions

FACULTY are employees who hold ranked academic positions (faculty members responsible for instruction or research), or unranked professional faculty positions (employees whose primary assignments are administrative tasks related to the operation of the university).

Note: An employee may have more than one job, but may not be receiving pay from all jobs.

ADJ SERVICE DATE:
For classified employees, this is the initial date of employment into a classified or unclassified position with OUS. If the employee was employed by OUS or another state agency in a classified position prior to July 1, 1996, this date reflects prior state service.

ANNUAL SALARY RATE:
This is the salary equivalent of this job at full time (100% appointment).

APPT BASIS:
The number of months on the appointment that the employee will work during a full year.

APPT BEGIN DATE:
The date the job originally started. This date is not changed when a job is reactivated after summer leave

APPT END DATE:
The date the job is scheduled to end.  If null, the job is indefinite.

APPT PERCENT:
Indicates the percentage of full time the position uses

FIRST HIRED:
The date the employee first started with OSU in any position.

HOME ORGN:
The organization code associated with the department the employee reports to.  If the employee has more than one job the Home Org is the org associated with the job which provides the highest level of benefits and leave eligibility.

JOB ORGN:
Organization code to which this job reports; the department responsible for time or leave entry on this job

JOB TITLE:
For classified employees job title reflects the position class title.  For unclassified employees this reflects the working title.

JOB TYPE:
Job type coded as P (primary), S (secondary) and O (overload).

RANK:
Descriptive title associated with the Faculty Rank Code. Example: Professor

RANK EFFECTIVE DATE:
The date the faculty member achieved the current rank


# Staff PDF Definitions

STAFF are employees represented by the Service Employees International Union (SEIU), Local 503, OPEU.

Note: An employee may have more than one job, but may not be receiving pay from all jobs.

ADJ SERVICE DATE:
For classified employees, this is the initial date of employment into a classified or unclassified position with OUS. If the employee was employed by OUS or another state agency in a classified position prior to July 1, 1996, this date reflects prior state service.

APPT PERCENT:
Indicates the percentage of full time the position uses

APPT:
A description associated with the specific Position Appt Type Code.

FIRST HIRED:
The date the employee first started with OSU in any position.

FULL-TIME MONTHLY SALARY:
The monthly rate corresponding to a Salary Grade and Step

HOME ORGN:
The organization code associated with the department the employee reports to.  If the employee has more than one job the Home Org is the org associated with the job which provides the highest level of benefits and leave eligibility.

HOURLY RATE:
The hourly rate of pay corresponding to a Salary Grade and Step

JOB ORGN:
Organization code to which this job reports; the department responsible for time or leave entry on this job

JOB TITLE:
For classified employees job title reflects the position class title.  For unclassified employees this reflects the working title.

JOB TYPE:
Job type coded as P (primary), S (secondary) and O (overload).

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt=
"Creative Commons License" style="border-width:0" src="https://i.creativecommons
.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel=
"license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons 
Attribution 4.0 International License</a>.
