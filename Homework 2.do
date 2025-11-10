clear all
set more off

*get name stata assigns to your computer
disp "`c(username)'"
local mypc = "`c(username)'"
disp "`mypc'"

*set root file directory
if "`c(username)'"=="c.justincook" {
	global filepath "/Users/c.justincook/Dropbox/Teaching/files_6100/data"
}
	
else if "`c(username)'"=="Justin" {
	global filepath "C:/Users/Justin/Dropbox/Teaching/files_6100/data"
}

else if "`c(username)'"=="navic"{
	global filepath "C:/Empirical Political Economy"
}

*set directory to data folder
cd "$filepath"

*Did not create locals, interacted dummys indiviual to be able to view each variable specifically for my own understanding 

*1a)
use "pwt100.dta", clear 
keep country year rgdpo emp
isid country year, sort
gen mex = country=="Mexico"
label var mex "Mexico=1"

*1b)
gen y_pw = rgdpo/emp
qui su y_pw if country=="Mexico" & year==1980
display r(mean)
*The mean for this variable is 47290.195.

*1c)
reg y_pw i.mex if year == 1980
lincom _cons + 1.mex
*The coefficient is 15288.45 and the constant is 32001.74, which add up the mean of 47290.2.

*1d)
reg y_pw i.mex if year == 1980, nocons
*The coefficient becomes 47290.2, which is just the mean.

*1e)
gen fra = country == "France"
label var fra "France=1"
reg y_pw i.mex i.fra if year == 1980
*The constant term is now 31803.11 which represents the mean for countries other than Mexico or France, as each dummy coefficient is that country's mean minus the total omitted group mean.

*1f)
keep if inrange(year, 1980, 2019)
bys country: gen nyrs = _N
keep if nyrs == 40
gen post84 = year >= 1985
label var post84 "Year >= 1985"
reg y_pw post84, vce(cluster country)
*The coefficient for post84 is 8314.256, which estimates hte average difference in output per worker between post-1984 and pre-1985 years for the sample.

*1g)
encode country, gen(country_id)
label var country_id "Country ID (from string 'country')"
reg y_pw post84 i.country_id, vce(cluster country_id)
reg y_pw post84 i.country_id i.year, vce(cluster country_id)
*After including country fixed effects the coefficient for post84 becomes 7182.686. After including country fixed effects and year fixed effects the coefficient for post84 becomes 14231.76. 
*(One sentence explanation) Once we include both country and year fixed effects, the coefficient no longer represents a simple time shift, since year dummies absorb the time effects, so the remaining variation in post84 represents differences caused by changes in which countries are present before and after 1984.

*1e)
reg y_pw i.country_id i.year c.post84#i.mex c.post84#i.fra, vce(cluster country)
*Interaction terms show how Mexico and France's post-1984 changes differ from the average country's change which is a difference-in-differences interpretation. So, Mexico's post-1984 change in output per worker is about 13,981 units higher than the average country's post-1984 change. And, France's post-1984 change in output per worker is about 16,492 units lower than the average country's post-1984 change. (Both are statistically significant)

*2a
use "pwt100.dta", clear
keep if inrange(year, 1990, 2019)
gen y_pw = rgdpo/emp
gen ln_y = ln(y_pw)
gen post01 = year>=2002
gen china = country=="China"
encode country, gen(country_id)
label var country_id "Country ID (from string 'country')"
reg ln_y c.post01#i.china i.country_id i.year, vce(cluster country_id)

*2b)
*The coefficient of the interaction term is -.6459348. So the the post-2001 change in output per worker for China relative to the average change for other countries, holding country and year effects fixed, is roughly 47.6% less after WTO entry (log-linear model: %Δy≈100×(exp(β)−1)). So, China's outper per worker grew slower than the average country following entry to the WTO.

*2c)
*note different label and graph function to add more advanced specifics like labels
use "pwt100.dta", clear
gen y_pw = rgdpo/emp
gen ln_y = ln(y_pw)
encode country, gen(country_id)
gen china = country=="China"
label var china "China (=1)"
keep if inrange(year, 1990, 2019)
gen rel_event = year - 2001
keep if inrange(rel_event, -10, 18)
gen rel_index = rel_event + 11
label var rel_index "Event time (shifted, base=11 -> year 2001)"
summ rel_index if rel_event==0
*omitted (base) period = 2001
local base = r(min)  

*Check that counts > 0 because was having issues of graph showing no effect
tab china
tab year if china
tab rel_index if china

reg ln_y i.country_id i.year i.china##ib`base'.rel_index, vce(cluster country_id)

summ rel_index if rel_event == 0
local base = r(min)

tempfile ev_wto
tempname P
postfile `P' int rel double b se lb ub using "`ev_wto'", replace

levelsof rel_index, local(ks)
*use base to omit
foreach k of local ks {
    if `k' != `base' {
        quietly lincom 1.china#`k'.rel_index
        local b  = r(estimate)
        local se = r(se)
        post `P' (`= `k' - `base'' ) (`b') (`se') ///
                 (`= `b' - 1.96*`se'' ) (`= `b' + 1.96*`se'' )
    }
}
postclose `P'

use "`ev_wto'", clear
rename rel rel_shift
gen zero = 0
sort rel_shift

twoway ///
 (rarea lb ub rel_shift, sort) ///
 (line  b  rel_shift, lwidth(medthick)) ///
 (line zero rel_shift, lpattern(dash)), ///
 xline(0, lpattern(shortdash)) yline(0, lpattern(shortdash)) ///
 xtitle("Years relative to 2001 (0 = 2001)") ///
 ytitle("China vs others (log points)") ///
 title("Q2(c) Event study: China after WTO (base = 2001)")
	   
*2d)
*Put in this code again even though included in part 2c because stata was saying the variables were not coded even though they were.
use "pwt100.dta", clear
gen y_pw = rgdpo/emp
gen ln_y = ln(y_pw)
encode country, gen(country_id)
gen china = country=="China"
label var china "China (=1)"
keep if inrange(year, 1990, 2019)
gen rel_event = year - 2001
keep if inrange(rel_event, -10, 18)
gen rel_index = rel_event + 11
label var rel_index "Event time (shifted, base=11 -> year 2001)"
summ rel_index if rel_event==0
*omitted (base) period = 2001
local base = r(min)  

reg ln_y i.country_id i.year i.china##ib`base'.rel_index, vce(cluster country_id)

levelsof rel_index if rel_event<0, local(leads)   // all pre-2001 bins
local H
foreach k of local leads {
    local H `H' 1.china#`k'.rel_index
}
test `H'
*Based on the results, the F-test and p-value strongly rejects the null that all pre-2001 coefficients are zero (F = 23.81, p < 0.001), indicating that China's output per worker was already trending differently from other countries before WTO accession. This means the parallel trends assumption is violated. Thus, the post-20021 effects should be considered cautiously as they may reflect pre-existing growth rather than being solely caused by WTO entry or that the change following entry into the WTO cannot be soley contributed to the entry into the WTO.

*2e)
use "pwt100.dta", clear
gen y_pw  = rgdpo/emp
gen ln_y  = ln(y_pw)
encode country, gen(country_id)
gen china = country=="China"
keep if inrange(year, 1950, 2019)
gen rel_event = year - 2001
keep if inrange(rel_event, -30, 18)
summ rel_event
local minrel = r(min)
gen rel_index = rel_event - `minrel' + 1
label var rel_index "Event time (shifted; base = year 2001)"
summ rel_index if rel_event==0
local base = r(min)

reg ln_y i.country_id i.year i.china##ib`base'.rel_index, vce(cluster country_id)

tempfile ev_wto_long
tempname P
postfile `P' int rel double b se lb ub using "`ev_wto_long'", replace

levelsof rel_index, local(ks)
foreach k of local ks {
    if `k' != `base' {
        quietly lincom 1.china#`k'.rel_index
        local b  = r(estimate)
        local se = r(se)
        post `P' (`= `k' - `base'' ) (`b') (`se') ///
                 (`= `b' - 1.96*`se'' ) (`= `b' + 1.96*`se'' )
    }
}
postclose `P'

use "`ev_wto_long'", clear
rename rel rel_shift
gen zero = 0
sort rel_shift

twoway ///
 (rarea lb ub rel_shift, sort) ///
 (line  b  rel_shift, lwidth(medthick)) ///
 (line zero rel_shift, lpattern(dash)), ///
 xline(0, lpattern(shortdash)) yline(0, lpattern(shortdash)) ///
 xtitle("Years relative to 2001 (0 = 2001)") ///
 ytitle("China vs others (log points)") ///
 title("Q2(e) Event study: China after WTO — extended window (base = 2001)")
 
*Extending the event window to 1950–2019 allows us to examine long-run pre-trends much before 1990 to see if pre-2000 coefficients hover near zero and are statistically flat then we can say that parallel trends are more credible. However, if effects begin only after 2002 and grow following, that will support the WTO-timing narrative which reflects the idea that if effects start earlier, then change may reflect broader reforms unrelated to WTO accession.
*Based on the graph, the event-study from 1950 to 2019 shows that China's output per worker was far below that of other countries for decades but began rising sharply in the late 1970s and 1980s which was before WTO accession. This indicates that China's economic catch up began before 2001 or entry to the WTO and WTO membership appears to have reinforced this trajectory. Thus, the effects of WTO entry should be seen as part of a longer-term development process rather than a single turning point that came about post 2001.

*3a)
use "nq_update.dta", clear
browse
isid country year, sort
encode country, gen(country_id)
gen post1 = year >= 1500
label var post "Year >= 1500" 
reg ln_population c.post##c.ln_wpot i.country_id i.year i.year#c.ln_oworld i.year#c.ln_elevation i.year#c.ln_tropical i.year#c.ln_rugged, vce(cluster country_id)

*3b)
*[Coefficient from 3a explained] The regression is estimating the effect of potato suitability (ln_wpot) on log population (ln_population) after 1500 CE by interacting it with the post-1500 dummy (post).
*The coefficient on c.post#c.ln_wpot is 0.03247 and is statistically significant, meaning that after 1500 CE, a 1-unit increase in the log of potato suitability is associated with a 3.25 percent increase in log population, which could imply that potato suitability became an important driver of population growth only after the Columbian Exchange introduced potatoes.

summ ln_wpot
scalar sd_wpot = r(sd)
di "POP per-SD (log points): " _b[c.post#c.ln_wpot]*sd_wpot
di "POP per-SD (percent): " 100*(exp(_b[c.post#c.ln_wpot]*sd_wpot)-1)

reg city_pop_share c.post##c.ln_wpot i.country_id i.year i.year#c.ln_oworld i.year#c.ln_elevation i.year#c.ln_tropical i.year#c.ln_rugged, vce(cluster country_id)
di "URB per-SD (units): " _b[c.post#c.ln_wpot]*sd_wpot

*The post-estimation calculation shows that a 1-standard-deviation increase in potato suitability increases population by about 0.117 log points, or 12.45%, meaning that regions more suitable for potato cultivation saw roughly 12.5% higher population growth after the Columbian Exchange. This is a large and significant effect.

*The interaction term c.post#c.ln_wpot for the urbanization regression (city population share) is 0.00363, significant at the 1% level, implying a 1-unit increase in potato suitability raises the city population share by about 0.36 percentage points after 1500 CE.
*The post-estimation result shows that a 1-standard-deviation increase in potato suitability increases urbanization by about 0.0131 units or 1.31 percentage points).

*3c)
reg ln_population c.post##c.ln_wpot c.post##i.cont_europe i.country_id i.year i.year#c.ln_oworld i.year#c.ln_elevation i.year#c.ln_tropical i.year#c.ln_rugged, vce(cluster country_id)

reg city_pop_share c.post##c.ln_wpot c.post##i.cont_europe i.country_id i.year i.year#c.ln_oworld i.year#c.ln_elevation i.year#c.ln_tropical i.year#c.ln_rugged, vce(cluster country_id)

*Controling for a post-1500 European effect, the interaction term between post-1500 (post) and potato suitability (ln_wpot) becomes small (0.0150) and NOT statistically significant (p = 0.256), indicating much of the population growth difference associated with potato suitability was not driven by potatoes alone.
*The coefficient on 1.cont_europe (≈ 7.76) shows that European countries had significantly higher population levels overall compared to non-European ones.
*The significant positive interaction cont_europe#c.post (≈ 0.3643) means European population growth accelerated after 1500, over and above the general trend — again suggesting that Europe-specific dynamics played a major role.

*The next interaction coefficient drops to 0.0026 and becomes only marginally significant (p ≈ 0.073).
*This suggests that the effect of potato suitability on urbanization weakens substantially once we account for Europe's post-1500 structural transformation.
*The large and significant European coefficient (≈ 0.248) again confirms that European countries experienced significantly higher urbanization growth, beyond what potato suitability alone can explain.

*3d)
gen rel = year - 1500
summarize rel
scalar min_rel = r(min)
local minrel = r(min)
gen rel_index = rel - `minrel' + 1
summ rel if year==1500
scalar base_rel = 0
summ rel_index if rel==0
scalar base_idx = r(min)
*omitted (base) period = 1500
local base_idx = r(min)

reg ln_population i.country_id i.year i.year#c.ln_oworld i.year#c.ln_elevation i.year#c.ln_tropical i.year#c.ln_rugged c.ln_wpot##ib`base_idx'.rel_index, vce(cluster country_id)
	
tempfile ev_pot
tempname P1
postfile `P1' int rel double b se lb ub using "`ev_pot'", replace

levelsof rel_index, local(ks)
*base to omit here again
foreach k of local ks {
    if `k' != `base_idx' {
        quietly lincom c.ln_wpot#`k'.rel_index
        local b  = r(estimate)
        local se = r(se)
        post `P1' (`= `k' - `base_idx'' ) (`b') (`se') ///
                  (`= `b' - 1.96*`se'' ) (`= `b' + 1.96*`se'' )
    }
}
postclose `P1'

use "`ev_pot'", clear
rename rel rel_shift
gen zero = 0
sort rel_shift

twoway ///
 (rarea lb ub rel_shift, sort) ///
 (line  b  rel_shift, lwidth(medthick)) ///
 (line zero rel_shift, lpattern(dash)), ///
 xline(0, lpattern(shortdash)) yline(0, lpattern(shortdash)) ///
 xtitle("Years relative to 1500 (0 = 1500)") ///
 ytitle("Effect of ln_wpot on log population") ///
 title("Q3(d) Event study: ln_wpot × year (base = 1500)")

*The event-study evidence is consistent with Nunn and Qian's main result that potato suitability had little to no impact before 1500, but became increasingly important for population growth in the centuries following their introduction because the red line (point estimates) is consistently negative and flat before 1500, hovering around –0.05 to –0.10 log points. *This suggests that prior to the Columbian exchange or pre-1500, potato suitability had no significant or positive association with population growth supporting the parrallel trends assumption.
*After 1500, the effect gradually moves closer to zero and eventually becomes slightly positive toward the end of the period which is 1800 to 1900.
*This indicates that after potatoes were introduced to the Old World, regions with higher potato suitability started experiencing higher population growth relative to less suitable regions.

*3e)
use "nq_update.dta", clear
encode country, gen(country_id)
merge m:1 isocode using "milk_lpf.dta", keep(match master) nogen
capture confirm variable lpf
assert _rc==0
replace lpf = lpf/100 if lpf>1 & lpf<=100
*For my own clarity
label var lpf "Lactase persistence frequency (0–1)"
browse

*3f)
*The interaction analysis examines how the effect of potato suitability (ln_wpot) on population growth changes after 1500 CE, and whether this effect differs depending on a country's lpf.

reg ln_population c.post##c.ln_wpot##c.lpf i.country_id i.year i.year#c.ln_oworld i.year#c.ln_elevation i.year#c.ln_tropical i.year#c.ln_rugged, vce(cluster country_id)
lincom c.post#c.ln_wpot + c.post#c.ln_wpot#c.lpf*0
lincom c.post#c.ln_wpot + c.post#c.ln_wpot#c.lpf*1

*When lactase persistence is absent (LPF = 0), the coefficient on the interaction term is 0.00248 (p = 0.887), which is NOT statistically significant (also zero is in the confidence interval), suggesting that for societies without a historical ability to digest lactose, potato suitability had no effect on population growth following 1500.
*When lactase persistence is present (LPF = 1), the coefficient is 0.06448 (p = 0.036), which statistically significant, suggesting that in regions with high lactase persistence, a one-unit increase in potato suitability is associated with about a 6.4% increase in population after 1500.

reg city_pop_share c.post##c.ln_wpot##c.lpf i.country_id i.year i.year#c.ln_oworld i.year#c.ln_elevation i.year#c.ln_tropical i.year#c.ln_rugged, vce(cluster country_id)
lincom c.post#c.ln_wpot + c.post#c.ln_wpot#c.lpf*0
lincom c.post#c.ln_wpot + c.post#c.ln_wpot#c.lpf*1

*When lactase persistence is absent, the estimated interaction effect of potato suitability on urbanization after 1500 is -0.00126 (p = 0.392), which is NOT statistically significant (zero in the CI again here). This suggests that in societies without widespread lactose tolerance, potato suitability did not meaningfully influence the degree of urbanization after 1500.
*When lactase persistence is present, the interaction term increases to 0.00803 (p = 0.010), which is statistically significant, implying that in lactose-tolerant societies, a one-unit increase in potato suitability is associated with about a 0.8 percentage point increase in the urban share of the population after 1500.

*So, potato suitability only translated into significant urbanization gains when combined with high lactase persistence.

*3g)
reg ln_population c.post##c.ln_wpot##c.lpf i.country_id i.year i.year#c.ln_oworld i.year#c.ln_elevation i.year#c.ln_tropical i.year#c.ln_rugged i.cont_europe##i.post, vce(cluster country_id)
lincom c.post#c.ln_wpot + c.post#c.ln_wpot#c.lpf*0
lincom c.post#c.ln_wpot + c.post#c.ln_wpot#c.lpf*1

*The interaction term when lpf is 0 and 1 is NOT significant, implying that neither the average effect of potato suitability on log population or it variation by lactase persistence shows evidence of statistical significance when considered seperately.

reg city_pop_share c.post##c.ln_wpot##c.lpf i.country_id i.year i.year#c.ln_oworld i.year#c.ln_elevation i.year#c.ln_tropical i.year#c.ln_rugged i.cont_europe##i.post, vce(cluster country_id)
lincom c.post#c.ln_wpot + c.post#c.ln_wpot#c.lpf*0
lincom c.post#c.ln_wpot + c.post#c.ln_wpot#c.lpf*1

*For societies without lactase persistence, the estimated effect of potato suitability on the share of population living in urban areas is negative but NOT statistically significant, suggesting that potato suitability had no meaningful impact on urbanization in regions where lactase persistence was absent.
*In societies with high lactase persistence, the effect of potato suitability on urbanization is positive and statistically significant at the 5% level. This suggests that greater suitability for potato cultivation is associated with roughly a 0.95 percentage point increase in urban population share after the Columbian exchange in these regions.

*So the interaction between potatoes and milk is affected as for population when lpf is 0, the interaction term remains insignificant, and when lpf is 1 it is now insignificant. *For urbanization, when lpf is 0, the interaction term remains insignificant, and when lpf is 1 the coefficient increases from .8 percentage point increase to .95 percentage point increase.

*3h)
gen rel = year - 1500
summ rel
local minrel = r(min)
gen rel_index = rel - `minrel' + 1
summ rel_index
summ rel_index if rel==0
local base_idx = r(min)

reg ln_population i.country_id i.year i.year#c.ln_oworld i.year#c.ln_elevation i.year#c.ln_tropical i.year#c.ln_rugged c.ln_wpot##c.lpf##ib`base_idx'.rel_index, vce(cluster country_id)

tempfile ev_trip
tempname P2
postfile `P2' int rel double b se lb ub using "`ev_trip'", replace

levelsof rel_index, local(ks)
foreach k of local ks {
    if `k' != `base_idx' {
        quietly lincom c.ln_wpot#c.lpf#`k'.rel_index
        local b  = r(estimate)
        local se = r(se)
        post `P2' (`= `k' - `base_idx'' ) (`b') (`se') ///
                  (`= `b' - 1.96*`se'' ) (`= `b' + 1.96*`se'' )
    }
}
postclose `P2'

use "`ev_trip'", clear
rename rel rel_shift
gen zero = 0
sort rel_shift

twoway ///
 (rarea lb ub rel_shift, sort) ///
 (line  b  rel_shift, lwidth(medthick)) ///
 (line zero rel_shift, lpattern(dash)), ///
 xline(0, lpattern(shortdash)) yline(0, lpattern(shortdash)) ///
 xtitle("Years relative to 1500 (0 = 1500)") ///
 ytitle("Effect of ln_wpot × LPF on log population") ///
 title("Q3(h) Event study: ln_wpot × LPF dynamics (base = 1500)")
	   
*Before 1500, their interaction was negative, indicating no combined population benefit from having both traits. After 1500, this effect moved toward zero, suggesting that the introduction of potatoes (and possibly dietary shifts tied to lactase persistence) reduced previous population disadvantages and may have even started to improve population growth.
*In conclusion, the graph indicates that the arrival and spread of potatoes changed how these two factors affected population, making them more beneficial together over time.

*4a)
use "nq_update.dta", clear
keep if inrange(year, 1000, 1900)
encode country, gen(country_id)
local flex i.year#c.ln_oworld i.year#c.ln_elevation i.year#c.ln_tropical i.year#c.ln_rugged
gen post1600 = year>=1600
label var post1600 "Year >= 1600"

merge m:1 isocode using "country_centroids.dta", keep(match master) nogen

*standard vals for latitude and longitude
scalar palos_lat = 37.23
scalar palos_lon = -6.89

*haverstine
gen double lat_rad = lat  * _pi/180
gen double lon_rad = lon  * _pi/180
scalar pal_lat = palos_lat * _pi/180
scalar pal_lon = palos_lon * _pi/180

gen double dlat = lat_rad - pal_lat
gen double dlon = lon_rad - pal_lon
gen double a = (sin(dlat/2))^2 + cos(lat_rad)*cos(pal_lat)*(sin(dlon/2))^2
gen double dist_palos_km = 2*6371*asin(sqrt(a))

label var dist_palos_km "Great-circle distance (km) to Palos, Spain"
drop lat_rad lon_rad dlat dlon a

summ dist_palos_km
list isocode country dist_palos_km in 1/10

*4b)
tempname results
tempfile grid
postfile `results' double speed adjR2 using "`grid'", replace
local bests = .
local bestr2 = -1
forvalues s = 5(5)80 {
    gen arrive_s = year >= (1600 + dist_palos_km/`s')
    qui reg ln_population arrive_s i.country_id i.year `flex', vce(cluster country_id)
    local r2 = e(r2_a)
    post `results' (`s') (`r2')
    drop arrive_s
    if (`r2' > `bestr2') {
        local bestr2 = `r2'
        local bests  = `s'
    }
}
postclose `results'
di as text "Best diffusion speed (km/year): " as result `bests' ///
   as text "  | adj R^2 = " as result %5.3f `bestr2'

gen arrive = year >= (1600 + dist_palos_km/`bests')
label var arrive "Adoption (year >= 1600 + distance/`bests')"

*Here I used a difference-in-differences model with staggered timing proxy, fixed effects, and flexible controls.
*Based on my model, the spatial diffusion of potatoes is best explained by an average speed of about 15 kilometers per year starting from Palos, Spain (the assumed entry point around 1600 CE).
*The high adjusted r squared suggests the observed population data is fit well by model and the model explains almost all variation once country and year fixed effects plus NQ controls are included.

*4c)
reg ln_population c.arrive##c.ln_wpot i.country_id i.year `flex', vce(cluster country_id)

summ ln_wpot, meanonly
scalar sd_w = r(sd)

reg city_pop_share c.arrive##c.ln_wpot i.country_id i.year `flex', vce(cluster country_id)

*4d)
*[Interpreted coefficients from 4c] The regression results indicate that before the arrival of potatoes, regions with high potato suitability had slightly lower populations. However, after potatoes diffused to these areas, the interaction between arrival and suitability becomes strongly positive and highly significant (β = 0.078, p < 0.001). This suggests that the introduction of potatoes caused substantial population increases in regions where they could be most successfully cultivated.
*And for urbanization, the interaction coefficient (β = 0.0067, p < 0.001) is smaller but positive, implying that potatoes modestly encouraged urban growth, likely through improved agricultural productivity supporting larger nonagricultural populations.
*These findings align closely with Nunn & Qian's conclusion that the spread of potatoes had profound demographic effects, primarily through its impact on rural population capacity and, secondarily, through urban expansion.

quietly summarize ln_wpot if e(sample), meanonly
scalar mean_w = r(mean)
lincom _b[arrive] + mean_w * _b[c.arrive#c.ln_wpot]

*At the mean level of potato suitability (ln_wpot ≈ 3.76), the arrival of potatoes is associated 1.8% increase in the urban population share, and this effect is statistically significant (p = 0.019), which supports the hypothesis of spatial diffusion from Palos, Spain, showing that as potatoes spread geographically, regions suitable for cultivation saw statistically significant urban growth.

*4e)
*Single entry point & centroid distance because I imagine this is highly simplified for convenience because in reality Palos likely involved multiple ports and inland routes and centroids ignore where people live within countries.
*Timing measurement error. The constructed arrival date is noisy.
*Spillovers from nearby countries' adoption may affect neighbors.
*Controls and confounding because even with NQ flexible controls, shocks correlated with distance can alter estimates.