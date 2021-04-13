

*年份分布
preserve 
areg newtotal c.faci_right#c.treat land_area lanform popu nature_vill ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)
keep if e(sample)
duplicates drop id ,force 
tab rightyear if rightyear!=.
restore
*设施类型分布
preserve 
areg newtotal c.faci_right#c.treat land_area lanform popu nature_vill ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)
keep if e(sample)
duplicates drop id ,force 
tab faci_type if rightyear!=.
restore
*产权配置分布
preserve 
areg newtotal c.faci_right#c.treat land_area lanform popu nature_vill ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)
keep if e(sample)
duplicates drop id ,force 
tab right_type if rightyear!=.
restore
*责任归属分布
preserve 
areg newtotal c.faci_right#c.treat land_area lanform popu nature_vill ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)
keep if e(sample)
duplicates drop id ,force 
tab man_type if rightyear!=.
restore
*制裁机制分布
preserve 
areg newtotal c.faci_right#c.treat land_area lanform popu nature_vill ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)
keep if e(sample)
duplicates drop id ,force 
tab sanction if rightyear!=.
restore
*激励传导机制分布
preserve 
areg newtotal c.faci_right#c.treat land_area lanform popu nature_vill ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)
keep if e(sample)
duplicates drop id ,force 
tab incen_type if rightyear!=.
restore



*一、基准回归

***********************工程确权对总体绩效的影响***********************************

*未添加控制变量
areg newtotal c.faci_right#c.treat i.year,robust absorb(id)
*添加控制变量 
areg newtotal c.faci_right#c.treat land_area lanform  nature_vill   first_batch ///
popu drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary i.faci_type oper_insti4 total_inves participation_ratio  i.year,robust absorb(id)


***********************工程确权对占用绩效的影响***********************************

*未添加控制变量
areg newf1 c.faci_right#c.treat i.year,robust absorb(id)
*添加控制变量
areg newf1 c.faci_right#c.treat land_area lanform  nature_vill   first_batch ///
popu drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary i.faci_type oper_insti4 total_inves participation_ratio  i.year,robust absorb(id)


***********************工程确权对供给绩效的影响***********************************

*未添加控制变量
areg newf2 c.faci_right#c.treat i.year,robust absorb(id)
*添加控制变量
areg newf2 c.faci_right#c.treat land_area lanform  nature_vill   first_batch ///
popu drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary i.faci_type oper_insti4 total_inves participation_ratio  i.year,robust absorb(id)



*二、平行趋势检验

areg newtotal before2 before1 current after1 after2 after3 land_area lanform popu nature_vill i.faci_type first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)

coefplot, baselevels ///
	keep(before2 before1 current after1 after2 after3) ///
	vertical ///
	coeflabels( ///
		before2 = "t-2" ///
		before1 = "t-1" ///
		current = "t" ///
		after1 = "t+1" ///
		after2 = "t+2" ///
		after3 = "t+3" ///
	) ///
	yline(0,lcolor(edkblue*0.8)) ///
	ylabel(-.75(0.5)2) ///
	xline(3,lwidth(vthin) lpattern(dash) lcolor(teal)) ///
	ylabel(,labsize(*1.25)) xlabel(,labsize(*1.25)) ///
	ytitle("工程确权的动态效应") ///
	xtitle("确权时点") ///
	addplot(line @b @at) ///
	level(99 95 90 ) ///
	ciopts( msize(medium) lwidth(2.5 ..) lcolor(*.1 *.3 *.6)) ///
	msymbol(circle_hollow) ///
	scheme(plotplainblind)

******************************************************************************************************************	
areg newf1   before2 before1 current after1 after2 after3 land_area lanform popu nature_vill i.faci_type first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio  i.year,robust absorb(id)

coefplot, baselevels ///
	keep(before2 before1 current after1 after2 after3) ///
	vertical ///
	coeflabels( ///
		before2 = "t-2" ///
		before1 = "t-1" ///
		current = "t" ///
		after1 = "t+1" ///
		after2 = "t+2" ///
		after3 = "t+3" ///
	) ///
	yline(0,lcolor(edkblue*0.8)) ///
	ylabel(-1(0.5)2.5) ///
	xline(3,lwidth(vthin) lpattern(dash) lcolor(teal)) ///
	ylabel(,labsize(*1.25)) xlabel(,labsize(*1.25)) ///
	ytitle("工程确权的动态效应") ///
	xtitle("确权时点") ///
	addplot(line @b @at) ///
	level(99 95 90) ///
	ciopts( msize(medium) lwidth(2.5 ..) lcolor(*.1 *.3 *.6))  ///
	msymbol(circle_hollow) ///
	scheme(plotplainblind)

******************************************************************************************************************		
areg newf2  before2 before1 current after1 after2 after3 land_area lanform popu nature_vill i.faci_type first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio  i.year,robust absorb(id)

coefplot, baselevels ///
	keep(before2 before1 current after1 after2 after3) ///
	vertical ///
	coeflabels( ///
		before2 = "t-2" ///
		before1 = "t-1" ///
		current = "t" ///
		after1 = "t+1" ///
		after2 = "t+2" ///
		after3 = "t+3" ///
	) ///
	yline(0,lcolor(edkblue*0.8)) ///
	ylabel(-1(0.5)3) ///
	xline(3,lwidth(vthin) lpattern(dash) lcolor(teal)) ///
	ylabel(,labsize(*1.25)) xlabel(,labsize(*1.25)) ///
	ytitle("工程确权的动态效应") ///
	xtitle("确权时点") ///
	addplot(line @b @at) ///
	level(99 95 90 ) ///
	ciopts( msize(medium) lwidth(2.5 ..) lcolor(*.1 *.3 *.6)) ///
	legend(order(0 "置信度" 1 "99%" 2 "95%" 3 "90%")) ///
	scheme(plotplainblind)


	
	
	
*三、稳健性检验

preserve
set seed 0001
gen ranorder = runiform()
sort ranorder
psmatch2 treat land_area lanform  nature_vill i.faci_type  first_batch ///
popu drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 association first_secretary oper_insti4 total_inves participation_ratio , ///
out(newtotal) logit ate neighbor(1) ///
common caliper(.05) ties
pstest  land_area lanform  nature_vill i.faci_type  first_batch ///
popu drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 association first_secretary oper_insti4 total_inves participation_ratio , both graph
gen commontotal = _support
drop if commontotal ==0
drop _*
areg newtotal c.faci_right#c.treat land_area lanform  nature_vill i.faci_type  first_batch ///
popu drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio  i.year,robust absorb(id)
restore

preserve
set seed 0001
gen ranorder = runiform()
sort ranorder
psmatch2 treat land_area lanform  nature_vill i.faci_type  first_batch ///
popu drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 association first_secretary oper_insti4 total_inves participation_ratio , ///
out(newf1) logit ate neighbor(1) ///
common caliper(.05) ties
pstest  land_area lanform  nature_vill i.faci_type  first_batch ///
popu drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 association first_secretary oper_insti4 total_inves participation_ratio , both graph
gen commontotal = _support
drop if commontotal ==0
drop _*
areg newf1 c.faci_right#c.treat land_area lanform  nature_vill i.faci_type  first_batch ///
popu drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio  i.year,robust absorb(id)
restore

preserve
set seed 0001
gen ranorder = runiform()
sort ranorder
psmatch2 treat land_area lanform  nature_vill i.faci_type  first_batch ///
popu drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 association first_secretary oper_insti4 total_inves participation_ratio , ///
out(newf2) logit ate neighbor(1) ///
common caliper(.05) ties
pstest  land_area lanform  nature_vill i.faci_type  first_batch ///
popu drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 association first_secretary oper_insti4 total_inves participation_ratio , both graph
gen commontotal = _support
drop if commontotal ==0
drop _*
areg newf2 c.faci_right#c.treat land_area lanform  nature_vill i.faci_type  first_batch ///
popu drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio  i.year,robust absorb(id)
restore	
	
*四、异质性分析

***********************不同设施类型***********************************


areg newtotal c.faci_right#c.treat#i.faci_type land_area lanform popu nature_vill i.faci_type  first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio  i.year,robust absorb(id)

areg newf1 c.faci_right#c.treat#i.faci_type land_area lanform popu nature_vill i.faci_type  first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio  i.year,robust absorb(id)

areg newf2 c.faci_right#c.treat#i.faci_type land_area lanform popu nature_vill i.faci_type  first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio  i.year,robust absorb(id)

***********************不同产权配置形式*******************************


areg newtotal c.faci_right#c.treat#i.right_type land_area lanform popu nature_vill  first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)

areg newf1 c.faci_right#c.treat#i.right_type land_area lanform popu nature_vill     ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association  oper_insti4 total_inves participation_ratio i.year,robust absorb(id)

areg newf2 c.faci_right#c.treat#i.right_type land_area lanform popu nature_vill first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)

***********************不同管护模式***********************************


areg newtotal c.faci_right#c.treat#i.man_type land_area lanform popu nature_vill i.faci_type first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)

areg newf1 c.faci_right#c.treat#i.man_type land_area lanform popu nature_vill i.faci_type first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)

areg newf2 c.faci_right#c.treat#i.man_type land_area lanform popu nature_vill i.faci_type first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)

***********************不同制裁机制***********************************


areg newtotal c.faci_right#c.treat#i.sanction land_area lanform popu nature_vill i.faci_type first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)

areg newf1 c.faci_right#c.treat#i.sanction land_area lanform popu nature_vill i.faci_type first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)

areg newf2 c.faci_right#c.treat#i.sanction land_area lanform popu nature_vill i.faci_type first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)

***********************不同激励传导机制*******************************


areg newtotal c.faci_right#c.treat#i.incen_type land_area lanform popu nature_vill i.faci_type first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)

areg newf1 c.faci_right#c.treat#i.incen_type land_area lanform popu nature_vill  i.faci_type first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)

areg newf2 c.faci_right#c.treat#i.incen_type land_area lanform popu nature_vill  i.faci_type first_batch ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)




***********************进一步讨论：首批试点效应***********************


areg newtotal c.faci_right#c.treat c.faci_right#c.treat#c.first_batch land_area lanform popu nature_vill i.faci_type  ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)

areg newf1 c.faci_right#c.treat c.faci_right#c.treat#c.first_batch land_area lanform popu nature_vill i.faci_type ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)

areg newf2 c.faci_right#c.treat c.faci_right#c.treat#c.first_batch land_area lanform popu nature_vill i.faci_type  ///
drou_flood_disa rel_econ2 irri_time workers_ratio harmony2 i.association i.first_secretary oper_insti4 total_inves participation_ratio i.year,robust absorb(id)



*五、机制分析：要素投入水平的增加? vs 要素配置效率的提升?
cap drop iy
cap drop invesy
cap drop ly
cap drop total_inves_q
cap drop labor_q
cap drop inv_lab
cap drop logperformance_c
cap drop loglabor
cap drop logtotal_inves
cap drop lninsti
cap drop insti_q
cap drop insti_lab
cap drop year_q
cap drop insti_inv
gen logperformance_c=log(performance_c)
gen loglabor=log(labor)
gen logtotal_inves=log(total_inves)
gen lninsti=log(oper_insti4+1)
gen iy=lninsti*year
gen invesy=logtotal_inves*year
gen ly= loglabor*year
gen insti_q=0.5*lninsti^2
gen total_inves_q=0.5*logtotal_inves^2
gen labor_q=0.5*loglabor^2
gen insti_inv=lninsti*logtotal_inves
gen inv_lab= logtotal_inves*loglabor
gen insti_lab=lninsti*loglabor
gen year_q=0.5*year^2



xtset id year 
sfpanel performance_c lninsti logtotal_inves loglabor ///
 insti_q total_inves_q labor_q year_q /// 
insti_inv inv_lab insti_lab ///
iy ly invesy  year  , model(bc95) distribution(tnormal) 

test  iy= invesy = ly 
test insti_q=total_inves_q = labor_q=insti_inv=inv_lab=insti_lab 
cap drop te5
predict te5,bc
sum te5



