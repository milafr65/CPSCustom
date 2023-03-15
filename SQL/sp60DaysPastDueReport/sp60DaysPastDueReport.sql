USE [CPS_DW]
GO

/****** Object:  StoredProcedure [dbo].[sp60DaysPastDueReport]    Script Date: 3/15/2023 11:07:34 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



-- =============================================
-- Author:		
-- Create date: 01/16/2020
-- Description:	60 Days Past Due Report (President's Report)
-- =============================================
ALTER PROCEDURE [dbo].[sp60DaysPastDueReport]
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here
with ReportAsOfDate as
(select distinct cast(max(AsofDate) as date) as 'As-of Date'
from dbo.CustomerAccountAging ag (nolock))

, EmployerMember as
(
	select distinct a.ERNumber as Customer
, a.ERCount
, sum(a.MemberCHPCount) as MemberCHPCount
, sum(a.MemberCDSPCount) as MemberCDSPCount
, sum(a.MemberCRPCount) as MemberCRPCount
from

(select e.ERNumber
	, count(distinct e.ERNumber) as ERCount
	, case
		when c.BenefitId in ('MEDICAL') and c.PlanId not in ('WAIVE') 
		then count(Distinct m.LawsonEmployeeId)
		else '0'
		end as MemberCHPCount
	, case
		when c.BenefitId in ('CRP') and c.PlanID not in ('WAIVE') 
		then count(Distinct m.LawsonEmployeeId)
		else '0'
		end as MemberCDSPCount
	, case
		when c.BenefitId in ('CRP') and c.PlanID not in ('WAIVE') 
		then count(Distinct m.LawsonEmployeeId)
		else '0'
		end as MemberCRPCount
	from dbo.BasEmployerElection e
	left join dbo.BasMemberDemographic m 
		on e.ERNumber = m.EmployerId
		and m.ETLRowIsCurrent = 1
		and m.EmploymentStatus in ('A','P')
	left join dbo.BasMemberCoverage c 
		on m.EmployeeSsn = c.EmployeeSsn
		and c.ETLRowIsCurrent = 1
	where e.ERNumber not in ('DISW','SEMFW','SEMSL','00002','00566','04797','07021','07169','07215','07240','07405','07406','007788','07846')
	group by ERNumber, BenefitId, PlanID, c.EmployeeSsn) as a
	group by a.ERNumber, a.ERCount)

,LawsonTables as (
	select distinct p.PROCESS_LEVEL as Customer
	 , p.NAME as CustomerName
	 , p.CITY as City
	 , p.STATE as State
	  , case
			when ar.ACM_USR_FLD_01 is not null
			then ar.ACM_USR_FLD_01
			else ' '
			end as PaymentPlan
	  , case
			when ar.ACM_USR_FLD_03 is not null
			then ar.ACM_USR_FLD_03
			else ' '
			end as PaymentArrangement
	  , pa.SCHEDULE
	  , ac.LAST_PMT_DATE as LastPaymentDate
	  , ac.LAST_PMT_AMT as LastPaymentAmount
	from dbo.DIM_PRSYSTEM p (nolock) 
	join dbo.DIM_PAPOSITION pa (nolock) 
		on p.PROCESS_LEVEL = pa.PROCESS_LEVEL 
		and pa.ETLRowIsCurrent = 1
	join dbo.DIM_ARCUSTOMER ac (nolock)
		on substring(ac.CUSTOMER,3,5) = pa.PROCESS_LEVEL
		and ac.ETLRowIsCurrent = 1
	left join dbo.DIM_ARACMUF ar (nolock)
		on ar.Customer = ac.CUSTOMER
		and ar.ETLRowIsCurrent = 1
		and (ar.ACM_USR_FLD_01 is not null or ar.ACM_USR_FLD_03 is not null)
	where p.ETLRowisCurrent = 1
		and datediff(year,ac.Last_Pmt_Date,getdate()) < 2)

select distinct upper(d.DistrictDesc) as District
 , concat('ER',ag.EmployerNumber) as Customer
 , upper(t.CustomerName) as 'Customer Name'
 , upper(t.City) as City
 , upper(t.State) as State
 , m.ERCount as 'ER Count'
 , m.MemberCHPCount as 'Member CHP Count'
 , m.MemberCDSPCount as 'Member CDSP Count'
 , m.MemberCRPCount as 'Member CRP Count'
 , ag.OpenAmt as 'Total Balance Due'
 , ag.BalancePastDueLt30Days as '1-30 Days Past Due'
 , ag.BalancePastDue31to60Days as '31-60 Days Past Due'
 , ag.BalancePastDue61to90Days as '61-90 Days Past Due'
 , ag.BalancePastDue91to120Days as '91-120 Days Past Due'
 , ag.BalancePastDueGt120Days as '>120 Days Past Due'
 , t.LastPaymentDate as 'Last Payment Date'
 , t.LastPaymentAmount as 'Last Payment Amount'
 , t.PaymentPlan as 'Payment Plan'
 , t.PaymentArrangement as 'Payment Arrangement'
-- , ag.BalancePastDue61to90Days/ag.BalancePastDue31to60Days as Percentage
 , case
        when ag.BalancePastDue31to60Days = 0
        THEN 0
        else ag.BalancePastDue61to90Days/ag.BalancePastDue31to60Days
   end as Percentage
 , case
	when datediff(month,max(ag.AsofDate),getdate()) <= .5 
	then 'TRUE'
	when datediff(month,max(ag.AsofDate),getdate()) > .5 
	then 'FALSE'
	end as 'On Previous Report'
 from dbo.CustomerAccountAging ag (nolock)
 join ReportAsOfDate r
	on ag.AsOfDate = r.[As-of Date]
 join EmployerMember m 
	on ag.EmployerNumber = m.Customer
 join LawsonTables t
	on t.Customer = m.Customer
 join dbo.District d (nolock) 
	on t.SCHEDULE = d.DistrictCode
 where (ag.BalancePastDue61to90Days > 0 or ag.BalancePastDue91to120Days > 0 or ag.BalancePastDueGt120Days > 0)
--	and ag.BalancePastDue31to60Days > 0
--	and (ag.BalancePastDue61to90Days/ag.BalancePastDue31to60Days) > '0.44'
group by  d.DistrictDesc
		, ag.EmployerNumber
		, t.CustomerName
		, t.City
		, t.State
		, m.ERCount
		, m.MemberCHPCount
		, m.MemberCDSPCount
		, m.MemberCRPCount
		, ag.OpenAmt
		, ag.BalancePastDueLt30Days
		, ag.BalancePastDue31to60Days
		, ag.BalancePastDue31to60Days 
		, ag.BalancePastDue61to90Days 
		, ag.BalancePastDue91to120Days
		, ag.BalancePastDueGt120Days 
		, t.LastPaymentDate
		, t.LastPaymentAmount
		, t.PaymentPlan
		, t.PaymentArrangement
		, ag.AsOfDate
order by District, Customer
END
GO


