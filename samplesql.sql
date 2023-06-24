exec sp_executesql N'


IF OBJECT_ID(''tempdb.dbo.#PayeeStepListAll'') IS NOT NULL
	DROP TABLE #PayeeStepListAll

create table #PayeeStepListAll
(
	IdNode INT,
	IdPayee INT,
	Level INT,
    Sort_Order INT ,
	IdStep INT
)

IF OBJECT_ID(''tempdb.dbo.#PayeeListAll'') IS NOT NULL
	DROP TABLE #PayeeListAll

create table #PayeeListAll
(
	IdNode INT,
	IdPayee INT,
	Level INT,
    Sort_Order INT 
)


IF OBJECT_ID(''tempdb.dbo.#WorkflowStepLocalization'') IS NOT NULL
DROP TABLE #WorkflowStepLocalization

CREATE TABLE #WorkflowStepLocalization
(
    id_wflstep INT,
    id_workflow INT,
	name_step_translated NVARCHAR(500),
    sort_step INT
)

INSERT INTO #WorkflowStepLocalization
(id_wflstep, name_step_translated, id_workflow, sort_step)
SELECT ws.id_wflstep, ISNULL(RPSLOC.[value], ws.name_step), ws.id_workflow, ws.sort_step
FROM k_m_workflow_step ws
LEFT JOIN(select distinct name, culture, value from rps_localization) [RPSLOC]
ON ws.name_step = RPSLOC.[name] AND RPSLOC.culture = @WFCulture 

DECLARE @CanSeeHimSelf BIT;
DECLARE @PlanType INT;

SELECT @PlanType = id_plan_layout_type FROM k_m_plans WHERE id_plan = @id_plan
SELECT @CanSeeHimSelf = selfAppraisal FROM k_m_plans WHERE id_plan = @id_plan

DECLARE @isApplyFilter BIT;
DECLARE @isIncluded BIT;
DECLARE @userlevel int;
DECLARE @managerlevel int;
DECLARE @diff int;
DECLARE @IdManager int;
DECLARE @hidUserNode hierarchyid;
DECLARE @hidNodeManager hierarchyid;
DECLARE @isManagerDescendantOfLoggedUser BIT;

select @IdManager = idChild from hm_NodelinkPublished where id = @IdNodeManager and idType = 14
select @userlevel = hidLevel, @hidUserNode = hid from hm_nodelinkPublishedhierarchy where id_nodelinkpublished = @IdLoggedInUserNode
select @managerlevel = hidLevel, @hidNodeManager = hid, @isManagerDescendantOfLoggedUser = hid.IsDescendantOf(@hidUserNode) from hm_nodelinkPublishedhierarchy where id_nodelinkpublished = @IdNodeManager

DECLARE @disableStdHierAccess bit;
SELECT @disableStdHierAccess = p.value_parameter FROM k_parameters p WHERE id_parameter = -33


SET @diff = @managerLevel- @userLevel

IF(@IdStepFilter = 0)
BEGIN
    -- SET SELF APPRAISAL IF NOT GIVEN
	IF(@isSelfAppraisal = 0)
	BEGIN
		-- checking whether node is leaf or simplified, it should check population exclusions and security exceptions
		IF EXISTS(SELECT 1 FROM hm_NodeLinkPublishedHierarchy WHERE hidParent = @hidNodeManager)
		BEGIN
			SET @isSelfAppraisal = -1;

			SELECT @isSelfAppraisal = uplan.self_appraisal FROM k_users_profiles uprof
				JOIN k_user_plan uplan ON uplan.id_user_profile = uprof.idUserProfile and uplan.id_plan = @id_plan
			WHERE uprof.id_user = @idUser 
				AND uprof.id_profile = @id_profile
				AND uplan.self_appraisal IS NOT NULL
				AND uplan.self_appraisal != 0
		END
		ELSE
		BEGIN
			SET @isSelfAppraisal = -2;
		END
	END

    IF(@PlanType = -2) -- FORM
    BEGIN
	    insert into #PayeeListAll(IdNode, IdPayee, Level)
	    select id, idChild,@diff from hm_NodelinkPublished where id = @IdNodeManager and idType = 14
    END
    ELSE
    BEGIN
        IF(@IdTreeSecurity = 0)
	    BEGIN
            IF(@disableStdHierAccess = 0 AND (@CanSeeHimSelf = 1 OR @IdNodeManager != @IdLoggedInUserNode) AND (@isSelfAppraisal = -2 OR @isSelfAppraisal = -3) AND @isManagerDescendantOfLoggedUser = 1)
		    BEGIN
				 INSERT INTO #PayeeListAll(IdNode, IdPayee, Level)
				 SELECT DISTINCT id_NodelinkPublished, @IdManager, @diff
				 FROM  hm_NodelinkPublishedHierarchy nph 
				 WHERE nph.idChild = @IdManager
					AND nph.idType = 14
					AND nph.idTree = @idTree
			END         
        END
        ELSE
	    BEGIN

        IF (@isSelfAppraisal = -2 OR @isSelfAppraisal = -3)
		    BEGIN
			    DECLARE @filterCTEForManagerNode NVARCHAR(MAX) = N''FilteredCTE
			            AS
			            (
				            select idPayee, firstname as fullname from py_payee
				            where idPayee = ''+ CAST(@IdManager AS nvarchar(10))  +''
			            )''  

			    DECLARE @ManagerNode table
			    (
                    IdNode INT,
				    IdPayee INT,
				    Level INT
			    )
			    insert into @ManagerNode(IdNode, IdPayee, Level)
			    exec dbo.Kernel_SP_Process_GetPayeeIdListGeneric @PayeeBasedLevel=1,@IdTreeSecurity=@IdTreeSecurity,@IdNodeManager=@IdNodeManager,@Level=''0'',@FilterCTE=@filterCTEForManagerNode,@FilterCTEParameterType = '''',@FilterCTEParameterValue=''''
			
			    insert into #PayeeListAll(IdNode, IdPayee, Level)
			    select IdNode, IdPayee, @managerlevel from @ManagerNode
            END
        END

        --Add child nodes, Same for hierarchy security and standard hierarchy
        IF(@isSelfAppraisal = -1 OR @isSelfAppraisal = -3)
        BEGIN
	        
            IF(@IdTreeSecurity <> 0 OR (@IdTreeSecurity = 0 AND @isManagerDescendantOfLoggedUser = 1 AND @disableStdHierAccess = 0))
			BEGIN
				insert into #PayeeListAll(IdNode, IdPayee, Level)
				exec dbo.Kernel_SP_Process_GetPayeeIdListGeneric @PayeeBasedLevel=@PayeeOnlyLevelCalculation,@IdTreeSecurity=@IdTreeSecurity,@IdNodeManager=@IdNodeManager,@Level=@Level,@FilterCTE=@FilterCTE,@FilterCTEParameterType = @FilterCTEParameterType,@FilterCTEParameterValue=@FilterCTEParameterValue 
			END

            UPDATE #PayeeListAll
            SET Level = @diff + Level
        END
	    ELSE IF (@isSelfAppraisal = -2)
	    BEGIN
	        UPDATE #PayeeListAll SET Level = @diff
	    END
    END

    INSERT INTO #PayeeStepListAll
    (IdNode, IdPayee, Level, Sort_Order,IdStep)
    SELECT DISTINCT pla.[IdNode] as idNode, pla.[IdPayee], pla.[Level], pla.Sort_Order,ps.id_step as IdStep
      FROM #PayeeListAll as pla
      INNER JOIN k_m_plans_payees_steps ps
      ON ps.id_payee =  pla.IdPayee
      AND ps.id_plan = @id_plan
      AND ps.start_step < @endDateFilter
      AND ps.end_step > @startDateFilter

    
update pa set pa.Sort_Order = ndp.sort_order, IdNode = ndp.Id
from #PayeeListAll pa
join hm_NodelinkPublished ndp
on pa.IdPayee = ndp.idChild and ndp.idType = 14 
where ndp.idTree = (select top 1 idTree from hm_NodelinkPublished where id = @IdNodeManager) 

    

    -- Delete Duplicate Records BY IdStep
    IF OBJECT_ID(''tempdb.dbo.#PayeeStepFilter'') IS NOT NULL
    BEGIN
    DELETE PSLA
    FROM #PayeeStepListAll PSLA
    WHERE PSLA.IdStep NOT IN (SELECT PSF.IdStep
							    FROM #PayeeStepFilter PSF)
    END
END
ELSE
BEGIN
	
IF(@IdTreeSecurity = 0 AND @disableStdHierAccess = 0)
 BEGIN

    INSERT INTO #PayeeStepListAll
	SELECT 
	    nph.id_NodelinkPublished, id_payee, 1, 1, @IdStepFilter
	FROM k_m_plans_payees_steps  ps
	JOIN hm_NodelinkPublishedHierarchy nph ON ps.id_payee =  nph.idChild
	WHERE ps.id_step = @IdStepFilter
		AND ps.id_plan = @id_plan
		AND nph.idType = 14
		AND nph.idTree = @idTree
		AND nph.hid.IsDescendantOf(@hidUserNode) = 1
 END
 ELSE
 BEGIN

	DECLARE @hidSecurityRootNode hierarchyid;

	SELECT 
		@isApplyFilter = sec.is_apply_filter
		,@isIncluded = sec.is_included
		,@hidSecurityRootNode = h.hid
	FROM k_tree_security sec -- to be sure that current user has access to this k_tree_security we should join to k_users_profiles but, this is checked via service 
	JOIN hm_NodeLinkPublishedHierarchy h ON sec.id_tree_node_published = h.id_NodelinkPublished
	WHERE sec.id_tree_security = @idTreeSecurity 
	AND sec.begin_date < GETUTCDATE() AND (sec.end_date IS NULL OR sec.end_date > GETUTCDATE()) 
	
	IF(@hidSecurityRootNode IS NULL)
		THROW 50001, ''RETURN: No such security record in k_tree_security'', 1

	INSERT INTO #PayeeStepListAll
	SELECT 
	    nph.id_NodelinkPublished, id_payee, 1, 1, @IdStepFilter
	FROM k_m_plans_payees_steps  ps
	JOIN hm_NodelinkPublishedHierarchy nph ON ps.id_payee =  nph.idChild
	LEFT JOIN k_tree_security_filter tsf ON tsf.id_tree_security = @IdTreeSecurity
	LEFT JOIN pop_Population pop ON pop.idPop = tsf.id_pop
	LEFT JOIN pop_VersionContent vc ON  vc.idPopVersion = pop.lastVersion AND vc.idColl = nph.idChild
	WHERE ps.id_step = @IdStepFilter
		AND ps.id_plan = @id_plan
		AND nph.idType = 14
		AND nph.idTree = @idTree
		AND nph.hid.IsDescendantOf(@hidSecurityRootNode) = 1 -- we need to be sure that this node is under root node of hierarchy
		AND (@isApplyFilter = 0							-- we need be sure that this node is not under population filter
			OR (@isIncluded = 1 AND vc.id IS NOT NULL)	-- or it is included by pop filter
			OR (@isIncluded = 0 AND vc.id IS NULL)		-- or it is not exluded by pop filter
			) 
        AND NOT EXISTS -- we need to be sure that this node is not under any exception
		(
			SELECT 1
			FROM k_tree_security_exception tse
			JOIN hm_NodeLinkPublishedHierarchy e on e.id_NodelinkPublished = tse.id_tree_node_published
			WHERE tse.id_tree_security = @IdTreeSecurity
			AND nph.hid.IsDescendantOf(e.hid) = 1
		)
 END

END


IF OBJECT_ID(''tempdb..#GlobalInfoColumns'') IS NOT NULL DROP TABLE #GlobalInfoColumns

SELECT ic.idPayee, ic.fullname, ic.start_date_histo, ic.end_date_histo, ic.[workerId], ic.[CommonLevel], ic.[Class Year], ic.[TalentRankDisplayPY], ic.[TalentRank], ic.[Promotion], ic.[SameStore], ic.[BonusTRWDFlag], ic.[TRWD Curr], ic.[2023 Annual TRWD USD], ic.[TRWD Ranges], ic.[Guarantee / Estimate], ic.[2022 Annual TRWD], ic.[2022 TRWD Curr], ic.[2020 Annual TRWD USD], ic.[2021 Annual TRWD USD], ic.[2022 Annual TRWD USD], ic.[2023 Actual YE Bonus], ic.[2023 Actual YE Bonus USD], ic.[Proration %], ic.[2022 Annual YE Bonus], ic.[2024 Salary Curr], ic.[Salary Matrix Step], ic.[Salary Ranges], ic.[2023 Annual Salary], ic.[baserate], ic.[2023 Actual Fixed Pay USD], ic.[Material Risk Taker Indicator], ic.[Matter For Review Indicator], ic.[2022 Actual YE Bonus], ic.[2022 Actual YE Bonus USD], ic.[2022 Advances], ic.[2022 Advances Curr], ic.[2022 Advances USD], ic.[2022 Annual Pre YE Var Pay], ic.[2022 Annual YE Bonus USD], ic.[2023 Mid-Year Decision], ic.[2023 Mid-Year Decision Curr], ic.[2022 Overtime], ic.[2022 Overtime Curr], ic.[2022 Pre YE Var Pay Curr], ic.[2022 YE Bonus Curr], ic.[2023 Actual Salary Lcl], ic.[2023 Actual Salary USD], ic.[2023 Actual TRWD], ic.[2023 Actual TRWD USD], ic.[2023 Actual YE Cash Bonus], ic.[2023 Actual YE Def Bonus USD], ic.[2023 Advances], ic.[2023 Advances Curr], ic.[2023 Advances USD], ic.[2023 Annual Pre YE Var Pay], ic.[2023 Annual Salary USD], ic.[2023 Annual YE Bonus USD], ic.[2023 Pre YE Var Pay Curr], ic.[2023 Salary Curr], ic.[2023 YE Bonus Curr], ic.[2023 YE Cash Bonus Curr], ic.[2023 YTD Overtime], ic.[2023 YTD Overtime Curr], ic.[2024 Salary USD], ic.[CYFinancials], ic.[Alt 0], ic.[Alt 1], ic.[City], ic.[Code Staff Indicator], ic.[Compensation Manager], ic.[Cost Center], ic.[Cost Center Region], ic.[Country], ic.[Decision Indicator], ic.[Dept], ic.[Division], ic.[Function], ic.[GIC], ic.[HireDate], ic.[Job Level], ic.[Job Title], ic.[New Job Level], ic.[Other TRWD Payments], ic.[Other TRWD Payments Curr], ic.[Compliance & Risk Mgmt], ic.[Culture & Leadership], ic.[Diversity & Inclusion], ic.[Manager Effectiveness], ic.[Performance & Contributions], ic.[Potential], ic.[Misc1], ic.[Region], ic.[Salary Change Lcl], ic.[Salary Change USD], ic.[Sub Dept], ic.[Sub Dept 2], ic.[Super Dept], ic.[Super Division], ic.[TRWD YoY Change Lcl], ic.[TotalRewardAnnUsdPy3]
INTO #GlobalInfoColumns
FROM ( (SELECT ic.idPayee, ic.fullname, ic.start_date_histo, ic.end_date_histo, ic.[workerId], ic.[CommonLevel], ic.[Class Year], ic.[TalentRankDisplayPY], ic.[TalentRank], ic.[Promotion], ic.[SameStore], ic.[BonusTRWDFlag], ic.[TRWD Curr], ic.[2023 Annual TRWD USD], ic.[TRWD Ranges], ic.[Guarantee / Estimate], ic.[2022 Annual TRWD], ic.[2022 TRWD Curr], ic.[2020 Annual TRWD USD], ic.[2021 Annual TRWD USD], ic.[2022 Annual TRWD USD], ic.[2023 Actual YE Bonus], ic.[2023 Actual YE Bonus USD], ic.[Proration %], ic.[2022 Annual YE Bonus], ic.[2024 Salary Curr], ic.[Salary Matrix Step], ic.[Salary Ranges], ic.[2023 Annual Salary], ic.[baserate], ic.[2023 Actual Fixed Pay USD], ic.[Material Risk Taker Indicator], ic.[Matter For Review Indicator], ic.[2022 Actual YE Bonus], ic.[2022 Actual YE Bonus USD], ic.[2022 Advances], ic.[2022 Advances Curr], ic.[2022 Advances USD], ic.[2022 Annual Pre YE Var Pay], ic.[2022 Annual YE Bonus USD], ic.[2023 Mid-Year Decision], ic.[2023 Mid-Year Decision Curr], ic.[2022 Overtime], ic.[2022 Overtime Curr], ic.[2022 Pre YE Var Pay Curr], ic.[2022 YE Bonus Curr], ic.[2023 Actual Salary Lcl], ic.[2023 Actual Salary USD], ic.[2023 Actual TRWD], ic.[2023 Actual TRWD USD], ic.[2023 Actual YE Cash Bonus], ic.[2023 Actual YE Def Bonus USD], ic.[2023 Advances], ic.[2023 Advances Curr], ic.[2023 Advances USD], ic.[2023 Annual Pre YE Var Pay], ic.[2023 Annual Salary USD], ic.[2023 Annual YE Bonus USD], ic.[2023 Pre YE Var Pay Curr], ic.[2023 Salary Curr], ic.[2023 YE Bonus Curr], ic.[2023 YE Cash Bonus Curr], ic.[2023 YTD Overtime], ic.[2023 YTD Overtime Curr], ic.[2024 Salary USD], ic.[CYFinancials], ic.[Alt 0], ic.[Alt 1], ic.[City], ic.[Code Staff Indicator], ic.[Compensation Manager], ic.[Cost Center], ic.[Cost Center Region], ic.[Country], ic.[Decision Indicator], ic.[Dept], ic.[Division], ic.[Function], ic.[GIC], ic.[HireDate], ic.[Job Level], ic.[Job Title], ic.[New Job Level], ic.[Other TRWD Payments], ic.[Other TRWD Payments Curr], ic.[Compliance & Risk Mgmt], ic.[Culture & Leadership], ic.[Diversity & Inclusion], ic.[Manager Effectiveness], ic.[Performance & Contributions], ic.[Potential], ic.[Misc1], ic.[Region], ic.[Salary Change Lcl], ic.[Salary Change USD], ic.[Sub Dept], ic.[Sub Dept 2], ic.[Super Dept], ic.[Super Division], ic.[TRWD YoY Change Lcl], ic.[TotalRewardAnnUsdPy3] FROM _ProcessYEInfoColumn2023 AS ic WHERE EXISTS (SELECT ic.idPayee FROM #PayeeStepListAll pl WHERE pl.idPayee = ic.idPayee)) ) ic
 -- join section for associated columns
WHERE @endDateFilter > ic.start_date_histo
AND @startDateFilter <= ic.end_date_histo

CREATE INDEX IX_idpayee ON #GlobalInfoColumns(idPayee,start_date_histo,end_date_histo) 


DECLARE @TotalCount INT;
;WITH InfoColumns AS 
(
SELECT * 
  FROM (SELECT ic.idPayee, ic.fullname, ic.start_date_histo, ic.end_date_histo, ic.[workerId], ic.[CommonLevel], ic.[Class Year], ic.[TalentRankDisplayPY], ic.[TalentRank], ic.[Promotion], ic.[SameStore], ic.[BonusTRWDFlag], ic.[TRWD Curr], ic.[2023 Annual TRWD USD], ic.[TRWD Ranges], ic.[Guarantee / Estimate], ic.[2022 Annual TRWD], ic.[2022 TRWD Curr], ic.[2020 Annual TRWD USD], ic.[2021 Annual TRWD USD], ic.[2022 Annual TRWD USD], ic.[2023 Actual YE Bonus], ic.[2023 Actual YE Bonus USD], ic.[Proration %], ic.[2022 Annual YE Bonus], ic.[2024 Salary Curr], ic.[Salary Matrix Step], ic.[Salary Ranges], ic.[2023 Annual Salary], ic.[baserate], ic.[2023 Actual Fixed Pay USD], ic.[Material Risk Taker Indicator], ic.[Matter For Review Indicator], ic.[2022 Actual YE Bonus], ic.[2022 Actual YE Bonus USD], ic.[2022 Advances], ic.[2022 Advances Curr], ic.[2022 Advances USD], ic.[2022 Annual Pre YE Var Pay], ic.[2022 Annual YE Bonus USD], ic.[2023 Mid-Year Decision], ic.[2023 Mid-Year Decision Curr], ic.[2022 Overtime], ic.[2022 Overtime Curr], ic.[2022 Pre YE Var Pay Curr], ic.[2022 YE Bonus Curr], ic.[2023 Actual Salary Lcl], ic.[2023 Actual Salary USD], ic.[2023 Actual TRWD], ic.[2023 Actual TRWD USD], ic.[2023 Actual YE Cash Bonus], ic.[2023 Actual YE Def Bonus USD], ic.[2023 Advances], ic.[2023 Advances Curr], ic.[2023 Advances USD], ic.[2023 Annual Pre YE Var Pay], ic.[2023 Annual Salary USD], ic.[2023 Annual YE Bonus USD], ic.[2023 Pre YE Var Pay Curr], ic.[2023 Salary Curr], ic.[2023 YE Bonus Curr], ic.[2023 YE Cash Bonus Curr], ic.[2023 YTD Overtime], ic.[2023 YTD Overtime Curr], ic.[2024 Salary USD], ic.[CYFinancials], ic.[Alt 0], ic.[Alt 1], ic.[City], ic.[Code Staff Indicator], ic.[Compensation Manager], ic.[Cost Center], ic.[Cost Center Region], ic.[Country], ic.[Decision Indicator], ic.[Dept], ic.[Division], ic.[Function], ic.[GIC], ic.[HireDate], ic.[Job Level], ic.[Job Title], ic.[New Job Level], ic.[Other TRWD Payments], ic.[Other TRWD Payments Curr], ic.[Compliance & Risk Mgmt], ic.[Culture & Leadership], ic.[Diversity & Inclusion], ic.[Manager Effectiveness], ic.[Performance & Contributions], ic.[Potential], ic.[Misc1], ic.[Region], ic.[Salary Change Lcl], ic.[Salary Change USD], ic.[Sub Dept], ic.[Sub Dept 2], ic.[Super Dept], ic.[Super Division], ic.[TRWD YoY Change Lcl], ic.[TotalRewardAnnUsdPy3] FROM _ProcessYEInfoColumn2023 AS ic) sub
 WHERE ''3000-01-01 00:00:00'' > sub.start_date_histo
   AND ''1980-01-01 00:00:00'' <= sub.end_date_histo
),
FilteredCTE
as
(
select distinct ic.idPayee, ic.fullname,ps.id_step 
from InfoColumns ic
inner join k_m_plans_payees_steps ps
	on ps.id_payee = ic.idPayee
 --left join k_m_plans_payees_steps_workflow ws on ps.id_step = ws.id_step

WHERE 1=1
	and ps.end_step > ic.start_date_histo
	and ps.start_step <= ic.end_date_histo
	and ps.id_plan = 60
	and ps.start_step < ''3000-01-01 00:00:00''
	and ps.end_step > ''1980-01-01 00:00:00''
     AND [idPayee] <> 226098 -- where cond
     -- wflStep filter
)
SELECT @TotalCount = COUNT(*) from #PayeeStepListAll psla
join FilteredCTE p on psla.IdStep = p.id_step

IF OBJECT_ID(''tempdb.dbo.#PayeeStepList'') IS NOT NULL
	DROP TABLE #PayeeStepList

create table #PayeeStepList
(
	IdNode INT,
	IdPayee INT,
	Level INT,
	IdStep INT,
	INDEX UIX_step UNIQUE CLUSTERED (IdStep),
	INDEX IX_payee NONCLUSTERED (IdPayee)
)


            ;WITH InfoColumns AS 
(
SELECT * 
  FROM (SELECT ic.idPayee, ic.fullname, ic.start_date_histo, ic.end_date_histo, ic.[workerId], ic.[CommonLevel], ic.[Class Year], ic.[TalentRankDisplayPY], ic.[TalentRank], ic.[Promotion], ic.[SameStore], ic.[BonusTRWDFlag], ic.[TRWD Curr], ic.[2023 Annual TRWD USD], ic.[TRWD Ranges], ic.[Guarantee / Estimate], ic.[2022 Annual TRWD], ic.[2022 TRWD Curr], ic.[2020 Annual TRWD USD], ic.[2021 Annual TRWD USD], ic.[2022 Annual TRWD USD], ic.[2023 Actual YE Bonus], ic.[2023 Actual YE Bonus USD], ic.[Proration %], ic.[2022 Annual YE Bonus], ic.[2024 Salary Curr], ic.[Salary Matrix Step], ic.[Salary Ranges], ic.[2023 Annual Salary], ic.[baserate], ic.[2023 Actual Fixed Pay USD], ic.[Material Risk Taker Indicator], ic.[Matter For Review Indicator], ic.[2022 Actual YE Bonus], ic.[2022 Actual YE Bonus USD], ic.[2022 Advances], ic.[2022 Advances Curr], ic.[2022 Advances USD], ic.[2022 Annual Pre YE Var Pay], ic.[2022 Annual YE Bonus USD], ic.[2023 Mid-Year Decision], ic.[2023 Mid-Year Decision Curr], ic.[2022 Overtime], ic.[2022 Overtime Curr], ic.[2022 Pre YE Var Pay Curr], ic.[2022 YE Bonus Curr], ic.[2023 Actual Salary Lcl], ic.[2023 Actual Salary USD], ic.[2023 Actual TRWD], ic.[2023 Actual TRWD USD], ic.[2023 Actual YE Cash Bonus], ic.[2023 Actual YE Def Bonus USD], ic.[2023 Advances], ic.[2023 Advances Curr], ic.[2023 Advances USD], ic.[2023 Annual Pre YE Var Pay], ic.[2023 Annual Salary USD], ic.[2023 Annual YE Bonus USD], ic.[2023 Pre YE Var Pay Curr], ic.[2023 Salary Curr], ic.[2023 YE Bonus Curr], ic.[2023 YE Cash Bonus Curr], ic.[2023 YTD Overtime], ic.[2023 YTD Overtime Curr], ic.[2024 Salary USD], ic.[CYFinancials], ic.[Alt 0], ic.[Alt 1], ic.[City], ic.[Code Staff Indicator], ic.[Compensation Manager], ic.[Cost Center], ic.[Cost Center Region], ic.[Country], ic.[Decision Indicator], ic.[Dept], ic.[Division], ic.[Function], ic.[GIC], ic.[HireDate], ic.[Job Level], ic.[Job Title], ic.[New Job Level], ic.[Other TRWD Payments], ic.[Other TRWD Payments Curr], ic.[Compliance & Risk Mgmt], ic.[Culture & Leadership], ic.[Diversity & Inclusion], ic.[Manager Effectiveness], ic.[Performance & Contributions], ic.[Potential], ic.[Misc1], ic.[Region], ic.[Salary Change Lcl], ic.[Salary Change USD], ic.[Sub Dept], ic.[Sub Dept 2], ic.[Super Dept], ic.[Super Division], ic.[TRWD YoY Change Lcl], ic.[TotalRewardAnnUsdPy3] FROM _ProcessYEInfoColumn2023 AS ic) sub
 WHERE ''3000-01-01 00:00:00'' > sub.start_date_histo
   AND ''1980-01-01 00:00:00'' <= sub.end_date_histo
),
FilteredCTE
as
(
select distinct ic.idPayee, ic.fullname,ps.id_step 
from InfoColumns ic
inner join k_m_plans_payees_steps ps
	on ps.id_payee = ic.idPayee
 --left join k_m_plans_payees_steps_workflow ws on ps.id_step = ws.id_step

WHERE 1=1
	and ps.end_step > ic.start_date_histo
	and ps.start_step <= ic.end_date_histo
	and ps.id_plan = 60
	and ps.start_step < ''3000-01-01 00:00:00''
	and ps.end_step > ''1980-01-01 00:00:00''
     AND [idPayee] <> 226098 -- where cond
     -- wflStep filter
)
		    INSERT INTO #PayeeStepList
		    select psla.IdNode, psla.IdPayee, psla.Level, psla.IdStep  
		    from #PayeeStepListAll psla
            join FilteredCTE p on psla.IdStep = p.id_step
            WHERE 1 = 1
            
    


IF OBJECT_ID(''tempdb..#ValidationSteps'') IS NOT NULL DROP TABLE #ValidationSteps
CREATE TABLE #ValidationSteps
(
	id_step				INT,
	idPayee				INT,
    Level				INT,
	WStep				NVARCHAR(50),
	WStatusId			NVARCHAR(255),
	WStatus				INT,
	WStatusByManagerId	INT	
)

CREATE CLUSTERED INDEX Cl_Ix_id_step_idPayee ON #ValidationSteps
(id_step,idPayee)


IF OBJECT_ID(''tempdb..#WorkFlowStepInfo'') IS NOT NULL DROP TABLE #WorkFlowStepInfo
CREATE TABLE #WorkFlowStepInfo
(
	id_step	 INT,
	idPayee	INT,
	id_wfl_step	INT,
	Status INT
)

CREATE CLUSTERED INDEX Cl_Ix_id_step_idPayee_#WorkFlowStepInfo ON #WorkFlowStepInfo
(id_step,idPayee)


INSERT INTO #ValidationSteps
(idPayee, Level, id_step)
SELECT psl.[IdPayee], psl.[Level], psl.[IdStep]
  FROM #PayeeStepList as psl

;with WorkFlowStepInfo
as
(
select vs.id_step,vs.idPayee
	   ,CASE WHEN sw.id_wflstep IS NOT NULL THEN sw.id_wflstep
			ELSE CASE WHEN sw2.id_wflstep IS NOT NULL THEN sw2.id_wflstep
			ELSE CASE WHEN mw.id_wflstep IS NOT NULL THEN mw.id_wflstep
			ELSE mw2.id_wflstep END
			END END as id_wfl_step
		,CASE WHEN sw.id_wflstep IS NOT NULL THEN sw.statut_step
			ELSE CASE WHEN sw2.id_wflstep IS NOT NULL THEN sw2.statut_step
			ELSE CASE WHEN mw.id_wflstep IS NOT NULL THEN -1
			ELSE -1 END
			END END as [Status]
  from #ValidationSteps as vs
	left outer join (
		select ws.id_wflstep,ws.id_workflow,ws.name_step,ws.sort_step,ps.id_step,ps.id_payee,sw.statut_step
						  from dbo.k_m_workflow_step ws
			inner join dbo.k_m_plans mp on ws.id_workflow = mp.id_workflow
							inner join dbo.k_m_plans_payees_steps_workflow as sw
				inner join dbo.k_m_workflow_step ws2 on sw.id_workflow_step = ws2.id_wflstep
								on ws.sort_step = ws2.sort_step
			inner join dbo.k_m_plans_payees_steps ps on sw.id_step = ps.id_step and ps.id_plan = @id_plan
			inner join k_m_workflow_step_group wsg on ws.id_wflstep = wsg.id_wflstep
			inner join k_m_workflow_step_group_profile wsgp on wsg.id_wflstepgroup = wsgp.id_wflstepgroup
			where wsgp.id_profile = @id_profile
						   and mp.id_plan = @id_plan
					) as sw
		on vs.id_step = sw.id_step
		and vs.idPayee = sw.id_payee
	left outer join (
		select ws.id_wflstep,ws.id_workflow,ws.name_step,ws.sort_step,ws.id_step,ws.id_payee, ws.statut_step
						  from (
								select ws.*, ps.id_step, ps.id_payee, sw.statut_step, ROW_NUMBER() OVER(PARTITION BY ps.id_step ORDER BY sw.id) AS RN
									from dbo.k_m_workflow_step ws
									inner join  dbo.k_m_plans_payees_steps_workflow as sw
										on ws.id_wflstep = sw.id_workflow_step
									inner join dbo.k_m_plans_payees_steps ps
										on sw.id_step = ps.id_step
										and ps.id_plan = @id_plan
							) ws
						 where ws.RN = 1
					) as sw2
		on vs.id_step = sw2.id_step
		and vs.idPayee = sw2.id_payee
	left outer join (
						select mw.id_wflstep,mw.id_workflow,mw.name_step,mw.sort_step
						  from k_m_workflow_step mw
							inner join k_m_plans mp
								on mw.id_workflow = mp.id_workflow
								and mp.id_plan = @id_plan
			inner join k_m_workflow_step_group wsg on mw.id_wflstep = wsg.id_wflstep
			inner join k_m_workflow_step_group_profile wsgp on wsg.id_wflstepgroup = wsgp.id_wflstepgroup
			where wsgp.id_profile = @id_profile			
						   and mw.sort_step = (select top 1 sort_step from k_m_workflow_step ws2 where mw.id_workflow = ws2.id_workflow order by ws2.sort_step asc, ws2.id_wflstep asc)
					) as mw
		on 1=1
	left outer join (
		select sub.id_wflstep,sub.id_workflow,sub.name_step,sub.sort_step
						  from (select mw.*, row_number() over(order by mw.sort_step asc, mw.id_wflstep asc) rn
						  from k_m_workflow_step mw
							inner join k_m_plans mp
								on mw.id_workflow = mp.id_workflow
								and mp.id_plan = @id_plan
							) as sub
						where rn = 1
					) as mw2
		on 1=1
)
INSERT INTO #WorkFlowStepInfo
SELECT * FROM WorkFlowStepInfo

;WITH Step2WFL as (
select vs.id_step, vs.idPayee, vs.Level, wsl.name_step_translated  as WStep, ws.id_wflstep as WStatusId, wsi.Status, wsg.level_step as levelStep, wsg.is_min_level as IsMinLevel, wsgp.id_wflstepgroupprofile
  from #ValidationSteps vs
	inner join #WorkFlowStepInfo wsi
		on vs.id_step = wsi.id_step
		and vs.idPayee = wsi.idPayee
	inner join dbo.k_m_workflow_step ws
		on wsi.id_wfl_step = ws.id_wflstep
    inner join k_m_workflow_step_group wsg on ws.id_wflstep = wsg.id_wflstep
    left join k_m_workflow_step_group_profile wsgp on wsg.id_wflstepgroup = wsgp.id_wflstepgroup AND wsgp.id_profile = @id_profile
    LEFT JOIN #WorkflowStepLocalization wsl on wsl.id_wflstep = ws.id_wflstep
)
,Step3WFL
as
(select s.*
	   ,CASE WHEN s.[Status] IN (-1, -3 /* EnWfStatus.Pending , EnWfStatus.Invalidated */)
			 THEN CASE WHEN id_wflstepgroupprofile IS NULL THEN 1 /*EnWfStatusByManager.ComingFromDown*/
                       WHEN s.Level = s.levelStep THEN 2 /*EnWfStatusByManager.PendingOnMe*/
					   WHEN s.Level > s.levelStep AND s.IsMinLevel = 1 THEN 4 /*EnWfStatusByManager.IAcceptedStillInProcess*/
					   WHEN s.Level > s.levelStep AND s.IsMinLevel != 1 THEN 1 /*EnWfStatusByManager.ComingFromDown*/
					   WHEN s.Level < s.levelStep AND s.Status = -3 /* EnWfStatus.Invalidated */ THEN 3 /*EnWfStatusByManager.Rejected*/
					   WHEN s.Level < s.levelStep AND s.Status != -3 /* EnWfStatus.Invalidated */ THEN 4 /*EnWfStatusByManager.IAcceptedStillInProcess*/
					END
			 WHEN s.[Status] = -2 /* EnWfStatus.Validated */
			 THEN 5 /* EnWfStatusByManager.AllAccepted */
			 ELSE 4 /* EnWfStatusByManager.IAcceptedStillInProcess */ END AS StatusByManager
  from Step2WFL AS s
)
update vs
   set WStatus = sw.StatusByManager, -- sw.[Status],
	   WStatusId = sw.WStatusId,
	   WStep = sw.WStep,
	   WStatusByManagerId = sw.StatusByManager
  from #ValidationSteps vs
	inner join Step3WFL sw
		on vs.id_step = sw.id_step
		and vs.idPayee = sw.idPayee



DECLARE @MaxLevelCount INT;
Select @MaxLevelCount = MAX(Level) FROM #PayeeStepList

IF OBJECT_ID(''tempdb..#TreePlanRecursiveExceptionFiltered'') IS NOT NULL DROP TABLE #TreePlanRecursiveExceptionFiltered
Create table #TreePlanRecursiveExceptionFiltered
(
	id									   INT,
	idChild								   INT,
	idParent							   INT,
	idType								   INT,
	id_tree_security_plan_level_exception  INT,
	is_override_workflow_permission		   BIT,
	is_validate							   BIT,
	is_invalidate						   BIT,
	is_mass_validate					   BIT,
	is_mass_invalidate					   BIT,
	is_edit								   BIT,
	is_read								   BIT,
	main_is_override_workflow_permission   BIT,
	main_is_validate					   BIT,
	main_is_invalidate					   BIT,
	main_is_mass_validate				   BIT,
	main_is_mass_invalidate				   BIT,
	main_is_edit						   BIT,
	main_is_read						   BIT,
	id_step								   INT
)

;WITH
TreeData
AS
(
	select id, idChild, idType, idParent, idTypeParent
	from hm_NodelinkPublished
	where idTree in 
	(
		select p.idTree from k_tree_security ts
		join hm_NodelinkPublished p
			on p.id = ts.id_tree_node_published
		where ts.id_tree_security =  447270
	)
)
, PlanLevelExceptions
as
(
	select np.id, np.idChild, tsple.*
	from k_tree_security_plan_level tspl
	join k_m_plan_data_security pds
		on tspl.id_tree_security_plan_level = pds.id_tree_security_plan_level
	join k_tree_security ts
		on ts.id_tree_security = tspl.id_tree_security
	join k_tree_security_plan_level_exception tsple
		on tsple.id_tree_security_plan_level = tspl.id_tree_security_plan_level
	join hm_NodelinkPublished np
		on np.id = tsple.id_tree_node_published
	where pds.id_process = 60
    AND tspl.id_tree_security_plan_level IN
    (
        SELECT TOP 1
		    id_tree_security_plan_level
		FROM k_tree_security_plan_level where id_tree_security = 447270
        AND begin_date <= getutcdate() 
        AND (end_Date is null OR end_date > getutcdate())
    )
)
, TreePlanExceptions
as
(
	SELECT 
		tr.*, pe.id_tree_security_plan_level_exception, pe.is_override_workflow_permission, pe.is_validate, pe.is_invalidate, 
		pe.is_mass_invalidate, pe.is_mass_validate, pe.is_edit, pe.is_read
	FROM TreeData tr
	LEFT JOIN PlanLevelExceptions pe
	ON pe.id = tr.id
)
, TreePlanExceptionsInherited
AS
(
	SELECT 
		 tr.id
		,tr.idChild
		,tr.idType
		,tr.idParent
		,tr.idTypeParent
		,pe.id_tree_security_plan_level_exception
		,pe.is_override_workflow_permission
		,pe.is_validate
		,pe.is_invalidate
		,pe.is_mass_invalidate
		,pe.is_mass_validate
		,pe.is_edit
		,pe.is_read
	FROM TreeData tr
	INNER JOIN PlanLevelExceptions pe
		ON pe.id = tr.id

	UNION ALL

	SELECT 
		 pe.id
		,pe.idChild
		,pe.idType
		,pe.idParent
		,pe.idTypeParent
		,pei.id_tree_security_plan_level_exception
		,pei.is_override_workflow_permission
		,pei.is_validate
		,pei.is_invalidate
		,pei.is_mass_invalidate
		,pei.is_mass_validate
		,pei.is_edit
		,pei.is_read
	FROM
	TreePlanExceptionsInherited pei
	JOIN TreePlanExceptions pe
		ON pei.idChild = pe.idParent AND pei.idType = pe.idTypeParent
	WHERE pe.id_tree_security_plan_level_exception IS NULL
)
, ActualPlanLevelExceptions
AS
(
	SELECT 
		 tr.id
		,tr.idChild
		,tr.idType
		,tr.idParent
		,tr.idTypeParent
		,pei.id_tree_security_plan_level_exception
		,ISNULL(pei.is_override_workflow_permission, M.main_is_override_workflow_permission) is_override_workflow_permission
		,ISNULL(pei.is_validate, M.main_is_validate) is_validate
		,ISNULL(pei.is_invalidate, M.main_is_invalidate) is_invalidate
		,ISNULL(pei.is_mass_validate, M.main_is_mass_validate) is_mass_validate
		,ISNULL(pei.is_mass_invalidate, M.main_is_mass_invalidate) is_mass_invalidate
		,ISNULL(pei.is_edit, M.main_is_edit) is_edit
		,ISNULL(pei.is_read, M.main_is_read) is_read
		,M.main_is_override_workflow_permission
		,M.main_is_validate
		,M.main_is_invalidate
		,M.main_is_mass_validate
		,M.main_is_mass_invalidate
		,M.main_is_edit
		,M.main_is_read
	FROM TreeData tr
	LEFT JOIN TreePlanExceptionsInherited pei
		ON tr.id = pei.id
	CROSS JOIN 
	(
		SELECT TOP 1
		 is_override_workflow_permission main_is_override_workflow_permission
		,is_edit main_is_edit
		,is_read main_is_read
		,is_mass_validate main_is_mass_validate
		,is_mass_invalidate main_is_mass_invalidate
		,is_invalidate main_is_invalidate
		,is_validate main_is_validate 
		FROM k_tree_security_plan_level where id_tree_security = 447270
        AND begin_date <= getutcdate() 
        AND (end_Date is null OR end_date > getutcdate())
	) AS M
)
,TreePlanRecursiveExceptionFiltered
as
(
	select te.* ,p.IdStep as id_step
	from ActualPlanLevelExceptions te
    join #PayeeStepList p on p.[IdPayee] = te.idChild AND te.idType = 14
)
INSERT INTO #TreePlanRecursiveExceptionFiltered 
(
 id
,idChild
,idParent
,idType
,id_tree_security_plan_level_exception
,is_override_workflow_permission
,is_validate
,is_invalidate
,is_mass_validate
,is_mass_invalidate
,is_edit
,is_read
,main_is_override_workflow_permission
,main_is_validate
,main_is_invalidate
,main_is_mass_validate
,main_is_mass_invalidate
,main_is_edit
,main_is_read
,id_step
)
select
 id
,idChild
,idParent
,idType
,id_tree_security_plan_level_exception
,is_override_workflow_permission
,is_validate
,is_invalidate
,is_mass_validate
,is_mass_invalidate
,is_edit
,is_read
,main_is_override_workflow_permission
,main_is_validate
,main_is_invalidate
,main_is_mass_validate
,main_is_mass_invalidate
,main_is_edit
,main_is_read
,id_step
from TreePlanRecursiveExceptionFiltered


IF OBJECT_ID(''tempdb..#SQLFieldQuery'') IS NOT NULL DROP TABLE #SQLFieldQuery

SELECT *
INTO #SQLFieldQuery
FROM 
(SELECT UP.[IdPayee], UP.[idStep] as IdStep, MIN([V0].[TRWDAnn]) AS [s_181_1333_42749] ,MIN([V0].[TRWDYoYPct]) AS [s_181_1335_42752] ,MIN([V0].[YEBAnn]) AS [s_182_1337_42750] ,MIN([V0].[YEBYoyPct]) AS [s_182_1338_42753] ,MIN([V0].[NewBaseAnn]) AS [s_183_1327_42751] ,MIN([V0].[NewBaseYoyPct]) AS [s_183_1331_42754] ,MIN([V0].[NewBaseRate]) AS [s_183_1330_42762] ,MIN([V0].[NewBaseEdit]) AS [s_187_1328_42759] ,MIN([V0].[TrwdEdit]) AS [s_187_1334_42758] ,MIN([V0].[YOY]) AS [s_187_1339_42755] ,MIN([V0].[standardHours]) AS [s_187_1332_42763] ,MIN([V0].[UsdToLcLTRWD]) AS [s_187_1336_42761] ,MIN([V0].[LcLToUsdTRWD]) AS [s_187_1326_42760] ,MIN([V0].[MFREdit]) AS [s_187_1351_42969] ,MIN([V0].[MRTEdit]) AS [s_187_1352_42970] ,MIN([V0].[CommMGREdit]) AS [s_187_1353_42971] 
FROM 
	(
		SELECT psl.IdPayee, psl.IdStep FROM #PayeeStepList psl
	) AS [UP] 
 LEFT OUTER JOIN _ProcessYEIndicators2023 AS [V0] ON [UP].[IdPayee] = [V0].[idPayee]  GROUP BY UP.IdStep, UP.IdPayee) AS t_SQLFieldQuery


;WITH filteredPayeeSteps AS 
(
	select ps.* ,psla.Sort_Order as RN from k_m_plans_payees_steps ps
            left join k_m_plans_payees_steps_workflow ws on ps.id_step = ws.id_step
            
             join #PayeeStepListAll psla on ps.id_step = psla.IdStep 
            WHERE 1=1
            
), 
sourcequery AS
(
select ps.RN, kv.id_value, kv.id_ind, kv.id_field, ps.id_step, ps.start_step, ps.end_step, 
kv.input_value as input_value,kfv.label as input_label
, 
CAST(60 AS nvarchar(5)) + ''_'' + CAST(mpi.id_ind AS nvarchar(5)) + ''_'' + CAST(mif.id_field AS nvarchar(5)) as ind_field,
CASE WHEN kf.id_control_type = -1
THEN
CAST(60 AS nvarchar(5)) + ''_'' + CAST(mpi.id_ind AS nvarchar(5)) + ''_'' + CAST(mif.id_field AS nvarchar(5)) + ''_lbl'' 
ELSE NULL
END as ind_field_lbl,
CAST(60 AS nvarchar(5)) + ''_'' + CAST(mpi.id_ind AS nvarchar(5)) + ''_'' + CAST(mif.id_field AS nvarchar(5))+''_'' +case when kv.id_value is not null then ''1'' else ''0'' end hasKMValueRow
  from filteredPayeeSteps ps
    inner join k_m_plans_indicators mpi
		on ps.id_plan = mpi.id_plan
	inner join k_m_indicators_fields mif
		on mpi.id_ind = mif.id_ind
	left outer join dbo.Kernel_View_k_m_values kv
		on kv.id_step = ps.id_step and kv.id_ind = mpi.id_ind and kv.id_field = mif.id_field 
    left outer join k_m_fields kf
		on kv.id_field = kf.id_field
 left outer join dbo.k_m_fields_values kfv
                                        on kv.id_field = kfv.id_field
                                        and kv.input_value = kfv.value
                                        and kfv.culture = @Culture
                                        and kf.id_control_type = -1
                                        
                                            
 
 where ps.id_plan = 60
   and ps.id_payee IN (SELECT [IdPayee] FROM #PayeeStepList)
)


,  tmp_InfoColumns AS (
    SELECT ic.* 
    FROM #GlobalInfoColumns ic
    WHERE ic.idPayee IN (SELECT [IdPayee] FROM #PayeeStepList) 
)









select  psl.IdNode, psl.Level as Level, pvt.id_step,ps.uid_reference, ic.idPayee, ic.fullname,
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[workerId]
WHEN is_override_workflow_permission = 0 then ic.[workerId]
END AS [workerId],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[CommonLevel]
WHEN is_override_workflow_permission = 0 then ic.[CommonLevel]
END AS [CommonLevel],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Class Year]
WHEN is_override_workflow_permission = 0 then ic.[Class Year]
END AS [Class Year],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[TalentRankDisplayPY]
WHEN is_override_workflow_permission = 0 then ic.[TalentRankDisplayPY]
END AS [TalentRankDisplayPY],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[TalentRank]
WHEN is_override_workflow_permission = 0 then ic.[TalentRank]
END AS [TalentRank],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Promotion]
WHEN is_override_workflow_permission = 0 then ic.[Promotion]
END AS [Promotion],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[SameStore]
WHEN is_override_workflow_permission = 0 then ic.[SameStore]
END AS [SameStore],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[BonusTRWDFlag]
WHEN is_override_workflow_permission = 0 then ic.[BonusTRWDFlag]
END AS [BonusTRWDFlag],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[TRWD Curr]
WHEN is_override_workflow_permission = 0 then ic.[TRWD Curr]
END AS [TRWD Curr],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Annual TRWD USD]
WHEN is_override_workflow_permission = 0 then ic.[2023 Annual TRWD USD]
END AS [2023 Annual TRWD USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[TRWD Ranges]
WHEN is_override_workflow_permission = 0 then ic.[TRWD Ranges]
END AS [TRWD Ranges],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Guarantee / Estimate]
WHEN is_override_workflow_permission = 0 then ic.[Guarantee / Estimate]
END AS [Guarantee / Estimate],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2022 Annual TRWD]
WHEN is_override_workflow_permission = 0 then ic.[2022 Annual TRWD]
END AS [2022 Annual TRWD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2022 TRWD Curr]
WHEN is_override_workflow_permission = 0 then ic.[2022 TRWD Curr]
END AS [2022 TRWD Curr],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2020 Annual TRWD USD]
WHEN is_override_workflow_permission = 0 then ic.[2020 Annual TRWD USD]
END AS [2020 Annual TRWD USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2021 Annual TRWD USD]
WHEN is_override_workflow_permission = 0 then ic.[2021 Annual TRWD USD]
END AS [2021 Annual TRWD USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2022 Annual TRWD USD]
WHEN is_override_workflow_permission = 0 then ic.[2022 Annual TRWD USD]
END AS [2022 Annual TRWD USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Actual YE Bonus]
WHEN is_override_workflow_permission = 0 then ic.[2023 Actual YE Bonus]
END AS [2023 Actual YE Bonus],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Actual YE Bonus USD]
WHEN is_override_workflow_permission = 0 then ic.[2023 Actual YE Bonus USD]
END AS [2023 Actual YE Bonus USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Proration %]
WHEN is_override_workflow_permission = 0 then ic.[Proration %]
END AS [Proration %],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2022 Annual YE Bonus]
WHEN is_override_workflow_permission = 0 then ic.[2022 Annual YE Bonus]
END AS [2022 Annual YE Bonus],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2024 Salary Curr]
WHEN is_override_workflow_permission = 0 then ic.[2024 Salary Curr]
END AS [2024 Salary Curr],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Salary Matrix Step]
WHEN is_override_workflow_permission = 0 then ic.[Salary Matrix Step]
END AS [Salary Matrix Step],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Salary Ranges]
WHEN is_override_workflow_permission = 0 then ic.[Salary Ranges]
END AS [Salary Ranges],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Annual Salary]
WHEN is_override_workflow_permission = 0 then ic.[2023 Annual Salary]
END AS [2023 Annual Salary],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[baserate]
WHEN is_override_workflow_permission = 0 then ic.[baserate]
END AS [baserate],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Actual Fixed Pay USD]
WHEN is_override_workflow_permission = 0 then ic.[2023 Actual Fixed Pay USD]
END AS [2023 Actual Fixed Pay USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Material Risk Taker Indicator]
WHEN is_override_workflow_permission = 0 then ic.[Material Risk Taker Indicator]
END AS [Material Risk Taker Indicator],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Matter For Review Indicator]
WHEN is_override_workflow_permission = 0 then ic.[Matter For Review Indicator]
END AS [Matter For Review Indicator],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2022 Actual YE Bonus]
WHEN is_override_workflow_permission = 0 then ic.[2022 Actual YE Bonus]
END AS [2022 Actual YE Bonus],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2022 Actual YE Bonus USD]
WHEN is_override_workflow_permission = 0 then ic.[2022 Actual YE Bonus USD]
END AS [2022 Actual YE Bonus USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2022 Advances]
WHEN is_override_workflow_permission = 0 then ic.[2022 Advances]
END AS [2022 Advances],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2022 Advances Curr]
WHEN is_override_workflow_permission = 0 then ic.[2022 Advances Curr]
END AS [2022 Advances Curr],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2022 Advances USD]
WHEN is_override_workflow_permission = 0 then ic.[2022 Advances USD]
END AS [2022 Advances USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2022 Annual Pre YE Var Pay]
WHEN is_override_workflow_permission = 0 then ic.[2022 Annual Pre YE Var Pay]
END AS [2022 Annual Pre YE Var Pay],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2022 Annual YE Bonus USD]
WHEN is_override_workflow_permission = 0 then ic.[2022 Annual YE Bonus USD]
END AS [2022 Annual YE Bonus USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Mid-Year Decision]
WHEN is_override_workflow_permission = 0 then ic.[2023 Mid-Year Decision]
END AS [2023 Mid-Year Decision],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Mid-Year Decision Curr]
WHEN is_override_workflow_permission = 0 then ic.[2023 Mid-Year Decision Curr]
END AS [2023 Mid-Year Decision Curr],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2022 Overtime]
WHEN is_override_workflow_permission = 0 then ic.[2022 Overtime]
END AS [2022 Overtime],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2022 Overtime Curr]
WHEN is_override_workflow_permission = 0 then ic.[2022 Overtime Curr]
END AS [2022 Overtime Curr],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2022 Pre YE Var Pay Curr]
WHEN is_override_workflow_permission = 0 then ic.[2022 Pre YE Var Pay Curr]
END AS [2022 Pre YE Var Pay Curr],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2022 YE Bonus Curr]
WHEN is_override_workflow_permission = 0 then ic.[2022 YE Bonus Curr]
END AS [2022 YE Bonus Curr],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Actual Salary Lcl]
WHEN is_override_workflow_permission = 0 then ic.[2023 Actual Salary Lcl]
END AS [2023 Actual Salary Lcl],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Actual Salary USD]
WHEN is_override_workflow_permission = 0 then ic.[2023 Actual Salary USD]
END AS [2023 Actual Salary USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Actual TRWD]
WHEN is_override_workflow_permission = 0 then ic.[2023 Actual TRWD]
END AS [2023 Actual TRWD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Actual TRWD USD]
WHEN is_override_workflow_permission = 0 then ic.[2023 Actual TRWD USD]
END AS [2023 Actual TRWD USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Actual YE Cash Bonus]
WHEN is_override_workflow_permission = 0 then ic.[2023 Actual YE Cash Bonus]
END AS [2023 Actual YE Cash Bonus],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Actual YE Def Bonus USD]
WHEN is_override_workflow_permission = 0 then ic.[2023 Actual YE Def Bonus USD]
END AS [2023 Actual YE Def Bonus USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Advances]
WHEN is_override_workflow_permission = 0 then ic.[2023 Advances]
END AS [2023 Advances],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Advances Curr]
WHEN is_override_workflow_permission = 0 then ic.[2023 Advances Curr]
END AS [2023 Advances Curr],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Advances USD]
WHEN is_override_workflow_permission = 0 then ic.[2023 Advances USD]
END AS [2023 Advances USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Annual Pre YE Var Pay]
WHEN is_override_workflow_permission = 0 then ic.[2023 Annual Pre YE Var Pay]
END AS [2023 Annual Pre YE Var Pay],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Annual Salary USD]
WHEN is_override_workflow_permission = 0 then ic.[2023 Annual Salary USD]
END AS [2023 Annual Salary USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Annual YE Bonus USD]
WHEN is_override_workflow_permission = 0 then ic.[2023 Annual YE Bonus USD]
END AS [2023 Annual YE Bonus USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Pre YE Var Pay Curr]
WHEN is_override_workflow_permission = 0 then ic.[2023 Pre YE Var Pay Curr]
END AS [2023 Pre YE Var Pay Curr],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 Salary Curr]
WHEN is_override_workflow_permission = 0 then ic.[2023 Salary Curr]
END AS [2023 Salary Curr],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 YE Bonus Curr]
WHEN is_override_workflow_permission = 0 then ic.[2023 YE Bonus Curr]
END AS [2023 YE Bonus Curr],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 YE Cash Bonus Curr]
WHEN is_override_workflow_permission = 0 then ic.[2023 YE Cash Bonus Curr]
END AS [2023 YE Cash Bonus Curr],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 YTD Overtime]
WHEN is_override_workflow_permission = 0 then ic.[2023 YTD Overtime]
END AS [2023 YTD Overtime],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2023 YTD Overtime Curr]
WHEN is_override_workflow_permission = 0 then ic.[2023 YTD Overtime Curr]
END AS [2023 YTD Overtime Curr],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[2024 Salary USD]
WHEN is_override_workflow_permission = 0 then ic.[2024 Salary USD]
END AS [2024 Salary USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[CYFinancials]
WHEN is_override_workflow_permission = 0 then ic.[CYFinancials]
END AS [CYFinancials],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Alt 0]
WHEN is_override_workflow_permission = 0 then ic.[Alt 0]
END AS [Alt 0],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Alt 1]
WHEN is_override_workflow_permission = 0 then ic.[Alt 1]
END AS [Alt 1],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[City]
WHEN is_override_workflow_permission = 0 then ic.[City]
END AS [City],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Code Staff Indicator]
WHEN is_override_workflow_permission = 0 then ic.[Code Staff Indicator]
END AS [Code Staff Indicator],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Compensation Manager]
WHEN is_override_workflow_permission = 0 then ic.[Compensation Manager]
END AS [Compensation Manager],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Cost Center]
WHEN is_override_workflow_permission = 0 then ic.[Cost Center]
END AS [Cost Center],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Cost Center Region]
WHEN is_override_workflow_permission = 0 then ic.[Cost Center Region]
END AS [Cost Center Region],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Country]
WHEN is_override_workflow_permission = 0 then ic.[Country]
END AS [Country],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Decision Indicator]
WHEN is_override_workflow_permission = 0 then ic.[Decision Indicator]
END AS [Decision Indicator],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Dept]
WHEN is_override_workflow_permission = 0 then ic.[Dept]
END AS [Dept],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Division]
WHEN is_override_workflow_permission = 0 then ic.[Division]
END AS [Division],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Function]
WHEN is_override_workflow_permission = 0 then ic.[Function]
END AS [Function],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[GIC]
WHEN is_override_workflow_permission = 0 then ic.[GIC]
END AS [GIC],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[HireDate]
WHEN is_override_workflow_permission = 0 then ic.[HireDate]
END AS [HireDate],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Job Level]
WHEN is_override_workflow_permission = 0 then ic.[Job Level]
END AS [Job Level],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Job Title]
WHEN is_override_workflow_permission = 0 then ic.[Job Title]
END AS [Job Title],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[New Job Level]
WHEN is_override_workflow_permission = 0 then ic.[New Job Level]
END AS [New Job Level],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Other TRWD Payments]
WHEN is_override_workflow_permission = 0 then ic.[Other TRWD Payments]
END AS [Other TRWD Payments],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Other TRWD Payments Curr]
WHEN is_override_workflow_permission = 0 then ic.[Other TRWD Payments Curr]
END AS [Other TRWD Payments Curr],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Compliance & Risk Mgmt]
WHEN is_override_workflow_permission = 0 then ic.[Compliance & Risk Mgmt]
END AS [Compliance & Risk Mgmt],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Culture & Leadership]
WHEN is_override_workflow_permission = 0 then ic.[Culture & Leadership]
END AS [Culture & Leadership],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Diversity & Inclusion]
WHEN is_override_workflow_permission = 0 then ic.[Diversity & Inclusion]
END AS [Diversity & Inclusion],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Manager Effectiveness]
WHEN is_override_workflow_permission = 0 then ic.[Manager Effectiveness]
END AS [Manager Effectiveness],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Performance & Contributions]
WHEN is_override_workflow_permission = 0 then ic.[Performance & Contributions]
END AS [Performance & Contributions],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Potential]
WHEN is_override_workflow_permission = 0 then ic.[Potential]
END AS [Potential],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Misc1]
WHEN is_override_workflow_permission = 0 then ic.[Misc1]
END AS [Misc1],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Region]
WHEN is_override_workflow_permission = 0 then ic.[Region]
END AS [Region],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Salary Change Lcl]
WHEN is_override_workflow_permission = 0 then ic.[Salary Change Lcl]
END AS [Salary Change Lcl],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Salary Change USD]
WHEN is_override_workflow_permission = 0 then ic.[Salary Change USD]
END AS [Salary Change USD],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Sub Dept]
WHEN is_override_workflow_permission = 0 then ic.[Sub Dept]
END AS [Sub Dept],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Sub Dept 2]
WHEN is_override_workflow_permission = 0 then ic.[Sub Dept 2]
END AS [Sub Dept 2],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Super Dept]
WHEN is_override_workflow_permission = 0 then ic.[Super Dept]
END AS [Super Dept],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[Super Division]
WHEN is_override_workflow_permission = 0 then ic.[Super Division]
END AS [Super Division],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[TRWD YoY Change Lcl]
WHEN is_override_workflow_permission = 0 then ic.[TRWD YoY Change Lcl]
END AS [TRWD YoY Change Lcl],
CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
WHEN is_override_workflow_permission = 1 AND is_read = 1 then ic.[TotalRewardAnnUsdPy3]
WHEN is_override_workflow_permission = 0 then ic.[TotalRewardAnnUsdPy3]
END AS [TotalRewardAnnUsdPy3] ,case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_181_1333_HasKMValue] = ''10'' THEN sq.[s_181_1333_42749] ELSE [60_181_1333] END
else 
 CASE WHEN [60_181_1333_HasKMValue] = ''10'' THEN sq.[s_181_1333_42749] ELSE [60_181_1333] END  end as [60_181_1333],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_181_1335_HasKMValue] = ''10'' THEN sq.[s_181_1335_42752] ELSE [60_181_1335] END
else 
 CASE WHEN [60_181_1335_HasKMValue] = ''10'' THEN sq.[s_181_1335_42752] ELSE [60_181_1335] END  end as [60_181_1335],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_182_1337_HasKMValue] = ''10'' THEN sq.[s_182_1337_42750] ELSE [60_182_1337] END
else 
 CASE WHEN [60_182_1337_HasKMValue] = ''10'' THEN sq.[s_182_1337_42750] ELSE [60_182_1337] END  end as [60_182_1337],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_182_1338_HasKMValue] = ''10'' THEN sq.[s_182_1338_42753] ELSE [60_182_1338] END
else 
 CASE WHEN [60_182_1338_HasKMValue] = ''10'' THEN sq.[s_182_1338_42753] ELSE [60_182_1338] END  end as [60_182_1338],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_183_1327_HasKMValue] = ''10'' THEN sq.[s_183_1327_42751] ELSE [60_183_1327] END
else 
 CASE WHEN [60_183_1327_HasKMValue] = ''10'' THEN sq.[s_183_1327_42751] ELSE [60_183_1327] END  end as [60_183_1327],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_183_1331_HasKMValue] = ''10'' THEN sq.[s_183_1331_42754] ELSE [60_183_1331] END
else 
 CASE WHEN [60_183_1331_HasKMValue] = ''10'' THEN sq.[s_183_1331_42754] ELSE [60_183_1331] END  end as [60_183_1331],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_183_1330_HasKMValue] = ''10'' THEN sq.[s_183_1330_42762] ELSE [60_183_1330] END
else 
 CASE WHEN [60_183_1330_HasKMValue] = ''10'' THEN sq.[s_183_1330_42762] ELSE [60_183_1330] END  end as [60_183_1330],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
pvt.[60_184_1340]
else 
 pvt.[60_184_1340]  end as [60_184_1340], pvt.[60_184_1340_lbl] AS [60_184_1340_lbl],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
pvt.[60_184_1342]
else 
 pvt.[60_184_1342]  end as [60_184_1342], pvt.[60_184_1342_lbl] AS [60_184_1342_lbl],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
pvt.[60_185_1343]
else 
 pvt.[60_185_1343]  end as [60_185_1343], pvt.[60_185_1343_lbl] AS [60_185_1343_lbl],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
pvt.[60_185_1344]
else 
 pvt.[60_185_1344]  end as [60_185_1344], pvt.[60_185_1344_lbl] AS [60_185_1344_lbl],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
pvt.[60_186_1345]
else 
 pvt.[60_186_1345]  end as [60_186_1345], pvt.[60_186_1345_lbl] AS [60_186_1345_lbl],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
pvt.[60_186_1346]
else 
 pvt.[60_186_1346]  end as [60_186_1346],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
pvt.[60_186_1347]
else 
 pvt.[60_186_1347]  end as [60_186_1347],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
pvt.[60_187_1348]
else 
 pvt.[60_187_1348]  end as [60_187_1348],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
pvt.[60_187_1349]
else 
 pvt.[60_187_1349]  end as [60_187_1349],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_187_1328_HasKMValue] = ''10'' THEN sq.[s_187_1328_42759] ELSE [60_187_1328] END
else 
 CASE WHEN [60_187_1328_HasKMValue] = ''10'' THEN sq.[s_187_1328_42759] ELSE [60_187_1328] END  end as [60_187_1328],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_187_1334_HasKMValue] = ''10'' THEN sq.[s_187_1334_42758] ELSE [60_187_1334] END
else 
 CASE WHEN [60_187_1334_HasKMValue] = ''10'' THEN sq.[s_187_1334_42758] ELSE [60_187_1334] END  end as [60_187_1334],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_187_1339_HasKMValue] = ''10'' THEN sq.[s_187_1339_42755] ELSE [60_187_1339] END
else 
 CASE WHEN [60_187_1339_HasKMValue] = ''10'' THEN sq.[s_187_1339_42755] ELSE [60_187_1339] END  end as [60_187_1339],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_187_1332_HasKMValue] = ''10'' THEN sq.[s_187_1332_42763] ELSE [60_187_1332] END
else 
 CASE WHEN [60_187_1332_HasKMValue] = ''10'' THEN sq.[s_187_1332_42763] ELSE [60_187_1332] END  end as [60_187_1332],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
pvt.[60_187_1350]
else 
 pvt.[60_187_1350]  end as [60_187_1350],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_187_1336_HasKMValue] = ''10'' THEN sq.[s_187_1336_42761] ELSE [60_187_1336] END
else 
 CASE WHEN [60_187_1336_HasKMValue] = ''10'' THEN sq.[s_187_1336_42761] ELSE [60_187_1336] END  end as [60_187_1336],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_187_1326_HasKMValue] = ''10'' THEN sq.[s_187_1326_42760] ELSE [60_187_1326] END
else 
 CASE WHEN [60_187_1326_HasKMValue] = ''10'' THEN sq.[s_187_1326_42760] ELSE [60_187_1326] END  end as [60_187_1326],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_187_1351_HasKMValue] = ''10'' THEN sq.[s_187_1351_42969] ELSE [60_187_1351] END
else 
 CASE WHEN [60_187_1351_HasKMValue] = ''10'' THEN sq.[s_187_1351_42969] ELSE [60_187_1351] END  end as [60_187_1351],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_187_1352_HasKMValue] = ''10'' THEN sq.[s_187_1352_42970] ELSE [60_187_1352] END
else 
 CASE WHEN [60_187_1352_HasKMValue] = ''10'' THEN sq.[s_187_1352_42970] ELSE [60_187_1352] END  end as [60_187_1352],case when pef.is_override_workflow_permission = 1 AND pef.is_read = 0 then null
when is_override_workflow_permission = 1 AND is_read = 1 then 
CASE WHEN [60_187_1353_HasKMValue] = ''10'' THEN sq.[s_187_1353_42971] ELSE [60_187_1353] END
else 
 CASE WHEN [60_187_1353_HasKMValue] = ''10'' THEN sq.[s_187_1353_42971] ELSE [60_187_1353] END  end as [60_187_1353] , case when pef.is_override_workflow_permission = 1 then 1
    when pef.is_override_workflow_permission = 0 then vs.WStatus
	                                                    else vs.WStatus end AS S_3,CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
	WHEN is_override_workflow_permission = 1 AND is_read = 1 then vs.WStatusId
    WHEN is_override_workflow_permission = 0 then vs.WStatusId
	                                                end as WStatusId,CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
	WHEN is_override_workflow_permission = 1 AND is_read = 1 then vs.WStep
    WHEN is_override_workflow_permission = 0 then vs.WStep
	                                                    end AS S_4,CASE WHEN pef.is_override_workflow_permission = 1 AND pef.is_read = 0 THEN null
	WHEN is_override_workflow_permission = 1 AND is_read = 1 then vs.WStatusByManagerId
    WHEN is_override_workflow_permission = 0 THEN vs.WStatusByManagerId
	                                                end as WStatusByManagerId  ,1 as is_tree_security_exist
    ,pef.id_tree_security_plan_level_exception 
    ,pef.is_override_workflow_permission
	,pef.is_read
	,pef.is_edit, @TotalCount as total_count
                                      from (
                                    select RN, id_step, MAX(pvt.[60_181_1333]) as [60_181_1333],MAX(pvt.[60_181_1333_lbl]) as [60_181_1333_lbl],ISNULL(MAX(pvt.[60_181_1333_0]),0) + ISNULL(MAX(pvt.[60_181_1333_1]),0) AS [60_181_1333_HasKMValue],MAX(pvt.[60_181_1335]) as [60_181_1335],MAX(pvt.[60_181_1335_lbl]) as [60_181_1335_lbl],ISNULL(MAX(pvt.[60_181_1335_0]),0) + ISNULL(MAX(pvt.[60_181_1335_1]),0) AS [60_181_1335_HasKMValue],MAX(pvt.[60_182_1337]) as [60_182_1337],MAX(pvt.[60_182_1337_lbl]) as [60_182_1337_lbl],ISNULL(MAX(pvt.[60_182_1337_0]),0) + ISNULL(MAX(pvt.[60_182_1337_1]),0) AS [60_182_1337_HasKMValue],MAX(pvt.[60_182_1338]) as [60_182_1338],MAX(pvt.[60_182_1338_lbl]) as [60_182_1338_lbl],ISNULL(MAX(pvt.[60_182_1338_0]),0) + ISNULL(MAX(pvt.[60_182_1338_1]),0) AS [60_182_1338_HasKMValue],MAX(pvt.[60_183_1327]) as [60_183_1327],MAX(pvt.[60_183_1327_lbl]) as [60_183_1327_lbl],ISNULL(MAX(pvt.[60_183_1327_0]),0) + ISNULL(MAX(pvt.[60_183_1327_1]),0) AS [60_183_1327_HasKMValue],MAX(pvt.[60_183_1331]) as [60_183_1331],MAX(pvt.[60_183_1331_lbl]) as [60_183_1331_lbl],ISNULL(MAX(pvt.[60_183_1331_0]),0) + ISNULL(MAX(pvt.[60_183_1331_1]),0) AS [60_183_1331_HasKMValue],MAX(pvt.[60_183_1330]) as [60_183_1330],MAX(pvt.[60_183_1330_lbl]) as [60_183_1330_lbl],ISNULL(MAX(pvt.[60_183_1330_0]),0) + ISNULL(MAX(pvt.[60_183_1330_1]),0) AS [60_183_1330_HasKMValue],MAX(pvt.[60_184_1340]) as [60_184_1340],MAX(pvt.[60_184_1340_lbl]) as [60_184_1340_lbl],ISNULL(MAX(pvt.[60_184_1340_0]),0) + ISNULL(MAX(pvt.[60_184_1340_1]),0) AS [60_184_1340_HasKMValue],MAX(pvt.[60_184_1342]) as [60_184_1342],MAX(pvt.[60_184_1342_lbl]) as [60_184_1342_lbl],ISNULL(MAX(pvt.[60_184_1342_0]),0) + ISNULL(MAX(pvt.[60_184_1342_1]),0) AS [60_184_1342_HasKMValue],MAX(pvt.[60_185_1343]) as [60_185_1343],MAX(pvt.[60_185_1343_lbl]) as [60_185_1343_lbl],ISNULL(MAX(pvt.[60_185_1343_0]),0) + ISNULL(MAX(pvt.[60_185_1343_1]),0) AS [60_185_1343_HasKMValue],MAX(pvt.[60_185_1344]) as [60_185_1344],MAX(pvt.[60_185_1344_lbl]) as [60_185_1344_lbl],ISNULL(MAX(pvt.[60_185_1344_0]),0) + ISNULL(MAX(pvt.[60_185_1344_1]),0) AS [60_185_1344_HasKMValue],MAX(pvt.[60_186_1345]) as [60_186_1345],MAX(pvt.[60_186_1345_lbl]) as [60_186_1345_lbl],ISNULL(MAX(pvt.[60_186_1345_0]),0) + ISNULL(MAX(pvt.[60_186_1345_1]),0) AS [60_186_1345_HasKMValue],MAX(pvt.[60_186_1346]) as [60_186_1346],MAX(pvt.[60_186_1346_lbl]) as [60_186_1346_lbl],ISNULL(MAX(pvt.[60_186_1346_0]),0) + ISNULL(MAX(pvt.[60_186_1346_1]),0) AS [60_186_1346_HasKMValue],MAX(pvt.[60_186_1347]) as [60_186_1347],MAX(pvt.[60_186_1347_lbl]) as [60_186_1347_lbl],ISNULL(MAX(pvt.[60_186_1347_0]),0) + ISNULL(MAX(pvt.[60_186_1347_1]),0) AS [60_186_1347_HasKMValue],MAX(pvt.[60_187_1348]) as [60_187_1348],MAX(pvt.[60_187_1348_lbl]) as [60_187_1348_lbl],ISNULL(MAX(pvt.[60_187_1348_0]),0) + ISNULL(MAX(pvt.[60_187_1348_1]),0) AS [60_187_1348_HasKMValue],MAX(pvt.[60_187_1349]) as [60_187_1349],MAX(pvt.[60_187_1349_lbl]) as [60_187_1349_lbl],ISNULL(MAX(pvt.[60_187_1349_0]),0) + ISNULL(MAX(pvt.[60_187_1349_1]),0) AS [60_187_1349_HasKMValue],MAX(pvt.[60_187_1328]) as [60_187_1328],MAX(pvt.[60_187_1328_lbl]) as [60_187_1328_lbl],ISNULL(MAX(pvt.[60_187_1328_0]),0) + ISNULL(MAX(pvt.[60_187_1328_1]),0) AS [60_187_1328_HasKMValue],MAX(pvt.[60_187_1334]) as [60_187_1334],MAX(pvt.[60_187_1334_lbl]) as [60_187_1334_lbl],ISNULL(MAX(pvt.[60_187_1334_0]),0) + ISNULL(MAX(pvt.[60_187_1334_1]),0) AS [60_187_1334_HasKMValue],MAX(pvt.[60_187_1339]) as [60_187_1339],MAX(pvt.[60_187_1339_lbl]) as [60_187_1339_lbl],ISNULL(MAX(pvt.[60_187_1339_0]),0) + ISNULL(MAX(pvt.[60_187_1339_1]),0) AS [60_187_1339_HasKMValue],MAX(pvt.[60_187_1332]) as [60_187_1332],MAX(pvt.[60_187_1332_lbl]) as [60_187_1332_lbl],ISNULL(MAX(pvt.[60_187_1332_0]),0) + ISNULL(MAX(pvt.[60_187_1332_1]),0) AS [60_187_1332_HasKMValue],MAX(pvt.[60_187_1350]) as [60_187_1350],MAX(pvt.[60_187_1350_lbl]) as [60_187_1350_lbl],ISNULL(MAX(pvt.[60_187_1350_0]),0) + ISNULL(MAX(pvt.[60_187_1350_1]),0) AS [60_187_1350_HasKMValue],MAX(pvt.[60_187_1336]) as [60_187_1336],MAX(pvt.[60_187_1336_lbl]) as [60_187_1336_lbl],ISNULL(MAX(pvt.[60_187_1336_0]),0) + ISNULL(MAX(pvt.[60_187_1336_1]),0) AS [60_187_1336_HasKMValue],MAX(pvt.[60_187_1326]) as [60_187_1326],MAX(pvt.[60_187_1326_lbl]) as [60_187_1326_lbl],ISNULL(MAX(pvt.[60_187_1326_0]),0) + ISNULL(MAX(pvt.[60_187_1326_1]),0) AS [60_187_1326_HasKMValue],MAX(pvt.[60_187_1351]) as [60_187_1351],MAX(pvt.[60_187_1351_lbl]) as [60_187_1351_lbl],ISNULL(MAX(pvt.[60_187_1351_0]),0) + ISNULL(MAX(pvt.[60_187_1351_1]),0) AS [60_187_1351_HasKMValue],MAX(pvt.[60_187_1352]) as [60_187_1352],MAX(pvt.[60_187_1352_lbl]) as [60_187_1352_lbl],ISNULL(MAX(pvt.[60_187_1352_0]),0) + ISNULL(MAX(pvt.[60_187_1352_1]),0) AS [60_187_1352_HasKMValue],MAX(pvt.[60_187_1353]) as [60_187_1353],MAX(pvt.[60_187_1353_lbl]) as [60_187_1353_lbl],ISNULL(MAX(pvt.[60_187_1353_0]),0) + ISNULL(MAX(pvt.[60_187_1353_1]),0) AS [60_187_1353_HasKMValue]
                                      from (
		                                            select RN, id_step, ind_field,ind_field_lbl, input_value, input_label, ''1'' as ind_field_2, hasKMValueRow
		                                      from sourcequery
                                    ) as SourceTable
                                    
                                        
                                        pivot(
                                            MIN(input_label)
                                            FOR ind_field_lbl IN ([60_181_1333_lbl],[60_181_1335_lbl],[60_182_1337_lbl],[60_182_1338_lbl],[60_183_1327_lbl],[60_183_1331_lbl],[60_183_1330_lbl],[60_184_1340_lbl],[60_184_1342_lbl],[60_185_1343_lbl],[60_185_1344_lbl],[60_186_1345_lbl],[60_186_1346_lbl],[60_186_1347_lbl],[60_187_1348_lbl],[60_187_1349_lbl],[60_187_1328_lbl],[60_187_1334_lbl],[60_187_1339_lbl],[60_187_1332_lbl],[60_187_1350_lbl],[60_187_1336_lbl],[60_187_1326_lbl],[60_187_1351_lbl],[60_187_1352_lbl],[60_187_1353_lbl]) 
                                        ) as pvt_lbl

                                                    pivot
                                                    (
                                    MIN(input_value)
                                    FOR ind_field IN ([60_181_1333],[60_181_1335],[60_182_1337],[60_182_1338],[60_183_1327],[60_183_1331],[60_183_1330],[60_184_1340],[60_184_1342],[60_185_1343],[60_185_1344],[60_186_1345],[60_186_1346],[60_186_1347],[60_187_1348],[60_187_1349],[60_187_1328],[60_187_1334],[60_187_1339],[60_187_1332],[60_187_1350],[60_187_1336],[60_187_1326],[60_187_1351],[60_187_1352],[60_187_1353])
                                            ) as pvt2
                                            pivot (
                                                MIN(ind_field_2)
					                            FOR hasKMValueRow IN ([60_181_1333_0],[60_181_1333_1],[60_181_1335_0],[60_181_1335_1],[60_182_1337_0],[60_182_1337_1],[60_182_1338_0],[60_182_1338_1],[60_183_1327_0],[60_183_1327_1],[60_183_1331_0],[60_183_1331_1],[60_183_1330_0],[60_183_1330_1],[60_184_1340_0],[60_184_1340_1],[60_184_1342_0],[60_184_1342_1],[60_185_1343_0],[60_185_1343_1],[60_185_1344_0],[60_185_1344_1],[60_186_1345_0],[60_186_1345_1],[60_186_1346_0],[60_186_1346_1],[60_186_1347_0],[60_186_1347_1],[60_187_1348_0],[60_187_1348_1],[60_187_1349_0],[60_187_1349_1],[60_187_1328_0],[60_187_1328_1],[60_187_1334_0],[60_187_1334_1],[60_187_1339_0],[60_187_1339_1],[60_187_1332_0],[60_187_1332_1],[60_187_1350_0],[60_187_1350_1],[60_187_1336_0],[60_187_1336_1],[60_187_1326_0],[60_187_1326_1],[60_187_1351_0],[60_187_1351_1],[60_187_1352_0],[60_187_1352_1],[60_187_1353_0],[60_187_1353_1])
                                    ) as pvt
					                        group by RN,id_step
                                    ) as pvt
                                        inner join dbo.k_m_plans_payees_steps ps
                                            on pvt.id_step = ps.id_step
    left outer join #TreePlanRecursiveExceptionFiltered pef
		on pef.idChild = ps.id_payee and pef.id_step = ps.id_step
    
    
    left outer join #SQLFieldQuery sq
        on (ps.id_payee = sq.IdPayee and ps.id_Step = sq.IdStep )
    
    inner join #ValidationSteps vs
        on ps.id_step = vs.id_step
        and ps.id_payee = vs.idPayee
    
    outer apply (
          select top 1 ic.*
            from tmp_InfoColumns ic
              inner join k_m_plans_payees_steps ps3
            on ps3.id_payee = ic.idPayee
            and ps3.end_step > ic.start_date_histo
            and ps3.start_step <= ic.end_date_histo
            and ps3.id_plan = @id_plan
          where ps.id_step = ps3.id_step 
          order by ic.start_date_histo DESC
            ) ic
    
join #PayeeStepList psl
    on ps.id_step = psl.IdStep 
    WHERE 1 = 1  AND ic.[idPayee] <> 226098 -- WhereCondition
  order by ic.fullname, ps.start_step
 
',N'@endDateFilter datetime,@startDateFilter datetime,@id_plan int,@id_profile int,@Culture int,@PayeeOnlyLevelCalculation bit,@IdTreeSecurity int,@IdNodeManager int,@Level nvarchar(10),@IdLoggedInUserNode int,@FilterCTE nvarchar(3048),@UseSort bit,@UsePaging bit,@pageNum int,@pageSize int,@isSelfAppraisal int,@WFCulture nvarchar(5),@IdStepFilter int,@idTree int,@idUser int,@FilterCTEParameterType nvarchar(4000),@FilterCTEParameterValue nvarchar(4000)',@endDateFilter='3000-01-01 00:00:00',@startDateFilter='1980-01-01 00:00:00',@id_plan=60,@id_profile=6,@Culture=-3,@PayeeOnlyLevelCalculation=0,@IdTreeSecurity=447270,@IdNodeManager=27539026,@Level=N'1,2,3,4,-1',@IdLoggedInUserNode=27539026,@FilterCTE=N'InfoColumns AS 
(
SELECT * 
  FROM (SELECT ic.idPayee, ic.fullname, ic.start_date_histo, ic.end_date_histo, ic.[workerId], ic.[CommonLevel], ic.[Class Year], ic.[TalentRankDisplayPY], ic.[TalentRank], ic.[Promotion], ic.[SameStore], ic.[BonusTRWDFlag], ic.[TRWD Curr], ic.[2023 Annual TRWD USD], ic.[TRWD Ranges], ic.[Guarantee / Estimate], ic.[2022 Annual TRWD], ic.[2022 TRWD Curr], ic.[2020 Annual TRWD USD], ic.[2021 Annual TRWD USD], ic.[2022 Annual TRWD USD], ic.[2023 Actual YE Bonus], ic.[2023 Actual YE Bonus USD], ic.[Proration %], ic.[2022 Annual YE Bonus], ic.[2024 Salary Curr], ic.[Salary Matrix Step], ic.[Salary Ranges], ic.[2023 Annual Salary], ic.[baserate], ic.[2023 Actual Fixed Pay USD], ic.[Material Risk Taker Indicator], ic.[Matter For Review Indicator], ic.[2022 Actual YE Bonus], ic.[2022 Actual YE Bonus USD], ic.[2022 Advances], ic.[2022 Advances Curr], ic.[2022 Advances USD], ic.[2022 Annual Pre YE Var Pay], ic.[2022 Annual YE Bonus USD], ic.[2023 Mid-Year Decision], ic.[2023 Mid-Year Decision Curr], ic.[2022 Overtime], ic.[2022 Overtime Curr], ic.[2022 Pre YE Var Pay Curr], ic.[2022 YE Bonus Curr], ic.[2023 Actual Salary Lcl], ic.[2023 Actual Salary USD], ic.[2023 Actual TRWD], ic.[2023 Actual TRWD USD], ic.[2023 Actual YE Cash Bonus], ic.[2023 Actual YE Def Bonus USD], ic.[2023 Advances], ic.[2023 Advances Curr], ic.[2023 Advances USD], ic.[2023 Annual Pre YE Var Pay], ic.[2023 Annual Salary USD], ic.[2023 Annual YE Bonus USD], ic.[2023 Pre YE Var Pay Curr], ic.[2023 Salary Curr], ic.[2023 YE Bonus Curr], ic.[2023 YE Cash Bonus Curr], ic.[2023 YTD Overtime], ic.[2023 YTD Overtime Curr], ic.[2024 Salary USD], ic.[CYFinancials], ic.[Alt 0], ic.[Alt 1], ic.[City], ic.[Code Staff Indicator], ic.[Compensation Manager], ic.[Cost Center], ic.[Cost Center Region], ic.[Country], ic.[Decision Indicator], ic.[Dept], ic.[Division], ic.[Function], ic.[GIC], ic.[HireDate], ic.[Job Level], ic.[Job Title], ic.[New Job Level], ic.[Other TRWD Payments], ic.[Other TRWD Payments Curr], ic.[Compliance & Risk Mgmt], ic.[Culture & Leadership], ic.[Diversity & Inclusion], ic.[Manager Effectiveness], ic.[Performance & Contributions], ic.[Potential], ic.[Misc1], ic.[Region], ic.[Salary Change Lcl], ic.[Salary Change USD], ic.[Sub Dept], ic.[Sub Dept 2], ic.[Super Dept], ic.[Super Division], ic.[TRWD YoY Change Lcl], ic.[TotalRewardAnnUsdPy3] FROM _ProcessYEInfoColumn2023 AS ic) sub
 WHERE ''''3000-01-01 00:00:00'''' > sub.start_date_histo
   AND ''''1980-01-01 00:00:00'''' <= sub.end_date_histo
),
FilteredCTE
as
(
select distinct ic.idPayee, ic.fullname,ps.id_step 
from InfoColumns ic
inner join k_m_plans_payees_steps ps
	on ps.id_payee = ic.idPayee
 --left join k_m_plans_payees_steps_workflow ws on ps.id_step = ws.id_step

WHERE 1=1
	and ps.end_step > ic.start_date_histo
	and ps.start_step <= ic.end_date_histo
	and ps.id_plan = 60
	and ps.start_step < ''''3000-01-01 00:00:00''''
	and ps.end_step > ''''1980-01-01 00:00:00''''
     AND [idPayee] <> 226098 -- where cond
     -- wflStep filter
)',@UseSort=0,@UsePaging=0,@pageNum=0,@pageSize=0,@isSelfAppraisal=-1,@WFCulture=N'en-US',@IdStepFilter=0,@idTree=474654,@idUser=25,@FilterCTEParameterType=N'',@FilterCTEParameterValue=N''
