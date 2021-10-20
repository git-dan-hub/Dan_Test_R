USE [RCPTemporary]
GO

/****** Object:  View [ann2021].[vHELOCBanding_WithModel]    Script Date: 8/25/2021 8:51:50 PM ******/
DROP VIEW [ann2021].[vHELOCBanding_WithModel]
GO

/****** Object:  View [ann2021].[vHELOCBanding_WithModel]    Script Date: 8/25/2021 8:51:50 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO














--Records are De-duped and Ready for modeling. Some [defaultDates]<[year] but those begin from 7-12, depending on when the file was cut. 
Create View [ann2021].[vHELOCBanding_WithModel] as 
SELECT distinct *
	,[caPD]			= coalesce(exp([caLogit])*1.0		/(1+exp([caLogit])), 10*1.0/180) 
	,[caPD_BIN]		= coalesce(exp([caLogit_BIN])*1.0	/(1+exp([caLogit_BIN])), 10*1.0/180) 
	,[caPD_PREV]	= coalesce(exp([caLogit_PREV])*1.0	/(1+exp([caLogit_PREV])), 10*1.0/180) 
	,[caPD_PREV_2016P]	= coalesce(exp([caLogit_PREV_2016P])*1.0	/(1+exp([caLogit_PREV_2016P])), 10*1.0/180) 
	,[caPD_PREV_2017P]	= coalesce(exp([caLogit_PREV_2017P])*1.0	/(1+exp([caLogit_PREV_2017P])), 10*1.0/180) 
	,[caPD_PROD]	= coalesce(exp([caLogit_PROD])*1.0	/(1+exp([caLogit_PROD])), 0.00715) 

FROM(
	SELECT distinct * 
		,[caPerformanceFlag]	= IIF([defaultDate] IS NULL, 0, 1)
		,[caBad]				= IIF([defaultDate] IS NULL, 0, 1)
		,[caGood]				= IIF([defaultDate] IS NULL, 1, 0)
		/*
		,[caLogit]				= CASE	WHEN [caSDFICO] IS NULL OR [caSDBKRPT] IS NULL 
										THEN log(0.00715/(1-0.00715))
										ELSE -6.8910118 + -0.8615223*[caSDFICO] + -0.2065963*[caSDBKRPT] 
										END*/

		--,[caLogit]				= 3.349099400 + -0.009930976*[caFICO] + -0.002253709*[caBKRPT] 
		--2021.08.24: 2011-2018
		,[caLogit]				= CASE	WHEN [FICO] IS NULL OR [BKRPT] IS NULL 
										THEN log(10*1.0/180/(1-10*1.0/180))
										--ELSE 3.77614182 + -0.01063051*[FICO] + -0.00236263*[BKRPT] 
										ELSE 2.064739416  + -0.008810238*[FICO] + 0.085252678*[age] + -0.002515458*[BKRPT]
										END

		/*
		(Intercept)                FICO binnedAge02 <= 8.91  binnedAge03 > 8.91               BKRPT 
        0.703500072        -0.007446577         0.783514686         2.344701226        -0.002575915
		*/
		--2021.08.24: 2011-2018
		,[caLogit_BIN]			= CASE	WHEN [FICO] IS NULL OR [BKRPT] IS NULL 
										THEN log(10*1.0/180/(1-10*1.0/180))
										--ELSE 3.77614182 + -0.01063051*[FICO] + -0.00236263*[BKRPT] 
										ELSE 0.703500072 
											+ -0.007446577*[FICO] 
											+ 0.783514686*[binnedAge_LE_9] 
											+ 2.344701226*[binnedAge_GT_9] 
											+ -0.002575915*[BKRPT]
										END

		--,[caLogit_PREV]			= 6.63860341 + -0.01635798*[caFICO]
		--2021.08.24: 2011-2018
		,[caLogit_PREV]			= CASE	WHEN [FICO] IS NULL OR [BKRPT] IS NULL 
										THEN log(10*1.0/180/(1-10*1.0/180))
										ELSE 3.77614182 + -0.01063051*[FICO] + -0.00236263*[BKRPT]
										END


		,[caLogit_PREV_2016P]			= CASE	WHEN [FICO] IS NULL OR [BKRPT] IS NULL 
										THEN log(10*1.0/180/(1-10*1.0/180))
										ELSE 5.87355602  + -0.01485077*[FICO]
										END


		,[caLogit_PREV_2017P]			= CASE	WHEN [FICO] IS NULL OR [BKRPT] IS NULL 
										THEN log(10*1.0/180/(1-10*1.0/180))
										ELSE 6.86705553  + -0.01611798*[FICO]
										END

		,[caLogit_PROD]			= CASE	WHEN [FICO] IS NULL OR [BKRPT] IS NULL 
										THEN log(0.00715/(1-0.00715))
										ELSE 7.6781928 + -0.0173825 *[FICO] + -0.0013067 *[BKRPT] 
										END
	FROM(
			SELECT distinct [year]
				  ,[facility]
				  ,[accountNumber]
				  ,[accountNumberJoin] = cast([accountNumber] as bigint)
				  ,[FICO]				= IIF([FICO] between 300 and 850, [FICO], NULL)
				  ,[BKRPT]				= IIF([BKRPT] between 1 and 1000, [BKRPT], NULL)

				  ,[caFICO]				= IIF([FICO] between 300 and 850, [FICO], 773.8327)
				  ,[caBKRPT]			= IIF([BKRPT] between 1 and 1000, [BKRPT], 755.5388)

				  ,[CustomerBalance]
				  ,[caExclusionFlag]	= IIF([CustomerBalance] = 0, 1, 0)
				  ,[recordCount]
				  ,[defaultCount]
				  ,[defaultDate]		= max(cast([defaultDate] as date)) over (partition by [year], [accountNumber])
				  ,[defaultBalance]		= max([defaultBalance]) over (partition by [year], [accountNumber])
				  --,[cureDate]			--comment out to remove re-default instance for 190625591
				  ,[origFICO]
				  ,[origBKRPT]
				  ,[faceAmountOfNote]
				  ,[vintage]
				  ,[origLTV]
				  ,[origDTI]
				  ,[origCollateralValue]
				  ,[collateralCode]
				  ,[productCode]
				  --,[ID]				--distinct values for each record
				  ,[enteredDate]
				  ,[age]

				  
				  ,[binnedAge]				= case
												when [age] <= 2.55 then '01: age <= 2.55'
												when [age] <= 8.91 then '02: age <= 8.91'
												when [age] > 8.91  then '03: age > 8.91'
												when [age] Is Null then '04: age Is Null'
												else '99: Error' end
												
				  ,[binnedAge_LE_3]			= case
											when [age] <= 2.55 then 1
											else 0 end
				  ,[binnedAge_LE_9]			= case
											when [age] <= 8.91 then 1
											else 0 end
				  ,[binnedAge_GT_9]			= case
											when [age] > 8.91  then 1
											else 0 end

				  ,[lienPosition]
				  ,[caSDFICO]			= IIF(NOT([FICO] IS NULL) AND [FICO] <> 0, ([FICO]-779.012)/49.62364, NULL)
				  ,[caSDBKRPT]			= IIF(NOT([BKRPT] IS NULL) AND [BKRPT]<>0, ([BKRPT]-786.3484)/158.3109, NULL)

				  ,[caFICO_Binned]	=	CASE
												WHEN [FICO] = 0		THEN '00. Missing'
												WHEN NOT([FICO] between 300 and 850) OR [FICO] IS NULL THEN '00. Missing'
												WHEN [FICO] <= 500	THEN '01. 500-'
												WHEN [FICO] <= 560	THEN '02. LE 560'
												WHEN [FICO] <= 580	THEN '03. LE 580'
												WHEN [FICO] <= 600	THEN '04. LE 600'
												WHEN [FICO] <= 620	THEN '05. LE 620'
												WHEN [FICO] <= 640	THEN '06. LE 640'
												WHEN [FICO] <= 660	THEN '07. LE 660'
												WHEN [FICO] <= 680	THEN '08. LE 680'
												WHEN [FICO] <= 700	THEN '09. LE 700'
												WHEN [FICO] <= 720	THEN '10. LE 720'
												WHEN [FICO] <= 740	THEN '11. LE 740'
												WHEN [FICO] <= 760	THEN '12. LE 760'
												WHEN [FICO] <= 780	THEN '13. LE 780'
												WHEN [FICO] > 780		THEN '14. 780+'
												END

				  ,[caBKRPT_Binned]		=	CASE			
												WHEN [BKRPT] = 0 OR [BKRPT]=9001 OR [BKRPT] IS NULL	THEN '00. Missing or BK'	
												WHEN [BKRPT] <=	100	THEN '01. LE 100'
												WHEN [BKRPT] <=	150	THEN '02. LE 150'
												WHEN [BKRPT] <=	200	THEN '03. LE 200'
												WHEN [BKRPT] <=	250	THEN '04. LE 250'
												WHEN [BKRPT] <=	300	THEN '05. LE 300'
												WHEN [BKRPT] <=	350	THEN '06. LE 350'
												WHEN [BKRPT] <=	400	THEN '07. LE 400'
												WHEN [BKRPT] <=	450	THEN '08. LE 450'
												WHEN [BKRPT] <=	500	THEN '09. LE 500'
												WHEN [BKRPT] <=	550	THEN '10. LE 550'
												WHEN [BKRPT] <=	600	THEN '11. LE 600'
												WHEN [BKRPT] <=	650	THEN '12. LE 650'
												WHEN [BKRPT] <=	700	THEN '13. LE 700'
												WHEN [BKRPT] <=	750	THEN '14. LE 750'
												WHEN [BKRPT] <=	800	THEN '15. LE 800'
												WHEN [BKRPT] <=	850	THEN '16. LE 850'
												WHEN [BKRPT] >	850	AND [BKRPT] <= 1000 THEN '17. 850+'
												ELSE '00. Missing or BK'	
												END		

			FROM(
			  SELECT distinct MAIN.*
				,[CustomerBalance] = cast(BAL.[Customer Balance] as float)
			  FROM [RCPTemporary].[ann2020].[HELOCBanding] MAIN
			  left outer join [RCPTemporary].[ann2021].[HELOCBanding_withBalance] BAL
			  on cast(MAIN.[accountNumber] as bigint) = cast(BAL.[accountNumber] as bigint)
			  AND MAIN.[year] = BAL.[#year]
			)OG
	)CLEAN
)LOGIT













GO


