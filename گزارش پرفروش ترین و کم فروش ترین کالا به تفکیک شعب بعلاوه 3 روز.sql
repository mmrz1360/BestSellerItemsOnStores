declare  @date datetime  = CAST(getdate() AS date )
DECLARE  @StartDate DATETIME = CAST('$StartDate' AS date )
DECLARE  @EndDate DATETIME = CAST('$EndDate' AS date )
 /********************************************لیست تامین کننده***************************************/ 
select SupplierID, name  INTO #SUP from SupplierView where ISNULL(LanguageID,314) = 314 
/*********************آخرین قیمت فروش شعب*****************/
;with cte_A as  (
select s.StoreID ,itemid,EffectiveDate,PriceAmount,ConsumerPrice from (

select RN = row_number() over (partition by iv.StoreID ,iv.ItemID order by  iv.EffectiveDate desc) ,
iv.StoreID ,iv.ItemID ,iv.EffectiveDate,PriceAmount,ConsumerPrice from ItemSalePriceView as iv 
where  ReasonID = 16
) AA inner  join dbo.Store as S on s.StoreID = aa.StoreID
or  AA.StoreID is null
where  rn = 1 ) , cte_final as (

select Storeid,itemid,PriceAmount,ConsumerPrice,RowNumber= row_number() over (partition by StoreID ,ItemID order by  EffectiveDate desc)
from cte_A )

SELECT PAS.Storeid,itemid,PriceAmount,ConsumerPrice,ST.StockID,ST.Name
INTO #PriceAmountStore
FROM cte_final  PAS
INNER JOIN Stock ST
 ON ST.StoreID=PAS.StoreID
WHERE  PAS.RowNumber = 1 
AND  PAS.ItemID IN ($ItemID)
AND ST.StockID   IN ($StockID)
/*********************آخرین قیمت مصوب*****************/

;WITH cteContract
AS (SELECT 
           SC.EffectiveDate,
           C.Date,
           SCI.ItemID,
           SCI.Amount,
           SCI.LastCost,
     SC.ContractNumber,
           ROW_NUMBER() OVER (PARTITION BY 
                                           SCI.ItemID
                              ORDER BY SC.EffectiveDate DESC
                             ) AS RowNumber
    FROM dbo.SupplierContract SC WITH (NOLOCK)
        INNER JOIN dbo.SupplierContractLineItems SCI WITH (NOLOCK)
            ON SC.ContractID = SCI.ContractID
  INNER JOIN Calendar C
   ON C.BusinessDate=CAST(SC.EffectiveDate AS DATE)
   WHERE ISNULL(C.LanguageID,314)=314
   AND SC.StatusID=246
    ) 
 SELECT * INTO #Contract FROM CTEContract
 WHERE RowNumber=1

 
/*****************************************************************************************************/
;WITH ctePurchaseQuantity AS (
SELECT 
  CAST(SD.DocumentDate AS DATE) AS BusinessDate,
  SD.TargetStockID AS StockID,
  SD.SupplierID,
  SDLI.ItemID,
  ((SDLI.PackUnitCount * SDLI.PackCount) + SDLI.UnitCount + SDLI.BonusCount) AS LastPurchaseQuantity,
   ROW_NUMBER() OVER (PARTITION BY SD.TargetStockID,------SD.SupplierID,
                                           SDLI.ItemID
                              ORDER BY SD.DocumentDate DESC
                             ) AS RowNumber
FROM StockDocument SD WITH(NOLOCK)
INNER JOIN StockDocumentLineItem SDLI WITH(NOLOCK)
  ON  SDLI.BookerStoreID = SD.BookerStoreID
  AND SDLI.BookerWorkstationID = SD.BookerWorkstationID
  AND SDLI.DocumentID = sd.DocumentID
  AND SD.DocumentTypeID = 433
  AND SD.StatusID=246
  AND SD.TargetStockID IN ($StockID)
WHERE  SD.DocumentDate BETWEEN @StartDate AND @EndDate
  
     )
  SELECT * INTO #ctePurchaseQuantity FROM  ctePurchaseQuantity WHERE RowNumber=1

/*****************************************************************************************************/ 
; WITH CTE_Supplier AS (
          SELECT MAXDetail.ItemID,MAXDetail.SupplierID FROM  

         (SELECT DISTINCT ItemID,MAX(DocumentDate ) AS MaxEffective   FROM StockDocument S WITH(NOLOCK)
         INNER JOIN StockDocumentLineItem SDLI  WITH(NOLOCK)
          ON S.DocumentID = SDLI.DocumentID 
         WHERE DocumentTypeID = 433
          AND StatusID = 246 
         GROUP BY ItemID ) MAXDATE

 LEFT JOIN ( 
        SELECT  SDLI.ItemID , S.DocumentDate,s.SupplierID          
        FROM StockDocument S WITH(NOLOCK)
  INNER JOIN StockDocumentLineItem SDLI WITH(NOLOCK)
   ON S.DocumentID = SDLI.DocumentID 
  WHERE DocumentTypeID = 433
        AND StatusID = 246
  GROUP BY SDLI.ItemID , S.DocumentDate ,s.SupplierID
              ) AS MAXDetail 
             ON MAXDetail.ItemID = MAXDATE.ItemID
             AND MAXDetail.DocumentDate = MAXDATE.MaxEffective)

    SELECT * INTO #CTE_Supplier FROM CTE_Supplier
/**************************دریافت  تعداد و تاریخ آخزین دریافتی************************************/ 
/*SELECT StockID,ItemID,[day]
  INTO #MLastCostStore
FROM
(
    SELECT SD.TargetStockID AS StockID,
           SDL.ItemID,
           [day] = DATEDIFF( day, SD.DocumentDate,@date),
           RN = ROW_NUMBER() OVER (PARTITION BY SD.TargetStockID,
                                                SDL.ItemID
                                   ORDER BY SD.DocumentDate DESC
                                  )
    FROM dbo.StockDocument AS SD WITH (NOLOCK)
    INNER JOIN dbo.StockDocumentLineItem AS SDL WITH (NOLOCK)
   ON SD.BookerStoreID = SDL.BookerStoreID
   AND SD.DocumentID = SDL.DocumentID
   AND SD.DocumentTypeID = 298
   AND SD.StatusID = 246
   AND SDL.ItemID        IN ($ItemID)
   AND SD.TargetStockID  IN ($StockID)
) D
WHERE RN = 1*/

;WITH CTEMLastCostStore
AS (SELECT 
           SD.TargetStockID AS StockID,
     SD.DocumentCode,
           C.Date,
           SDI.ItemID,
      [day] = DATEDIFF( day, SD.DocumentDate,@date),
     (SDI.UnitCount + SDI.BonusCount) AS BuyCount , 
          CASE WHEN (SDI.UnitCount+(SDI.PackCount*SDI.PackUnitCount))<>0 THEN (SDI.NETCOST/(SDI.UnitCount+(SDI.PackCount*SDI.PackUnitCount)) + ((SDI.Tax + SDI.Toll)/(SDI.UnitCount+(SDI.PackCount*SDI.PackUnitCount))))  ELSE 1 END AS MLastCostStore ,
           ROW_NUMBER() OVER (PARTITION BY SD.TargetStockID,
                                           SDI.ItemID
                              ORDER BY SD.DocumentDate DESC
                             ) AS RowNumber
    FROM dbo.StockDocument SD WITH (NOLOCK)
        INNER JOIN dbo.StockDocumentLineItem SDI WITH (NOLOCK)
            ON SD.DocumentID = SDI.DocumentID
  LEFT JOIN Calendar C
   ON C.BusinessDate=CAST(SD.DocumentDate AS DATE)
 AND C.LanguageID = 314
   WHERE  SD.StatusID=246
   AND TargetStockID   IN ($StockID)
   AND SD.DocumentTypeID=433
   
    ) 
  SELECT * INTO #MLastCostStore FROM CTEMLastCostStore
  WHERE RowNumber=1
  AND  ItemID IN ($ItemID)
  AND StockID   IN ($StockID)

/*********************آخرین تاریخ وارده به  فروشگاه*****************/

;WITH CTEMLastCostStoreEnter
AS (SELECT 
           SD.TargetStockID AS StockID,
     SD.DocumentCode,
  SD.DocumentID ,
           C.Date,
           SDI.ItemID,
      [day] = DATEDIFF( day, SD.DocumentDate,@date),
           ROW_NUMBER() OVER (PARTITION BY SD.TargetStockID,
                                           SDI.ItemID
                              ORDER BY SD.DocumentDate DESC
                             ) AS RowNumber
    FROM dbo.StockDocument SD WITH (NOLOCK)
        INNER JOIN dbo.StockDocumentLineItem SDI WITH (NOLOCK)
            ON SD.DocumentID = SDI.DocumentID
  INNER JOIN Calendar C
   ON C.BusinessDate=CAST(SD.DocumentDate AS DATE)
   WHERE ISNULL(C.LanguageID,314)=314
   AND SD.StatusID=246
   AND TargetStockID   IN ($StockID)
   AND SD.DocumentTypeID=298
    ) 
  SELECT CTEMLastCostStoreEnter.* 
  , StockDocumentLineItem.UnitCount
  INTO #MLastCostStoreEnter FROM CTEMLastCostStoreEnter
  INNER JOIN dbo.StockDocumentLineItem
  ON StockDocumentLineItem.ItemID = CTEMLastCostStoreEnter.ItemID
  AND StockDocumentLineItem.DocumentID = CTEMLastCostStoreEnter.DocumentID
  WHERE RowNumber=1
  AND  CTEMLastCostStoreEnter.ItemID IN ($ItemID)
  AND CTEMLastCostStoreEnter.StockID   IN ($StockID)
 /**************************************************************************************************/

SELECT 
 ISS.ItemID,StockID,
    (CurrentUnitCount + ReservedUnitCount) AS QTY, 
 CASE WHEN 
 ISS.StatusID = 243 THEN N'فعال' ELSE N'غیر فعال' END AS Status
 INTO #CTEISS
FROM ItemStockState AS ISS 
  WHERE StockID  IN ($StockID)
  AND ISS.ItemID IN ($ItemID)

SELECT 
  StockID,
  ItemID,
  MAX([Date]) AS MAXDate
  INTO #CTELastRecive
  FROM ItemCardex WITH (NOLOCK)
  WHERE DocumentTypeID = 296
  AND ItemID  IN ($ItemID)
  AND StockID IN ($StockID)
  GROUP BY StockID,ItemID


/*************************************اطلاعات فروش فروشگاه******************************************/ 
SELECT 
  ItemID , StockID, 
  SUM(NetQuantity) AS NetQuantity,SUM(NetAmount) AS NetAmount,
  SUM(NetDiscount) AS NetDiscount,SUM(NetTax) AS NetTax 
INTO #Sale
FROM (
SELECT SILI.ItemID,
           StockID,
           CASE
               WHEN SILI.TypeID = 302 THEN
                   Quantity + BonusQuantity
               ELSE
                   CASE
                       WHEN SILI.TypeID = 303 THEN
        (Quantity + BonusQuantity) * -1
                       ELSE
                           0
                   END
           END AS NetQuantity,
           CASE
               WHEN SILI.TypeID = 302 THEN
      (Quantity + BonusQuantity) * PriceAmount
               ELSE
                   CASE
                       WHEN SILI.TypeID = 303 THEN
        ((Quantity + BonusQuantity) * PriceAmount) * -1
                       ELSE
                           0
                   END
           END AS NetAmount,
           CASE
               WHEN SILI.TypeID = 302 THEN
                   ISNULL(SILI.DiscountAmount, 0) + ISNULL(SILI.InvoiceDiscount, 0) + ISNULL(SILI.ManualDiscount, 0)
               ELSE
                   CASE
                       WHEN SILI.TypeID = 303 THEN
       (ISNULL(SILI.DiscountAmount, 0) + ISNULL(SILI.InvoiceDiscount, 0) + ISNULL(SILI.ManualDiscount, 0)) * 1
                       ELSE
                           0
                   END
           END AS NetDiscount,
           CASE
               WHEN SILI.TypeID = 302 THEN
                   ISNULL(SILI.TaxAmount, 0) + ISNULL(SILI.TollAmount, 0)
               ELSE
                   CASE
                       WHEN SILI.TypeID = 303 THEN
           (ISNULL(SILI.TaxAmount, 0) + ISNULL(SILI.TollAmount, 0)) * -1
                       ELSE
                           0
                   END
           END AS NetTax
FROM dbo.SaleInvoice SI WITH (NOLOCK)
INNER JOIN dbo.SaleInvoiceLineItem SILI WITH (NOLOCK)
  ON SI.BookerStoreID = SILI.BookerStoreID
  AND SI.BookerWorkstationID = SILI.BookerWorkstationID
  AND SI.InvoiceID = SILI.InvoiceID
WHERE SILI.TypeID IN ( 302, 303 )
  AND SI.InvoiceTypeID IS NULL
  AND SI.BusinessDate BETWEEN @StartDate AND @EndDate
  AND SILI.ItemID  IN ($ItemID)
  AND SILI.StockID IN ($StockID)
) AS Sale
GROUP BY ItemID,StockID
--------------------------------------*--------------------------*------------------------------------
; WITH CTE_LAST30DAY AS ( SELECT ItemID,
           StockID,
           SUM(NetQuantity) AS monthlyAvg
    FROM
    (
        SELECT ItemID,
               CASE
                   WHEN (TypeID) = 302 THEN
                       Quantity + BonusQuantity
                   ELSE
               (Quantity + BonusQuantity) * -1
               END AS NetQuantity,
               StockID,
               7 AS DayCount
        FROM SaleInvoice S WITH (NOLOCK)
            INNER JOIN SaleInvoiceLineItem SI WITH (NOLOCK)
                ON S.InvoiceID = SI.InvoiceID
        WHERE InvoiceTypeID IS NULL
              AND S.BusinessDate
              BETWEEN  DATEADD(DAY, -30, CAST(GETDATE() AS date) ) AND  CAST(GETDATE() AS date )
     AND SI.StockID IN ($StockID)
    ) AS O
    GROUP BY ItemID,
             StockID)

SELECT * INTO #CTE_LAST30DAY FROM CTE_LAST30DAY
/******************************************************************************************************/
; WITH CTE_LAST7DAY AS ( SELECT ItemID,
           StockID,
           SUM(NetQuantity) AS sevenAvg
    FROM
    (
        SELECT ItemID,
               CASE
                   WHEN (TypeID) = 302 THEN
                       Quantity + BonusQuantity
                   ELSE
               (Quantity + BonusQuantity) * -1
               END AS NetQuantity,
               StockID,
               7 AS DayCount
        FROM SaleInvoice S WITH (NOLOCK)
            INNER JOIN SaleInvoiceLineItem SI WITH (NOLOCK)
                ON S.InvoiceID = SI.InvoiceID
        WHERE InvoiceTypeID IS NULL
              AND S.BusinessDate
              BETWEEN  DATEADD(DAY, -7, CAST(GETDATE() AS date) ) AND  CAST(GETDATE() AS date )
     AND SI.StockID IN ($StockID)
    ) AS O
    GROUP BY ItemID,
             StockID)

SELECT * INTO #CTE_LAST7DAY FROM CTE_LAST7DAY
/*******************************************************************************************************/
; WITH CTE_LAST15DAY AS ( SELECT ItemID,
           StockID,
           SUM(NetQuantity) AS weekavg
    FROM
    (
        SELECT ItemID,
               CASE
                   WHEN (TypeID) = 302 THEN
                       Quantity + BonusQuantity
                   ELSE
               (Quantity + BonusQuantity) * -1
               END AS NetQuantity,
               StockID,
               7 AS DayCount
        FROM SaleInvoice S WITH (NOLOCK)
            INNER JOIN SaleInvoiceLineItem SI WITH (NOLOCK)
                ON S.InvoiceID = SI.InvoiceID
        WHERE InvoiceTypeID IS NULL
              AND S.BusinessDate
              BETWEEN  DATEADD(DAY, -15, CAST(GETDATE() AS date) ) AND  CAST(GETDATE() AS date )
     AND SI.StockID IN ($StockID)
    ) AS O
    GROUP BY ItemID,
             StockID)

SELECT * INTO #CTE_LAST15DAY FROM CTE_LAST15DAY
/*****************************************************************************************************/
; WITH CTE_LAST3DAY AS ( SELECT ItemID,
           StockID,
           SUM(NetQuantity) AS threeAvg
    FROM
    (
        SELECT ItemID,
               CASE
                   WHEN (TypeID) = 302 THEN
                       Quantity + BonusQuantity
                   ELSE
               (Quantity + BonusQuantity) * -1
               END AS NetQuantity,
               StockID,
               7 AS DayCount
        FROM SaleInvoice S WITH (NOLOCK)
            INNER JOIN SaleInvoiceLineItem SI WITH (NOLOCK)
                ON S.InvoiceID = SI.InvoiceID
        WHERE InvoiceTypeID IS NULL
              AND S.BusinessDate
              BETWEEN  DATEADD(DAY, -3, CAST(GETDATE() AS date) ) AND  CAST(GETDATE() AS date )
     AND SI.StockID IN ($StockID)
    ) AS O
    GROUP BY ItemID,
             StockID)

SELECT * INTO #CTE_LAST3DAY FROM CTE_LAST3DAY
/*****************************************************************************************************/
;WITH CTE_SaleQuantity AS (
SELECT StockID,ItemID,SUM(Qty) AS SaleQuantity   FROM (
SELECT StockID,ItemID,CASE
               WHEN TypeID = 302 THEN
                   Quantity + BonusQuantity
               ELSE
                   CASE
                       WHEN TypeID = 303 THEN
        (Quantity + BonusQuantity) * -1
                       ELSE
                           0
                   END
           END AS Qty FROM dbo.SaleInvoice SI
INNER JOIN dbo.SaleInvoiceLineItem SILI WITH (NOLOCK)
  ON SI.BookerStoreID = SILI.BookerStoreID
  AND SI.BookerWorkstationID = SILI.BookerWorkstationID
  AND SI.InvoiceID = SILI.InvoiceID
WHERE SILI.StockID IN ($StockID)
AND  SI.BusinessDate BETWEEN @StartDate AND @EndDate
) AS  T
GROUP BY StockID,ItemID
)

SELECT * INTO #CTE_SaleQuantity FROM CTE_SaleQuantity
/*****************************************************************************************************/ 
 IF DATEDIFF(DAY,@StartDate,@EndDate) > 187
 BEGIN
  SELECT N'بازه تاریخی بیش از 6 ماه است '
 END
 ELSE 
 BEGIN




/*****************************************************************************************************/ 
SELECT Status
,StockName
,Name
,[ نام تامین کننده]
,PriceAmount
,ConsumerPrice
,Barcode
,AllBarcode
,Brand
,[تعداد فروش رفته]
,[جمع خالص فروش]
,[تخفیف فروش منهای برگشت از فروش]
,[مالیات و عوارض فروش منهای برگشت از فروش]
,[فروش 30 روز]
,[فروش 15 روز گذشته]
,[فروش 7 روز گذشته]
,[فروش 3 روز گذشته]
,[موجودی  کالا]
,[آخرین روز دریافت]
--,CASE WHEN CAST([اتمام بار] AS nvarchar(100))< 0 THEN N'اتمام موجودی' ELSE CAST([اتمام بار] AS nvarchar(100)) END AS [اتمام بار]
/*,replace(CASE WHEN 
[اتمام بار] < 0 THEN 999889099.99 ELSE [اتمام بار]  END ,999889099.99,N'اتمام موجودی') AS [اتمام بار]*/
,[اتمام بار]
,[بازه]
,[تعداد آخرین دریافت تامین کننده]
,[ساب فمیلی]
,[فمیلی] 
,[سکشن]
,[دپارتمان]
,[آخرین دریافت از انبار]
,[آخرین قیمت مصوب]  
,[قیمت مصوب قبلی]
,[تاریخ مصوب]
,[ساعت مصوب]
,[شماره سند مصوبه]
,T.BoxCount [تعداد در کارتن]
,[گروه مالیاتی]
 ,case when [موجودی  کالا] <=0 and [تعداد فروش رفته] > 0 then [تعداد فروش رفته] * -1 else  0  end  [کسری بار] 
, FORMAT([ آخرین فی خرید ( با مالیات)],'#.##') [ آخرین فی خرید ( با مالیات)]
, [تفاضل موجودی و آخرین تعداد خرید]
,[حاشیه سود]
,[تعداد فروش از آخرین دریافت]
 , [فروش در بازه گزارش]
 ,
 CASE WHEN [تعداد فروش از آخرین دریافت] > 0 AND [موجودی  کالا] > 0 THEN 
 [موجودی  کالا] * [تعداد آخرین دریافت تامین کننده] / [تعداد فروش از آخرین دریافت]
 ELSE 0 end
 [گردش دوره]
  FROM (
SELECT   
ISS.Status  ,
   stock.Name AS StockName,
  SI.ItemID,
  II.Name,
  --ISNULL(SUP.Name,SU.Name) as  [ نام تامین کننده] ,
----  CASE WHEN MLastCostStore.day > 1 THEN  ISNULL(ISNULL(SUP.Name,SU.Name),N'ورود از انبار مرکزی') ELSE ISNULL(SUP.Name,SU.Name) END
 CASE WHEN (ISNULL(MLastCostStore.BuyCount , 0) < 0 OR ISNULL(MLastCostStoreEnter.UnitCount , 0) > 0) AND MLastCostStore.Date < MLastCostStoreEnter.Date 
  THEN
 N'ورود از انبار مرکزی' ELSE ISNULL(SUP.Name,SU.Name) END
AS [ نام تامین کننده],
  PAS.PriceAmount,
  PAS.ConsumerPrice,
  II.Barcode,
  II.AllBarcode,
  (NetQuantity) AS [تعداد فروش رفته],
  (NetAmount)   AS [جمع خالص فروش],
  (NetDiscount) AS [تخفیف فروش منهای برگشت از فروش],
  (NetTax) AS [مالیات و عوارض فروش منهای برگشت از فروش],
  LAST30DAY.monthlyAvg [فروش 30 روز],
  ISS.QTY [موجودی  کالا],
    
  DATEDIFF(DAY, LastReceived.MaxDate, GETDATE()) AS [آخرین روز دریافت],
  /*FORMAT(CASE WHEN NetQuantity <> 0 AND ISS.QTY <> 0 AND  DATEDIFF(DAY, CAST(@StartDate AS DATE),CAST(@EndDate AS DATE)) > 0  THEN 
  ISS.QTY/(NetQuantity/DATEDIFF(DAY, CAST(@StartDate AS DATE),CAST(@EndDate AS DATE)))  ELSE 0 END,'##') AS [اتمام بار],*/
   FORMAT(CASE WHEN LAST30DAY.monthlyAvg <> 0 AND ISS.QTY <> 0 AND  DATEDIFF(DAY, CAST(@StartDate AS DATE),CAST(@EndDate AS DATE)) > 0  THEN 
  ISS.QTY/(LAST30DAY.monthlyAvg/30) ELSE 0 END,'##') AS [اتمام بار],

  /* ROUND(LAST30DAY.monthlyAvg / 21 ,0) AS  [اتمام بار] , */
  DATEDIFF(DAY, CAST(@StartDate AS DATE),CAST(@EndDate AS DATE)) AS [بازه],
  --ROUND(LastBuy.LastPurchaseQuantity , 0) [تعداد آخرین دریافت]
  ISNULL(MLastCostStore.BuyCount , 0) [تعداد آخرین دریافت تامین کننده]
  ,IDA.DepartmentID,
  DSF.Name [ساب فمیلی],DF.Name [فمیلی] ,DSection.Name [سکشن],DD.Name  [دپارتمان],TAXD.Name [گروه مالیاتی],
  MLastCostStoreEnter.day as  [آخرین دریافت از انبار],
  ISNULL(SC.Amount,0) [آخرین قیمت مصوب]  ,
ISNULL(SC.LastCost,0) [قیمت مصوب قبلی] ,
SC.Date [تاریخ مصوب],
CAST(CAST(SC.EffectiveDate AS TIME ) AS NVARCHAR(8)) [ساعت مصوب],
ContractNumber [شماره سند مصوبه]
,CAST(IC.BoxCount AS decimal(18,0)) AS BoxCount
,II.Brand
 ,ISNULL(iss.QTY , 0 ) - ISNULL(MLastCostStore.BuyCount , 0) [تفاضل موجودی و آخرین تعداد خرید]
 , ISNULL(iss.QTY , 0 )  AS A ,ISNULL(MLastCostStore.BuyCount , 0)  AS B
,MLastCostStore.MLastCostStore [ آخرین فی خرید ( با مالیات)]
,((ISNULL(PAS.PriceAmount,ISNULL(II.CurrentPrice,0))-
  (ISNULL(MLastCostStore.MLastCostStore, 0)))/
  ISNULL(CASE WHEN ISNULL(PAS.PriceAmount,ISNULL(II.CurrentPrice,0))=0 THEN 1 ELSE PAS.PriceAmount END ,1))*100 AS [حاشیه سود]
  ,LAST3dAY.threeAvg [فروش 3 روز گذشته]
  ,LAST7dAY.sevenAvg [فروش 7 روز گذشته]
  ,LAST15dAY.weekavg [فروش 15 روز گذشته]
  ,(ISNULL(LastBuy.LastPurchaseQuantity , 0) - ISNULL(SaleQuantity.SaleQuantity,0)) AS [تعداد فروش از آخرین دریافت]
   ,SaleQuantity.SaleQuantity AS [فروش در بازه گزارش]
   /*,
   CASE WHEN iss.QTY >= 0 AND  then
   ISNULL(iss.QTY , 0 ) * DATEDIFF(DAY, LastReceived.MaxDate, GETDATE()) / (ISNULL(LastBuy.LastPurchaseQuantity , 0) - ISNULL(SaleQuantity.SaleQuantity,0))
   ELSE 0 END AS [گردش دوره]
   */
FROM #CTEISS ISS
INNER JOIN  ItemForMobileView II
  ON ISS.ItemID = II.ItemID
  AND II.LanguageID = 314
LEFT JOIN stock stock
 ON stock.StockID  = iss.StockID
INNER JOIN  #Sale SI 
  ON ISS.ItemID = SI.ItemID
  AND ISS.StockID = SI.StockID

LEFT JOIN #CTELastRecive AS LastReceived
  ON LastReceived.ItemID = ISS.ItemID
  AND LastReceived.StockID = ISS.StockID

LEFT JOIN #ctePurchaseQuantity LastBuy
  ON LastBUY.ItemID=ISS.ItemID 
  AND LastBUY.StockID=ISS.StockID 

  /*     گروه بندی کالاهای دپارتمان -سکشن-فمیلی  و ساب فمیلی      */
INNER JOIN ItemDepartmentAssignment IDA
  ON IDA.ItemID=ISS.ItemID 
  AND IDA.TypeID=1

INNER JOIN Department DSF
 ON DSF.DepartmentID=IDA.DepartmentID
LEFT JOIN Department  DF 
 ON DSF.ParentID=DF.DepartmentID
LEFT JOIN Department  DSection 
 ON DF.ParentID=DSection.DepartmentID
LEFT JOIN Department DD
 ON DSection.ParentID=DD.DepartmentID

 /**/
 LEFT JOIN #MLastCostStoreEnter MLastCostStoreEnter WITH(NOLOCK) 
ON MLastCostStoreEnter.ItemID = ISS.ItemID
AND MLastCostStoreEnter.StockID = ISS.StockID

LEFT JOIN #MLastCostStore MLastCostStore  WITH(NOLOCK) 
 ON MLastCostStore.ItemID=II.ItemID
 AND MLastCostStore.StockID=ISS.StockID
 /**/
LEFT JOIN #PriceAmountStore PAS  
 ON PAS.StockID=ISS.StockID
 AND PAS.ItemID=ISS.ItemID

LEFT JOIN #Contract SC
 ON SC.ItemID=ISS.ItemID
LEFT JOIN #SUP SUP
 ON SUP.SupplierID=LastBuy.SupplierID
  LEFT JOIN ItemCustomField IC
 ON II.ItemID = IC.ItemID
  LEFT JOIN ItemDepartmentAssignment TAXIDA
  ON TAXIDA.ItemID=ISS.ItemID 
  AND  TAXIDA.TypeID = 3
  LEFT JOIN Department TAXD
  ON TAXIDA.DepartmentID = TAXD.DepartmentID
LEFT JOIN #CTE_Supplier CTE_Supplier
 ON CTE_Supplier.ItemID = ISS.ItemID
LEFT JOIN #SUP SU
 ON SU.SupplierID =CTE_Supplier.SupplierID
LEFT JOIN #CTE_LAST30DAY LAST30DAY
 ON LAST30DAY.ItemID = II.ItemID
 AND LAST30DAY.StockID = ISS.StockID
 /**********/
  LEFT JOIN #CTE_LAST3DAY LAST3dAY
 ON LAST3dAY.ItemID = II.ItemID
 AND LAST3dAY.StockID = ISS.StockID
 LEFT JOIN #CTE_LAST7DAY LAST7dAY
 ON LAST7dAY.ItemID = II.ItemID
 AND LAST7dAY.StockID = ISS.StockID
 LEFT JOIN #CTE_LAST15DAY LAST15dAY
 ON LAST15dAY.ItemID = II.ItemID
 AND LAST15dAY.StockID = ISS.StockID
 /*********************/
 LEFT JOIN #PriceAmountStore PriceAmountStore
 ON PriceAmountStore.ItemID = ISS.ItemID
 AND PriceAmountStore.StockID = ISS.StockID

 /******************************************/
 LEFT JOIN #CTE_SaleQuantity SaleQuantity
  ON SaleQuantity.ItemID = ISS.ItemID
 AND SaleQuantity.StockID = ISS.StockID
 /******************************************/
   WHERE ISS.ItemID IN ($ItemID)
  AND ISS.StockID  IN ($StockID)
  AND  ISNULL(IDA.DepartmentID,'3E240A6D-86BE-4111-83B9-DE821FBB0F59') in (SELECT DepartmentID FROM Department)


 UNION 
  
SELECT 
ISS.Status  ,
   stock.Name AS StockName,
  II.ItemID,
  II.Name,
  --ISNULL(SUP.Name,SU.Name) ,
--  CASE WHEN MLastCostStore.day > 1 THEN  ISNULL(ISNULL(SUP.Name,SU.Name),N'ورود از انبار مرکزی') ELSE ISNULL(SUP.Name,SU.Name) END , 
 CASE WHEN (ISNULL(MLastCostStore.BuyCount , 0) < 0 OR ISNULL(MLastCostStoreEnter.UnitCount , 0) > 0) AND MLastCostStore.Date < MLastCostStoreEnter.Date 
  THEN
 N'ورود از انبار مرکزی' ELSE ISNULL(SUP.Name,SU.Name) END ,
  PAS.PriceAmount,
  PAS.ConsumerPrice,
  II.Barcode,
  II.AllBarcode,
  0 AS [تعداد فروش رفته],
  0  AS [جمع خالص فروش],
  0 AS [تخفیف فروش منهای برگشت از فروش],
  0 AS [مالیات و عوارض فروش منهای برگشت از فروش],
  0,
  (ISS.QTY) [موجودی  کالا],
  DATEDIFF(DAY, LastReceived.MaxDate, GETDATE()) AS [آخرین روز دریافت],
 
  0 AS [اتمام بار],
  DATEDIFF(DAY, CAST(@StartDate AS DATE) ,CAST(@EndDate AS DATE)) AS [بازه]
  ,ISNULL(MLastCostStore.BuyCount , 0)
  ,IDA.DepartmentID ,
  IDA.Name [ساب فمیلی],DF.Name [فمیلی] ,DSection.Name [سکشن],DD.Name  [دپارتمان],TAXD.Name [گروه مالیاتی],
  MLastCostStoreEnter.day as  [آخرین دریافت از انبار],
    ISNULL(SC.Amount,0) [آخرین قیمت مصوب]  ,
    ISNULL(SC.LastCost,0) [قیمت مصوب قبلی] ,
    SC.Date [تاریخ مصوب],
    CAST(CAST(SC.EffectiveDate AS TIME ) AS NVARCHAR(8)) [ساعت مصوب],
 ContractNumber [شماره سند مصوبه]
 ,CAST(IC.BoxCount AS decimal(18,0)) AS BoxCount
 ,II.Brand
,ISNULL(iss.QTY , 0 ) - ISNULL(MLastCostStore.BuyCount , 0) [تفاضل موجودی و آخرین تعداد خرید]
, ISNULL(iss.QTY , 0 )  AS A ,ISNULL(MLastCostStore.BuyCount , 0)  AS B
,MLastCostStore.MLastCostStore [ آخرین فی خرید ( با مالیات)]
,((ISNULL(PAS.PriceAmount,ISNULL(II.CurrentPrice,0))-
  (ISNULL(MLastCostStore.MLastCostStore, 0)))/
  ISNULL(CASE WHEN ISNULL(PAS.PriceAmount,ISNULL(II.CurrentPrice,0))=0 THEN 1 ELSE PAS.PriceAmount END ,1))*100 AS [حاشیه سود]
  ,LAST3dAY.threeAvg [فروش 3 روز گذشته]
   ,LAST7dAY.sevenAvg [فروش 7 روز گذشته]
  ,LAST15dAY.weekavg [فروش 15 روز گذشته]
  ,(ISNULL(LastBuy.LastPurchaseQuantity , 0) - ISNULL(SaleQuantity.SaleQuantity,0)) AS [تعداد فروش از آخرین دریافت]
  ,SaleQuantity.SaleQuantity AS [فروش در بازه گزارش]
  /*,
   CASE WHEN iss.QTY >= 0 AND  then
   ISNULL(iss.QTY , 0 ) * DATEDIFF(DAY, LastReceived.MaxDate, GETDATE()) / (ISNULL(LastBuy.LastPurchaseQuantity , 0) - ISNULL(SaleQuantity.SaleQuantity,0))
   ELSE 0 END AS [گردش دوره]
   */
FROM #CTEISS ISS
INNER JOIN  ItemForMobileView II
  ON ISS.ItemID = II.ItemID
  AND II.LanguageID = 314
  
LEFT JOIN stock stock
 ON stock.StockID  = iss.StockID
  /**/
 LEFT JOIN #MLastCostStoreEnter MLastCostStoreEnter WITH(NOLOCK) 
ON MLastCostStoreEnter.ItemID = ISS.ItemID
AND MLastCostStoreEnter.StockID = ISS.StockID

LEFT JOIN #MLastCostStore MLastCostStore  WITH(NOLOCK) 
 ON MLastCostStore.ItemID=II.ItemID
 AND MLastCostStore.StockID=ISS.StockID
 /**/
LEFT  JOIN #PriceAmountStore PAS   
  ON  PAS.StockID=ISS.StockID
  AND PAS.ItemID=ISS.ItemID
LEFT JOIN #CTELastRecive AS LastReceived
  ON LastReceived.ItemID = ISS.ItemID
  AND LastReceived.StockID = ISS.StockID
LEFT JOIN #ctePurchaseQuantity LastBuy
  ON LastBuy.ItemID=ISS.ItemID 
  AND LastBuy.StockID=ISS.StockID
LEFT JOIN #SUP SUP 
 ON SUP.SupplierID=LastBuy.SupplierID

  /*     گروه بندی کالاهای دپارتمان -سکشن-فمیلی  و ساب فمیلی      */
INNER JOIN ItemDepartmentAssignmentView IDA
  ON IDA.ItemID=ISS.ItemID 
  AND  IDA.TypeID = 1
INNER JOIN Department DSF
  ON DSF.DepartmentID=IDA.DepartmentID
LEFT JOIN Department  DF 
  ON DSF.ParentID=DF.DepartmentID
LEFT JOIN Department  DSection 
  ON DF.ParentID=DSection.DepartmentID
LEFT JOIN Department DD
  ON DSection.ParentID=DD.DepartmentID
LEFT JOIN #Contract SC
  ON SC.ItemID=II.ItemID

LEFT JOIN ItemDepartmentAssignment TAXIDA
  ON TAXIDA.ItemID=ISS.ItemID 
  AND  TAXIDA.TypeID = 3
  LEFT JOIN Department TAXD
  ON TAXIDA.DepartmentID = TAXD.DepartmentID
LEFT JOIN #CTE_Supplier CTE_Supplier
 ON CTE_Supplier.ItemID = ISS.ItemID
LEFT JOIN #SUP SU
 ON SU.SupplierID =CTE_Supplier.SupplierID
LEFT JOIN #CTE_LAST30DAY LAST30DAY
 ON LAST30DAY.ItemID = II.ItemID
 AND LAST30DAY.StockID = ISS.StockID
  /**********/
   LEFT JOIN #CTE_LAST3DAY LAST3dAY
 ON LAST3dAY.ItemID = II.ItemID
 AND LAST3dAY.StockID = ISS.StockID
 LEFT JOIN #CTE_LAST7DAY LAST7dAY
 ON LAST7dAY.ItemID = II.ItemID
 AND LAST7dAY.StockID = ISS.StockID
 LEFT JOIN #CTE_LAST15DAY LAST15dAY
 ON LAST15dAY.ItemID = II.ItemID
 AND LAST15dAY.StockID = ISS.StockID
 /*********************/
 LEFT JOIN ItemCustomField IC
 ON II.ItemID = IC.ItemID
 LEFT JOIN #PriceAmountStore PriceAmountStore
 ON PriceAmountStore.ItemID = ISS.ItemID
 AND PriceAmountStore.StockID = ISS.StockID
  /******************************************/
 LEFT JOIN #CTE_SaleQuantity SaleQuantity
  ON SaleQuantity.ItemID = ISS.ItemID
 AND SaleQuantity.StockID = ISS.StockID
 /******************************************/
WHERE NOT EXISTS (SELECT * FROM  #Sale S WHERE  S.ItemID=ISS.ItemID AND S.StockID=ISS.StockID)

 AND II.ItemID   IN ($ItemID)
 AND ISS.StockID IN ($StockID)
 AND  ISNULL(IDA.DepartmentID,'3E240A6D-86BE-4111-83B9-DE821FBB0F59') in (SELECT DepartmentID FROM Department)
   ) AS T
 
END


DROP TABLE #PriceAmountStore,#SUP,#Contract,#ctePurchaseQuantity,#CTEISS,#CTELastRecive,#Sale,#CTE_Supplier,#CTE_LAST30DAY,#MLastCostStore,#MLastCostStoreEnter,#CTE_LAST15DAY,#CTE_LAST7DAY,#CTE_LAST3DAY
,#CTE_SaleQuantity

