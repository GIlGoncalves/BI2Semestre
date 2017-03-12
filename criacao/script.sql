use LEI;

INSERT into LEI.dbo.CartaoCredito(Tipo)(
   select CreditCard.CardType
   from AdventureWorks2014.Sales.CreditCard
);

INSERT into LEI.dbo.Pais(id,Nome,Grupo,Codigo)(
   select TerritoryID, Name,[AdventureWorks2014].[Sales].[SalesTerritory].[Group],CountryRegionCode
   from AdventureWorks2014.Sales.SalesTerritory
);
USE [LEI];

INSERT INTO [LEI].dbo.[DataOrdem]
	SELECT DISTINCT
		OrderDate AS Data,
		DATEPART(yyyy,OrderDate) AS Ano,
		DATEPART(mm,OrderDate) AS Mes,
		DATEPART(ww,OrderDate) AS Semana,
		DATEPART(dd,OrderDate) AS Dia
		FROM [AdventureWorks2014].[Sales].[SalesOrderHeader]
		ORDER BY
			OrderDate,
			DATEPART(yyyy,OrderDate),
			DATEPART(mm,OrderDate),
			DATEPART(ww,OrderDate),
			DATEPART(dd,OrderDate);

INSERT INTO LEI.dbo.Cliente(id,NomeCompleto,Email) (
	select distinct  [AdventureWorks2014].Person.Person.BusinessEntityID,CONCAT(FirstName,',',MiddleName,',',LastName),
			EA.EmailAddress
	from [AdventureWorks2014].[Sales].SalesOrderHeader inner join [AdventureWorks2014].[Sales].CreditCard
	     ON [AdventureWorks2014].[Sales].SalesOrderHeader.CreditCardID=[AdventureWorks2014].[Sales].CreditCard.CreditCardID
		 inner join [AdventureWorks2014].[Sales].PersonCreditCard on 
		    [AdventureWorks2014].[Sales].PersonCreditCard.CreditCardID=[AdventureWorks2014].[Sales].CreditCard.CreditCardID
		 inner join  [AdventureWorks2014].Person.Person on
		 [AdventureWorks2014].[Sales].PersonCreditCard.BusinessEntityID = [AdventureWorks2014].Person.Person.BusinessEntityID
		 inner join   [AdventureWorks2014].Person.EmailAddress as EA on EA.BusinessEntityID =[AdventureWorks2014].Person.Person.BusinessEntityID
);



insert into LEI.dbo.FactTable(IdCliente,IdDataOrdem,IdPais,IdCartao,Quantidade,
UnitPrice,UnitPriceDiscount,LineTotal)(

select LEIC.id,LEID.id,LEIP.id,LEICC.id,SOD.OrderQty,SOD.UnitPrice,SOD.UnitPriceDiscount,SOD.LineTotal 
from AdventureWorks2014.Sales.SalesOrderHeader as SH INNER JOIN LEI.dbo.CartaoCredito as LEICC on
 SH.CreditCardID = LEICC.id inner join LEI.dbo.Pais as LEIP on 
 LEIP.id = sh.TerritoryID inner join LEI.dbo.DataOrdem as LEID ON LEID.Data = SH.OrderDate
 INNER JOIN AdventureWorks2014.Sales.PersonCreditCard AS APC on
   APC.CreditCardID=SH.CreditCardID INNER JOIN LEI.dbo.Cliente as LEIC ON LEIC.id =APC.BusinessEntityID
   inner join AdventureWorks2014.Sales.SalesOrderDetail AS SOD 
   ON SOD.SalesOrderID= SH.SalesOrderID 
  
);
